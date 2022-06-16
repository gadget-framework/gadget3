library(magrittr)
library(unittest)

library(gadget3)

# Mini action to run suitability function for stocks and report output
suit_report_action <- function (pred_stock, stock, suit_f, run_at = 99) {
    out <- new.env(parent = emptyenv())
    suit_fn_name <- as.character(sys.call()[[4]][[1]])
    suit_var_name <- paste0('stock__', suit_fn_name)
    assign(suit_var_name, gadget3:::stock_instance(stock))
    
    out[[gadget3:::step_id(run_at, suit_fn_name)]] <- gadget3:::g3_step(gadget3:::f_substitute(~{
        debug_label("Testing ", suit_fn_name)
        stock_iterate(stock, stock_intersect(pred_stock, {
            stock_ss(suit_var) <- (suit_f)
        }))
        stock_with(stock, g3_report(suit_var))
    }, list(
        suit_fn_name = suit_fn_name,
        suit_var = as.symbol(suit_var_name),
        suit_f = suit_f)))
    return(as.list(out))
}


fleet_stock <- g3_fleet('fleet') %>% g3s_livesonareas(1:3)
pred_stock <- g3_stock('pred', c(10, 20, 30, 40, 50, 75, 100)) %>%
    g3s_livesonareas(1:3) %>%
    g3s_age(1, 3)
input_stock <- g3_stock('input_stock', c(10, 20, 30, 40, 50, 75, 100)) %>%
    g3s_livesonareas(1:3) %>%
    g3s_age(1, 3)
actions <- list(
    g3a_time(2000, 2000, steps = c(3, 3, 5, 1), project_years = 0),
    g3a_initialconditions_normalparam(
        input_stock,
        factor_f = ~ 1 + age * g3_param('factor.age') + area * g3_param('factor.area'),
        mean_f = ~(age * 10) + 30,
        stddev_f = ~25,
        alpha_f = ~0.1,
        beta_f = ~0.2),
    g3a_initialconditions_normalparam(
        pred_stock,
        factor_f = ~ 1 + age * g3_param('factor.age') + area * g3_param('factor.area'),
        mean_f = ~(age * 10) + 30,
        stddev_f = ~25,
        alpha_f = ~0.1,
        beta_f = ~0.2),
    suit_report_action(fleet_stock, input_stock, g3_suitability_exponentiall50(
        ~g3_param("exponentiall50.alpha"),
        ~g3_param("exponentiall50.l50"))),
    suit_report_action(pred_stock, input_stock, g3_suitability_andersen(
        p0 = ~g3_param("andersen_p0"),
        p1 = ~g3_param("andersen_p1"),
        p2 = ~g3_param("andersen_p2"),
        p4 = ~g3_param("andersen_p4"))),
    suit_report_action(fleet_stock, input_stock, g3_suitability_gamma(
        ~g3_param("gamma_alpha"),
        ~g3_param("gamma_beta"),
        ~g3_param("gamma_gamma"))),
    suit_report_action(pred_stock, input_stock, g3_suitability_exponential(
        ~g3_param("exponential_alpha"),
        ~g3_param("exponential_beta"),
        ~g3_param("exponential_gamma"),
        ~g3_param("exponential_delta"))),
    suit_report_action(fleet_stock, input_stock, g3_suitability_straightline(
        ~g3_param("straightline_alpha"),
        ~g3_param("straightline_beta"))),
    suit_report_action(fleet_stock, input_stock, g3_suitability_constant(
        ~g3_param("constant_alpha"))),
    suit_report_action(pred_stock, input_stock, g3_suitability_richards(
        ~g3_param("richards_p0"),
        ~g3_param("richards_p1"),
        ~g3_param("richards_p2"),
        ~g3_param("richards_p3"),
        ~g3_param("richards_p4"))),
    list('999' = ~{
        g3_report(input_stock__num)
        g3_report(input_stock__wgt)
        g3_report(input_stock__midlen)
        nll <- g3_param('x')
    }))
default_params <- list(
    factor.age = 0,
    factor.area = 0,
    exponentiall50.alpha = 0,
    exponentiall50.l50 = 0,
    andersen_p0 = 0,
    andersen_p1 = 0,
    andersen_p2 = 0,
    andersen_p4 = 0,
    gamma_alpha = 0,
    gamma_beta = 0,
    gamma_gamma = 0,
    exponential_alpha = 0,
    exponential_beta = 0,
    exponential_gamma = 0,
    exponential_delta = 0,
    straightline_alpha = 0,
    straightline_beta = 0,
    constant_alpha = 0,
    richards_p0 = 0,
    richards_p1 = 0,
    richards_p2 = 0,
    richards_p3 = 0,
    richards_p4 = 0,
    x=1.0)

# Compile model
model_fn <- g3_to_r(actions, trace = TRUE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, default_params, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

ok_group("exponentiall50", {
    params <- c(list(
        factor.age = 1,  # Abundance increases with age
        exponentiall50.alpha = 0.1,
        exponentiall50.l50 = 50,
        x=1.0), default_params)
    params <- params[!duplicated(names(params))]
    result <- model_fn(params)
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)
    #print(round(r$input_stock__g3_suitability_exponentiall50, 3))

    ok(all(r$input_stock__g3_suitability_exponentiall50[,,'age1'] == r$input_stock__g3_suitability_exponentiall50[,,'age2']), 'all of age1 = age2')
    ok(all(r$input_stock__g3_suitability_exponentiall50[,,'age2'] == r$input_stock__g3_suitability_exponentiall50[,,'age3']), 'all of age2 = age3')

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})

ok_group("compare_all", {
    params <- lapply(default_params, function (x) runif(1, min=0.1, max=0.9))
    params <- params[!duplicated(names(params))]
    result <- model_fn(params)
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)

    # NB: Don't bother checking the values themselves, but make sure the end result matches in TMB

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})
