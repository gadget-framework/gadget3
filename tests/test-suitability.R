library(magrittr)
library(unittest)

library(gadget3)

# Mini action to run suitability function for stocks and report output
suit_report_action <- function (pred_stock, stock, suit_f, run_at = 99) {
    out <- new.env(parent = emptyenv())
    suit_fn_name <- as.character(sys.call()[[4]][[1]])
    suit_var_name <- paste0('stock__', suit_fn_name)
    assign(suit_var_name, g3_stock_instance(stock))
    
    out[[gadget3:::step_id(run_at, suit_fn_name)]] <- gadget3:::g3_step(gadget3:::f_substitute(~{
        debug_label("Testing ", suit_fn_name)
        stock_iterate(stock, stock_intersect(pred_stock, {
            stock_ss(suit_var) <- (suit_f)
        }))
        stock_with(stock, REPORT(suit_var))
    }, list(
        suit_fn_name = suit_fn_name,
        suit_var = as.symbol(suit_var_name),
        suit_f = suit_f)))
    return(as.list(out))
}

ok_group("g3_suitability_andersen", {
    nondiff_andersen <- function (p0, p1, p2, p3 = p4, p4, p5, stock__midlen) {
        p0 + p2 * exp(-(log(p5/stock__midlen) - p1)^2 / ifelse(log(p5/stock__midlen) <= p1, p4, p3))
    }
    g3_andersen <- function (p0, p1, p2, p3 = p4, p4, p5, stock__midlen) {
        g3_eval(g3_suitability_andersen(p0, p1, p2, p3, p4, p5), stock__midlen = stock__midlen)
    }

    params <- list(p0 = 0, p1 = 0.659, p2 = 1, p3 = 0.15, p4 = 9942, p5 = 120, stock__midlen = 1:120)
    ok(ut_cmp_equal(
        do.call(g3_andersen, params),
        do.call(nondiff_andersen, params),
        tolerance = 1e-6), paste0("g3_suitability_andersen matches ", deparse1(params)))
})

ok_group("g3_suitability_andersenfleet", {
    nondiff_andersenfleet <- function (
            lg,
            param.fish.andersen.p0 = 0,
            param.fish.andersen.p1 = log(2),
            param.fish.andersen.p2 = 1,
            param.fish.andersen.p3_exp = 0.1,
            param.fish.andersen.p4_exp = 0.1) {
        p0 <- param.fish.andersen.p0
        p1 <- param.fish.andersen.p1
        p2 <- param.fish.andersen.p2
        p3 <- exp(param.fish.andersen.p3_exp)
        p4 <- exp(param.fish.andersen.p4_exp)
        # Work out open-ended midlen, use maximum for p5
        dl <- diff(lg) / 2 ; dl <- c(dl, dl[[length(dl)]])
        stock__midlen <- lg + dl
        p5 <- max(stock__midlen)

        p0 + p2 * ifelse(
            log(p5/stock__midlen) <= p1,
            exp(-(log(p5/stock__midlen) - p1)**2/p4),
            exp(-(log(p5/stock__midlen) - p1)**2/p3))
    }
    g3_andersenfleet <- function (lg, ...) {
        g3_eval(
            g3_suitability_andersenfleet(),
            stock=g3_stock('fish', lg),
            ...)
    }

    params <- list(lg = seq(100, 500, by = 100))
    ok(ut_cmp_equal(
        as.numeric(do.call(g3_andersenfleet, params)),
        do.call(nondiff_andersenfleet, params),
        tolerance = 1e-6), paste0("g3_suitability_andersen matches ", deparse1(params)))
    params <- list(lg = seq(100, 500, by = 100), param.fish.andersen.p3_exp = -Inf)
    ok(ut_cmp_equal(
        as.numeric(do.call(g3_andersenfleet, params)),
        do.call(nondiff_andersenfleet, params),
        tolerance = 1e-6), paste0("g3_suitability_andersen matches ", deparse1(params)))
    params <- list(lg = seq(100, 500, by = 100), param.fish.andersen.p4_exp = 0.999)
    ok(ut_cmp_equal(
        as.numeric(do.call(g3_andersenfleet, params)),
        do.call(nondiff_andersenfleet, params),
        tolerance = 1e-6), paste0("g3_suitability_andersen matches ", deparse1(params)))
})

fleet_stock <- g3_fleet('fleet') %>% g3s_livesonareas(1:3)
pred_stock <- g3_stock('pred', c(10, 20, 30, 40, 50, 75, 100)) %>%
    g3s_livesonareas(1:3) %>%
    g3s_age(1, 3)
input_stock <- g3_stock('input_stock', c(10, 20, 30, 40, 50, 75, 100)) %>%
    g3s_livesonareas(1:3) %>%
    g3s_age(1, 3)
actions <- list(
    g3a_time(2000, 2000, step_lengths = c(3, 3, 5, 1), project_years = 0),
    g3a_initialconditions_normalparam(
        input_stock,
        factor_f = ~ 1 + age * g3_param('p_factor.age') + area * g3_param('p_factor.area'),
        mean_f = ~(age * 10) + 30,
        stddev_f = ~25,
        alpha_f = ~0.1,
        beta_f = ~0.2),
    g3a_initialconditions_normalparam(
        pred_stock,
        factor_f = ~ 1 + age * g3_param('p_factor.age') + area * g3_param('p_factor.area'),
        mean_f = ~(age * 10) + 30,
        stddev_f = ~25,
        alpha_f = ~0.1,
        beta_f = ~0.2),
    suit_report_action(fleet_stock, input_stock, g3_suitability_exponentiall50(
        ~g3_param("p_exponentiall50.alpha"),
        ~g3_param("p_exponentiall50.l50"))),
    suit_report_action(pred_stock, input_stock, g3_suitability_andersen(
        p0 = ~g3_param("p_andersen_p0"),
        p1 = ~g3_param("p_andersen_p1"),
        p2 = ~g3_param("p_andersen_p2"),
        p4 = ~g3_param("p_andersen_p4"))),
    suit_report_action(pred_stock, input_stock, g3_suitability_andersen(
        p0 = ~g3_param("p_andersen_ne_p0"),
        p1 = ~g3_param("p_andersen_ne_p1"),
        p2 = ~g3_param("p_andersen_ne_p2"),
        p3 = ~g3_param("p_andersen_ne_p3"),
        p4 = ~g3_param("p_andersen_ne_p4"))),
    suit_report_action(fleet_stock, input_stock, g3_suitability_gamma(
        ~g3_param("p_gamma_alpha"),
        ~g3_param("p_gamma_beta"),
        ~g3_param("p_gamma_gamma"))),
    suit_report_action(pred_stock, input_stock, g3_suitability_exponential(
        ~g3_param("p_exponential_alpha"),
        ~g3_param("p_exponential_beta"),
        ~g3_param("p_exponential_gamma"),
        ~g3_param("p_exponential_delta"))),
    suit_report_action(fleet_stock, input_stock, g3_suitability_straightline(
        ~g3_param("p_straightline_alpha"),
        ~g3_param("p_straightline_beta"))),
    suit_report_action(fleet_stock, input_stock, g3_suitability_constant(
        ~g3_param("p_constant_alpha"))),
    suit_report_action(pred_stock, input_stock, g3_suitability_richards(
        ~g3_param("p_richards_p0"),
        ~g3_param("p_richards_p1"),
        ~g3_param("p_richards_p2"),
        ~g3_param("p_richards_p3"),
        ~g3_param("p_richards_p4"))),
    list('999' = ~{
        REPORT(input_stock__num)
        REPORT(input_stock__wgt)
        REPORT(input_stock__midlen)
        nll <- g3_param('x', value = 1.0)
    }))

# Compile model
model_fn <- g3_to_r(actions, trace = TRUE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

ok_group("exponentiall50", {
    params <- attr(model_fn, 'parameter_template')
    params$p_factor.age <- 1  # Abundance increases with age
    params$p_exponentiall50.alpha <- 0.1
    params$p_exponentiall50.l50 <- 50

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
    params <- attr(model_fn, 'parameter_template')
    params[startsWith('p_', names(params))] <- runif(
        sum(startsWith('p_', names(params))),
        min=0.1,
        max=0.9)

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
