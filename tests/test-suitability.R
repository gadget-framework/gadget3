library(magrittr)
library(unittest)

library(gadget3)

tmb_r_compare <- function (model_fn, model_tmb, params) {
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        # Reformat params into a single vector in expected order
        par <- unlist(params[attr(model_cpp, 'parameter_template')$switch])
        model_tmb_report <- model_tmb$report(par)
        for (n in ls(environment(model_fn)$model_report)) {
            ok(ut_cmp_equal(
                as.vector(model_tmb_report[[n]]),
                as.vector(environment(model_fn)$model_report[[n]]),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}


# Mini action to run suitability function for stocks and report output
suit_report_action <- function (pred_stock, prey_stock, suit_f, run_at = 99) {
    out <- new.env(parent = emptyenv())
    suit_fn_name <- as.character(sys.call()[[4]][[1]])
    suit_var_name <- paste0(prey_stock$name, '__', suit_fn_name)
    assign(suit_var_name, gadget3:::stock_instance(prey_stock))
    
    out[[gadget3:::step_id(run_at, suit_fn_name)]] <- gadget3:::g3_step(gadget3:::f_substitute(~{
        debug_label("Testing ", suit_fn_name)
        stock_iterate(prey_stock, stock_intersect(pred_stock, {
            stock_ss(suit_var) <- (suit_f)
        }))
        g3_report(suit_var)
    }, list(
        suit_fn_name = suit_fn_name,
        suit_var = as.symbol(suit_var_name),
        suit_f = suit_f)))
    return(as.list(out))
}


fleet_stock <- g3_fleet('fleet') %>% g3s_livesonareas(1:3)
input_stock <- g3_stock('input_stock', c(10, 20, 30, 40, 50, 75, 100)) %>%
    g3s_livesonareas(1:3) %>%
    g3s_age(1, 3)
actions <- list(
    g3a_time(2000, 2000, steps = c(3, 3, 5, 1)),
    g3a_initialconditions_normalparam(
        input_stock,
        factor_f = ~ 1 + age * g3_param('factor.age') + area * g3_param('factor.area'),
        mean_f = ~(age * 10) + 30,
        stddev_f = ~25,
        alpha_f = ~0.1,
        beta_f = ~0.2),
    suit_report_action(fleet_stock, input_stock, g3_suitability_exponentiall50(
        ~g3_param("exponentiall50.alpha"),
        ~g3_param("exponentiall50.l50"))),
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
    r <- environment(model_fn)$model_report
    # str(result)
    # str(as.list(r), vec.len = 10000)
    #print(round(r$input_stock__g3_suitability_exponentiall50, 3))

    ok(all(r$input_stock__g3_suitability_exponentiall50[,,'age1'] == r$input_stock__g3_suitability_exponentiall50[,,'age2']), 'all of age1 = age2')
    ok(all(r$input_stock__g3_suitability_exponentiall50[,,'age2'] == r$input_stock__g3_suitability_exponentiall50[,,'age3']), 'all of age2 = age3')

    tmb_r_compare(model_fn, model_tmb, params)
})
