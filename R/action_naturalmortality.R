g3a_naturalmortality_exp <- function (param_f, action_step_size_f = ~cur_step_size) {
    f_substitute(
        ~exp(-(param_f) * action_step_size_f),
        list(param_f = param_f, action_step_size_f = action_step_size_f))
}

g3a_naturalmortality <- function (stock, mortality_f, run_f = TRUE, run_at = 4) {
    # See Stock::reducePop, NaturalMortality::Reset
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    stock__num <- stock_instance(stock)
    stock__wgt <- stock_instance(stock)

    out[[step_id(run_at, stock, action_name)]] <- g3_step(f_substitute(~if (run_f) {
        debug_label("Natural mortality for ", stock)
        stock_iterate(stock, {
            stock_ss(stock__num) <- stock_ss(stock__num) * (mortality_f)
        })
    }, list(
        run_f = run_f,
        mortality_f = mortality_f)))

    return(as.list(out))
}
