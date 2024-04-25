g3a_naturalmortality_exp <- function (
        param_f = g3_parameterized('M', by_stock = by_stock, by_age = TRUE),
        by_stock = TRUE,
        action_step_size_f = ~cur_step_size) {
    f_substitute(
        ~exp(-(param_f) * action_step_size_f),
        list(param_f = param_f, action_step_size_f = action_step_size_f))
}

g3a_naturalmortality <- function (
        stock,
        mortality_f = g3a_naturalmortality_exp(),
        run_f = TRUE,
        run_at = g3_action_order$naturalmortality) {
    # See Stock::reducePop, NaturalMortality::Reset
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    stock__num <- g3_stock_instance(stock, 0)
    stock__wgt <- g3_stock_instance(stock, 1)

    out[[step_id(run_at, stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("Natural mortality for ", stock)
        stock_iterate(stock, if (run_f) {
            stock_ss(stock__num) <- stock_ss(stock__num) * (mortality_f)
        })
    }, list(
        run_f = run_f,
        mortality_f = mortality_f)))

    return(as.list(out))
}
