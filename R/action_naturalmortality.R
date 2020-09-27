g3a_naturalmortality_exp <- function (inner_f) {
    f_substitute(
        ~exp(-inner_f * cur_step_len),
        list(inner_f = inner_f))
}

g3a_naturalmortality <- function (stock, mortality_f, run_f = TRUE, run_at = 4) {
    # See Stock::reducePop, NaturalMortality::Reset
    out <- list()

    stock__num <- stock_instance(stock)
    stock__wgt <- stock_instance(stock)

    out[[step_id(run_at, stock)]] <- stock_step(f_substitute(~if (run_f) {
        stock_comment("Natural mortality for ", stock)
        stock_iterate(stock, {
            stock__num[stock__iter] <- stock__num[stock__iter] * mortality_f
        })
    }, list(
        run_f = run_f,
        mortality_f = mortality_f)))

    return(out)
}
