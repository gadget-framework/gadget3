# Steps to set up initial states of stocks on first step
g3a_initialconditions <- function (stock, factor_f, mean_f, stddev_f, alpha_f, beta_f) {
    # See InitialCond::Initialise
    # TODO: Scaling from initialcond values to "real" values
    initcond_dnorm <- array(dim = length(stock_definition(stock, 'stock__meanlen')))
    initcond_scaler <- 0.0

    out <- list()
    out[[paste0("000:", stock$name)]] <- stock_step(stock, run_if = ~cur_time == 0, iter = f_substitute(~{
        initcond_dnorm <- (stock__meanlen - mean_f) * (1.0 / stddev_f)
        stock__num[stock__iter] <- exp(-(initcond_dnorm ** 2) * 0.5)
        initcond_scaler <- 10000.0 / sum(stock__num[stock__iter])
        stock__num[stock__iter] <- stock__num[stock__iter] * initcond_scaler * factor_f
        stock__wgt[stock__iter] <- alpha_f * stock__meanlen ** beta_f
    }, list(
        factor_f = factor_f,
        mean_f = mean_f,
        stddev_f = stddev_f,
        alpha_f = alpha_f,
        beta_f = beta_f)))
    return(out)
}
