# Steps to set up initial states of stocks on first step
g3a_initialconditions <- function (stock, factor_f, mean_f, stddev_f, alpha_f, beta_f) {
    # See InitialCond::Initialise
    # TODO: Scaling from initialcond values to "real" values
    initcond_dnorm <- array(dim = length(stock_definition(stock, 'stock_meanlen')))
    initcond_scaler <- 0.0

    list(
        step00 = stock_step(stock, run_if = ~cur_time == 0, iter = f_substitute(~{
            initcond_dnorm <- (stock_meanlen - mean_f) * (1.0 / stddev_f)
            stock_num <- exp(-(initcond_dnorm ** 2) * 0.5)
            initcond_scaler <- 10000.0 / sum(stock_num)
            stock_num <- stock_num * initcond_scaler * factor_f
            stock_wgt <- alpha_f * stock_meanlen ** beta_f
        }, list(
            factor_f = factor_f,
            mean_f = mean_f,
            stddev_f = stddev_f,
            alpha_f = alpha_f,
            beta_f = beta_f))))
}
