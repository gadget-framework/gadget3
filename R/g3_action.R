g3a_time <- function(start_year, end_year, steps = c(12)) {
    steps <- steps
    step_count <- length(steps)
    cur_time <- as.integer(0)
    cur_step_len <- as.integer(0)
    total_steps <- ~length(steps) * (end_year - start_year)

    list(step0 = ~{
        comment("g3a_time")
        cur_time <- cur_time + 1
        cur_step_len <- 55 # TODO: More realistic definition, can we remove if not required?
        if (cur_time < total_steps) break
    })
}

g3a_age <- function(stock) {
    prev_age <- "TODO:"
    tmp <- "TODO:"
    list(
        step125 = stock_step(stock, ~{
            if ((cur_time %% step_count) == step_count - 1) {  # TODO: Can g3a_time define this for us? Or do we nest the loop?
                if (prev_age) {
                    # Swap this age for previous entry
                    tmp <- stock_num
                    stock_num <- prev_age
                    prev_age <- tmp
                    # TODO: Final age bracket?
                } else {
                    # Note first age group
                    prev_age <- stock_num
                }
            }
        }))
}

g3a_grow_lengthvbsimple <- function (linf, kappa, alpha, beta) {
    f_substitute(
        ~(linf - stock_lenmid) * (1 - exp(-kappa * cur_step_len)),
        list(linf = linf, kappa = kappa, alpha = alpha, beta = beta))
}

g3a_grow_impl_bbinom <- function (beta, maxlengthgroupgrowth) {
    f_substitute(
        ~growth_bbinom(beta, maxlengthgroupgrowth),
        list(beta = beta, maxlengthgroupgrowth = maxlengthgroupgrowth))
}

g3a_grow <- function(stock, growth_fn, impl_fn) {
    stock_growth_num <- stock_definition(stock, 'stock_num')
    stock_delt_l <- array(dim = dim(stock_growth_num)[[1]])
    stock_growth_ratio <- matrix(nrow = length(stock_delt_l), ncol = length(stock_delt_l))
    list(
        step055b = stock_step(stock,
            init = stock_growth_num ~ 0,
            final = stock_num ~ stock_growth_num,
            f_substitute(~{
                stock_delt_l <- growth_fn
                stock_growth_ratio <- impl_fn

                stock_growth_num <- stock_num * stock_growth_ratio
            }, list(
                growth_fn = growth_fn,
                impl_fn = impl_fn))))
}
