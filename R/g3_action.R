g3a_time <- function(start_year, end_year, steps = c(12)) {
    steps <- steps
    step_count <- length(steps)
    cur_time <- as.integer(0)
    cur_step_len <- as.integer(0)
    cur_step_final <- FALSE
    total_steps <- ~length(steps) * (end_year - start_year)

    list(step0 = ~{
        comment("g3a_time")
        cur_time <- cur_time + 1
        cur_step_len <- 55 # TODO: More realistic definition, can we remove if not required?
        cur_step_final <- (cur_time %% step_count) == step_count - 1
        if (cur_time < total_steps) break
    })
}

g3a_age <- function(stock) {
    # Mangle stock_num / stock_wgt to remove non-age parameters
    stock_num_age <- as.call(lapply(ling_imm$stock_num, function (x) {
        if (as.character(x) %in% c("[", "stock_num", "age")) x else ling_imm$stock_num[[3]]
    }))
    stock_num_age_older <- as.call(lapply(ling_imm$stock_num, function (x) {
        if (as.character(x) %in% c("[", "stock_num")) x
        else if (as.character(x) %in% c("age")) quote(age + 1)
        else ling_imm$stock_num[[3]]
    }))
    stock_wgt_age <- as.call(lapply(ling_imm$stock_wgt, function (x) {
        if (as.character(x) %in% c("[", "stock_wgt", "age")) x else ling_imm$stock_wgt[[3]]
    }))
    stock_wgt_age_older <- as.call(lapply(ling_imm$stock_wgt, function (x) {
        if (as.character(x) %in% c("[", "stock_wgt")) x
        else if (as.character(x) %in% c("age")) quote(age + 1)
        else ling_imm$stock_wgt[[3]]
    }))

    list(
        step125 = stock_step(stock, run_if = ~cur_step_final, init = f_substitute(~for (age in rev(stock_ages)) {
            if (age == stock_ages[length(stock_ages)]) {
                comment("TODO: Plus group migration shenanigans")
            } else {
                stock_num_older <- stock_num_older + stock_num
                stock_num <- 0
            }
        }, list(
            stock_num = stock_num_age,
            stock_num_older = stock_num_age_older))))
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
            iter = f_substitute(~{
                stock_delt_l <- growth_fn
                stock_growth_ratio <- impl_fn

                stock_growth_num <- stock_num * stock_growth_ratio
            }, list(
                growth_fn = growth_fn,
                impl_fn = impl_fn)),
            final = stock_num ~ stock_growth_num))
}
