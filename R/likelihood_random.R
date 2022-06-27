g3l_random_dnorm <- function (
        nll_name,
        param_f,
        mean_f = 0,
        sigma_f = 1,
        log_f = TRUE,
        weight = 1.0,
        run_at = 10) {
    out <- new.env(parent = emptyenv())
    
    nllstock <- g3_storage(paste0('nll_random_dnorm_', nll_name))
    nllstock__dnorm <- stock_instance(nllstock, 0)
    nllstock__weight <- stock_instance(nllstock, 0)

    out[[step_id(run_at, 'g3l_random_dnorm', nll_name)]] <- g3_step(f_substitute(~ if (run_f) {
        debug_label("g3l_random_dnorm: ", nll_name)

        stock_iterate(nllstock, g3_with(n := dnorm(param_f, mean_f, sigma_f, log_f), {
            stock_ss(nllstock__dnorm) <- stock_ss(nllstock__dnorm) + n
            stock_ss(nllstock__weight) <- weight
            nll <- nll + (weight) * n
        }))

    }, list(
        weight = weight,
        param_f = param_f,
        mean_f = mean_f,
        sigma_f = sigma_f,
        log_f = log_f,
        run_f = quote( cur_time == total_steps ),
        nll_name = nll_name)))
    return(as.list(out))
}

g3l_random_walk <- function (
        nll_name,
        param_f,
        sigma_f = 1,
        log_f = TRUE,
        period = 'year',
        nll_breakdown = FALSE,
        weight = 1.0,
        run_at = 10) {
    stopifnot(period %in% c('year', 'step'))
    out <- new.env(parent = emptyenv())
    
    nllstock <- g3_storage(paste0('nll_random_walk_', nll_name))
    if (nll_breakdown) nllstock <- g3s_modeltime(nllstock, by_year = period == 'year')
    nllstock__dnorm <- stock_instance(nllstock, 0)
    nllstock__weight <- stock_instance(nllstock, 0)
    nllstock__prevrec <- g3_global_formula(init_val = NaN)

    out[[step_id(run_at, 'g3l_random_walk', nll_name)]] <- g3_step(f_substitute(~ if (run_f) {
        debug_label("g3l_random_walk: ", nll_name)

        stock_iterate(nllstock, {
            if (!is.nan(nllstock__prevrec)) g3_with(n := dnorm(param_f, nllstock__prevrec, sigma_f, log_f), {
                stock_ss(nllstock__dnorm) <- stock_ss(nllstock__dnorm) + n
                stock_ss(nllstock__weight) <- weight
                nll <- nll + (weight) * n
            })
            nllstock__prevrec <- param_f
        })

    }, list(
        param_f = param_f,
        sigma_f = sigma_f,
        log_f = log_f,
        run_f = if (period == 'year') quote( cur_step_final ) else TRUE,
        nll_name = nll_name)))
    return(as.list(out))
}
