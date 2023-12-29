guess_period <- function (param_f) {
    tryCatch({
        cols <- sort(g3_parameterized_breakdown(param_f), method = 'radix')
    }, error = function (e) stop("Cannot use period='auto', set period manually: ", e))

    if (length(cols) == 0) return('single')
    if ('stock' %in% cols) stop("Per-stock random parameters not supported yet")
    if (identical(cols, c('cur_step', 'cur_year'))) return('step')
    if (identical(cols, c('cur_year')) == 1) return('year')
    if (identical(cols, c('cur_step')) == 1) stop("Seasonal random parameters not supported yet")
    stop("Cannot use period='auto', set period manually: ", cols)
}

g3l_random_dnorm <- function (
        nll_name,
        param_f,
        mean_f = 0,
        sigma_f = 1,
        log_f = TRUE,
        period = 'auto',
        nll_breakdown = FALSE,
        weight = substitute(
            g3_param(n, optimise = FALSE, value = 1),
            list(n = paste0(nll_name, "_weight"))),
        run_at = g3_action_order$likelihood) {
    stopifnot(period %in% c('year', 'step', 'single', 'auto'))
    stopifnot(is.logical(log_f))
    if (period == 'auto') period <- guess_period(param_f)
    out <- new.env(parent = emptyenv())
    
    nllstock <- g3_storage(paste0('nll_random_dnorm_', nll_name))
    if (nll_breakdown && period != 'single') nllstock <- g3s_modeltime(nllstock, by_year = period == 'year')
    nllstock__dnorm <- g3_stock_instance(nllstock, 0)
    nllstock__weight <- g3_stock_instance(nllstock, 0)
    nll <- 0.0

    out[[step_id(run_at, 'g3l_random_dnorm', nll_name)]] <- g3_step(f_substitute(~ if (run_f) {
        debug_label("g3l_random_dnorm: ", nll_name)

        stock_iterate(nllstock, g3_with(n := dist_sense * dnorm(param_f, mean_f, sigma_f, log_f), {
            stock_ss(nllstock__dnorm) <- stock_ss(nllstock__dnorm) + n
            stock_ss(nllstock__weight) <- weight
            nll <- nll + (weight) * n
        }))

    }, list(
        weight = weight,
        param_f = param_f,
        mean_f = mean_f,
        sigma_f = sigma_f,
        dist_sense = if (log_f) -1.0 else 1.0,
        log_f = log_f,
        run_f = if (period == 'single') quote( cur_time == total_steps ) else if (period == 'year') quote( cur_step_final ) else TRUE,
        nll_name = nll_name)))
    return(as.list(out))
}

g3l_random_walk <- function (
        nll_name,
        param_f,
        sigma_f = 1,
        log_f = TRUE,
        period = 'auto',
        nll_breakdown = FALSE,
        weight = substitute(
            g3_param(n, optimise = FALSE, value = 1),
            list(n = paste0(nll_name, "_weight"))),
        run_at = g3_action_order$likelihood) {
    stopifnot(period %in% c('year', 'step', 'single', 'auto'))
    stopifnot(is.logical(log_f))
    if (period == 'auto') period <- guess_period(param_f)
    out <- new.env(parent = emptyenv())
    
    nllstock <- g3_storage(paste0('nll_random_walk_', nll_name))
    if (nll_breakdown && period != 'single') nllstock <- g3s_modeltime(nllstock, by_year = period == 'year')
    nllstock__dnorm <- g3_stock_instance(nllstock, 0)
    nllstock__weight <- g3_stock_instance(nllstock, 0)
    nllstock__prevrec <- g3_global_formula(init_val = NaN)
    nll <- 0.0

    out[[step_id(run_at, 'g3l_random_walk', nll_name)]] <- g3_step(f_substitute(~ if (run_f) {
        debug_label("g3l_random_walk: ", nll_name)

        stock_iterate(nllstock, {
            if (!is.nan(nllstock__prevrec)) g3_with(n := dist_sense * dnorm(param_f, nllstock__prevrec, sigma_f, log_f), {
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
        dist_sense = if (log_f) -1.0 else 1.0,
        run_f = if (period == 'single') quote( cur_time == total_steps ) else if (period == 'year') quote( cur_step_final ) else TRUE,
        nll_name = nll_name)))
    return(as.list(out))
}
