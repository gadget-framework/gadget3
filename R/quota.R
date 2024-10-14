g3_quota_hockeyfleet <- function (
        predstocks,
        preystocks,  # Mature spawning-stocks
        preyprop_fs = 1,  # NB: Doesn't have to sum to 1
        btrigger = g3_parameterized("hf.btrigger", by_stock = predstocks),
        harvest_rate = g3_parameterized("hf.harvest_rate", by_stock = predstocks),
        stddev = g3_parameterized("hf.stddev", by_stock = predstocks, value = 0)) {
    if (g3_is_stock(predstocks)) predstocks <- list(predstocks)
    if (g3_is_stock(preystocks)) preystocks <- list(preystocks)
    stopifnot(is.list(predstocks) && all(sapply(predstocks, g3_is_stock)))
    stopifnot(is.list(preystocks) && all(sapply(preystocks, g3_is_stock)))

    # totalsuit: Total suitable spawning-stock biomass
    # == sum(predstock1_preystock1__suit) + sum(predstock1_preystock2__suit) + ...
    totalsuit <- lapply(predstocks, function (predstock) lapply(preystocks, function (preystock) {
        f_substitute(quote( preyprop * sum(suit_var) ), list(
            preyprop = resolve_stock_list(preyprop_fs, preystock),
            # NB: Should match action_predate's g3s_stockproduct()
            suit_var = as.symbol(paste0(preystock$name, "_", predstock$name, "__suit")) ))
    }))
    totalsuit <- f_chain_op(do.call(c, totalsuit), "+")

    # totalssb: Total spawning-stock biomass
    # == sum(preystock1__num * preystock1__wgt) + ...
    totalssb <- lapply(predstocks, function (predstock) lapply(preystocks, function (preystock) {
        substitute(sum(num * wgt), list(
            num = as.symbol(paste0(preystock$name, "__num")),
            wgt = as.symbol(paste0(preystock$name, "__wgt")) ))
    }))
    totalssb <- f_chain_op(do.call(c, totalssb), "+")

    out <- f_substitute(
        ~harvest_rate * totalsuit * dif_pminmax(totalssb / btrigger, 0, 1, 1e3),
        list(
            btrigger = btrigger,
            harvest_rate = harvest_rate ))

    # If stddev provided, wrap in log-normal distribution
    if (!(is.numeric(stddev) && stddev == 0)) out <- f_substitute(
        quote( if (stddev > 0L) exp(rnorm(1, out - stddev**2 / 2, stddev))[[1]] else out ),
        list(
            stddev = stddev,
            out = out ))

    attr(out, "quota_name") <- c("hockeyfleet", sapply(predstocks, function (ps) ps$name))
    return(out)
}

g3_quota <- function (
        function_f,
        quota_name = attr(function_f, 'quota_name'),
        year_length = 1L,
        start_step = 1L,
        init_val = 0.0,
        run_revstep = -1,
        run_f = TRUE,
        run_at = g3_action_order$quota ) {

    if (!is.null(run_revstep)) run_f <- f_substitute(quote(quotastock__fishingyear_revstep == x && run_f), list(
        run_f = run_f,
        x = as.integer(run_revstep) ))

    quotastock <- g3_storage(c('quota', quota_name))
    quotastock <- g3s_modeltime_fishingyear(quotastock, year_length = year_length, start_step = start_step)
    quotastock__var <- g3_stock_instance(quotastock, init_val, desc = paste0("Quota values for ", quotastock$name))

    # Formula to select current quota value, for use in catchability
    out <- g3_step(f_substitute(~(
        stock_with(quotastock, stock_ss(quotastock__var, vec = single))
    ), list(
        end = NULL )), recursing = TRUE)

    # Ancillary step to calculate quota at assessent step
    environment(out)[[step_id(run_at, "g3a_quota", quotastock)]] <- g3_step(f_substitute(~{
        debug_label("g3_quota: generate quota for ", quotastock)
        stock_iterate(quotastock, if (run_f) {
            # NB: "- g3_idx(0)" reverses g3_idx already applied to default
            # TODO: Splice all of function_f into vector, not just single value
            stock_ss(quotastock__var, fishingyear = g3_idx(min(default - g3_idx(0) + 1L, total)) ) <- function_f
        })
    }, list(
        total = quotastock$dim$fishingyear,
        function_f = function_f,
        run_f = run_f )))

    return(out)
}
