g3_quota_hockeyfleet <- function (
        predstocks,
        preystocks,  # Mature spawning-stocks
        preyprop_f = 1,  # NB: Doesn't have to sum to 1
        btrigger = g3_parameterized("hf.btrigger", by_stock = predstocks),
        harvest_rate = g3_parameterized("hf.harvest_rate", by_stock = predstocks) ) {
    stopifnot(is.list(predstocks) && all(sapply(predstocks, g3_is_stock)))

    # totalsuit: Total suitable spawning-stock biomass
    # == sum(predstock1_preystock1__suit) + sum(predstock1_preystock2__suit) + ...
    totalsuit <- lapply(predstocks, function (predstock) lapply(preystocks, function (preystock) {
        f_substitute(quote( preyprop * sum(suit_var) ), list(
            preyprop = resolve_stock_list(preyprop_f, preystock),
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
    attr(out, "quota_name") <- c("hockeyfleet", sapply(predstocks, function (ps) ps$name))
    return(out)
}

g3_quota_assess <- function (
        predstocks,
        preystocks,
        assess_f ) {
    # Turn list of calls into a single list() call
    to_list_call <- function (x) as.call(c(list(as.symbol("list")), x ))

    pred_names <- vapply(predstocks, function (s) s$name, character(1))
    prey_names <- vapply(preystocks, function (s) s$name, character(1))

    hist_cons <- to_list_call(sapply(pred_names, function (pred_n) {
        to_list_call(sapply(prey_names, function (prey_n) {
            as.symbol(paste0("detail_", prey_n, "_", pred_n, "__cons"))
        }, simplify = FALSE))
    }, simplify = FALSE))
    hist_abund <- to_list_call(sapply(prey_names, function (prey_n) {
        as.symbol(paste0("detail_", prey_n, "__num"))
    }, simplify = FALSE))
    hist_meanwgt <- to_list_call(sapply(prey_names, function (prey_n) {
        as.symbol(paste0("detail_", prey_n, "__wgt"))
    }, simplify = FALSE))

    out <- f_substitute(assess_f, list(
        cons = hist_cons,
        abund = hist_abund,
        meanwgt = hist_meanwgt ))
    attr(out, "quota_name") <- c("assess", sapply(predstocks, function (ps) ps$name))
    return(out)
}

g3_quota <- function (
        function_f,
        quota_name = attr(function_f, 'quota_name'),
        lag = 1,  # In steps if by_step, else in years
        by_step = TRUE,
        run_step = NULL,
        run_f = quote( cur_year >= end_year - 1 ),  # TODO: Allow for lag
        init_val = 0,
        run_at = g3_action_order$quota) {
    if (!is.null(run_step)) run_f <- f_substitute(quote(cur_step == x && run_f), list(
        run_f = run_f,
        x = run_step ))
    # NB: We run at the very end of the model, to avoid reporting timing complication,
    # as a result we can only set the quota for the next timestep
    if (lag < 1) stop("Lag should be at least 1, not ", lag)

    quotastock <- g3_storage(c('quota', quota_name))
    quotastock <- g3s_modeltime(quotastock, by_year = isFALSE(by_step))
    quotastock__var <- g3_stock_instance(quotastock, init_val, desc = paste0("Quota values for ", quotastock$name))

    out <- g3_step(f_substitute(~(
        stock_with(quotastock, stock_ss(quotastock__var, vec = single))
    ), list(
        end = NULL )), recursing = TRUE)

    environment(out)[[step_id(run_at, "g3a_quota", quotastock)]] <- g3_step(f_substitute(~stock_with(quotastock, {
        if (run_f) {
            # NB: "- g3_idx(0)" reverses g3_idx already applied to default
            stock_ss(quotastock__var, time = min(default - g3_idx(0) + lag, total):total) <- function_f
        }
    }), list(
        total = quotastock$dim$time,
        function_f = function_f,
        lag = as.integer(lag),
        run_f = run_f)))

    return(out)
}
