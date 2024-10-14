g3_quota_hockeyfleet <- function (
        predstocks,
        output_ratios = 1 / length(predstocks),
        btrigger = g3_parameterized("hf.btrigger", by_stock = predstocks),
        harvest_rate = g3_parameterized("hf.harvest_rate", by_stock = predstocks) ) {
    stopifnot(is.list(predstocks) && all(sapply(predstocks, g3_is_stock)))

    # predstocks__totalsuit == sum(predstock1__totalsuit) + sum(predstock2__totalsuit) + ...
    predstocks__totalsuit <- lapply(predstocks, function (ps) {
        rlang::f_rhs(g3_step(~stock_with(ps, sum(ps__totalsuit)), recursing = TRUE))
    })
    predstocks__totalsuit <- f_chain_op(predstocks__totalsuit, "+")

    out <- f_substitute(
        ~output_ratio * harvest_rate * dif_pmin(predstocks__totalsuit / btrigger, 1, 1e3),
        list(
            btrigger = btrigger,
            harvest_rate = harvest_rate,
            output_ratio = list_to_stock_switch(output_ratios) ))
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
        run_f = quote( cur_year_projection ),
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
            stock_ss(quotastock__var, time = min(default + lag, g3_idx(total)):g3_idx(total)) <- function_f
        }
    }), list(
        total = quotastock$dim$time,
        function_f = function_f,
        run_f = run_f)))

    return(out)
}
