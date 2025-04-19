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

    # totalssb: Total spawning-stock biomass
    # == sum(preyprop * preystock1__num * preystock1__wgt) + ...
    totalssb <- lapply(predstocks, function (predstock) lapply(preystocks, function (preystock) {
        stock <- preystock
        g3_step(f_substitute(
            ~stock_with(stock, hockeyfleet_mult_sum(stock__num * stock__wgt, preyprop)),
            list(
                preyprop = resolve_stock_list(preyprop_fs, preystock) )), recursing = TRUE)
    }))
    totalssb <- f_chain_op(do.call(c, totalssb), "+")

    # Work out function used to multiply & sum preyprop
    if (is.numeric(preyprop_fs) && length(preyprop_fs) == 1) {
        # preyprop_fs is just a constant, can use regular *
        totalssb <- call_replace(totalssb, hockeyfleet_mult_sum = function (x) {
            substitute(sum(a * b), list(
                a = x[[2]],
                b = x[[3]] ))
        })
    } else {
        # Function equivalent to sum(nonconform_mult(base_ar, extra_vec))
        # NB: To use nonconform_mult() we'd need a template version accepting array+vector,
        #     which doesn't work for C++ template reasons, and would likely write the new
        #     array to memory just to sum it anyway.
        environment(totalssb)$hockeyfleet_mult_sum <- g3_native(r = function (base_ar, extra_vec) {
            sum(base_ar * as.vector(extra_vec))
        }, cpp = '[](array<Type> base_ar, vector<Type> extra_vec) -> Type {
            assert(base_ar.size() % extra_vec.size() == 0);
            return (base_ar * (extra_vec.replicate(base_ar.size() / extra_vec.size(), 1))).sum();
        }')
    }

    out <- f_substitute(
        ~harvest_rate * dif_pmin(totalssb / btrigger, 1, 1e3),
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
    attr(out, "catchability_unit") <- "harvest-rate-year"
    return(out)
}

g3_quota_assess <- function (
        predstocks,
        preystocks,
        assess_f,
        unit = c("biomass-year", "biomass", "harvest-rate", "harvest-rate-year",
                 "individuals", "individuals-year") ) {
    # Turn list of calls into a single list() call
    to_list_call <- function (x) as.call(c(list(as.symbol("list")), x ))
    unit <- match.arg(unit)

    pred_names <- vapply(predstocks, function (s) s$name, character(1))
    prey_names <- vapply(preystocks, function (s) s$name, character(1))

    hist_cons <- to_list_call(sapply(pred_names, function (pred_n) {
        to_list_call(sapply(prey_names, function (prey_n) {
            as.symbol(paste0("detail_", prey_n, "_", pred_n, "__cons"))
        }, simplify = FALSE))
    }, simplify = FALSE))
    hist_abund <- to_list_call(sapply(prey_names, function (prey_n) {
        as.symbol(paste0("dstart_", prey_n, "__num"))
    }, simplify = FALSE))
    hist_meanwgt <- to_list_call(sapply(prey_names, function (prey_n) {
        as.symbol(paste0("dstart_", prey_n, "__wgt"))
    }, simplify = FALSE))

    out <- f_substitute(assess_f, list(
        cons = hist_cons,
        abund = hist_abund,
        meanwgt = hist_meanwgt ))
    attr(out, "quota_name") <- c("assess", sapply(predstocks, function (ps) ps$name))
    attr(out, "catchability_unit") <- unit
    return(out)
}

g3_quota <- function (
        function_f,
        quota_name = attr(function_f, 'quota_name'),
        year_length = 1L,
        start_step = 1L,
        interim_value = NULL,
        run_revstep = -1,
        run_f = TRUE,
        run_at = g3_action_order$quota ) {

    if (!is.null(run_revstep)) run_f <- f_substitute(quote(quotastock__fishingyear_revstep == x && run_f), list(
        run_f = run_f,
        x = as.integer(run_revstep) ))

    quotastock <- g3_storage(c('quota', quota_name))
    quotastock <- g3s_modeltime_fishingyear(quotastock, year_length = year_length, start_step = start_step)
    quotastock__var <- g3_stock_instance(quotastock, 0.0, desc = paste0("Quota values for ", quotastock$name))

    # Formula to select current quota value, for use in catchability
    out <- g3_step(f_substitute(~(
        stock_with(quotastock, stock_ss(quotastock__var, vec = single))
    ), list(
        end = NULL )), recursing = TRUE)
    attr(out, "catchability_unit") <- attr(function_f, "catchability_unit")

    if (!is.null(interim_value)) {
        function_f <- f_substitute(quote(
            if (cur_year_projection) f else i
        ), list(f = function_f, i = interim_value))
    }

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
