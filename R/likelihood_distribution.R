call_append <- function (call, extra) as.call(c(as.list(call), extra))

g3l_distribution_sumofsquares <- function (over = c('area')) {
    f_substitute(~sum((
        stock_ss(modelstock__x) / avoid_zero(sum(modelstock_total_f)) -
        stock_ss(obsstock__x) / avoid_zero(sum(obsstock_total_f))) ** 2), list(
            modelstock_total_f = call_append(quote(stock_ssinv(modelstock__x, 'time')), over),
            obsstock_total_f = call_append(quote(stock_ssinv(obsstock__x, 'time')), over)))
}

g3l_distribution_multinomial <- function (epsilon = 10) {
    # data == obs, dist == model
    # multinomial.cc defines sumlog/likely as below
    sumlog <- substitute(
        sum(lgamma_vec(1 + data)) - lgamma(1 + sum(data)), list(
            data = quote(stock_ss(obsstock__x))))

    likely <- substitute(
        -sum(data * log(logspace_add_vec(
            dist / avoid_zero(sum(dist)) * 10000,
            (1 / (length(data) * epsilon)) * 10000) / 10000)), list(
                data = quote(stock_ss(obsstock__x)),
                dist = quote(stock_ss(modelstock__x)),
                epsilon = epsilon))

    f_substitute(~2 * (likely + sumlog), list(
        likely = likely,
        sumlog = sumlog))
}

g3l_distribution_multivariate <- function (rho_f, sigma_f, over = c('area')) {
    multivariate_fn <- g3_native(r = function (x, rho, sigma) {
        sum(dnorm(rho * lag(x, 1), sqrt(1 - rho**2) * sigma))
    }, cpp = '[](auto x, Type rho, Type sigma) -> Type { // NB: "auto" because it could be vector<Type> or array<Type>
       // http://kaskr.github.io/adcomp/_book/Densities.html#autoregressive-processes
       using namespace density;

       return SCALE(AR1(rho), sigma)(x);
    }')

    f_substitute(~multivariate_fn(
            stock_ss(modelstock__x) / sum(modelstock_total_f) -
                stock_ss(obsstock__x) / sum(obsstock_total_f),
            rho_f,
            sigma_f), list(
                modelstock_total_f = call_append(quote(stock_ssinv(modelstock__x, 'time')), over),
                obsstock_total_f = call_append(quote(stock_ssinv(obsstock__x, 'time')), over),
                rho_f = rho_f,
                sigma_f = sigma_f))
}

g3l_distribution_surveyindices <- function (fit = 'log', alpha = NULL, beta = NULL) {
    stopifnot(fit == 'linear' || fit == 'log')

    surveyindices_linreg <- g3_native(r = function (N, I, fixed_alpha, fixed_beta) {
        meanI <- mean(I)
        meanN <- mean(N)
        beta <- if (is.na(fixed_beta)) sum((I - meanI) * (N - meanN)) / avoid_zero(sum((N - meanN)**2)) else fixed_beta
        alpha <- if (is.na(fixed_alpha)) meanI - beta * meanN else fixed_alpha
        return((alpha + beta * N - I)**2)
    }, cpp = '[&avoid_zero](vector<Type> N, vector<Type> I, Type fixed_alpha, Type fixed_beta) -> vector<Type> {
        auto meanI = I.mean();
        auto meanN = N.mean();
        auto beta = std::isnan(asDouble(fixed_beta)) ? ((I - meanI) * (N - meanN)).sum() / avoid_zero((pow(N - meanN, (Type)2)).sum()) : fixed_beta;
        auto alpha = std::isnan(asDouble(fixed_alpha)) ? meanI - beta * meanN : fixed_alpha;
        return pow(alpha + beta * N - I, (Type)2);
    }', depends = c('avoid_zero'))

    # NB: stock_ss(..., 'time') means we keep the time dimension, and have a vector of model time for each area,
    #     checking max_time_idx means we only do this once all data is collected.
    # NB: Outer sum sums the vector of timepoints, this formula is called for each area separately.
    f_substitute(~if (modelstock__time_idx == modelstock__max_time_idx) sum(surveyindices_linreg(
            if(is_log) log(avoid_zero_vec(stock_ss(modelstock__x, 'time'))) else stock_ss(modelstock__x, 'time'),
            if(is_log) log(avoid_zero_vec(stock_ss(obsstock__x, 'time'))) else stock_ss(obsstock__x, 'time'),
            intercept_f,
            slope_f)) else 0, list(
        is_log = (fit == 'log'),
        # NB: Can't use NULL in C++, Use NaN instead
        intercept_f = if (is.null(alpha)) NaN else alpha,
        slope_f = if (is.null(beta)) NaN else beta))
}
g3l_distribution_surveyindices_log <- function (alpha = NULL, beta = NULL) g3l_distribution_surveyindices('log', alpha, beta)
g3l_distribution_surveyindices_linear <- function (alpha = NULL, beta = NULL) g3l_distribution_surveyindices('linear', alpha, beta)

# Compare numbers caught by fleets to observation data
g3l_catchdistribution <- function (
        nll_name,
        obs_data,
        fleets = list(),
        stocks,
        function_f,
        missing_val = 0,
        area_group = NULL,
        report = FALSE,
        nll_breakdown = FALSE,
        weight = substitute(g3_param(n, optimise = FALSE, value = 1), list(n = paste0(nll_name, "_weight"))),
        run_at = 10) {
    stopifnot(length(fleets) > 0)
    g3l_distribution(nll_name, obs_data, fleets, stocks, function_f, missing_val = missing_val, area_group = area_group, report = report, nll_breakdown = nll_breakdown, weight = weight, run_at = run_at)
}

# Compare model state to observation data
# - obs_data: Real-world observations. data.frame containing colums:
#    - year
#    - (optional) step (otherwise data summed over the current year)
#    - (optional) area (otherwise all areas summed)
#    - (optional) age (otherwise all ages summed)
#    - length
#    - number - expected number for given combination (optional, but must provide one)
#    - weight - expected total biomass for given combination (optional, but must provide one)
# - fleets: Gather catch from these fleets (or abundance, if no fleets)
# - stocks: Gather catch/abundance from these stocks
# - function_f: Comparison function to compare stock_ss(modelstock__x) & stock_ss(obsstock__x)
# - weight: Weighting of parameter in final nll
g3l_distribution <- function (
        nll_name,
        obs_data,
        fleets = list(),
        stocks,
        function_f,
        missing_val = 0,
        area_group = NULL,
        report = FALSE,
        nll_breakdown = FALSE,
        weight = substitute(g3_param(n, optimise = FALSE, value = 1), list(n = paste0(nll_name, "_weight"))),
        run_at = 10) {
    out <- new.env(parent = emptyenv())

    # Define prefix matching our public name
    parent.call <- if (length(sys.call(-1)) > 0) as.character(sys.call(-1)[[1]]) else ''
    if (parent.call == 'g3l_catchdistribution') {
        prefix <- "g3l_catchdistribution: "
    } else {
        prefix <- "g3l_distribution: "
    }

    # Convert data to stocks
    ld <- g3l_likelihood_data(nll_name, obs_data, missing_val = missing_val, area_group = area_group, model_history = report)
    modelstock <- ld$modelstock
    obsstock <- ld$obsstock
    if (!is.null(ld$number)) {
        modelstock__num <- stock_instance(modelstock, 0)
        obsstock__num <- stock_instance(obsstock, ld$number)
    }
    if (!is.null(ld$weight)) {
        modelstock__wgt <- stock_instance(modelstock, 0)
        obsstock__wgt <- stock_instance(obsstock, ld$weight)
    }

    # If there are no fleets, do abundance comparison
    if (length(fleets) == 0) for (stock in stocks) {
        # Work out stock index for obs/model variables
        if (!is.null(ld$stock_map)) {
            # Skip over stocks not part of the observation data, map to an index
            # NB: This is what stock_iterate() would do for us
            if (is.null(ld$stock_map[[stock$name]])) next
            stockidx_f <- f_substitute(~g3_idx(x), list(x = ld$stock_map[[stock$name]]))
        } else {
            # Not using stock grouping, __stock_idx variable not needed
            stockidx_f <- ~-1
        }

        # Collect all of stock and dump it in modelstock
        out[[step_id(run_at, 'g3l_distribution', nll_name, 1, stock)]] <- f_substitute(~{
            debug_label(prefix, "Collect abundance from ", stock, " for ", nll_name)
            stock_iterate(stock, stock_intersect(modelstock, {
                if (compare_num) {
                    debug_trace("Add ", stock, " individuals to our count")
                    stock_ss(modelstock__num) <- stock_ss(modelstock__num) +
                        stock_reshape(modelstock, stock_ss(stock__num))
                }
                if (compare_wgt) {
                    debug_trace("Take ", stock, " total biomass to our count")
                    stock_ss(modelstock__wgt) <- stock_ss(modelstock__wgt) +
                        stock_reshape(modelstock, stock_ss(stock__num) * stock_ss(stock__wgt))
                }
            }))
        }, list(
            compare_num = !is.null(ld$number),
            compare_wgt = !is.null(ld$weight)))

        # Fix-up stock intersection, add in stockidx_f
        out[[step_id(run_at, 'g3l_distribution', nll_name, 1, stock)]] <- g3_step(f_substitute(out[[step_id(run_at, 'g3l_distribution', nll_name, 1, stock)]], list(
            stockidx_f = stockidx_f)))
    }

    # Otherwise, do fleet catch comparison
    for (fleet_stock in fleets) for (prey_stock in stocks) {
        # Work out stock index for obs/model variables
        if (!is.null(ld$stock_map)) {
            # Skip over stocks not part of the observation data, map to an index
            # NB: This is what stock_iterate() would do for us
            if (is.null(ld$stock_map[[prey_stock$name]])) next
            stockidx_f <- f_substitute(~g3_idx(x), list(x = ld$stock_map[[prey_stock$name]]))
        } else {
            # Not using stock grouping, __stock_idx variable not needed
            stockidx_f <- ~-1
        }

        # Collect all of fleet's sampling of prey and dump it in modelstock
        out[[step_id(run_at, 'g3l_distribution', nll_name, 1, fleet_stock, prey_stock)]] <- g3_step(f_substitute(~{
            debug_label(prefix, "Collect catch from ", fleet_stock, "/", prey_stock, " for ", nll_name)
            stock_iterate(prey_stock, stock_intersect(modelstock, {
                if (compare_num) {
                    debug_trace("Take prey_stock__fleet_stock weight, convert to individuals, add to our count")
                    stock_ss(modelstock__num) <- stock_ss(modelstock__num) +
                        stock_reshape(modelstock, stock_ss(prey_stock__fleet_stock) / avoid_zero_vec(stock_ss(prey_stock__wgt)))
                }
                if (compare_wgt) {
                    debug_trace("Take prey_stock__fleet_stock weight, add to our count")
                    stock_ss(modelstock__wgt) <- stock_ss(modelstock__wgt) +
                        stock_reshape(modelstock, stock_ss(prey_stock__fleet_stock))
                }
            }))
        }, list(
            compare_num = !is.null(ld$number),
            compare_wgt = !is.null(ld$weight),
            # Find catch from predation step
            prey_stock__fleet_stock = as.symbol(paste0('prey_stock__', fleet_stock$name)))), recursing = TRUE)

        # Fix-up stock intersection, add in stockidx_f
        out[[step_id(run_at, 'g3l_distribution', nll_name, 1, fleet_stock, prey_stock)]] <- f_substitute(out[[step_id(run_at, 'g3l_distribution', nll_name, 1, fleet_stock, prey_stock)]], list(
            stockidx_f = stockidx_f))
        out[[step_id(run_at, 'g3l_distribution', nll_name, 1, fleet_stock, prey_stock)]] <- g3_step(out[[step_id(run_at, 'g3l_distribution', nll_name, 1, fleet_stock, prey_stock)]])
    }

    nllstock <- g3_storage(paste0("nll_cdist_", nll_name))
    if (nll_breakdown) nllstock <- g3s_modeltime(nllstock)
    nllstock__num <- stock_instance(nllstock, 0)
    nllstock__wgt <- stock_instance(nllstock, 0)
    nllstock__weight <- stock_instance(nllstock, 0)

    # Generic comparison step with __x instead of __num or __wgt
    compare_generic_f <- g3_step(f_substitute(~{
        debug_label(prefix, "Compare ", modelstock, " to ", obsstock)
        if (done_aggregating_f) {
            stock_iterate(modelstock, stock_intersect(obsstock, stock_intersect(nllstock, if (function_comare_f) g3_with(
                cur_cdist_nll := function_f, {
                nll <- nll + (weight) * cur_cdist_nll
                stock_ss(nllstock__x) <- stock_ss(nllstock__x) + cur_cdist_nll
                g3_report(nllstock__x)  # TODO: Only on penultimate step?
                stock_ss(nllstock__weight) <- weight
                g3_report(nllstock__weight)  # TODO: Only on penultimate step?
                if (report) g3_report(modelstock__x)
                if (report) g3_report(obsstock__x)
            }))))
            if (!report) {
                debug_trace("Zero counters for next reporting period")
                stock_with(modelstock, modelstock__x[] <- 0)
            }
        }
    }, list(
        done_aggregating_f = ld$done_aggregating_f,
        function_comare_f = if (is.null(attr(function_f, 'do_compare_f'))) TRUE else attr(function_f, 'do_compare_f'),
        function_f = function_f,
        report = report,
        weight = weight)))
    compare_generic_f <- f_substitute(compare_generic_f, list(stockidx_f = as.symbol(paste0(modelstock$name, "__stock_idx"))))

    # Build list of variables that will need replacing with num/wgt later
    var_replacements <- all.vars(compare_generic_f)[endsWith(all.vars(compare_generic_f), '__x')]
    names(var_replacements) <- var_replacements

    if (!is.null(ld$number)) {
        compare_f <- f_substitute(
            compare_generic_f,
            lapply(gsub('__x$', '__num', var_replacements), as.symbol))
        # step.R isn't doing environment for us, so we have to
        assign(paste0(modelstock$name, '__num'), modelstock__num, envir = environment(compare_f))
        assign(paste0(obsstock$name, '__num'), obsstock__num, envir = environment(compare_f))
        assign(paste0(nllstock$name, '__num'), nllstock__num, envir = environment(compare_f))
        out[[step_id(run_at, 'g3l_distribution', nll_name, 2, 'num')]] <- compare_f
    }

    if (!is.null(ld$weight)) {
        compare_f <- f_substitute(
            compare_generic_f,
            lapply(gsub('__x$', '__wgt', var_replacements), as.symbol))
        # step.R isn't doing environment for us, so we have to
        assign(paste0(modelstock$name, '__wgt'), modelstock__wgt, envir = environment(compare_f))
        assign(paste0(obsstock$name, '__wgt'), obsstock__wgt, envir = environment(compare_f))
        assign(paste0(nllstock$name, '__wgt'), nllstock__wgt, envir = environment(compare_f))
        out[[step_id(run_at, 'g3l_distribution', nll_name, 2, 'wgt')]] <- compare_f
    }

    return(as.list(out))
}
