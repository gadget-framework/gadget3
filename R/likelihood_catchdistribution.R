call_append <- function (call, extra) as.call(c(as.list(call), extra))

g3l_catchdistribution_sumofsquares <- function (over = c('area')) {
    f_substitute(~sum((
        stock_ss(modelstock__x) / avoid_zero(sum(modelstock_total_f)) -
        stock_ss(obsstock__x) / avoid_zero(sum(obsstock_total_f))) ** 2), list(
            modelstock_total_f = call_append(quote(stock_ssinv(modelstock__x)), over),
            obsstock_total_f = call_append(quote(stock_ssinv(obsstock__x, 'time')), over)))
}

g3l_catchdistribution_multinomial <- function () {
    ~2 * (lgamma(1 + sum(stock_ss(obsstock__x))) -
        sum(lgamma_vec(1 + stock_ss(obsstock__x))) +
        sum(stock_ss(obsstock__x) * log(
            stock_ss(modelstock__x) / sum(stock_ss(modelstock__x))))
    )
}

g3l_catchdistribution_multivariate <- function (rho_f, sigma_f, over = c('area')) {
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
                modelstock_total_f = call_append(quote(stock_ssinv(modelstock__x)), over),
                obsstock_total_f = call_append(quote(stock_ssinv(obsstock__x, 'time')), over),
                rho_f = rho_f,
                sigma_f = sigma_f))
}

g3l_catchdistribution_surveyindices_log <- function (alpha = 0, beta = 1) {
    f_substitute(~sum((alpha +
        beta * log(avoid_zero_vec(stock_ss(modelstock__x))) -
        log(avoid_zero_vec(stock_ss(obsstock__x))))**2), list(
            alpha = alpha,
            beta = beta))
}

g3l_catchdistribution_surveyindices_linear <- function (alpha = 0, beta = 1) {
    f_substitute(~sum((alpha +
        beta * stock_ss(modelstock__x) -
        stock_ss(obsstock__x))**2), list(
            alpha = alpha,
            beta = beta))
}

# Compare numbers caught by fleets to observation data
# - obs_data: Real-world observations. data.frame containing colums:
#    - year
#    - (optional) step (otherwise data summed over the current year)
#    - (optional) area (otherwise all areas summed)
#    - (optional) age (otherwise all ages summed)
#    - length
#    - number - expected number for given combination (optional, but must provide one)
#    - weight - expected total biomass for given combination (optional, but must provide one)
# - fleets: Gather catch from these fleets
# - stocks: Gather catch (by fleets) for these stocks
# - function_f: Comparison function to compare stock_ss(modelstock__x) & stock_ss(obsstock__x)
# - weight: Weighting of parameter in final nll
g3l_catchdistribution <- function (nll_name, obs_data, fleets, stocks, function_f, missing_val = 0, area_group = NULL, weight = 1.0, run_at = 10) {
    out <- new.env(parent = emptyenv())

    # Convert data to stocks
    ld <- g3l_likelihood_data(nll_name, obs_data, missing_val = missing_val, area_group = area_group)
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
        out[[step_id(run_at, 'g3l_catchdistribution', nll_name, 1, prey_stock)]] <- g3_step(f_substitute(~{
            debug_label("g3l_catchdistribution: Collect catch from ", fleet_stock, "/", prey_stock, " for ", nll_name)
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
            prey_stock__fleet_stock = as.symbol(paste0('prey_stock__', fleet_stock$name)))))

        # Fix-up stock intersection, add in stockidx_f
        out[[step_id(run_at, 'g3l_catchdistribution', nll_name, 1, prey_stock)]] <- f_substitute(out[[step_id(run_at, 'g3l_catchdistribution', nll_name, 1, prey_stock)]], list(
            stockidx_f = stockidx_f))
    }

    nllstock <- g3_storage(paste0("nll_cdist_", nll_name))
    nllstock__num <- stock_instance(nllstock, 0)
    nllstock__wgt <- stock_instance(nllstock, 0)
    out[[step_id(run_at, 'g3l_catchdistribution', nll_name, 2)]] <- g3_step(f_substitute(~{
        debug_label("g3l_catchdistribution: Compare ", modelstock, " to ", obsstock)
        if (done_aggregating_f) {
            stock_iterate(modelstock, stock_intersect(obsstock, stock_intersect(nllstock, {
                if (compare_num) g3_with(cur_cdist_nll, number_f, {
                    nll <- nll + (weight) * cur_cdist_nll
                    stock_ss(nllstock__num) <- stock_ss(nllstock__num) + cur_cdist_nll
                    g3_report(nllstock__num)  # TODO: Only on penultimate step?
                })
                if (compare_wgt) g3_with(cur_cdist_nll, biomass_f, {
                    nll <- nll + (weight) * cur_cdist_nll
                    stock_ss(nllstock__wgt) <- stock_ss(nllstock__wgt) + cur_cdist_nll
                    g3_report(nllstock__wgt)  # TODO: Only on penultimate step?
                })
            })))

            # TODO: Need to disable these if there's a time dimension, otherwise we throw away data
            if (compare_num) stock_with(modelstock, modelstock__num[] <- 0)
            if (compare_wgt) stock_with(modelstock, modelstock__wgt[] <- 0)
        }
    }, list(
        done_aggregating_f = ld$done_aggregating_f,
        compare_num = !is.null(ld$number),
        compare_wgt = !is.null(ld$weight),
        number_f = f_substitute(function_f, list(
            modelstock__x = as.symbol('modelstock__num'),
            obsstock__x = as.symbol('obsstock__num'))),
        biomass_f = f_substitute(function_f, list(
            modelstock__x = as.symbol('modelstock__wgt'),
            obsstock__x = as.symbol('obsstock__wgt'))),
        weight = weight)))
    # Fix-up stock intersection: index should be the same as observation
    out[[step_id(run_at, 'g3l_catchdistribution', nll_name, 2)]] <- f_substitute(out[[step_id(run_at, 'g3l_catchdistribution', nll_name, 2)]], list(
        stockidx_f = as.symbol(paste0(modelstock$name, "__stock_idx"))))

    return(as.list(out))
}
