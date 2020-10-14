call_append <- function (call, extra) as.call(c(as.list(call), extra))

g3l_catchdistribution_sumofsquares <- function (over = c('area')) {
    f_substitute(~sum((
        stock_ss(modelstock__num) / logspace_add_vec(sum(modelstock_total_f), 0.00001) -
        stock_ss(obsstock__num) / logspace_add_vec(sum(obsstock_total_f), 0.00001)) ** 2), list(
            modelstock_total_f = call_append(quote(stock_ssinv(modelstock__num)), over),
            obsstock_total_f = call_append(quote(stock_ssinv(obsstock__num, 'time')), over)))
}

g3l_catchdistribution_multinomial <- function () {
    ~2 * (lgamma(1 + sum(stock_ss(obsstock__num))) -
        sum(lgamma_vec(1 + stock_ss(obsstock__num))) +
        sum(stock_ss(obsstock__num) * log(
            stock_ss(modelstock__num) / sum(stock_ss(modelstock__num))))
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
            stock_ss(modelstock__num) / sum(modelstock_total_f) -
                stock_ss(obsstock__num) / sum(obsstock_total_f),
            rho_f,
            sigma_f), list(
                modelstock_total_f = call_append(quote(stock_ssinv(modelstock__num)), over),
                obsstock_total_f = call_append(quote(stock_ssinv(obsstock__num, 'time')), over),
                rho_f = rho_f,
                sigma_f = sigma_f))
}

g3l_catchdistribution_surveyindices_log <- function (alpha = 0, beta = 1) {
    f_substitute(~sum((alpha +
        beta * log(logspace_add_vec(stock_ss(modelstock__num),0.00001)) -
        log(logspace_add_vec(stock_ss(obsstock__num),0.00001)))**2), list(
            alpha = alpha,
            beta = beta))
}

g3l_catchdistribution_surveyindices_linear <- function (alpha = 0, beta = 1) {
    f_substitute(~sum((alpha +
        beta * stock_ss(modelstock__num) -
        stock_ss(obsstock__num))**2), list(
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
#    - number - expected number for given combination
# - fleets: Gather catch from these fleets
# - stocks: Gather catch (by fleets) for these stocks
# - function_f: Comparison function to compare stock_ss(modelstock__num) & stock_ss(obsstock__num)
# - weight: Weighting of parameter in final nll
g3l_catchdistribution <- function (nll_name, obs_data, fleets, stocks, function_f, missing_val = 0, area_group = NULL, weight = 1.0, run_at = 10) {
    out <- new.env(parent = emptyenv())

    # Convert data to stocks
    ld <- g3l_likelihood_data(nll_name, obs_data, missing_val = missing_val, area_group = area_group)
    modelstock <- ld$modelstock
    obsstock <- ld$obsstock
    modelstock__num <- stock_instance(modelstock)
    obsstock__num <- stock_instance(obsstock)
    assign(paste0(nll_name, '_number'), ld$number)

    out[[step_id(run_at, nll_name, 0)]] <- stock_step(f_substitute(~{
        comment(step_comment)
        if (cur_time == 0) {
            # Populate stock with data
            stock_with(obsstock, obsstock__num[] <- obsnumber)
            stock_with(modelstock, modelstock__num[] <- 0)
        }
    }, list(
        step_comment = paste0("Initial data / reset observations for ", nll_name),
        obsnumber = as.symbol(paste0(nll_name, '_number')))))
    
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
        out[[step_id(run_at, nll_name, 1, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("Collect catch from ", fleet_stock, "/", prey_stock, " for ", modelstock)
            stock_iterate(prey_stock, stock_intersect(modelstock, {
                # Take prey_stock__fleet_stock weight, convert to individuals, add to our count
                stock_ss(modelstock__num) <- stock_ss(modelstock__num) +
                    stock_reshape(modelstock, stock_ss(prey_stock__fleet_stock) / logspace_add_vec(stock_ss(prey_stock__wgt), 0.00001))
            }))
        }, list(
            # Find catch from predation step
            prey_stock__fleet_stock = as.symbol(paste0('prey_stock__', fleet_stock$name)))))

        # Fix-up stock intersection, add in stockidx_f
        out[[step_id(run_at, nll_name, 1, prey_stock)]] <- f_substitute(out[[step_id(run_at, nll_name, 1, prey_stock)]], list(
            stockidx_f = stockidx_f))
    }

    out[[step_id(run_at, nll_name, 2)]] <- stock_step(f_substitute(~{
        if (done_aggregating_f) {
            stock_comment("Compare ", modelstock, " to ", obsstock)
            stock_iterate(modelstock, stock_intersect(obsstock, {
                nll <- nll + weight * function_f
            }))
            stock_with(modelstock, modelstock__num[] <- 0)
        }
    }, list(
        done_aggregating_f = ld$done_aggregating_f,
        weight = weight,
        function_f = function_f)))
    # Fix-up stock intersection: index should be the same as observation
    out[[step_id(run_at, nll_name, 2)]] <- f_substitute(out[[step_id(run_at, nll_name, 2)]], list(
        stockidx_f = as.symbol(paste0(modelstock$name, "__stock_idx"))))

    return(as.list(out))
}
