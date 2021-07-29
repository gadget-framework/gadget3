g3l_tagging_ckmr <- function (
        nll_name,
        obs_data,
        fleets = list(),
        parent_stocks,
        offspring_stocks,
        weight = substitute(g3_param(n, optimise = FALSE, value = 1), list(n = paste0(nll_name, "_weight"))),
        run_at = 10) {
    stopifnot(is.character(nll_name))
    stopifnot(is.list(fleets) && all(sapply(fleets, g3_is_stock)))
    stopifnot(is.list(parent_stocks) && all(sapply(parent_stocks, g3_is_stock)))
    stopifnot(is.list(offspring_stocks) && all(sapply(offspring_stocks, g3_is_stock)))
    
    # Get definition for all stocks provided
    stock_definitions <- function (var_name, stocks) {
        vapply(
            stocks,
            function (s) stock_definition(s, var_name),
            numeric(1))
    }

    # TODO: To support multi-area w/migration, we'd need to...
    #     * Split modelhist into the same areas as the stocks
    #     * Track migrations and apply to historical data, cancelling out migration
    #     ...but can't do this until we know how migration works.
    modelhist <- g3_storage(paste0(nll_name, 'model'))
    modelhist <- g3s_age(modelhist,
        min(stock_definitions('stock__minage', c(parent_stocks, offspring_stocks))),
        max(stock_definitions('stock__maxage', c(parent_stocks, offspring_stocks))))
    modelhist <- g3s_modeltime(modelhist, by_year = TRUE)
    modelhist__spawning <- stock_instance(modelhist, 0, desc = "Total number of spawning parents by year, parent age")
    modelhist__spawned <- stock_instance(modelhist, 0, desc = "Total number of offspring by year, parent age")
    modelhist__total <- stock_instance(modelhist, 0, desc = "Total adults (spawning and not), by year, parent age")
    modelhist__catch <- stock_instance(modelhist, 0, desc = "Individuals caught by fleet, by age")

    out <- new.env(parent = emptyenv())
    step_f <- g3_step(~{
        debug_label("g3l_tagging_ckmr: Gather historical record of spawning / spawned stock")
    })
    for (parent_stock in parent_stocks) {
        step_f <- f_concatenate(list(step_f, g3_step(~{
            if (sum(stock_with(parent_stock, parent_stock__spawningnum)) > 0) {  # i.e. currently in a spawning step. TODO: Safe? Better way?
                stock_iterate(parent_stock, stock_intersect(modelhist, {
                    debug_trace("Collect total numbers of spawning / spawned / total for year")
                    stock_ss(modelhist__spawning) <- stock_ss(modelhist__spawning) +
                        stock_reshape(modelhist, stock_ss(parent_stock__spawningnum))
                    # TODO: We don't actually count offspring_stocks. Kinda stupid for these to be different, but should make this obvious.
                    stock_ss(modelhist__spawned) <- stock_ss(modelhist__spawned) + 
                        stock_reshape(modelhist, stock_ss(parent_stock__offspringnum))
                    stock_ss(modelhist__total) <- stock_ss(modelhist__total) +
                        stock_reshape(modelhist, stock_ss(parent_stock__num))
                }))
            }
        })))
    }
    for (fleet_stock in fleets) {
        fleet_stock_var <- as.symbol(paste0('prey_stock__', fleet_stock$name))

        for (prey_stock in c(parent_stocks, offspring_stocks)) {
            step_f <- f_concatenate(list(step_f, g3_step(f_substitute(~{
                stock_iterate(prey_stock, stock_intersect(modelhist, {
                    stock_with(fleet_stock, debug_trace("Convert ", fleet_stock, " catch of ", prey_stock, " to numbers, add it to our total"))
                    stock_ss(modelhist__catch) <- stock_ss(modelhist__catch) +
                        stock_reshape(modelhist, stock_ss(prey_stock__fleet_stock) / avoid_zero_vec(stock_ss(prey_stock__wgt)))
                }))
            }, list(
                prey_stock__fleet_stock = fleet_stock_var)))))
        }
    }
    out[[step_id(run_at, 'g3l_tagging', nll_name, 1)]] <- step_f

    out[[step_id(run_at, 'g3l_tagging', nll_name, 2)]] <- g3_step(~{
        debug_label("g3l_tagging_ckmr: Work out expected pairs and compare to existing data")

        if (cur_step_final) stock_with(modelhist, {
            for (spawning_year in seq(cur_year, start_year, by = -1)) g3_with(offspring_age := cur_year - start_year, {
                for (parent_age in seq(offspring_age, modelhist__maxage, by = 1)) {
                    g3_with(
                        # parents_caught: Number of spawning parents (spawning_year) event
                        parents_caught := modelhist__catch[parent_age] * (modelhist__spawning[,g3_idx(cur_year - start_year + 1)] / avoid_zero_vec(modelhist__total[,g3_idx(cur_year - start_year + 1)])),
                        # offspring_per_parent: Number of offspring caught per parent
                        offspring_per_parent := (modelhist__catch[offspring_age] / avoid_zero_vec(modelhist__total[,g3_idx(cur_year - start_year + 1)])) * 
                                                (modelhist__spawned[,g3_idx(cur_year - start_year + 1)] / avoid_zero_vec(modelhist__spawning[,g3_idx(cur_year - start_year + 1)])),
                        {
                            # TODO: Do the actual comparison, likelihood of getting x parents with y offspring?
                    })
                }
            })
        })
    })

    return(as.list(out))
}
