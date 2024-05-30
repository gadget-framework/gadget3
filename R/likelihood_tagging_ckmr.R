g3l_tagging_ckmr <- function (
        nll_name,
        obs_data,
        fleets,
        parent_stocks,
        offspring_stocks,
        weight = substitute(g3_param(n, optimise = FALSE, value = 1), list(n = paste0(nll_name, "_weight"))),
        run_at = g3_action_order$likelihood) {
    stopifnot(is.character(nll_name))
    stopifnot(is.list(fleets) && all(sapply(fleets, g3_is_stock)))
    stopifnot(is.list(parent_stocks) && all(sapply(parent_stocks, g3_is_stock)))
    stopifnot(is.list(offspring_stocks) && all(sapply(offspring_stocks, g3_is_stock)))

    # Convert obsdata into an array
    stopifnot(colnames(obs_data) == c('year', 'parent_age', 'offspring_age', 'mo_pairs'))
    obsdata_pairs_var_name <- paste0(nll_name, '_obspairs')
    assign(obsdata_pairs_var_name, t(array(
        as.integer(c(obs_data$year, obs_data$parent_age, obs_data$offspring_age, obs_data$mo_pairs)),
        dim = c(length(obs_data$year), 4),
        dimnames = list(NULL, c('year', 'parent_age', 'offspring_age', 'mo_pairs')))))

    # Get definition for all stocks provided
    stock_definitions <- function (var_name, stocks) {
        vapply(
            stocks,
            function (s) g3_stock_def(s, var_name),
            numeric(1))
    }

    # TODO: To support multi-area w/migration, we'd need to...
    #     * Split modelhist into the same areas as the stocks
    #     * Track migrations and apply to historical data, cancelling out migration
    #     ...but can't do this until we know how migration works.
    modelhist <- g3_storage(paste0(nll_name, 'model'))
    modelhist <- g3s_age(modelhist,
        min(stock_definitions('minage', c(parent_stocks, offspring_stocks))),
        max(stock_definitions('maxage', c(parent_stocks, offspring_stocks))))
    modelhist <- g3s_modeltime(modelhist, by_year = TRUE)
    modelhist__spawning <- g3_stock_instance(modelhist, 0, desc = "Total number of spawning parents by year, parent age")
    modelhist__spawned <- g3_stock_instance(modelhist, 0, desc = "Total number of offspring by year, parent age")
    modelhist__total <- g3_stock_instance(modelhist, 0, desc = "Total adults (spawning and not), by year, parent age")
    modelhist__catch <- g3_stock_instance(modelhist, 0, desc = "Individuals caught by fleet, by age")

    out <- new.env(parent = emptyenv())
    step_f <- g3_step(~{
        debug_label("g3l_tagging_ckmr: Gather historical record of spawning / spawned stock")
    })
    for (parent_stock in parent_stocks) {
        # TODO: f_concatenate won't do what we want here, environment won't be merged, see R/action_mature.R
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
    for (predstock in fleets) {
        for (prey_stock in c(parent_stocks, offspring_stocks)) {
            # NB: In lockstep with action_predate()
            predprey <- g3s_stockproduct(prey_stock, pred = predstock, ignore_dims = c('pred_area'))
            predprey__cons <- g3_stock_instance(predprey, desc = paste0("Total biomass consumption of ", predprey$name))

            step_f <- f_concatenate(list(step_f, g3_step(f_substitute(~{
                stock_iterate(prey_stock, stock_interact(predstock, stock_intersect(modelhist, stock_with(predprey, {
                    stock_with(predstock, debug_trace("Convert ", predstock, " catch of ", prey_stock, " to numbers, add it to our total"))
                    stock_ss(modelhist__catch) <- stock_ss(modelhist__catch) +
                        stock_reshape(modelhist, stock_ss(predprey__cons) / avoid_zero_vec(stock_ss(prey_stock__wgt)))
                }))))
            }, list(
                end = NULL)))))
        }
    }
    out[[step_id(run_at, 'g3l_tagging', nll_name, 1)]] <- step_f

    nll <- 0.0

    out[[step_id(run_at, 'g3l_tagging', nll_name, 2)]] <- g3_step(f_substitute(~{
        debug_label("g3l_tagging_ckmr: Work out expected pairs and compare to existing data")

        # Iterate over sensible spawning_year / offspring_age / parent_age combinations
        stock_with(modelhist, if (cur_step_final) {
            for (pairs_idx in seq(g3_idx(1), g3_idx(ncol(obsdata_pairs)), by = 1)) if (as_integer(obsdata_pairs[[g3_idx(1), pairs_idx]]) == cur_year) {
                g3_with(
                  parent_age := as_integer(obsdata_pairs[[g3_idx(2), pairs_idx]]),
                  modelhist__parent_idx := g3_idx(parent_age - modelhist__minage + 1),
                  offspring_age := as_integer(obsdata_pairs[[g3_idx(3), pairs_idx]]),
                  modelhist__offspring_idx := g3_idx(offspring_age - modelhist__minage + 1),
                  mopairs := as_integer(obsdata_pairs[[g3_idx(4), pairs_idx]]),
                  # i.e. # spawned per-parent at this time
                  fecundity_of_parents := modelhist__spawned[,modelhist__offspring_idx] / avoid_zero_vec(modelhist__spawning[,modelhist__offspring_idx]),
                  # Convert to a probability using (3.4):-
                  cur_ckmr_p := (fecundity_of_parents[[modelhist__parent_idx]] / modelhist__catch[[modelhist__parent_idx]]) / sum(fecundity_of_parents), {
                    # Pseudo-likelihood as per (4.1)
                    nll <- nll + (weight) * log((mopairs) * unname(cur_ckmr_p))
                    # TODO: Consider (4.2) here too?
                })
            }
        })
    }, list(
        obsdata_pairs = as.symbol(obsdata_pairs_var_name),
        weight = weight)))

    return(as.list(out))
}
