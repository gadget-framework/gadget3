g3l_tagging_ckmr <- function (
        nll_name,
        obs_data,
        parent_stocks,
        offspring_stocks,
        weight = g3_parameterized(paste0(nll_name, "_weight"),
            optimise = FALSE, value = 1),
        run_at = g3_action_order$likelihood) {
    stopifnot(is.character(nll_name))
    stopifnot(is.list(parent_stocks) && all(sapply(parent_stocks, g3_is_stock)))
    stopifnot(is.list(offspring_stocks) && all(sapply(offspring_stocks, g3_is_stock)))

    # Convert obsdata into an array
    stopifnot(colnames(obs_data) == c('year', 'parent_age', 'offspring_age', 'mo_pairs', 'n_comparisons'))
    obsdata_pairs_var_name <- paste0(nll_name, '_obspairs')
    assign(obsdata_pairs_var_name, t(array(
        as.integer(c(obs_data$year, obs_data$parent_age, obs_data$offspring_age, obs_data$mo_pairs, obs_data$n_comparisons)),
        dim = c(length(obs_data$year), 5),
        dimnames = list(NULL, c('year', 'parent_age', 'offspring_age', 'mo_pairs', 'n_comparisons')))))

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

    out <- new.env(parent = emptyenv())
    step_f <- g3_step(~{
        debug_label("g3l_tagging_ckmr: Gather historical record of spawning / spawned stock")
    })
    for (parent_stock in parent_stocks) {
        # TODO: f_concatenate won't do what we want here, environment won't be merged, see R/action_mature.R
        step_f <- f_concatenate(list(step_f, g3_step(~{
            if (sum(stock_with(parent_stock, parent_stock__spawningnum)) > 0) {  # i.e. currently in a spawning step. TODO: Safe? Better way?
                stock_iterate(parent_stock, stock_intersect(modelhist, {
                    debug_trace("Collect total numbers of spawning / spawned for year")
                    stock_ss(modelhist__spawning) <- stock_ss(modelhist__spawning) +
                        stock_reshape(modelhist, stock_ss(parent_stock__spawningnum))
                    # TODO: We don't actually count offspring_stocks. Kinda stupid for these to be different, but should make this obvious.
                    stock_ss(modelhist__spawned) <- stock_ss(modelhist__spawned) +
                        stock_reshape(modelhist, stock_ss(parent_stock__offspringnum))
                }))
            }
        })))
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
                  offspring_age := as_integer(obsdata_pairs[[g3_idx(3), pairs_idx]]),
                  birth_parent_age_idx := g3_idx(parent_age - offspring_age - modelhist__minage + 1),
                  modelhist__offspring_idx := g3_idx(cur_year - offspring_age - start_year + 1L),
                  mopairs := as_integer(obsdata_pairs[[g3_idx(4), pairs_idx]]),
                  n_comparisons := as_integer(obsdata_pairs[[g3_idx(5), pairs_idx]]),
                  # i.e. # spawned per-parent at this time, at their age in the birth year
                  fecundity_of_parents := modelhist__spawned[,modelhist__offspring_idx] / avoid_zero(modelhist__spawning[,modelhist__offspring_idx]),
                  # Convert to a probability using (3.4):-
                  pr_pop_bya := fecundity_of_parents[[birth_parent_age_idx]] / avoid_zero(sum(modelhist__spawned[, modelhist__offspring_idx])), {
                    nll <- nll - (weight) * dpois(mopairs, n_comparisons * unname(pr_pop_bya), log = TRUE)
                })
            }
        })
    }, list(
        obsdata_pairs = as.symbol(obsdata_pairs_var_name),
        weight = weight)))

    return(as.list(out))
}
