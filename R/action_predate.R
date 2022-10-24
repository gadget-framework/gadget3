g3a_predate_catchability_totalfleet <- function (E) {
    f_substitute(
        ~stock_ss(stock__predby_fleet_stock) * (E / stock_ss(fleet_stock__catch)),
        list(E = E))
}

g3a_predate_catchability_numberfleet <- function (E) {
    f_substitute(
        ~(stock_ss(stock__predby_fleet_stock) / stock_ss(stock__wgt)) * (E / stock_ss(fleet_stock__catchnum)),
        list(E = E))
}

g3a_predate_catchability_linearfleet <- function (E) {
    f_substitute(
        ~E * cur_step_size * stock_ss(stock__predby_fleet_stock),
        list(E = E))
}

g3a_predate_catchability_effortfleet <- function (catchability_fs, E) {
    f_substitute(
        ~catchability_fs * E * cur_step_size * stock_ss(stock__predby_fleet_stock),
        list(
            catchability_fs = list_to_stock_switch(catchability_fs),
            E = E))
}

g3a_predate_catchability_quotafleet <- function (quota_table, E, sum_stocks = list(), recalc_f = NULL) {
    stopifnot(is.data.frame(quota_table) && identical(names(quota_table), c('biomass', 'quota')))
    stopifnot(nrow(quota_table) > 0 && is.infinite(quota_table[nrow(quota_table), 'biomass']))

    stopifnot(is.list(sum_stocks) && all(sapply(sum_stocks, g3_is_stock)))
    stopifnot(is.null(recalc_f) || rlang::is_formula(recalc_f))

    # Generate code to produce biomass sum
    if (length(sum_stocks) == 0) {
        biomass_c <- quote(sum(stock__num * stock__wgt))
    } else {
        biomass_c <- 0
        for (stock in sum_stocks) {
            biomass_c <- substitute(stock_with(s, sum(s__num * s__wgt)) + biomass_c, list(
                s = as.symbol(stock$name),
                s__num = as.symbol(paste0(stock$name, "__num")),
                s__wgt = as.symbol(paste0(stock$name, "__wgt")),
                biomass_c = biomass_c))
        }
    }

    # Make tertiary condition encoding quota
    for (i in rev(seq_len(nrow(quota_table)))) {
        if (i == nrow(quota_table)) {
            # NB: [[1]] picks out value from a list data.frame col (e.g. for formula)
            quota_f <- quota_table[i, 'quota'][[1]]
        } else {
            quota_f <- f_substitute(~if (biomass_c < cond) val else quota_f, list(
                biomass_c = biomass_c,
                cond = quota_table[i, 'biomass'],
                val = quota_table[i, 'quota'][[1]],
                quota_f = quota_f))
        }
    }

    if (!is.null(recalc_f)) {
        quota_var_name <- paste0("stock__quotafleet_", unique_action_name())
        # NB: This should remain global to model
        assign(quota_var_name, structure(
            0.0,
            desc = "Current quota of stock"))
        quota_f <- f_substitute(~(quota_var <- if (recalc_f) quota_f else quota_var), list(
            recalc_f = recalc_f,
            quota_f = quota_f,
            quota_var = as.symbol(quota_var_name)))
    }

    f_substitute(
        ~quota_f * E * cur_step_size * stock_ss(stock__predby_fleet_stock), list(
        quota_f = quota_f,
        E = E))
}

g3a_predate_fleet <- function (fleet_stock,
                                    prey_stocks,
                                    suitabilities,
                                    catchability_f,
                                    overconsumption_f = quote( logspace_add_vec(stock__consratio * -1e3, 0.95 * -1e3) / -1e3 ),
                                    run_f = ~TRUE,
                                    run_at = 3) {
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    # Variables used:
    # stock__predby_fleet_stock: Biomass of (prey_stock) caught by (fleet_stock) (prey matrix)
    # fleet_stock__catch: Biomass caught by that fleet (fleet matrix, i.e. area)
    # stock__totalpredate: Biomass of total consumed (prey_stock) (prey matrix)
    # stock__consratio: Ratio of prey_stock__totalpredate / (current biomass), capped by overconsumption rule
    fleet_stock__catch <- stock_instance(fleet_stock, desc = "Biomass caught by fleet (fleet matrix, i.e. area)")
    fleet_stock__catchnum <- stock_instance(fleet_stock, desc = "Individuals caught by fleet (fleet matrix, i.e. area)")

    # For each prey stock...
    for (stock in prey_stocks) {
        # Create variable to store biomass of stock caught
        fleet_stock_var <- as.symbol(paste0('stock__predby_', fleet_stock$name))
        suit_var <- as.symbol(paste0('stock__suit_', fleet_stock$name))
        stock__num <- stock_instance(stock, 0)
        stock__wgt <- stock_instance(stock, 1)
        assign(as.character(fleet_stock_var), stock_instance(stock, desc = paste0("Total biomass of ", stock$name, " captured by ", fleet_stock$name)))
        assign(as.character(suit_var), stock_instance(stock, 0, desc = paste("Suitability of ", stock$name, " for ", fleet_stock$name)))
        stock__totalpredate <- stock_instance(stock, desc = paste0("Biomass of total consumed ", stock$name, " (prey matrix)"))
        stock__overconsumption <- structure(0.0, desc = paste0("Total overconsumption of ", stock$name))
        stock__consratio <- stock_instance(stock, desc = paste0("Ratio of ", stock$name, "__totalpredate / (current biomass), capped by overconsumption rule"))

        # Make sure counter for this fleet is zeroed
        # NB: We only have one of these per-fleet (we replace it a few times though)
        out[[step_id(run_at, 0, fleet_stock)]] <- g3_step(f_substitute(~{
            debug_trace("Zero biomass-caught counter for ", fleet_stock)
            stock_with(fleet_stock, fleet_stock__catch[] <- 0)
            if (catchnum_required) stock_with(fleet_stock, fleet_stock__catchnum[] <- 0)
        }, list(
            catchnum_required = "fleet_stock__catchnum" %in% all.vars(catchability_f))))

        # Make sure the counter for this prey is zeroed
        # NB: We only have one of these per-prey (we replace it a few times though)
        out[[step_id(run_at, 0, stock)]] <- g3_step(~{
            debug_trace("Zero total predation counter for ", stock)
            stock_with(stock, stock__totalpredate[] <- 0)
        })

        # Main predation step, iterate over prey and pull out everything this fleet needs
        out[[step_id(run_at, 1, fleet_stock, stock, action_name)]] <- g3_step(f_substitute(~{
            debug_label("g3a_predate_fleet for ", stock)
            debug_trace("Zero ", fleet_stock, "-", stock, " biomass-consuming counter")
            stock_with(stock, stock__predby_fleet_stock[] <- 0)

            stock_iterate(stock, stock_interact(fleet_stock, if (run_f) {
                debug_trace("Collect all suitable ", stock, " biomass for ", fleet_stock)
                stock_ss(stock__suit_fleet_stock) <- suit_f
                stock_ss(stock__predby_fleet_stock) <- (stock_ss(stock__suit_fleet_stock)
                    * stock_ss(stock__num)
                    * stock_ss(stock__wgt))
                stock_ss(fleet_stock__catch) <- (stock_ss(fleet_stock__catch)
                    + sum(stock_ss(stock__predby_fleet_stock)))
                if (catchnum_required) stock_ss(fleet_stock__catchnum) <- (stock_ss(fleet_stock__catchnum)
                    + sum(stock_ss(stock__suit_fleet_stock) * stock_ss(stock__num)))
            }, prefix = 'fleet'))
        }, list(
            catchnum_required = "fleet_stock__catchnum" %in% all.vars(catchability_f),
            suit_f = list_to_stock_switch(suitabilities),
            run_f = run_f,
            stock__suit_fleet_stock = suit_var,
            stock__predby_fleet_stock = fleet_stock_var)))

        # After all prey is collected (not just this stock), scale by total expected, update catch params
        out[[step_id(run_at, 2, fleet_stock, stock, action_name)]] <- g3_step(f_substitute(~{
            debug_trace("Scale ", fleet_stock, " catch of ", stock, " by total expected catch")
            stock_iterate(stock, stock_interact(fleet_stock, if (run_f) {
                # NB: In gadget2, E == wanttoeat, stock_ss(fleet__catch) == totalcons[inarea][predl]
                stock_ss(stock__predby_fleet_stock) <- catchability_f
                stock_ss(stock__totalpredate) <- stock_ss(stock__totalpredate) + stock_ss(stock__predby_fleet_stock)
                # NB: In gadget2, stock__predby_fleet_stock == (*cons[inarea][prey])[predl], totalpredator.cc#68
            }, prefix = 'fleet'))
        }, list(
            catchability_f = f_substitute(catchability_f, list(stock__predby_fleet_stock = fleet_stock_var)),
            run_f = run_f,
            stock__predby_fleet_stock = fleet_stock_var)))

        # Scale stock__predby_fleet_stock to be out of stock__totalpredate (so we can apply post-overstocking amount)
        out[[step_id(run_at, 3, fleet_stock, stock)]] <- g3_step(f_substitute(~{
            debug_trace("Temporarily convert to being proportion of totalpredate")
            stock_with(stock, {
                stock__predby_fleet_stock <- stock__predby_fleet_stock / avoid_zero_vec(stock__totalpredate)
            })
        }, list(
            stock__predby_fleet_stock = fleet_stock_var)))

        # Overconsumption: Prey adjustments
        out[[step_id(run_at, 4, stock)]] <- g3_step(f_substitute(~{
            debug_trace("Calculate ", stock, " overconsumption coefficient")
            stock_with(stock, {
                # NB: See prey.cc::208
                # stock__consratio == ratio
                stock__consratio <- stock__totalpredate / avoid_zero_vec(stock__num * stock__wgt)

                # Overconsumption rule
                stock__consratio <- overconsumption_f
                if (strict_mode) {
                    stock_assert(
                        all(stock__consratio <= 1),
                        "g3a_predate_fleet: ", stock, "__consratio <= 1, can't consume more fish than currently exist")
                }

                debug_trace("Apply overconsumption to prey")
                stock__overconsumption <- sum(stock__totalpredate)
                stock__totalpredate <- (stock__num * stock__wgt) * stock__consratio
                stock__overconsumption <- stock__overconsumption - sum(stock__totalpredate)
                stock__num <- stock__num * (1 - stock__consratio)
            })
        }, list(
            overconsumption_f = overconsumption_f)))

        # Overconsumption: Zero catch counter again, so we can sum adjusted values this time
        out[[step_id(run_at, 5, fleet_stock)]] <- g3_step(f_substitute(~{
            debug_trace("Zero ", fleet_stock, " catch before working out post-adjustment value")
            stock_with(fleet_stock, fleet_stock__catch[] <- 0)
            if (catchnum_required) stock_with(fleet_stock, fleet_stock__catchnum[] <- 0)
        }, list(
            catchnum_required = "fleet_stock__catchnum" %in% all.vars(catchability_f))))

        # Overconsumption: if we went over the limit, scale back, remove from stock
        out[[step_id(run_at, 5, fleet_stock, stock)]] <- g3_step(f_substitute(~{
            stock_with(stock, if (run_f) {
                debug_trace("Revert to being total biomass (applying overconsumption in process)")
                # Rough equivalent fleetpreyaggregator.cc:109
                stock__predby_fleet_stock <- stock__predby_fleet_stock * stock__totalpredate
            })

            debug_trace("Update total catch")
            stock_iterate(stock, stock_interact(fleet_stock, {
                stock_ss(fleet_stock__catch) <- (stock_ss(fleet_stock__catch)
                    + sum(stock_ss(stock__predby_fleet_stock)))
                if (catchnum_required) stock_ss(fleet_stock__catchnum) <- (stock_ss(fleet_stock__catchnum)
                    + sum(stock_ss(stock__predby_fleet_stock) / stock_ss(stock__wgt)))
            }, prefix = 'fleet'))
        }, list(
            catchnum_required = "fleet_stock__catchnum" %in% all.vars(catchability_f),
            run_f = run_f,
            stock__predby_fleet_stock = fleet_stock_var)))
    }

    return(as.list(out))
}
# NB:
# * All consumption is in biomass, conversion done in Prey::addNumbersConsumption

# Wrapper for old interface
g3a_predate_totalfleet <- function (fleet_stock,
                                    prey_stocks,
                                    suitabilities,
                                    amount_f,
                                    overconsumption_f = quote( logspace_add_vec(stock__consratio * -1e3, 0.95 * -1e3) / -1e3 ),
                                    run_f = ~TRUE,
                                    run_at = 3) {
    g3a_predate_fleet(fleet_stock, prey_stocks, suitabilities, overconsumption_f = overconsumption_f, run_f = run_f, run_at = run_at,
        catchability_f = g3a_predate_catchability_totalfleet(amount_f))
}
