g3a_predate_totalfleet <- function (fleet_stock,
                                    prey_stocks,
                                    suitabilities,
                                    amount_f,
                                    overconsumption_f = ~0.96 - logspace_add_vec((0.96 - prey_stock__consratio) * 100, 0.96) / 100,
                                    run_at = 3) {
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    # Variables used:
    # prey_stock__fleet_stock: Biomass of (prey_stock) caught by (fleet_stock) (prey matrix)
    # fleet_stock__catch: Biomass caught by that fleet (fleet matrix, i.e. area)
    # prey_stock__totalpredate: Biomass of total consumed (prey_stock) (prey matrix)
    # prey_stock__consratio: Ratio of prey_stock__totalpredate / (current biomass), capped by overconsumption rule
    fleet_stock__catch <- stock_instance(fleet_stock, desc = "Biomass caught by fleet (fleet matrix, i.e. area)")

    # For each prey stock...
    for (prey_stock in prey_stocks) {
        if (is.null(suitabilities[[prey_stock$name]])) stop("No suitability function found for ", prey_stock$name)

        # Create variable to store biomass of stock caught
        fleet_stock_var <- as.symbol(paste0('prey_stock__', fleet_stock$name))
        suit_var <- as.symbol(paste0('prey_stock__suit_', fleet_stock$name))
        prey_stock__num <- stock_instance(prey_stock)
        prey_stock__wgt <- stock_instance(prey_stock)
        assign(as.character(fleet_stock_var), stock_instance(prey_stock))
        assign(as.character(suit_var), stock_instance(prey_stock, 0))
        prey_stock__totalpredate <- stock_instance(prey_stock, desc = paste0("Biomass of total consumed ", prey_stock$name, " (prey matrix)"))
        prey_stock__overconsumption <- structure(0.0, desc = paste0("Total overconsumption of ", prey_stock$name))
        prey_stock__consratio <- stock_instance(prey_stock, desc = "Ratio of prey_stock__totalpredate / (current biomass), capped by overconsumption rule")

        # Make sure counter for this fleet is zeroed
        # NB: We only have one of these per-fleet (we replace it a few times though)
        out[[step_id(run_at, 0, fleet_stock)]] <- g3_step(~{
            debug_trace("Zero biomass-caught counter for ", fleet_stock)
            stock_with(fleet_stock, fleet_stock__catch[] <- 0)
        })

        # Make sure the counter for this prey is zeroed
        # NB: We only have one of these per-prey (we replace it a few times though)
        out[[step_id(run_at, 0, prey_stock)]] <- g3_step(~{
            debug_trace("Zero total predation counter for ", prey_stock)
            stock_with(prey_stock, prey_stock__totalpredate[] <- 0)
        })

        # Main predation step, iterate over prey and pull out everything this fleet needs
        out[[step_id(run_at, 1, fleet_stock, prey_stock, action_name)]] <- g3_step(f_substitute(~{
            debug_label("g3a_predate_totalfleet for ", prey_stock)
            debug_trace("Zero ", fleet_stock, "-", prey_stock, " biomass-consuming counter")
            stock_with(prey_stock, prey_stock__fleet_stock[] <- 0)

            stock_iterate(prey_stock, stock_intersect(fleet_stock, {
                debug_trace("Collect all suitable ", prey_stock, " biomass for ", fleet_stock)
                stock_ss(prey_stock__suit_fleet_stock) <- suit_f
                stock_ss(prey_stock__fleet_stock) <- (stock_ss(prey_stock__suit_fleet_stock)
                    * stock_ss(prey_stock__num)
                    * stock_ss(prey_stock__wgt))
                stock_ss(fleet_stock__catch) <- (stock_ss(fleet_stock__catch)
                    + sum(stock_ss(prey_stock__fleet_stock)))
            }))
        }, list(
            suit_f = suitabilities[[prey_stock$name]],
            prey_stock__suit_fleet_stock = suit_var,
            prey_stock__fleet_stock = fleet_stock_var)))

        # After all prey is collected (not just this stock), scale by total expected, update catch params
        out[[step_id(run_at, 2, fleet_stock, prey_stock, action_name)]] <- g3_step(f_substitute(~{
            debug_trace("Scale ", fleet_stock, " catch of ", prey_stock, " by total expected catch")
            stock_iterate(prey_stock, stock_intersect(fleet_stock, {
                # NB: In gadget2, predate_totalfleet_E == wanttoeat, stock_ss(fleet__catch) == totalcons[inarea][predl]
                g3_with(predate_totalfleet_E, amount_f, {
                    stock_ss(prey_stock__fleet_stock) <- stock_ss(prey_stock__fleet_stock) * (predate_totalfleet_E / stock_ss(fleet_stock__catch))
                })
                stock_ss(prey_stock__totalpredate) <- stock_ss(prey_stock__totalpredate) + stock_ss(prey_stock__fleet_stock)
                # NB: In gadget2, prey_stock__fleet_stock == (*cons[inarea][prey])[predl], totalpredator.cc#68
            }))
        }, list(
            amount_f = amount_f,
            prey_stock__fleet_stock = fleet_stock_var)))

        # Scale prey_stock__fleet_stock to be out of prey_stock__totalpredate (so we can apply post-overstocking amount)
        out[[step_id(run_at, 3, fleet_stock, prey_stock)]] <- g3_step(f_substitute(~{
            debug_trace("Temporarily convert to being proportion of totalpredate")
            stock_with(prey_stock, {
                prey_stock__fleet_stock <- prey_stock__fleet_stock / avoid_zero_vec(prey_stock__totalpredate)
            })
        }, list(
            prey_stock__fleet_stock = fleet_stock_var)))

        # Overconsumption: Prey adjustments
        out[[step_id(run_at, 4, prey_stock)]] <- g3_step(f_substitute(~{
            debug_trace("Calculate ", prey_stock, " overconsumption coefficient")
            stock_with(prey_stock, {
                # NB: See prey.cc::208
                # prey_stock__consratio == ratio
                prey_stock__consratio <- prey_stock__totalpredate / avoid_zero_vec(prey_stock__num * prey_stock__wgt)

                # Overconsumption rule
                prey_stock__consratio <- overconsumption_f
                if (strict_mode) {
                    stock_assert(
                        all(prey_stock__consratio <= 1),
                        "g3a_predate_totalfleet: ", prey_stock, "__consratio <= 1, can't consume more fish than currently exist")
                }

                debug_trace("Apply overconsumption to prey")
                prey_stock__overconsumption <- sum(prey_stock__totalpredate)
                prey_stock__totalpredate <- (prey_stock__num * prey_stock__wgt) * prey_stock__consratio
                prey_stock__overconsumption <- prey_stock__overconsumption - sum(prey_stock__totalpredate)
                prey_stock__num <- prey_stock__num * (1 - prey_stock__consratio)
            })
        }, list(
            overconsumption_f = overconsumption_f)))

        # Overconsumption: Zero catch counter again, so we can sum adjusted values this time
        out[[step_id(run_at, 5, fleet_stock)]] <- g3_step(~{
            debug_trace("Zero ", fleet_stock, " catch before working out post-adjustment value")
            stock_with(fleet_stock, fleet_stock__catch[] <- 0)
        })

        # Overconsumption: if we went over the limit, scale back, remove from prey_stock
        out[[step_id(run_at, 5, fleet_stock, prey_stock)]] <- g3_step(f_substitute(~{
            stock_with(prey_stock, {
                debug_trace("Revert to being total biomass (applying overconsumption in process)")
                # Rough equivalent fleetpreyaggregator.cc:109
                prey_stock__fleet_stock <- prey_stock__fleet_stock * prey_stock__totalpredate
            })

            debug_trace("Update total catch")
            stock_iterate(prey_stock, stock_intersect(fleet_stock, {
                stock_ss(fleet_stock__catch) <- (stock_ss(fleet_stock__catch)
                    + sum(stock_ss(prey_stock__fleet_stock)))
            }))
        }, list(
            prey_stock__fleet_stock = fleet_stock_var)))
    }

    return(as.list(out))
}
# NB:
# * All consumption is in biomass, conversion done in Prey::addNumbersConsumption
