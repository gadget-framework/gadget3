g3a_predate_totalfleet <- function (fleet_stock, prey_stocks, suitabilities, amount_f, run_at = 3) {
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()
    predate_totalfleet_E <- 0.0

    # Variables used:
    # prey_stock__fleet_stock: Biomass selected for that fleet/prey combination (prey matrix)
    # fleet_stock__catch: Biomass caught by that fleet (fleet matrix, i.e. area)
    # prey_stock__totalpredate: Biomass predated for that stock (prey matrix)
    # prey_stock__overconsumption: Factor to scale prey_stock__fleet_stock: / fleet_stock__catch / prey_stock__totalpredate
    #                              to bring it down to 95% of current stock
    fleet_stock__catch <- stock_instance(fleet_stock)

    # For each prey stock...
    for (prey_stock in prey_stocks) {
        # Create variable to store biomass of stock caught
        fleet_stock_var <- as.symbol(paste0('prey_stock__', fleet_stock$name))
        prey_stock__num <- stock_instance(prey_stock)
        prey_stock__wgt <- stock_instance(prey_stock)
        assign(as.character(fleet_stock_var), stock_instance(prey_stock))
        prey_stock__totalpredate <- stock_instance(prey_stock)
        prey_stock__overconsumption <- stock_instance(prey_stock)

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
                stock_ss(prey_stock__fleet_stock) <- ((suit_f)
                    * stock_ss(prey_stock__num)
                    * stock_ss(prey_stock__wgt))
                stock_ss(fleet_stock__catch) <- (stock_ss(fleet_stock__catch)
                    + sum(stock_ss(prey_stock__fleet_stock)))
            }))
        }, list(
            suit_f = suitabilities[[prey_stock$name]],
            prey_stock__fleet_stock = fleet_stock_var)))

        # After all prey is collected (not just this stock), scale by total expected, update catch params
        out[[step_id(run_at, 2, fleet_stock, prey_stock, action_name)]] <- g3_step(f_substitute(~{
            debug_trace("Scale ", fleet_stock, " catch of ", prey_stock, " by total expected catch")
            stock_iterate(prey_stock, stock_intersect(fleet_stock, {
                # NB: In gadget2, predate_totalfleet_E == wanttoeat, stock_ss(fleet__catch) == totalcons[inarea][predl
                predate_totalfleet_E <- (amount_f)
                stock_ss(prey_stock__fleet_stock) <- predate_totalfleet_E * stock_ss(prey_stock__fleet_stock) / stock_ss(fleet_stock__catch)
                stock_ss(prey_stock__totalpredate) <- stock_ss(prey_stock__totalpredate) + stock_ss(prey_stock__fleet_stock)
                # NB: In gadget2, prey_stock__fleet_stock == (*cons[inarea][prey])[predl], totalpredator.cc#68
            }))
        }, list(
            amount_f = amount_f,
            prey_stock__fleet_stock = fleet_stock_var)))

        # Overconsumption: Prey adjustments
        out[[step_id(run_at, 3, prey_stock)]] <- g3_step(~{
            debug_trace("Calculate ", prey_stock, " overconsumption coefficient")
            stock_with(prey_stock, {
                prey_stock__overconsumption <- logspace_add_vec(-200 * (prey_stock__num * prey_stock__wgt * 0.95) / logspace_add_vec(prey_stock__totalpredate, 0), -200) / -200
                prey_stock__totalpredate <- prey_stock__totalpredate * prey_stock__overconsumption
                prey_stock__num <- prey_stock__num - (prey_stock__totalpredate / logspace_add_vec(prey_stock__wgt, 0))
            })
        })

        # Overconsumption: Zero catch counter again, so we can sum adjusted values this time
        out[[step_id(run_at, 3, fleet_stock)]] <- g3_step(~{
            debug_trace("Zero ", fleet_stock, " catch before working out post-adjustment value")
            stock_with(fleet_stock, fleet_stock__catch[] <- 0)
        })

        # Overconsumption: if we went over the limit, scale back, remove from prey_stock
        out[[step_id(run_at, 4, fleet_stock, prey_stock)]] <- g3_step(f_substitute(~{
            debug_trace("Scale caught amount by overconsumption, update variables")
            stock_with(prey_stock, prey_stock__fleet_stock <- prey_stock__fleet_stock * prey_stock__overconsumption)

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
