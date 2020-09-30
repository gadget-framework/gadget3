g3a_predate_totalfleet <- function (fleet_stock, prey_stocks, suitabilities, amount_f, run_at = 3) {
    out <- new.env(parent = emptyenv())
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
        out[[step_id(run_at, 0, fleet_stock)]] <- stock_step(~{
            stock_comment("g3a_predate_totalfleet for ", fleet_stock)
            stock_with(fleet_stock, fleet_stock__catch[] <- 0)
        })

        # Make sure the counter for this prey is zeroed
        # NB: We only have one of these per-prey (we replace it a few times though)
        out[[step_id(run_at, 0, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("g3a_predate_totalfleet for ", prey_stock)
            stock_with(prey_stock, prey_stock__totalpredate[] <- 0)
        }, list(amount_f = amount_f)))

        # Main predation step, iterate over prey and pull out everything this fleet needs
        out[[step_id(run_at, 1, fleet_stock, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("g3a_predate_totalfleet for ", prey_stock)
            comment("Zero counter of biomass caught for this fleet")
            stock_with(prey_stock, prey_stock__fleet_stock[] <- 0)

            stock_iterate(prey_stock, stock_intersect(fleet_stock, {
                comment("Collect all suitable biomass for fleet")
                stock_ss(prey_stock__fleet_stock) <- (suit_f
                    * stock_ss(prey_stock__num)
                    * stock_ss(prey_stock__wgt))
                stock_ss(fleet_stock__catch) <- (stock_ss(fleet_stock__catch)
                    + sum(stock_ss(prey_stock__fleet_stock)))
            }))
        }, list(
            suit_f = f_substitute(suitabilities[[prey_stock$name]], list(prey_l = as.symbol("prey_stock__midlen"))),
            prey_stock__fleet_stock = fleet_stock_var)))

        # After all prey is collected (not just this stock), scale by total expected, update catch params
        out[[step_id(run_at, 2, fleet_stock, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("g3a_predate_totalfleet for ", prey_stock)
            stock_iterate(prey_stock, stock_intersect(fleet_stock, {
                comment("Scale fleet amount by total expected catch")
                predate_totalfleet_E <- (amount_f)
                stock_ss(prey_stock__fleet_stock) <- predate_totalfleet_E * stock_ss(prey_stock__fleet_stock) / stock_ss(fleet_stock__catch)
                stock_ss(prey_stock__totalpredate) <- stock_ss(prey_stock__totalpredate) + stock_ss(prey_stock__fleet_stock)
            }))
        }, list(
            amount_f = amount_f,
            prey_stock__fleet_stock = fleet_stock_var)))

        # Overconsumption: Zero catch counter again, so we can sum adjusted values this time
        out[[step_id(run_at, 3, fleet_stock)]] <- stock_step(~{
            comment("Zero fleet catch before working out post-adjustment value")
            stock_with(fleet_stock, fleet_stock__catch[] <- 0)
        })

        # Overconsumption: Prey adjustments
        out[[step_id(run_at, 3, prey_stock)]] <- stock_step(~{
            stock_comment("g3a_predate_totalfleet for ", prey_stock)
            stock_iterate(prey_stock, {
                comment("Prey overconsumption coefficient")
                # TODO: Should replace pmin() here with something differentiable
                stock_ss(prey_stock__overconsumption) <- pmin((stock_ss(prey_stock__num) * stock_ss(prey_stock__wgt) * 0.95) / pmax(stock_ss(prey_stock__totalpredate), 0.00001), 1)
                stock_ss(prey_stock__totalpredate) <- stock_ss(prey_stock__totalpredate) * stock_ss(prey_stock__overconsumption)
                stock_ss(prey_stock__num) <- stock_ss(prey_stock__num) - (stock_ss(prey_stock__totalpredate) / pmax(stock_ss(prey_stock__wgt), 0.00001))
            })
        })

        # Overconsumption: if we went over the limit, scale back, remove from prey_stock
        out[[step_id(run_at, 4, fleet_stock, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("g3a_predate_totalfleet for ", prey_stock)
            stock_iterate(prey_stock, stock_intersect(fleet_stock, {
                comment("Scale caught amount by overconsumption, update variables")
                stock_ss(prey_stock__fleet_stock) <- stock_ss(prey_stock__fleet_stock) * stock_ss(prey_stock__overconsumption)
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
