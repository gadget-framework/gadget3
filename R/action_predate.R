g3a_predate_totalfleet <- function (fleet_stock, prey_stocks, suitabilities, amount_f, run_at = 3) {
    out <- new.env(parent = emptyenv())
    predate_totalfleet_E <- 0.0

    # Variables used:
    # fleet_stock__prey_stock: Biomass selected for that fleet/prey combination (prey matrix)
    # fleet_stock__catch: Biomass caught by that fleet (fleet matrix, i.e. area)
    # prey_stock__totalpredate: Biomass predated for that stock (prey matrix)
    # prey_stock__overconsumption: Factor to scale fleet_stock__prey_stock: / fleet_stock__catch / prey_stock__totalpredate
    #                              to bring it down to 95% of current stock

    # For each prey stock...
    for (prey_stock in prey_stocks) {
        # Create variable to store biomass of stock caught
        fleet_stock_var <- as.symbol(paste0(fleet_stock$name, '__', prey_stock$name))
        assign(as.character(fleet_stock_var), stock_definition(prey_stock, 'stock__num'))
        prey_stock__totalpredate <- stock_definition(prey_stock, 'stock__wgt')
        prey_stock__overconsumption <- stock_definition(prey_stock, 'stock__wgt')

        # Make sure counter for this fleet is zeroed
        # NB: We only have one of these per-fleet (we replace it a few times though)
        out[[step_id(run_at, 0, fleet_stock)]] <- stock_step(~{
            stock_comment("g3a_predate_totalfleet for ", fleet_stock)
            stock_rename(fleet_stock, fleet_stock__catch[] <- 0)
        })

        # Make sure the counter for this prey is zeroed
        # NB: We only have one of these per-prey (we replace it a few times though)
        out[[step_id(run_at, 0, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("g3a_predate_totalfleet for ", prey_stock)
            stock_rename(prey_stock, prey_stock__totalpredate[] <- 0)
        }, list(amount_f = amount_f)))

        # Main predation step, iterate over prey and pull out everything this fleet needs
        out[[step_id(run_at, 1, fleet_stock, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("g3a_predate_totalfleet for ", prey_stock)
            comment("Zero counter of biomass caught for this fleet")
            fleet_stock__prey_stock[] <- 0

            stock_iterate(prey_stock, stock_intersect(fleet_stock, {
                comment("Collect all suitable biomass for fleet")
                fleet_stock__prey_stock[prey_stock__iter] <- (suit_f
                    * prey_stock__num[prey_stock__iter]
                    * prey_stock__wgt[prey_stock__iter])
                fleet_stock__catch[fleet_stock__iter] <- (fleet_stock__catch[fleet_stock__iter]
                    + sum(fleet_stock__prey_stock[prey_stock__iter]))
            }))
        }, list(
            suit_f = f_substitute(suitabilities[[prey_stock$name]], list(prey_l = as.symbol("prey_stock__midlen"))),
            fleet_stock__prey_stock = fleet_stock_var)))

        # After all prey is collected (not just this stock), scale by total expected, update catch params
        out[[step_id(run_at, 2, fleet_stock, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("g3a_predate_totalfleet for ", prey_stock)
            stock_iterate(prey_stock, stock_intersect(fleet_stock, {
                comment("Scale fleet amount by total expected catch")
                predate_totalfleet_E <- (amount_f)
                fleet_stock__prey_stock[prey_stock__iter] <- predate_totalfleet_E * fleet_stock__prey_stock[prey_stock__iter] / fleet_stock__catch[fleet_stock__iter]
                prey_stock__totalpredate[prey_stock__iter] <- prey_stock__totalpredate[prey_stock__iter] + fleet_stock__prey_stock[prey_stock__iter]
            }))
        }, list(
            amount_f = amount_f,
            fleet_stock__prey_stock = fleet_stock_var)))

        # Overconsumption: Zero catch counter again, so we can sum adjusted values this time
        out[[step_id(run_at, 3, fleet_stock)]] <- stock_step(~{
            comment("Zero fleet catch before working out post-adjustment value")
            stock_rename(fleet_stock, fleet_stock__catch[] <- 0)
        })

        # Overconsumption: Prey adjustments
        out[[step_id(run_at, 3, prey_stock)]] <- stock_step(~{
            stock_comment("g3a_predate_totalfleet for ", prey_stock)
            stock_iterate(prey_stock, {
                comment("Prey overconsumption coefficient")
                # TODO: Should replace pmin() here with something differentiable
                prey_stock__overconsumption[prey_stock__iter] <- pmin((prey_stock__num[prey_stock__iter] * prey_stock__wgt[prey_stock__iter] * 0.95) / prey_stock__totalpredate[prey_stock__iter], 1)
                prey_stock__totalpredate[prey_stock__iter] <- prey_stock__totalpredate[prey_stock__iter] * prey_stock__overconsumption[prey_stock__iter]
                prey_stock__num[prey_stock__iter] <- prey_stock__num[prey_stock__iter] - (prey_stock__totalpredate[prey_stock__iter] / prey_stock__wgt[prey_stock__iter])
            })
        })

        # Overconsumption: if we went over the limit, scale back, remove from prey_stock
        out[[step_id(run_at, 4, fleet_stock, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("g3a_predate_totalfleet for ", prey_stock)
            stock_iterate(prey_stock, stock_intersect(fleet_stock, {
                comment("Scale caught amount by overconsumption, update variables")
                fleet_stock__prey_stock[prey_stock__iter] <- fleet_stock__prey_stock[prey_stock__iter] * prey_stock__overconsumption[prey_stock__iter]
                fleet_stock__catch[fleet_stock__iter] <- (fleet_stock__catch[fleet_stock__iter]
                    + sum(fleet_stock__prey_stock[prey_stock__iter]))
            }))
        }, list(
            fleet_stock__prey_stock = fleet_stock_var)))
    }

    return(as.list(out))
}
# NB:
# * All consumption is in biomass, conversion done in Prey::addNumbersConsumption
