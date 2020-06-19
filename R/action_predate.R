g3a_predate_totalfleet <- function (fleet_stock, prey_stocks, suitabilities, amount_f) {
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
        out[[paste0('030:0:', fleet_stock$name)]] <- stock_step(fleet_stock,
            init_f = ~{
                fleet_stock__catch[] <- 0
            })

        # Make sure the counter for this prey is zeroed
        # NB: We only have one of these per-prey (we replace it a few times though)
        out[[paste0('030:0:', prey_stock$name)]] <- stock_step(prey_stock,
            init_f = f_substitute(~{
                prey_stock__totalpredate[] <- 0
            }, list(amount_f = amount_f)))

        # Main predation step, iterate over prey and pull out everything this fleet needs
        out[[paste0('030:1:', fleet_stock$name, ":", prey_stock$name)]] <- stock_step(prey_stock, extra_stock = fleet_stock,
            init_f = f_substitute(~{
                comment("Zero counter of biomass caught for this fleet")
                fleet_stock__prey_stock[] <- 0
            }, list(
                fleet_stock__prey_stock = fleet_stock_var)),
            iter_f = f_substitute(~{
                comment("Collect all suitable biomass for fleet")
                fleet_stock__prey_stock[prey_stock__iter] <- (suit_f
                    * prey_stock__num[prey_stock__iter]
                    * prey_stock__wgt[prey_stock__iter])
                fleet_stock__catch[fleet_stock__iter] <- (fleet_stock__catch[fleet_stock__iter]
                    + sum(fleet_stock__prey_stock[prey_stock__iter]))
            }, list(
                suit_f = f_substitute(suitabilities[[prey_stock$name]], list(prey_l = as.symbol("prey_stock__meanlen"))),
                fleet_stock__prey_stock = fleet_stock_var)))

        # After all prey is collected (not just this stock), scale by total expected, update catch params
        out[[paste0('030:2:', fleet_stock$name, ":", prey_stock$name)]] <- stock_step(prey_stock, extra_stock = fleet_stock,
            iter_f = f_substitute(~{
                comment("Scale fleet amount by total expected catch")
                predate_totalfleet_E <- (amount_f)
                fleet_stock__prey_stock[prey_stock__iter] <- predate_totalfleet_E * fleet_stock__prey_stock[prey_stock__iter] / fleet_stock__catch[fleet_stock__iter]
                prey_stock__totalpredate[prey_stock__iter] <- prey_stock__totalpredate[prey_stock__iter] + fleet_stock__prey_stock[prey_stock__iter]
            }, list(
                amount_f = amount_f,
                fleet_stock__prey_stock = fleet_stock_var)))

        # Overconsumption: Zero catch counter again, so we can sum adjusted values this time
        out[[paste0('030:3:', fleet_stock$name)]] <- stock_step(fleet_stock,
            init_f = ~{
                comment("Zero fleet catch before working out post-adjustment value")
                fleet_stock__catch[] <- 0
            })

        # Overconsumption: Prey adjustments
        out[[paste0('030:3:', fleet_stock$name)]] <- stock_step(prey_stock,
            iter_f = ~{
                comment("Prey overconsumption coefficient")
                # TODO: Should replace min() here with something differentiable
                prey_stock__overconsumption[prey_stock__iter] <- min((prey_stock__num[prey_stock__iter] * prey_stock__wgt[prey_stock__iter] * 0.95) / prey_stock__totalpredate[prey_stock__iter], 1)
                prey_stock__totalpredate[prey_stock__iter] <- prey_stock__totalpredate[prey_stock__iter] * prey_stock__overconsumption[prey_stock__iter]
            })

        # Overconsumption: if we went over the limit, scale back, remove from prey_stock
        out[[paste0('030:3:', fleet_stock$name, ":", prey_stock$name)]] <- stock_step(prey_stock, extra_stock = fleet_stock,
            iter_f = f_substitute(~{
                comment("Scale caught amount by overconsumption, update variables")
                fleet_stock__prey_stock[prey_stock__iter] <- fleet_stock__prey_stock[prey_stock__iter] * prey_stock__overconsumption[prey_stock__iter]
                fleet_stock__catch[fleet_stock__iter] <- (fleet_stock__catch[fleet_stock__iter]
                    + sum(fleet_stock__prey_stock[prey_stock__iter]))

                # TODO: Have we officially documented if stock__wgt is total biomass or mean weight?
                prey_stock__num[prey_stock__iter] <- prey_stock__num[prey_stock__iter] - (prey_stock__totalpredate[prey_stock__iter] / prey_stock__wgt[prey_stock__iter])
            }, list(
                fleet_stock__prey_stock = fleet_stock_var)))
    }

    return(as.list(out))
}
# NB:
# * All consumption is in biomass, conversion done in Prey::addNumbersConsumption
