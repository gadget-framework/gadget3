g3l_understocking <- function (prey_stocks, power_f = ~2, weight = 1.0, run_at = 10) {
    out <- new.env(parent = emptyenv())

    for (prey_stock in prey_stocks) {
        g3l_understocking_total <- 0.0
        out[[step_id(run_at, 'g3l_understocking', prey_stock)]] <- g3_step(f_substitute(~{
            debug_label("g3l_understocking for ", prey_stock)

            g3l_understocking_total <- 0
            stock_iterate(prey_stock, {
                debug_trace("Add understocking from ", prey_stock, " as biomass to nll")
                g3l_understocking_total <- g3l_understocking_total + sum(
                    (stock_ss(prey_stock__totalpredate) / stock_ss(prey_stock__consratio)) *
                    (1 - stock_ss(prey_stock__consratio))
                ) ^ (power_f)
            })
            nll <- nll + (weight) * g3l_understocking_total
        }, list(
            power_f = power_f,
            weight = weight)))
        
    }
    return(as.list(out))
}
