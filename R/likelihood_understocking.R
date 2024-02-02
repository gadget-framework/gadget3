g3l_understocking <- function (
        prey_stocks,
        power_f = ~2,
        nll_breakdown = FALSE,
        weight = 1e+08,
        run_at = g3_action_order$likelihood) {
    out <- new.env(parent = emptyenv())

    nllstock <- g3_storage("nll_understocking")
    if (nll_breakdown) nllstock <- g3s_modeltime(nllstock)
    nllstock__wgt <- g3_stock_instance(nllstock, 0)
    nllstock__weight <- g3_stock_instance(nllstock, 0)
    nll <- 0.0

    g3l_understocking_total <- 0.0
    out[[step_id(run_at, 'g3l_understocking', 0)]] <- g3_step(~{
        debug_trace("Reset understocking total")
        g3l_understocking_total <- 0
    })

    for (prey_stock in prey_stocks) {
        out[[step_id(run_at, 'g3l_understocking', 1, prey_stock)]] <- g3_step(f_substitute(~{
            debug_label("g3l_understocking for ", prey_stock)
            # understocking.cc:134

            # NB: To match gadget2, g3l_understocking_total should be per-area and we sum-of-squares that,
            #     but seems like wasted effort.
            stock_with(prey_stock, {
                debug_trace("Add understocking from ", prey_stock, " as biomass to nll")
                g3l_understocking_total <- g3l_understocking_total + prey_stock__overconsumption
            })
        }, list()))
    }

    out[[step_id(run_at, 'g3l_understocking', 2)]] <- g3_step(f_substitute(~{
        debug_label("g3l_understocking: Combine and add to nll")
        g3l_understocking_total <- g3l_understocking_total ^ (power_f)
        nll <- nll + (weight) * g3l_understocking_total

        stock_iterate(nllstock, {
            stock_ss(nllstock__wgt) <- stock_ss(nllstock__wgt) + g3l_understocking_total
            stock_ss(nllstock__weight) <- weight
        })
    }, list(
        power_f = power_f,
        weight = weight)))

    return(c(as.list(out), g3a_report_var(
        "nllstock__wgt",
        nllstock__wgt,
        stock = nllstock,
        out_prefix = NULL )))  # Don't add history
}
