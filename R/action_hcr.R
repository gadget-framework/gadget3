g3a_hcr_tac_singletrigger <- function(
        trigger = g3_parameterized('tac.trigger', by_stock = by_stock),
        hr_low = g3_parameterized('tac.hr.low', by_stock = by_stock),
        hr_high = g3_parameterized('tac.hr.high', by_stock = by_stock),
        by_stock = FALSE) {  # TODO: Ideally would be by_stock = hcr, but cannae do that.
    f_substitute(quote(
        (
            hr_low * logspace_add(-logspace_add(stock_ss(hcr__trigger), trigger), 0) * exp(trigger) +
            hr_high * (1 - logspace_add(-logspace_add(stock_ss(hcr__trigger), trigger), 0) * exp(trigger))
        ) * stock_ss(hcr__fishable)
    ), list(
        hr_low = hr_low,
        hr_high = hr_high,
        trigger = trigger))
}
# e.g. curve(
#     gadget3:::f_eval(
#         g3a_hcr_tac_singletrigger(trigger = 50, hr_low = 100, hr_high = 200),
#         list(hcr__trigger = x, hcr__fishable = 1, stock_ss = identity)), 0, 100)

g3a_hcr_biomass_weighted <- function(
        # TODO: More desciptive names?
        param0,
        param1,
        param2,
        by_stock = TRUE) {
    f_substitute(quote(
        # TODO: Is this right? The example has triggercoeffs=c(0,0,0,0,0,1), which I assume is "all the mature", but it's actually half.
        sum( param2 * (stock_ss(stock__num) * stock_ss(stock__wgt)) / ( 1.0 + exp(-param0 * (param1 - stock__midlen))) )
    ), list(
        param0 = param0,
        param1 = param1,
        param2 = param2))
}

g3a_hcr_assess <- function (
        stocks,             # Stock(s) to assess
        fishable_fs,        # List of stock names -> formula to gather, or single value applied to all
        trigger_fs,         # List of stock names -> formula to gather, or single value applied to all
        tac_f,              # hcr__fishable, hcr__trigger --> hcr__tac function
        prevtacweight = 0,  # Weighting of previous TAC in hcr__tac
        assess_run_f = quote(cur_step == 1),  # When to assess & regenerate TAC
        gather_run_f = assess_run_f,     # When to gather data for TAC
        hcr_name = 'hcr',
        run_at = 12) {
    if (g3_is_stock(stocks)) stocks <- list(stocks)
    stopifnot(is.list(stocks) & all(sapply(stocks, g3_is_stock)))

    hcr <- g3_storage(hcr_name)
    hcr__trigger <- stock_instance(hcr, 0)
    hcr__fishable <- stock_instance(hcr, 0)
    hcr__tac <- stock_instance(hcr, 1)

    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    out[[step_id(run_at, hcr, action_name, 1)]] <- f_concatenate(c(

        g3_step(f_substitute(~{
            debug_label("g3a_hcr_assess: Collect ", hcr, " data")
            if (gather_run_f) stock_with(hcr, {
                hcr__trigger[] <- 0
                hcr__fishable[] <- 0
            })
        }, list(
            gather_run_f = gather_run_f))),

        lapply(stocks, function (stock) {
            # Copy hcr from parent environment
            hcr <- hcr
            hcr__trigger <- hcr__trigger
            hcr__fishable <- hcr__fishable
            hcr__tac <- hcr__tac
            g3_step(f_substitute(~{
                debug_trace("Collecting data from ", stock)
                if (gather_run_f) stock_iterate(stock, stock_intersect(hcr, {
                    stock_ss(hcr__trigger) <- stock_ss(hcr__trigger) + trigger_f
                    stock_ss(hcr__fishable) <- stock_ss(hcr__fishable) + fishable_f
                }))
            }, list(
                fishable_f = list_to_stock_switch(fishable_fs),
                trigger_f = list_to_stock_switch(trigger_fs),
                gather_run_f = gather_run_f)))
        }),

        list()))

    out[[step_id(run_at, hcr, action_name, 2)]] <- g3_step(f_substitute(~{
        debug_label("g3a_hcr_assess: Assess ", hcr, " and reset TAC")
        if (assess_run_f) stock_iterate(hcr, {
            stock_ss(hcr__tac) <- prevtacweight * stock_ss(hcr__tac) + (prevtacweight - 1) * tac_f
        })
    }, list(
        prevtacweight = prevtacweight,
        tac_f = tac_f,
        assess_run_f = assess_run_f)))

    return(as.list(out))
}

g3a_predate_catchability_hcr <- function (
    inner_f,
    stock_prop_fs,
    fleet_prop_fs,
    hcr_name = 'hcr') {

    # Define identical hcr stock to g3a_hcr_assess()
    hcr <- g3_storage(hcr_name)
    hcr__trigger <- stock_instance(hcr, 0)
    hcr__fishable <- stock_instance(hcr, 0)
    hcr__tac <- stock_instance(hcr, 1)

    return(f_substitute(~(
        # TODO: What we really want here is stock_intersect(hcr, stock_ss(hcr__tac)), but that's not easy.
        (stock_with(hcr, sum(hcr__tac)) * stock_prop_f * fleet_prop_f) * inner_f
    ), list(
        stock_prop_f = list_to_stock_switch(stock_prop_fs, stock_var = "stock"),
        fleet_prop_f = list_to_stock_switch(fleet_prop_fs, stock_var = "fleet_stock"),
        inner_f = inner_f)))
}
