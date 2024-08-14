g3a_weightloss <- function (
        stock,
        rel_loss = NULL,
        abs_loss = NULL,
        min_weight = 1e-7,
        apply_to_pop = quote( stock__num ),
        run_f = TRUE,
        run_step = NULL,
        run_at = g3_action_order$naturalmortality ) {
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    if (!is.null(run_step)) run_f <- f_substitute(quote(cur_step == x && run_f), list(
        run_f = run_f,
        x = run_step ))

    stock__num <- g3_stock_instance(stock, 0, desc = paste0(stock$name, "Abundance"))
    stock__wgt <- g3_stock_instance(stock, 1, desc = paste0(stock$name, "Mean weight"))

    # Add rel/abs losses where specified
    wl_f <- quote( stock_ss(stock__wgt) )
    if (!is.null(rel_loss)) wl_f <- f_substitute(
        g3_formula(
            quote( ((wl_f - min_weight) * (1 - rel_loss)) + min_weight ),
            min_weight = min_weight,
            rel_loss = rel_loss ),
        list(wl_f = wl_f) )
    if (!is.null(abs_loss)) wl_f <- f_substitute(
        g3_formula(
            quote( logspace_add_vec((wl_f - abs_loss) * 1e7, min_weight) / 1e7 ),
            min_weight = min_weight,
            abs_loss = abs_loss ),
        list(wl_f = wl_f) )

    # If not applying to whole population, wrap in ratio_add_vec()
    if (!identical(apply_to_pop, quote( stock__num ))) wl_f <- f_substitute(
        quote( ratio_add_vec(
            stock_ss(stock__wgt),
            stock_ss(stock__num) - stock_ss(apply_to_pop),
            wl_f,
            stock_ss(apply_to_pop) ) ),
        list(
            apply_to_pop = apply_to_pop,
            wl_f = wl_f ))

    out[[step_id(run_at, "g3a_weightloss", stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("g3a_weightloss: Weight loss for ", stock)
        stock_iterate(stock, if (run_f) {
            stock_ss(stock__wgt) <- wl_f
        })
    }, list(
        wl_f = wl_f,
        run_f = run_f )))

    return(as.list(out))
}
