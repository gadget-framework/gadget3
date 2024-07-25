g3a_weightloss <- function (
        stock,
        rel_loss = 0,
        abs_loss = 0,
        min_weight = 1e-7,
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

    out[[step_id(run_at, "g3a_weightloss", stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("Weight loss for ", stock)
        stock_iterate(stock, if (run_f) {
            stock_ss(stock__wgt) <- logspace_add_vec((stock_ss(stock__wgt) * (1 - rel_loss)) - abs_loss, min_weight)
        })
    }, list(
        min_weight = min_weight,
        run_f = run_f )))

    return(as.list(out))
}
