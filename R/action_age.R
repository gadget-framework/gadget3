# On final step of year, move stock into the next age bracket
g3a_age <- function(
        stock,
        output_stocks = list(),
        output_ratios = rep(1 / length(output_stocks), times = length(output_stocks)),
        run_f = ~cur_step_final,
        run_at = g3_action_order$age,
        transition_at = g3_action_order$age ) {
    if (is.null(tryCatch(g3_stock_def(stock, 'minage'), error = function(x) NULL))) stop("stock missing age ", stock$name)

    out <- new.env(parent = emptyenv())

    stock__num <- g3_stock_instance(stock, 0)
    stock__wgt <- g3_stock_instance(stock, 1)
    stock_movement <- g3s_age(
        g3s_clone(stock, paste0(stock$name, '_movement')),
        g3_stock_def(stock, 'maxage') + 1,
        g3_stock_def(stock, 'maxage') + 1)
    stock_movement__transitioning_num <- g3_stock_instance(stock_movement)
    stock_movement__transitioning_wgt <- g3_stock_instance(stock_movement)

    # Handle single-age special case separately
    if (g3_stock_def(stock, 'maxage') == g3_stock_def(stock, 'minage')) {
        if (length(output_stocks) == 0) {
            # Single age, no output stocks -> nothing to do
            return(list())
        }

        # Instead of using the below, just move-and-zero stocks
        out[[step_id(run_at, 1, stock)]] <- g3_step(f_substitute(~if (run_f) {
            debug_label("g3a_age for ", stock)
            stock_iterate(stock, stock_with(stock_movement, {
                debug_trace("Move oldest ", stock, " into ", stock_movement)
                # NB: We should be doing this once in a normal iterate case, but here there's only one loop so doesn't matter
                # NB: This relies on the dimension ordering between stock_movement & stock matching
                stock_ss(stock_movement__transitioning_num, age = g3_idx(1), vec = age) <- stock_reshape(stock_movement, stock_ss(stock__num, age = default, vec = age))
                stock_ss(stock_movement__transitioning_wgt, age = g3_idx(1), vec = age) <- stock_reshape(stock_movement, stock_ss(stock__wgt, age = default, vec = age))
                stock_ss(stock__num, age = default, vec = age) <- 0
            }))
        }, list(
            run_f = run_f )))

        # NB: move_remainder = FALSE because it's pointless here (and we can't move back into stock_movement)
        out[[step_id(transition_at, 90, stock)]] <- g3a_step_transition(stock_movement, output_stocks, output_ratios, move_remainder = FALSE, run_f = run_f)
        return(as.list(out))
    }

    # Add transition steps if output_stocks provided
    if (length(output_stocks) > 0) {
        # NB: move_remainder = FALSE because it's pointless here (and we can't move back into stock_movement)
        out[[step_id(transition_at, 90, stock)]] <- g3a_step_transition(stock_movement, output_stocks, output_ratios, move_remainder = FALSE, run_f = run_f)
    }

    out[[step_id(run_at, 1, stock)]] <- g3_step(f_substitute(~if (run_f) {
        debug_label("g3a_age for ", stock)

        stock_iterate(stock, {
            debug_trace("Check stock has remained finite for this step")

            if (age == stock__maxage) {
                if (have_output_stocks) stock_with(stock_movement, {
                    debug_trace("Move oldest ", stock, " into ", stock_movement)
                    # NB: We should be doing this once in a normal iterate case, but here there's only one loop so doesn't matter
                    # NB: This relies on the dimension ordering between stock_movement & stock matching
                    stock_ss(stock_movement__transitioning_num, age = g3_idx(1), vec = age) <- stock_reshape(stock_movement, stock_ss(stock__num, age = default, vec = age))
                    stock_ss(stock_movement__transitioning_wgt, age = g3_idx(1), vec = age) <- stock_reshape(stock_movement, stock_ss(stock__wgt, age = default, vec = age))
                    stock_ss(stock__num, age = default, vec = age) <- stock_ss(stock__num, age = default - 1, vec = age)
                    stock_ss(stock__wgt, age = default, vec = age) <- stock_ss(stock__wgt, age = default - 1, vec = age)
                }) else {
                    debug_trace("Oldest ", stock, " is a plus-group, combine with younger individuals")
                    stock_combine_subpop(
                        stock_ss(stock__num, age = default, vec = age),
                        stock_ss(stock__num, age = default - 1, vec = age) )
                }
            } else if (age == stock__minage) {
                debug_trace("Empty youngest ", stock, " age-group")
                stock_ss(stock__num, age = default, vec = age) <- 0
                # NB: Leave stock__wgt[age] as-is, it's value is irrelevant with zero stock, and will result in NaN if we zero it.
            } else {
                debug_trace("Move ", stock, " age-group to next one up")
                # NB: This is reliant on the below reversing the order that we iterate over ages
                stock_ss(stock__num, age = default, vec = age) <- stock_ss(stock__num, age = default - 1, vec = age)
                stock_ss(stock__wgt, age = default, vec = age) <- stock_ss(stock__wgt, age = default - 1, vec = age)
            }
        })
    }, list(
        have_output_stocks = length(output_stocks) > 0,
        run_f = run_f )))

    # Find for (age in seq(...)) and reverse it, so we count down not up
    out[[step_id(run_at, 1, stock)]] <- call_replace(out[[step_id(run_at, 1, stock)]], "for" = function (x, recurse) {
        if (!is.call(x)) return(x)
        if (as.character(x[[2]]) == "age" && is.call(x[[3]]) && as.character(x[[3]][[1]]) == "seq") {
            # Turn seq arguments the other way around, set by to -1
            x[[3]] <- call(
                "seq",
                x[[3]][[3]],
                x[[3]][[2]],
                by = -1 )
        }
        for (i in seq_len(length(x))) x[[i]] <- recurse(x[[i]])
        return(x)
    })

    return(as.list(out))
}
