# On final step of year, move stock into the next age bracket
g3a_age <- function(
        stock,
        output_stocks = list(),
        output_ratios = rep(1 / length(output_stocks), times = length(output_stocks)),
        run_f = ~cur_step_final,
        run_at = g3_action_order$age,
        transition_at = g3_action_order$age ) {
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
            stock_with(stock, stock_with(stock_movement, for (age in seq(stock__maxage, stock__minage, by = -1)) g3_with(
                   stock__age_idx := g3_idx(age - stock__minage + 1L), {
                debug_trace("Move oldest ", stock, " into ", stock_movement)
                # NB: We should be doing this once in a normal iterate case, but here there's only one loop so doesn't matter
                # NB: This relies on the dimension ordering between stock_movement & stock matching
                stock_ss(stock_movement__transitioning_num, age = g3_idx(1), vec = age) <- stock_reshape(stock_movement, stock_ss(stock__num, age = default, vec = age))
                stock_ss(stock_movement__transitioning_wgt, age = g3_idx(1), vec = age) <- stock_reshape(stock_movement, stock_ss(stock__wgt, age = default, vec = age))
                stock_ss(stock__num, age = default, vec = age) <- 0
            })))
        }, list(
            run_f = run_f )))

        # NB: move_remainder = FALSE because it's pointless here (and we can't move back into stock_movement)
        out[[step_id(transition_at, 90, stock)]] <- g3a_step_transition(stock_movement, output_stocks, output_ratios, move_remainder = FALSE, run_f = run_f)
        return(as.list(out))
    }

    # Add transition steps if output_stocks provided
    if (length(output_stocks) == 0) {
        final_year_f <- ~{
            debug_trace("Oldest ", stock, " is a plus-group, combine with younger individuals")
            stock_ss(stock__wgt, age = default, vec = age) <- ratio_add_vec(
                stock_ss(stock__wgt, age = default, vec = age), stock_ss(stock__num, age = default, vec = age),
                stock_ss(stock__wgt, age = default - 1, vec = age), stock_ss(stock__num, age = default - 1, vec = age))
            stock_ss(stock__num, age = default, vec = age) <- stock_ss(stock__num, age = default, vec = age) + stock_ss(stock__num, age = default - 1, vec = age)
        }
    } else {
        final_year_f <- ~stock_with(stock_movement, {
            debug_trace("Move oldest ", stock, " into ", stock_movement)
            # NB: We should be doing this once in a normal iterate case, but here there's only one loop so doesn't matter
            # NB: This relies on the dimension ordering between stock_movement & stock matching
            stock_ss(stock_movement__transitioning_num, age = g3_idx(1), vec = age) <- stock_reshape(stock_movement, stock_ss(stock__num, age = default, vec = age))
            stock_ss(stock_movement__transitioning_wgt, age = g3_idx(1), vec = age) <- stock_reshape(stock_movement, stock_ss(stock__wgt, age = default, vec = age))
            stock_ss(stock__num, age = default, vec = age) <- stock_ss(stock__num, age = default - 1, vec = age)
            stock_ss(stock__wgt, age = default, vec = age) <- stock_ss(stock__wgt, age = default - 1, vec = age)
        })
        # NB: move_remainder = FALSE because it's pointless here (and we can't move back into stock_movement)
        out[[step_id(transition_at, 90, stock)]] <- g3a_step_transition(stock_movement, output_stocks, output_ratios, move_remainder = FALSE, run_f = run_f)
    }

    out[[step_id(run_at, 1, stock)]] <- g3_step(f_substitute(~if (run_f) {
        debug_label("g3a_age for ", stock)

        stock_with(stock, for (age in seq(stock__maxage, stock__minage, by = -1)) g3_with(
                stock__age_idx := g3_idx(age - stock__minage + 1L), {
            debug_trace("Check stock has remained finite for this step")

            if (age == stock__maxage) {
                final_year_f
            } else if (age == stock__minage) {
                debug_trace("Empty youngest ", stock, " age-group")
                stock_ss(stock__num, age = default, vec = age) <- 0
                # NB: Leave stock__wgt[age] as-is, it's value is irrelevant with zero stock, and will result in NaN if we zero it.
            } else {
                debug_trace("Move ", stock, " age-group to next one up")
                stock_ss(stock__num, age = default, vec = age) <- stock_ss(stock__num, age = default - 1, vec = age)
                stock_ss(stock__wgt, age = default, vec = age) <- stock_ss(stock__wgt, age = default - 1, vec = age)
            }
        }))
    }, list(
        final_year_f = final_year_f,
        run_f = run_f )))

    return(as.list(out))
}
