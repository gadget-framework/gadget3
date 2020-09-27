# On final step of year, move stock into the next age bracket
g3a_age <- function(stock, output_stocks = list(), output_ratios = rep(1 / length(output_stocks), times = length(output_stocks)), run_f = ~cur_step_final, run_at = 12, transition_at = 12) {
    out <- new.env(parent = emptyenv())

    # Mangle stock_num / stock_wgt to remove non-age parameters
    age_iter_ss <- as.call(lapply(stock$iter_ss, function (x) {
        if (as.character(x) %in% c("[", ".", "stock__age_idx")) x
        else quote(x[,1])[[3]]  # i.e. anything else should be missing
    }))
    age_older_iter_ss <- as.call(lapply(stock$iter_ss, function (x) {
        if (as.character(x) %in% c("[", ".")) x
        else if (as.character(x) %in% c("stock__age_idx")) call("+", x, 1)  # Add 1 to age paramter
        else quote(x[,1])[[3]]  # i.e. anything else should be missing
    }))

    stock__num <- stock_instance(stock)
    stock__wgt <- stock_instance(stock)
    stock__transitioning_num <- stock_instance(stock)
    stock__transitioning_wgt <- stock_instance(stock)

    # Add transition steps if output_stocks provided
    if (length(output_stocks) == 0) {
        final_year_f = ~stock_comment("Oldest ", stock, " is a plus-group")
    } else {
        final_year_f = f_substitute(~{
            stock_comment("Move oldest ", stock)
            # NB: We should be doing this once in a normal iterate case, but here there's only one loop so doesn't matter
            stock__transitioning_num[] <- 0
            stock__transitioning_wgt[] <- stock__wgt[]

            stock__transitioning_num[age_iter_ss] <- stock__num[age_iter_ss]
            stock__num[age_iter_ss] <- 0
        }, list(
            age_iter_ss = age_iter_ss))
        # TODO: Think about ordering of step parts
        out[[step_id(transition_at, 90, stock)]] <- g3a_step_transition(stock, output_stocks, output_ratios, run_f = run_f)
    }

    out[[step_id(run_at, 1, stock)]] <- stock_step(f_substitute(~if (run_f) {
        stock_comment("g3a_age for ", stock)

        stock_with(stock, for (age in seq(stock__maxage, stock__minage, by = -1)) g3_with(
                stock__age_idx, g3_idx(age - stock__minage + 1), {

            if (age == stock__maxage) {
                final_year_f
            } else {
                # To total weight
                stock__wgt[age_older_iter_ss] <- stock__wgt[age_older_iter_ss] * stock__num[age_older_iter_ss]
                stock__num[age_older_iter_ss] <- stock__num[age_older_iter_ss] + stock__num[age_iter_ss]
                stock__wgt[age_older_iter_ss] <- stock__wgt[age_older_iter_ss] + (stock__wgt[age_iter_ss] * stock__num[age_iter_ss])
                # Back to mean weight
                stock__wgt[age_older_iter_ss] <- stock__wgt[age_older_iter_ss] / pmax(stock__num[age_older_iter_ss], 0.00001)
                stock__num[age_iter_ss] <- 0
                stock__wgt[age_iter_ss] <- 1e-05  # NB: In theory undefined, as num is 0, but we do similar elsewhere.
            }
        }))
    }, list(
        final_year_f = final_year_f,
        run_f = run_f,
        age_iter_ss = age_iter_ss,
        age_older_iter_ss = age_older_iter_ss)))

    return(as.list(out))
}
