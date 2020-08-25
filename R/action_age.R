# On final step of year, move stock into the next age bracket
g3a_age <- function(stock, run_at = 12) {
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

    out <- list()
    out[[step_id(run_at, stock)]] <- stock_step(f_substitute(~if (cur_step_final) {
        stock_comment("g3a_age for ", stock)

        stock_with(stock, for (age in seq(stock__maxage, stock__minage)) g3_with(
                stock__age_idx, g3_idx(age - stock__minage + 1), {

            if (age == stock__maxage) {
                comment("TODO: Plus group migration shenanigans")
            } else {
                stock__num[age_older_iter_ss] <- stock__num[age_older_iter_ss] + stock__num[age_iter_ss]
                stock__num[age_iter_ss] <- 0
            }
        }))
    }, list(
        age_iter_ss = age_iter_ss,
        age_older_iter_ss = age_older_iter_ss)))
    return(out)
}
