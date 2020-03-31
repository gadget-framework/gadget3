g3a_age <- function(stock) {
    # Mangle stock_num / stock_wgt to remove non-age parameters
    stock_num_age <- as.call(lapply(ling_imm$stock_num, function (x) {
        if (as.character(x) %in% c("[", "stock_num")) x
        else if (as.character(x) %in% c("age_idx")) quote(age_idx)
        else ling_imm$stock_num[[3]]
    }))
    stock_num_age_older <- as.call(lapply(ling_imm$stock_num, function (x) {
        if (as.character(x) %in% c("[", "stock_num")) x
        else if (as.character(x) %in% c("age_idx")) quote(age_idx + 1)
        else ling_imm$stock_num[[3]]
    }))

    list(
        step125 = stock_step(stock, run_if = ~cur_step_final, init = f_substitute(~for (age in seq(stock_maxage, stock_minage)) {
            age_idx <- g3_idx(age - stock_minage + 1)

            if (age == stock_maxage) {
                comment("TODO: Plus group migration shenanigans")
            } else {
                stock_num_older <- stock_num_older + stock_num
                stock_num <- 0
            }
        }, list(
            stock_num = stock_num_age,
            stock_num_older = stock_num_age_older))))
}
