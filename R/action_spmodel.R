g3a_spmodel_logistic <- function(
        r = g3_parameterized("spm_r", lower = 0.01, upper = 1, value = 0.5,
            by_stock = by_stock),
        p = g3_parameterized("spm_p", lower = 0.01, upper = 10, value = 1,
            by_stock = by_stock),
        K = g3_parameterized("spm_K", lower = 100, upper = 1e6, value = 1000,
            by_stock = by_stock),
        by_stock = TRUE) {

    s <- ~sum(stock_ss(stock__num))
    ~r *  s * (1-(s/K)^p)
}

g3a_spmodel <- function (
        stock,
        spm_num = g3a_spmodel_logistic(),
        spm_num_init = g3_parameterized("spm_n0", by_stock = TRUE),
        spm_wgt = 1,
        run_f = TRUE,
        run_at = g3_action_order$initial) {

    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    stock__renewalnum <- g3_stock_instance(stock, 0)
    stock__num <- g3_stock_instance(stock, 0)
    stock__wgt <- g3_stock_instance(stock, 1)

    out[[step_id(run_at, "g3a_spmodel", stock, action_name)]] <- g3_step(f_substitute(~{
        debug_label("Surplus production model for ", stock)
        stock_iterate(stock, {
            if (cur_time == 0) {
                stock_ss(stock__num) <- 0 * stock__midlen + spm_num_init
                stock_ss(stock__wgt) <- 0 * stock__midlen + spm_wgt
            } else if (run_f) {
                stock_ss(stock__renewalnum) <- 0 * stock__midlen + spm_num
                stock_ss(stock__num) <- stock_ss(stock__num) + stock_ss(stock__renewalnum)
                stock_ss(stock__wgt) <- 0 * stock__midlen + spm_wgt
            } else {
                stock_ss(stock__renewalnum) <- 0 * stock__midlen
            }
        })
    }, list(
        spm_num = spm_num,
        spm_wgt = spm_wgt,
        run_f = run_f )))

    return(as.list(out))
}
