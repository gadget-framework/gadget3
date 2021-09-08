g3l_controlset <- function (
        nll_name,
        controlset_df,
        input_betas = rep(1, nrow(controlset_df)),
        transform_fs = list(),
        run_f = ~cur_time == 0,
        weight = substitute(g3_param(n, optimise = FALSE, value = 1), list(n = paste0(nll_name, "_weight"))),
        run_at = 10) {
    stopifnot(is.data.frame(controlset_df))
    stopifnot(names(controlset_df) == c("age", names(transform_fs)))
    stopifnot(length(input_betas) == nrow(controlset_df))
    stopifnot(is.list(transform_fs))

    out <- new.env(parent = emptyenv())

    # Formulate variable to store results
    nll_controlset_name <- paste0("nll_controlset_", nll_name)
    assign(nll_controlset_name, structure(
        array(dim = nrow(controlset_df), dimnames = list(age = controlset_df$age)),
        desc = "nll for each reader"))

    cset <- g3s_agegroup(g3_storage(paste0('cset_', nll_name)), as.list(controlset_df$age))
    cset__betas <- stock_instance(cset, input_betas)

    # Formulae to gather transformed age probability from all readers into nll_controlset
    gather_f <- f_concatenate(lapply(seq_along(transform_fs), function (reader_idx) {
        cset__reader_name <- paste0("cset__", names(transform_fs)[[reader_idx]])
        assign(cset__reader_name, stock_instance(cset, as.integer(controlset_df[[names(transform_fs)[[reader_idx]]]])))
        f_substitute(
            ~g3_with(destage := cset__reader[[cset__agegroup_idx]], nll_controlset[cset__agegroup_idx] <- nll_controlset[cset__agegroup_idx] * transform_f),
            list(
                cset__reader = as.symbol(cset__reader_name),
                transform_f = transform_fs[[reader_idx]],
                nll_controlset = as.symbol(nll_controlset_name)))
    }))

    out[[step_id(run_at, 'g3l_controlset', 2)]] <- g3_step(f_substitute(~if (run_f) {
        debug_label("g3l_controlset: Compare readers against a control set")

        stock_with(cset, nll_controlset[] <- cset__betas)
        stock_iterate(cset, gather_f)
        nll <- nll + (weight) * -log(sum(nll_controlset))
        
    }, list(
        gather_f = gather_f,
        run_f = run_f,
        nll_controlset = as.symbol(nll_controlset_name),
        weight = weight)))

    return(as.list(out))
}