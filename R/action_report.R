g3a_report_stock <- function (report_stock, input_stock, report_f, include_adreport = FALSE, run_f = TRUE, run_at = 11) {
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    # Find first stock__inst variable, and use that to name ours
    inst_var_name <- all.vars(report_f)
    inst_var_name <- inst_var_name[grepl('__', inst_var_name, fixed = TRUE)][[1]]
    if (!is.null(inst_var_name)) {
        instance_name <- gsub('^.*__', '', inst_var_name)
    } else {
        instance_name <- 'rep'
    }

    report_stock_instance_name <- paste0('report_stock__', instance_name)
    assign(report_stock_instance_name, g3_stock_instance(report_stock))

    out[[step_id(run_at, report_stock, instance_name, action_name)]] <- g3_step(f_substitute(~{
        debug_label("g3a_report_stock for ", report_stock, " from ", input_stock)
        if (run_f) {
            if (cur_time == 0L) {
                stock_with(report_stock, report_instance[] <- 0)
            }
            stock_iterate(input_stock, stock_intersect(report_stock, {
                stock_ss(report_instance) <- stock_ss(report_instance) + (report_f)
            }))
            if (include_adreport && cur_time == total_steps) {
                ADREPORT(report_instance)
            }
        }
    }, list(
        report_instance = as.symbol(report_stock_instance_name),
        report_f = report_f,
        include_adreport = as.logical(include_adreport),
        run_f = run_f)))

    return(as.list(out))
}

g3a_report_history <- function (
        actions,
        var_re = "__num$|__wgt$",
        out_prefix = "hist_",
        run_f = TRUE,
        run_at = 11) {
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()
    var_re <- paste(var_re, collapse = "|")

    # Form list of definitions as we would do when compiling
    collated_actions <- g3_collate(actions)
    all_actions <- f_concatenate(collated_actions, parent = g3_global_env, wrap_call = call("while", TRUE))
    code <- rlang::f_rhs(all_actions)
    env <- environment(all_actions)
    all_defns <- mget(all.names(code, unique = TRUE), envir = env, inherits = TRUE, ifnotfound = list(NULL))
    all_defns <- all_defns[!is.null(all_defns)]

    # Resolve list of variables we'd like history for
    hist_vars <- grep(var_re, names(all_defns), value = TRUE)

    for (var_name in hist_vars) {
        defn <- all_defns[[var_name]]
        if (is.array(defn)) {
            # No point adding to array which already has time
            if ('time' %in% names(dim(defn))) next

            # Add time dimension to dims
            dimnames <- dimnames(defn)
            if (is.null(dimnames)) dimnames <- lapply(dim(defn), function (x) NULL)
            dim(defn) <- c(dim(defn), time = 1)
            dimnames(defn) <- c(dimnames, list(time = NULL))

            # Make sure dynamic_dims are defined
            if (is.null(attr(defn, "dynamic_dim"))) {
                attr(defn, "dynamic_dim") <- as.list(dim(defn))
                attr(defn, "dynamic_dimnames") <- as.list(dimnames(defn))
            } else {
                attr(defn, "dynamic_dim") <- attr(defn, "dynamic_dim")
                attr(defn, "dynamic_dimnames") <- attr(defn, "dynamic_dimnames")
            }

            # Add dynamic dims for time dimension
            attr(defn, "dynamic_dim")$time <- quote(as_integer(total_steps + 1))
            attr(defn, "dynamic_dimnames")$time <- quote(sprintf("%d-%02d",
                rep(seq(start_year, start_year + total_years - 1L), each = length(step_lengths)),
                rep(seq_along(step_lengths), times = total_years)))
        } else {
            stop("Don't know how to add history to ", var_name, ": ", paste(deparse(defn), collapse = ""))
        }

        # Generate code/env to define history report
        hist_var_name <- paste0(out_prefix, var_name)
        x <- f_substitute(quote(
            if (run_f) hist_var_ss <- var
        ), list(
            hist_var_ss = as.call(c(
                # "hist_var["
                list(as.symbol("["), as.symbol(hist_var_name)),
                # One (missing) for each other dimension
                rep(list(quote(x[])[[3]]), length(dim(defn)) - 1),
                # "cur_time", which is zero-based, needs converting into an index
                list(quote( g3_idx(cur_time + 1) )))),
            run_f = run_f,
            var = as.symbol(var_name)))
        environment(x)[[hist_var_name]] <- defn

        # Turn back into formula, add to out
        out[[step_id(run_at, 'g3a_report_history', var_name)]] <- x
    }

    return(as.list(out))
}

g3a_report_detail <- function (actions,
    run_f = quote( g3_param('report_detail', optimise = FALSE, value = 0L) == 1 ),
    abundance_run_at = 1,
    run_at = 11) {
    c(
        g3a_report_history(
            actions = actions,
            var_re = c('__num$', '__wgt$'),
            out_prefix = "detail_",
            run_f = run_f,
            run_at = abundance_run_at),
        g3a_report_history(
            actions = actions,
            var_re = c('__renewalnum$', '__suit_', '__predby_'),
            out_prefix = "detail_",
            run_f = run_f,
            run_at = run_at),
        NULL)
}
