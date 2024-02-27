g3a_report_stock <- function (report_stock, input_stock, report_f, include_adreport = FALSE, run_f = TRUE, run_at = g3_action_order$report) {
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

    out[[step_id(run_at, 0, report_stock, instance_name, action_name)]] <- g3_step(f_substitute(~{
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

    return(c(as.list(out), g3a_report_var(
        report_stock_instance_name,
        get(report_stock_instance_name),
        stock = report_stock,
        out_prefix = NULL )))  # Don't add history
}

g3a_report_var <- function (
        var_name,
        defn,
        out_prefix = 'hist_',
        stock = NULL,
        final_run_f = quote( cur_time > total_steps ),
        final_run_at = g3_action_order$report_early,
        run_f = TRUE,
        run_at = g3_action_order$report) {
    out <- new.env(parent = emptyenv())

    if (!is.null(stock)) {
        # Resolve actual stock instance name
        # NB: stock_with() would expect the instance symbol to match the stock variable name,
        #     which it won't. Fixing this isn't worth the faff.
        stock_name <- sys.call()[['stock']]
        var_name <- gsub(
            paste0('^', stock_name, '__'),
            paste0(stock$name, '__'),
            var_name)
    }

    if (is.array(defn) && 'time' %in% names(dim(defn))) {
        # Array with time, we don't need to modify it
    } else if (is.null(out_prefix)) {
        # out_prefix turned off, don't add history
    } else if (is.array(defn)) {
        # Array without time, add time dimension to dims
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
        attr(defn, "dynamic_dimnames")$time <- quote( attributes(gen_dimnames(param))[['time']] )

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

        out[[step_id(run_at, 0, 'g3a_report_var', hist_var_name)]] <- x
        var_name <- hist_var_name
    } else {
        stop("Don't know how to add history to ", var_name, ": ", paste(deparse(defn), collapse = ""))
    }

    # NB: 9 as this has to happen after any early reporting
    out[[step_id(final_run_at, 9, 'g3a_report_var', var_name)]] <- f_optimize(f_substitute(quote(
        if (reporting_enabled > 0L && final_run_f) {
            REPORT(var_sym)
        }
    ), list(
        var_sym = as.symbol(var_name),
        final_run_f = final_run_f )))

    return(as.list(out))
}

g3a_report_history <- function (
        actions,
        var_re = "__num$|__wgt$",
        out_prefix = "hist_",
        run_f = TRUE,
        run_at = g3_action_order$report) {
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()
    var_re <- paste(var_re, collapse = "|")

    # Form list of definitions as we would do when compiling
    collated_actions <- g3_collate(actions)
    all_actions <- f_concatenate(collated_actions, parent = g3_env, wrap_call = call("while", TRUE))
    code <- rlang::f_rhs(all_actions)
    env <- environment(all_actions)
    all_vars <- all.names(code, unique = TRUE)
    all_defns <- mget(all_vars, envir = env, inherits = TRUE, ifnotfound = list(NULL))
    all_defns <- all_defns[!is.null(all_defns)]

    # Resolve list of variables we'd like history for
    hist_vars <- grep(var_re, names(all_defns), value = TRUE)

    out <- lapply(hist_vars, function (var_name) g3a_report_var(
        var_name,
        all_defns[[var_name]],
        out_prefix = out_prefix,
        # Only check total_steps if it's already defined
        final_run_f = if ('total_steps' %in% all_vars) quote( cur_time > total_steps ) else TRUE,
        run_f = run_f,
        run_at = run_at))
    out <- do.call(c, out)

    return(as.list(out))
}

g3a_report_detail <- function (actions,
    # NB: We could in theory remove report_detail, but g3_fit looks for it in params
    run_f = quote( g3_param('report_detail', optimise = FALSE, value = 1L) == 1 ),
    abundance_run_at = g3_action_order$report_early,
    run_at = g3_action_order$report) {
    c(
        g3a_report_history(
            actions = actions,
            var_re = paste(c(
                '^.+_surveyindices_.+__params$',
                '^nll$' ), collapse = "|"),
            out_prefix = NULL,  # Don't log history
            run_f = run_f,
            run_at = run_at),
        g3a_report_history(
            actions = actions,
            var_re = c('__num$', '__wgt$'),
            out_prefix = "detail_",
            run_f = f_substitute(quote(cur_time <= total_steps && run_f), list(run_f = run_f)),
            run_at = abundance_run_at),
        g3a_report_history(
            actions = actions,
            var_re = c('__renewalnum$', '__spawnednum$', '__suit_', '__predby_'),
            out_prefix = "detail_",
            run_f = run_f,
            run_at = run_at),
        NULL)
}

# Find all vars from collated actions that get assigned to, we'll report those.
# ... is list of functions to regex filter, e.g. REPORT = '.'
action_reports <- function (actions, ...) {
    terms <- new.env(parent = emptyenv())
    find_assignments <- function (f, ignore_vars) call_replace(f,
        g3_with = function (x) find_assignments(
            x[[length(x)]],
            # Ignore any scoped variables when recursing
            c(ignore_vars, vapply(
                g3_with_extract_terms(x),
                function (term) as.character(term[[2]]),
                character(1)))),
        "<-" = function(x) {
            lhs <- x[[2]]
            # Strip off any subsetting calls
            while (is.call(lhs)) {
                lhs <- lhs[[2]]
            }
            if (!is.symbol(lhs)) stop("Unknown lhs: ", lhs)
            lhs <- as.character(lhs)
            if (!(lhs %in% ignore_vars)) {
                terms[[lhs]] <<- TRUE
            }
        })
    for (a in actions) find_assignments(a, c())

    # For each action_reports() argument, filter terms by the regex value
    # and treat the name as the name of the report function.
    args <- list(...)
    concatenate_calls <- function (x) as.call(c(as.symbol("{"), x))
    f_optimize(concatenate_calls(lapply(seq_along(args), function (arg_i) {
        fn_name <- names(args)[[arg_i]]  # i.e. the argument name
        fn_regex <- args[[arg_i]]  # i.e. the argument value
        report_var_names <- sort(grep(fn_regex, names(terms), value = TRUE))

        concatenate_calls(lapply(
            report_var_names,
            function (x) call(fn_name, as.symbol(x))))
    })))
}

g3a_report_vars <- function (actions,
        var_re = '.',
        run_at = g3_action_order$report_early) {
    out <- new.env(parent = emptyenv())
    action_name <- unique_action_name()

    # Form list of definitions as we would do when compiling
    collated_actions <- g3_collate(actions)
    all_actions <- f_concatenate(collated_actions, parent = g3_env, wrap_call = call("while", TRUE))
    code <- rlang::f_rhs(all_actions)
    env <- environment(all_actions)
    all_vars <- all.names(code, unique = TRUE)

    # NB: 9 as this has to happen after any early reporting
    out[[step_id(run_at, 9, 'g3a_report_vars', var_re)]] <- f_optimize(f_substitute(quote(
        if (reporting_enabled > 0L && cond) x
    ), list(
        # Only check total_steps if it's already defined
        cond = if ('total_steps' %in% all_vars) quote( cur_time > total_steps ) else TRUE,
        x = action_reports(collated_actions, REPORT = var_re) )))

    return(as.list(out))
}
