# run: Common utilities from turning a set of steps into code

# Combine all provided action lists into one action list, throwing away duplicates
g3_collate <- function(action_list) {
    # Combine all lists into an environment, later lists win over previous
    # TODO: Just concatenate the lists backwards?
    actions <- new.env(parent = emptyenv())

    # For any lone formulas without names, assume they just go on the end
    # This will mostly be test case convenince, not general use
    if (is.null(names(action_list))) {
        names(action_list) <- vapply(seq_along(action_list), function (i) step_id(999, i), character(1))
    }

    for (n in names(action_list)) {
        l <- action_list[[n]]
        if (rlang::is_formula(l)) {
            # One level of list, add this formula
            actions[[n]] <- l
        } else {
            # 2 levels, recurse over inner list too
            for (sub_n in names(l)) {
                actions[[sub_n]] <- l[[sub_n]]
            }
        }
    }
    actions <- as.list(actions)

    # Order items in alphanumeric order
    return(actions[order(names(actions))])
}

# Find all vars from collated actions that get assigned to, we'll report those.
# ... is list of functions to regex filter, e.g. g3_report = '.'
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
            # lhs is either a symbol or a subsetting call
            if (is.symbol(lhs)) {
                lhs <- as.character(lhs)
            } else if (is.call(lhs)) {
                lhs <- as.character(lhs[[2]])
            } else {
                stop("Unknown lhs: ", lhs)
            }
            if (!(lhs %in% ignore_vars)) {
                terms[[lhs]] <<- TRUE
            }
        })
    for (a in actions) find_assignments(a, c())

    # For each action_reports() argument, filter terms by the regex value
    # and treat the name as the name of the report function.
    concatenate_calls <- function (x) as.call(c(as.symbol("{"), x))
    f_optimize(concatenate_calls(lapply(seq_len(...length()), function (arg_i) {
        fn_name <- ...names()[arg_i]  # i.e. the argument name
        fn_regex <- ...elt(arg_i)  # i.e. the argument value
        report_var_names <- sort(grep(fn_regex, names(terms), value = TRUE))

        concatenate_calls(lapply(
            report_var_names,
            function (x) call(fn_name, as.symbol(x))))
    })))
}

scope_to_parameter_template <- function (scope, return_type) {
    parts <- lapply(scope, function (val) attr(val, 'param_template'))
    names(parts) <- NULL
    if (return_type == 'data.frame') do.call(rbind, parts) else do.call(c, c(list(), parts))
}

# Given a g3_with(x := 2, y := 4, exp) call, extract calls to set terms
g3_with_extract_terms <- function(x) {
    do.call(c, lapply(as.list(x), function (arg) {
        if (is.call(arg) && arg[[1]] == ":=") {
            list(call("<-", arg[[2]], arg[[3]]))
        } else {
            list()
        }
    }))
}
# g3_with_extract_terms(quote(g3_with(x := 2, parp.x := 4 + 4, moo)))
