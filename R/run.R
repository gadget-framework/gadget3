# run: Common utilities from turning a set of steps into code

# Return (x) as vector, to be treated as a vector even if 1-element long
as_force_vector <- function (x) {
    class(x) <- c("force_vector", class(x))
    return(x)
}

# Return all args as vector, to be treated as a vector even if 1-element long
force_vector <- function (...) as_force_vector(c(...))

# Remove mark, so we don't leak it to the outside world
hide_force_vector <- function (x) {
    # NB: Doing this indiscrimitely upsets array classes & makes them more matrix-ish(?)
    if (inherits(x, "force_vector")) class(x) <- class(x)[class(x) != "force_vector"]
    if (inherits(x, "force_numeric")) class(x) <- class(x)[class(x) != "force_numeric"]
    return(x)
}

# Should (x) be treated as a vector?
is_force_vector <- function (x) length(x) > 1 || inherits(x, "force_vector")

# Add force_numeric marker to (x), so we define as array<double>, e.g.
as_force_numeric <- function (x) {
    class(x) <- c("force_numeric", class(x))
    return(x)
}
is_force_numeric <- function (x) inherits(x, "force_numeric")

# Combine all provided action lists into one action list, throwing away duplicates
g3_collate <- function(action_list) {
    # Collapse lists / sub-lists into a single list, preserving order
    unnest <- function (l) {
        # Recurse along l generating sub-lists, then concatentate together
        do.call(c, lapply(seq_along(l), function (i) {
            if (is.null(l[[i]])) {
                # Strip NULLs (and avoid calling environment(NULL))
                c()
            } else if (is.list(l[[i]])) {
                # Flattern sub-lists by recursing
                unnest(l[[i]])
            } else {
                # Convert non-lists into a single-item list
                out <- structure(list(l[[i]]), names = names(l)[[i]])

                # Pull out items stuffed in environment()
                ancillary_step_names <- grep("^(?:\\d|-)\\d{2}:", names(environment(l[[i]])), value = TRUE, perl = TRUE)
                if (length(ancillary_step_names) > 0) out <- c(out, mget(ancillary_step_names, envir = environment(l[[i]])))

                out
            }
        }))
    }
    actions <- unnest(action_list)

    # Filter NULL
    actions <- actions[vapply(actions, Negate(is.null), logical(1))]

    # If no names (probably a test), nothing else to do
    if (is.null(names(actions))) return(actions)

    # If name is missing, assume it goes on the end
    names(actions) <- vapply(seq_along(actions), function (i) {
        if (nzchar(names(actions)[[i]])) names(actions)[[i]] else step_id(999, "g3_collate", i)
    }, character(1))

    # De-duplicate by action-order, then sort by name
    actions <- actions[!duplicated(names(actions), fromLast = TRUE)]
    actions <- actions[order(names(actions), method = 'radix')]

    return(actions)
}

scope_to_parameter_template <- function (scope, return_type) {
    parts <- lapply(scope, function (val) attr(val, 'param_template'))
    names(parts) <- NULL
    if (return_type == 'data.frame') do.call(rbind, parts) else do.call(c, c(list(), parts))
}

# Return map of all names in scope that would get escaped
scope_to_cppnamemap <- function (scope) {
    out <- names(scope)
    out <- out[!is.null(out)]
    if (length(out) == 0) return(c())
    names(out) <- cpp_escape_varname(out)
    out[names(out) != out]
}

# Update any bounds values in model_data
update_data_bounds <- function (model_data, param_tmpl) {
    if (is.null(param_tmpl)) {
        # User didn't supply extra parameters, nothing to do
    } else if (is.data.frame(param_tmpl)) {
        for (param_type in c('lower', 'upper')) {
            for (i in seq_len(nrow(param_tmpl))) {
                data_var <- paste0(param_tmpl[i, 'switch'], '__', param_type)
                if (!exists(data_var, envir = model_data)) next

                data_val <- param_tmpl[i, param_type]
                model_data[[data_var]] <- if (is.na(data_val)) NaN else data_val
            }
        }
    } else {
        stop("Unknown param_tmpl type: ", deparse1(param_tmpl))
    }
    return(model_data)
}

# Given a g3_with(x := 2, y := 4, exp) call, extract calls to set terms
g3_with_extract_terms <- function(x) {
    # Strip off g3_with symbol, exp
    x <- head(tail(as.list(x), -1), -1)
    lapply(x, function (arg) {
      if (is.call(arg) && arg[[1]] == ":=") {
          call("<-", arg[[2]], arg[[3]])
      } else {
          stop("Unknown g3_with assignment format ", arg)
      }
    })
}
# g3_with_extract_terms(quote(g3_with(x := 2, parp.x := 4 + 4, moo)))

gen_param_tbl_name <- function (base, vals) {
    postfix <- if (endsWith(base, '_exp')) '_exp' else ''
    out <- paste0(c(
        gsub('_exp$', '', base),
        vals,
        NULL), collapse = ".")
    if (endsWith(base, '_exp')) out <- paste0(out, '_exp')
    return(out)
}
