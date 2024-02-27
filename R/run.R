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
    return(x)
}

# Should (x) be treated as a vector?
is_force_vector <- function (x) length(x) > 1 || inherits(x, "force_vector")

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
    return(actions[order(names(actions), method = 'radix')])
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
            for (i in which(is.finite(param_tmpl[[param_type]])) ) {
                data_var <- cpp_escape_varname(paste0(param_tmpl[i, 'switch'], '__', param_type))
                if (!exists(data_var, envir = model_data)) next

                data_val <- param_tmpl[i, param_type]
                model_data[[data_var]] <- if (is.finite(data_val)) data_val else NaN
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
