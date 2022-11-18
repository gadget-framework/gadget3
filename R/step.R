# Adapt a formulae into one that will iterate over stocks. Will replace the following functions:
# - debug_label(...) - Replace any stock arguments with the stock name
# - debug_trace(...) - Replace any stock arguments with the stock name
# - stock_iterate(stock, block) - wrap (block) with code to iterate over (stock) lengthgroup vectors
# - stock_intersect(stock, block) - wrap (block) with code to intersect with an outer stock we're iterating over
# - stock_interact(stock, block, prefix) - wrap (block) with code to interact with an outer stock we're iterating over
#       interact here means intersect over physical dimensions (area / time), and consider combinatoral explosion of rest (e.g. age)
#       (prefix) is a string to distinguish between variables, e.g. if prefix = "prey", there will be age and prey_age variables.
# - stock_with(stock, block) - Make sure any references to (stock) in (block) uses the right name
# References to the stock will also be renamed to their final name
g3_step <- function(step_f, recursing = FALSE, orig_env = environment(step_f)) {
    stopifnot(rlang::is_formula(step_f))

    # For formula (f), rename all (old_name)__var variables to (new_name)__var, mangling environment to match
    stock_rename <- function(f, old_name, new_name) {
        old_name <- as.character(old_name)
        old_g3_global_init_val <- attr(f, "g3_global_init_val")
        if (length(old_name) != 1) stop("Stocks should be variable references")
        f_vars <- all.vars(f)
        if (rlang::is_formula(f)) {
            f_vars <- union(f_vars, ls(rlang::f_env(f)))
        }
        stock_re <- paste0("^", old_name, "__")

        # Find all vars matching stock_re
        subs <- new.env(parent = emptyenv())
        for (old_var in f_vars[grepl(stock_re, f_vars)]) {
            new_var <- sub(stock_re, paste0(new_name, "__"), old_var)
            assign(old_var, as.symbol(new_var), envir = subs)
        }

        # Update environment with new names of variables
        if (rlang::is_formula(f)) {
            new_env <- rlang::env_clone(rlang::f_env(f))
            for (old_var in f_vars[grepl(stock_re, f_vars)]) {
                if (exists(old_var, envir = rlang::f_env(f), inherits = FALSE)) {
                    new_var <- sub(stock_re, paste0(new_name, "__"), old_var)
                    remove(list = old_var, envir = new_env)
                    assign(new_var, get(old_var, envir = rlang::f_env(f), inherits = FALSE), envir = new_env)
                }
            }
            f <- f_substitute(f, as.list(subs))
            rlang::f_env(f) <- new_env
        } else {
            # Just use regular substitute
            f <- eval(call("substitute", f, as.list(subs)))
        }

        # Preserve g3_global_init_val
        # NB: We can't preserve all attributes, since then we copy the environment
        attr(f, "g3_global_init_val") <- old_g3_global_init_val

        return(f)
    }

    # Turn nested g3_with() into a single call
    collapse_g3_with <- function (f) {
        call_replace(f, g3_with = function (x) {
            inner <- collapse_g3_with(x[[length(x)]])
            defns <- head(as.list(x), -1)

            # Remove anything := adf_marker from defns
            defns <- Filter(
                function(y) !(is.call(y) && identical(y[[1]], as.symbol(':=')) && identical(y[[3]], as.symbol("adf_marker"))),
                defns)

            if (length(defns) == 1) {
                # No definitions left in this g3_with
                return(as.call(inner))
            }
            if (is.call(inner) && inner[[1]] == as.symbol("g3_with")) {
                # There's a nested g3_with, merge with our call
                return(as.call(c(
                    defns,
                    tail(as.list(inner), -1))))
            }
            # Not nested g3_with, replace with recursed version
            return(as.call(c(
                defns,
                list(inner))))
        })
    }

    stock_interactvar_prefix <- function (f, prefix) {
        stopifnot(rlang::is_formula(f))
        stopifnot(is.character(prefix) && length(prefix) == 1)

        # Find all vars prefixed with "interactvar_", and remove prefixes
        interactvars <- all.vars(f)
        names(interactvars) <- interactvars
        interactvars <- interactvars[startsWith(interactvars, "interactvar_")]
        interactvars <- gsub("^interactvar_(.+)", paste0(prefix, "_\\1"), interactvars)

        # Replace iterator interactvars in expression
        f_substitute(f, lapply(interactvars, as.symbol))
    }

    # Add any formula definitions from f into f, if they include depend_vars
    add_dependent_formula <- function (f, depend_vars, filter_fn = NULL) {
        # Repeatedly look for definitions we should be adding (so we add sub-definitions)
        while(TRUE) {
            added_defn <- FALSE
            for (var_name in all_undefined_vars(f)) {  # NB: all_undefined_vars will get rid of definitions from previous loop
                defn <- environment(f)[[var_name]]
                if (!is.call(defn)) next
                if (!isTRUE(depend_vars) && !('stock_ss' %in% all.names(defn)) && length(intersect(all_undefined_vars(defn), depend_vars)) == 0) {
                    # There's a depend vars, but this formula doesn't depend on any of them, optionally modify and return
                    if (!is.null(filter_fn)) {
                        assign(var_name, filter_fn(defn), envir = environment(f))
                    }
                    next
                }

                if (is.null(attr(defn, 'g3_global_init_val')) ) {
                    # Non-global, add scoped variable with g3_with()
                    impl_f <- ~g3_with(var := defn, f)
                } else if (identical(rlang::f_rhs(defn), as.symbol("noop"))) {
                    # Global with only a initial definition, do ~nothing
                    # NB: adf_marker will get removed later with collapse_g3_with()
                    impl_f <- ~g3_with(var := adf_marker, f)
                } else {
                    # Global, add definition and marker to stop repetition
                    impl_f <- ~g3_with(var := adf_marker, {var <- defn ; f})
                }
                f <- f_substitute(impl_f, list(var = as.symbol(var_name), defn = defn, f = f))
                added_defn <- TRUE
            }
            if (!added_defn) break
        }

        return(f)
    }

    repl_stock_fn <- function (x, to_replace) {
        stock_var <- x[[2]]
        stock <- get(as.character(stock_var), envir = orig_env)
        prefix <- x[['prefix']]
        if (is.null(prefix)) prefix <- ""  # NB: Remove any interactvar prefix by default

        # Recurse first, filling out any inner functions
        inner_f <- call_to_formula(x[[3]], rlang::f_env(step_f))
        inner_f <- g3_step(inner_f, recursing = TRUE, orig_env = orig_env)

        # Wrap with stock's code
        if (is.list(stock[[to_replace]])) {
            repl_list <- stock[[to_replace]]
            out_f <- quote(extension_point)
            inner_vars <- all.vars(inner_f)
            # List of formulas, select the relevant ones and combine
            for (i in seq_along(repl_list)) {
                if (!is.symbol(stock$iter_ss[[i]])) next
                if (as.character(stock_rename(stock$iter_ss[[i]], "stock", stock_var)) %in% inner_vars) {
                    # We use the subset-iterator in inner code, so wrap with this iterator
                    # (e.g. stock__area_idx in code ==> iterate over area)
                    out_f <- do.call(substitute, list(repl_list[[i]], list(extension_point = out_f)))
                } else if (as.character(stock_rename(stock$iter_ss[[i]], "stock", stock$name)) %in% inner_vars) {
                    # We have to check the final name too in case it's used in e.g. g3a_report_stock
                    out_f <- do.call(substitute, list(repl_list[[i]], list(extension_point = out_f)))
                }
            }
            # Add in stock environment
            out_f <- call_to_formula(out_f, stock$env)
        } else {
            stop("Unknown stock_fn type: ", out_f)
        }
        out_f <- stock_interactvar_prefix(out_f, prefix)
        out_f <- stock_rename(out_f, "stock", stock_var)
        # Make sure formulas are defined if they need anything the stock code defines
        inner_f <- add_dependent_formula(inner_f, setdiff(all.vars(out_f), all_undefined_vars(out_f)), function (f) {
            # Attach outer environment so items resolve
            if (rlang::is_formula(f)) {
                environment(f) <- rlang::env_clone(
                    environment(f),
                    parent = environment(step_f))
            } else {
                f <- call_to_formula(f, rlang::f_env(step_f))
            }
            # Fill out any stock functions, rename stocks
            f <- stock_rename(f, stock_var, stock$name)
            f <- g3_step(f, recursing = TRUE, orig_env = orig_env)
            return(f)
        })
        # Run g3_step again to fix up dependents that got added
        inner_f <- g3_step(inner_f, recursing = TRUE, orig_env = orig_env)
        out_f <- f_substitute(out_f, list(extension_point = inner_f))
        out_f <- stock_rename(out_f, stock_var, stock$name)

        # Add environment to formulae's environment, return inner call
        environment_merge(rlang::f_env(step_f), rlang::f_env(out_f), ignore_overlap = TRUE)
        return(rlang::f_rhs(out_f))
    }

    debug_label_fn <- function (x) {
        comment_str <- paste(vapply(tail(x, -1), function (a) {
            if (is.symbol(a)) {
                # Dereference symbols
                a <- get(as.character(a), envir = rlang::f_env(step_f))
                # Stocks have a name attribute
                if (is.list(a) && 'name' %in% names(a)) a <- a$name
            }
            return(as.character(a))
        }, character(1)), collapse = "")

        return(call(as.character(x[[1]]), comment_str))
    }
    if (!recursing) {
        # Add post-renamed versions of stocks to environment, so e.g.
        # g3a_report_stock(agg_report, prey_a, ~stock_ss(prey_a__num))
        # ...can still find prey_a (when "stock__num" might have been more accurate)
        for (n in ls(orig_env)) {
            if (g3_is_stock(orig_env[[n]]) && !exists(orig_env[[n]]$name, envir = orig_env)) {
                assign(orig_env[[n]]$name, orig_env[[n]], envir = orig_env)
            }
        }
    }

    rv <- f_optimize(call_replace(step_f,
        debug_label = debug_label_fn,  # Arguments: stock variable, comment, stock variable, ...
        debug_trace = debug_label_fn,  # Arguments: stock variable, comment, stock variable, ...
        stock_assert = function (x) {
            comment_str <- paste(vapply(tail(x, -2), function (a) {
                if (is.symbol(a)) {
                    # Dereference symbols
                    a <- get(as.character(a), envir = rlang::f_env(step_f))
                    # Stocks have a name attribute
                    if (is.list(a) && 'name' %in% names(a)) a <- a$name
                }
                return(as.character(a))
            }, character(1)), collapse = "")

            return(call('assert_msg', g3_step(as.formula(call("~", x[[2]]), env = environment(step_f)), recursing = TRUE, orig_env = orig_env), comment_str))
        },
        stock_reshape = function (x) {  # Arguments: dest_stock, source expression, will use the first variable we come across
            stock_var <- x[[2]]
            stock <- get(as.character(stock_var), envir = orig_env)

            # Recurse first, letting renames happen
            inner_f <- call_to_formula(x[[3]], rlang::f_env(step_f))
            inner_f <- g3_step(inner_f, recursing = TRUE, orig_env = orig_env)

            if (!("length" %in% names(stock$dim))) {
                # No length dimension, so sum everything
                out_f <- f_substitute(quote( sum(inner_f) ), list(inner_f = inner_f))
                environment_merge(rlang::f_env(step_f), rlang::f_env(out_f))
                return(rlang::f_rhs(out_f))
            }

            # Assume source stock is the first in inner_f, probably true(?)
            source_stock_var <- sub('__.*$', '', all.vars(inner_f)[[1]])
            source_stock <- get(source_stock_var, envir = orig_env)
            if (!("length" %in% names(source_stock$dim))) stop("Source stock ", source_stock$name, " has no length, can't resize")
            source_lg <- g3_stock_def(source_stock, 'minlen')

            dest_lg <- g3_stock_def(stock, 'minlen')

            # Get upper bound for length groups, if infinite guess something that should work
            # NB: Assuming plus-group is one-long is cheating, but probably no easy general answers
            source_upperlen <- g3_stock_def(source_stock, 'upperlen')
            if (is.infinite(source_upperlen)) source_upperlen <- tail(source_lg, 1) + 1
            dest_upperlen <- g3_stock_def(stock, 'upperlen')
            if (is.infinite(dest_upperlen)) dest_upperlen <- max(tail(source_lg, 1), tail(dest_lg, 1)) + 1

            if (isTRUE(all.equal(source_lg, dest_lg))) {
                # Source == dest, so no point doing a transform
                out_f <- inner_f
            } else {
                matrix_name <- paste0(source_stock$name, '_', stock$name, '_lgmatrix')

                # Formulae to apply matrix
                out_f <- f_substitute(quote( g3_matrix_vec(lg_matrix, inner_f) ), list(
                    lg_matrix = as.symbol(matrix_name),
                    inner_f = inner_f))

                # Generate a matrix to transform one to the other
                assign(matrix_name, do.call('rbind', lapply(seq_along(dest_lg), function (dest_idx) vapply(seq_along(source_lg), function (source_idx) {
                    lower_s <- source_lg[[source_idx]]
                    upper_s <- if (source_idx >= length(source_lg)) source_upperlen else source_lg[[source_idx + 1]]

                    lower_d <- dest_lg[[dest_idx]]
                    upper_d <- if (dest_idx >= length(dest_lg)) dest_upperlen else dest_lg[[dest_idx + 1]]

                    intersect_size <- min(upper_s, upper_d) - max(lower_s, lower_d)
                    return(if (intersect_size > 0) intersect_size / (upper_s - lower_s) else 0)
                }, numeric(1)))), envir = environment(out_f))
            }

            # Add environment to formulae's environment, return inner call
            environment_merge(rlang::f_env(step_f), rlang::f_env(out_f))
            return(rlang::f_rhs(out_f))
        },
        # stock_ss subsets stock data var, overriding any set expressions
        stock_ss = function (x) { # Arguments: stock data variable (i.e. stock__num), [dim_name = override expr, ...]
            stock_inst <- x[[2]]  # NB: A "stock__num", not "stock"
            stock_var <- gsub('__.*$', '', stock_inst)
            stock <- get(stock_var, envir = orig_env)
            ss_overrides <- as.list(tail(x, -2))

            # By default length is "missing"
            if (!('length' %in% names(ss_overrides)) && 'length' %in% names(stock$iter_ss)) ss_overrides$length <- quote(.[])[[3]]

            # Remove "length = default" overrides, so we have a means to win over the above
            ss_overrides <- Filter(function (x) !identical(x, as.symbol('default')), ss_overrides)

            # Get subset arguments
            ss <- stock$iter_ss

            # No dimensions mean a 1-entry array (see g3_stock_instance)
            if (length(ss) == 0) ss <- list(quote(g3_idx(1)))

            # Swap in overrides
            ss[names(ss_overrides)] <- ss_overrides
            return(stock_rename(as.call(c(list(as.symbol("["), stock_inst), unname(ss))), "stock", stock_var))
        },
        # stock_ssinv subsets stock data var, keeping the specified dimensions (i.e. blanking it's part in the normal subset)
        stock_ssinv = function (x) { # Arguments: stock data variable (i.e. stock__num), dimension names.
            stock_inst <- x[[2]]  # NB: A "stock__num", not "stock"
            stock_var <- gsub('__.*$', '', stock_inst)
            stock <- get(stock_var, envir = orig_env)
            wanted_dims <- as.character(tail(x, -2))

            # Get subset arguments
            ss <- stock$iter_ss

            # No dimensions mean a 1-entry array (see g3_stock_instance)
            if (length(ss) == 0) ss <- list(quote(g3_idx(1)))

            # Replace unwanted dimensions with missing symbol
            ss[!(names(stock$dimnames) %in% wanted_dims)] <- list(quote(x[])[[3]])
            return(stock_rename(as.call(c(list(as.symbol("["), stock_inst), unname(ss))), "stock", stock_var))
        },
        stock_switch = function (x) {  # Arguments: stock variable, stock_name = answer, ... default
            stock_var <- x[[2]]
            stock <- get(as.character(stock_var), envir = orig_env)

            if (is.null(names(x))) {
                # Nothing is named, so return final (presumably only) answer
                out <- x[[length(x)]]
            } else if (!is.null(x[[stock$name]])) {
                # Find param with name matching stock, return it
                out <- x[[stock$name]]
            } else if (!nzchar(names(x)[[length(x)]])) {
                # Final one isn't named, return that
                out <- x[[length(x)]]
            } else {
                stop("stock_switch has no result for ", stock$name, ": ", deparse(x))
            }

            # Wrap with implicit stock_with(stock_var, ...)
            # NB: Not strictly necessary, but given other stock_* will rename, it seems ugly not to.
            out <- call("stock_with", stock_var, out)

            # Recurse, filling out any inner functions
            # NB: The formula uses the same env as the outer, so we don't need to merge afterwards
            out <- g3_step(call_to_formula(out, environment(step_f)), recursing = TRUE, orig_env = orig_env)
            return(rlang::f_rhs(out))
        },
        stock_param = function (x) { # Arguments: stock variable, name_part = NULL, name, ....
            stock_var <- x[[2]]
            stock <- get(as.character(stock_var), envir = orig_env)
            rest <- tail(as.list(x), -2)

            # Use name_part if part of the call
            if (!is.null(rest$name_part)) {
                param_name <- paste(stock$name_part[eval(rest$name_part, envir = baseenv())], collapse = '_')
            } else {
                param_name <- stock$name
            }
            rest$name_part <- NULL

            # Append first argument to complete name
            param_name <- paste(c(param_name, rest[[1]]), collapse = ".")
            rest <- tail(rest, -1)

            # Make a g3_param call with the newly-named param
            as.call(c(list(quote(g3_param), param_name), rest))
        },
        stock_param_table = function (x) { # Arguments: stock variable, name_part = NULL, name, ....
            stock_var <- x[[2]]
            stock <- get(as.character(stock_var), envir = orig_env)
            rest <- tail(as.list(x), -2)

            # Use name_part if part of the call
            if (!is.null(rest$name_part)) {
                param_name <- paste(stock$name_part[eval(rest$name_part, envir = baseenv())], collapse = '_')
            } else {
                param_name <- stock$name
            }
            rest$name_part <- NULL

            # Append first argument to complete name
            param_name <- paste(c(param_name, rest[[1]]), collapse = ".")
            rest <- tail(rest, -1)

            # Apply stock rename to table_defn
            table_defn <- rest[[1]]
            table_defn <- call("stock_with", stock_var, table_defn)  # stock_with(stock, ...) is implicit
            table_defn <- rlang::f_rhs(g3_step(
                call_to_formula(table_defn, env = as.environment(list())),
                recursing = TRUE, orig_env = orig_env))
            rest <- tail(rest, -1)

            # Make a g3_param call with the newly-named param
            as.call(c(list(quote(g3_param_table), param_name, table_defn), rest))
        },
        stock_with = function (x) {  # Arguments: stock variable, inner code block
            return(repl_stock_fn(x, 'with'))
        },
        stock_iterate = function (x) {  # Arguments: stock variable, inner code block
            return(repl_stock_fn(x, 'iterate'))
        },
        stock_interact = function (x) {  # Arguments: stock variable, inner code block
            return(repl_stock_fn(x, 'interact'))
        },
        stock_intersect = function (x) {  # Arguments: stock variable, inner code block
            return(repl_stock_fn(x, 'intersect'))
        }))

        if (!recursing) {
            # Add anything that's not a global_formula to this level
            rv <- add_dependent_formula(rv, TRUE)
            # Neaten output code by collapsing the stack of g3_with()s we made
            rv <- collapse_g3_with(rv)
            rv <- f_optimize(rv)
        }

        return(rv)
}

# Turn number/stock/string params into a single string indentifying that step
step_id <- function (...) {
    parts <- vapply(list(...), function (part) {
        if (is.numeric(part)) {
            # Number, format with leading zeros
            return(sprintf("%03d", part))
        }
        if (is.list(part) && !is.null(part$name)) {
            # Stock, extract name
            return(sprintf("%-20s", part$name))
        }
        if (is.character(part)) {
            # Character, normalised to 20 chars
            return(sprintf("%-20s", part[[1]]))
        }
        stop("Invalid parameter to step_id: ", deparse(part))
    }, character(1))
    paste(parts, collapse = ":")
}

# Generate action name based on parent call
unique_action_name <- function () {
    out <- paste(deparse(sys.call(-1)), collapse = "")
    out <- substr(digest::sha1(out), 1, 20)
    out
}

# Find first label (or trace if minor_steps) for a given step
step_find_desc <- function (s, minor_steps = FALSE) {
    labels <- c()
    traces <- c()
    call_replace(s,
        debug_label = function (x) labels <<- c(labels, x[[2]]),
        debug_trace = function (x) traces <<- c(traces, x[[2]]))
    if (length(labels) > 0) return(labels[[1]])
    if (minor_steps && length(traces) > 0) return(traces[[1]])
    return(as.character(NA))
}

# Convert a list into a stock_switch call
list_to_stock_switch <- function(l, stock_var = "stock") {
    if (is.list(l) && is.null(names(l))) {
        # No stocks to choose from, so should be a single-item list
        if (length(l) > 1) stop("Only one default option allowed")
        l <- l[[1]]
    }
    if (!is.list(l)) {
        # Choosing one item is just stock_with()
        return(f_substitute(quote(
            stock_with(stock_var, l)
        ), list(stock_var = stock_var, l = l)))
    }

    # NB: Substituting "" doesn't work, so we turn the non-named
    #     parameter to _def internally
    if (sum(!nzchar(names(l))) > 1) stop("Only one default option allowed")
    names(l)[!nzchar(names(l))] <- "_def"

    # Build stock_switch call mapping element_name = element_name
    out_call <- lapply(names(l), as.symbol)
    names(out_call) <- ifelse(names(l) == '_def', "", names(l))
    out_call <- as.call(c(as.symbol("stock_switch"), as.symbol(stock_var), out_call))

    # Use input list as our substitutions
    f_substitute(out_call, l)
}
