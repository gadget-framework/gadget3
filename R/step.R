# Adapt a formulae into one that will iterate over stocks. Will replace the following functions:
# - debug_label(...) - Replace any stock arguments with the stock name
# - debug_trace(...) - Replace any stock arguments with the stock name
# - stock_iterate(stock, block) - wrap (block) with code to iterate over (stock) lengthgroup vectors
# - stock_intersect(stock, block) - wrap (block) with code to intersect with an outer stock we're iterating over
# - stock_interact(stock, block, prefix) - wrap (block) with code to interact with an outer stock we're iterating over
#       interact here means intersect over physical dimensions (area / time), and consider combinatoral explosion of rest (e.g. age)
#       (prefix) is a string to distinguish between variables, e.g. if prefix = "prey", there will be age and prey_age variables.
# - stock_with(stock, block) - Make sure any references to (stock) in (block) uses the right name
# - stock_isdefined(var) - Make sure variable var is defined at this point (e.g. an iterator)
# References to the stock will also be renamed to their final name
g3_step <- function(step_f, recursing = FALSE, orig_env = environment(step_f)) {
    stopifnot(rlang::is_formula(step_f) || is.call(step_f))
    if (!rlang::is_formula(step_f)) step_f <- call_to_formula(step_f, new.env(parent = emptyenv()))

    # Run g3_step an all dependent formulas
    add_dependent_formula_filter <- function (f) {
        if (!is.call(f)) return(f)

        stacked_env <- rlang::env_clone(environment(step_f), parent = orig_env)
        f <- g3_step(f, recursing = TRUE, orig_env = stacked_env)

        if (is.call(attr(f, "g3_global_init_val"))) {
            attr(f, "g3_global_init_val") <- g3_step(attr(f, "g3_global_init_val"), recursing = TRUE, orig_env = stacked_env)
        }
        return(f)
    }

    # Traverse (in_c), converting stock_isdefined() to TRUE/FALSE, depending if it's var was found
    resolve_stock_isdefined <- function (in_c, sym_names = c()) {
        call_replace(in_c,
            g3_with = function (x) {
                # Find all assignments in g3_with(), add to list of things we're looking for
                new_syms <- unlist(lapply(x, function (y) {
                    if (is.call(y) && identical(y[[1]], as.symbol(":="))) as.character(y[[2]]) else NULL
                }))
                x[[length(x)]] <- resolve_stock_isdefined(x[[length(x)]], c(sym_names, new_syms))
                return(x)
            },
            "for" = function (x) {
                # Add iterator from for loop to things we're looking for
                new_syms <- as.character(x[[2]])
                x[[length(x)]] <- resolve_stock_isdefined(x[[length(x)]], c(sym_names, new_syms))
                return(x)
            },
            stock_isdefined = function (x) {
                # Replace with TRUE iff argument is in sym_names
                return(as.character(x[[2]]) %in% sym_names)
            })
    }

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
                return(if (is.symbol(inner)) inner else as.call(inner))
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

    repl_stock_fn <- function (x, to_replace) {
        stock_var <- x[[2]]
        stock <- get(as.character(stock_var), envir = orig_env)
        prefix <- x[['prefix']]
        if (is.null(prefix)) prefix <- ""  # NB: Remove any interactvar prefix by default

        # Recurse first, filling out any inner functions
        inner_f <- call_to_formula(x[[3]], environment(step_f))
        inner_f <- g3_step(inner_f, recursing = TRUE, orig_env = orig_env)

        # Wrap with stock's code
        if (is.list(stock[[to_replace]])) {
            repl_list <- stock[[to_replace]]
            out_f <- quote(extension_point)
            inner_vars <- all_undefined_vars(inner_f, recursive = TRUE)
            # List of formulas, select the relevant ones and combine
            for (i in seq_along(repl_list)) {
                if (!is.symbol(stock$iter_ss[[i]])) {
                    # Iterator isn't a symbol, assume we need to iterate over it (see g3s_modeltime)
                    out_f <- do.call(substitute, list(repl_list[[i]], list(extension_point = out_f)))
                } else if (as.character(stock_rename(stock$iter_ss[[i]], "stock", stock_var)) %in% inner_vars) {
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
            stop("stock_", to_replace, "() not available in stock whilst calling: ", deparse1(x))
        }
        out_f <- stock_interactvar_prefix(out_f, prefix)
        out_f <- stock_rename(out_f, "stock", stock_var)
        # Make sure formulas are defined if they need anything the stock code defines
        defines <- setdiff(all.vars(out_f), all_undefined_vars(out_f))
        # Also try the renamed equivalents of each of the defines
        defines <- c(defines, vapply(
            defines,
            function (x) gsub(paste0('^', stock_var, '__'), paste0(stock$name, '__'), x),
            character(1) ))
        inner_f <- add_dependent_formula(inner_f, defines, function (f) {
            if (is.call(f)) f <- stock_rename(f, stock_var, stock$name)
            add_dependent_formula_filter(f)
        })
        # Run g3_step again to fix up dependents that got added
        inner_f <- g3_step(inner_f, recursing = TRUE, orig_env = orig_env)
        out_f <- f_substitute(out_f, list(extension_point = inner_f))
        out_f <- stock_rename(out_f, stock_var, stock$name)

        # Add environment to formulae's environment, return inner call
        environment_merge(environment(step_f), rlang::f_env(out_f), ignore_overlap = TRUE)
        return(rlang::f_rhs(out_f))
    }

    debug_label_fn <- function (x) {
        comment_str <- paste(vapply(tail(x, -1), function (a) {
            if (is.symbol(a)) {
                # Dereference symbols
                a <- get(as.character(a), envir = environment(step_f))
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
                    a <- get(as.character(a), envir = environment(step_f))
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
            inner_f <- call_to_formula(x[[3]], environment(step_f))
            inner_f <- g3_step(inner_f, recursing = TRUE, orig_env = orig_env)

            if (!("length" %in% names(stock$dim))) {
                # No length dimension, so sum everything
                out_f <- f_substitute(quote( sum(inner_f) ), list(inner_f = inner_f))
                environment_merge(environment(step_f), rlang::f_env(out_f))
                return(rlang::f_rhs(out_f))
            }

            # Find first (stock)__(instance)-named var in inner_f, assume that's source stock.
            source_stock_var <- grep("__", all.vars(inner_f), value = TRUE)
            source_stock_var <- sub('__.*$', '', source_stock_var[[1]])
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

            # NB: The force_vector class stops all.equal() from ignoring int/num
            if (isTRUE(all.equal(hide_force_vector(source_lg), hide_force_vector(dest_lg)))) {
                # Source == dest, so no point doing a transform
                out_f <- inner_f
            } else {
                matrix_name <- paste0(source_stock$name, '_', stock$name, '_lgmatrix')

                # Formulae to apply matrix
                out_f <- f_substitute(quote( as.vector(lg_matrix %*% inner_f) ), list(
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
            environment_merge(environment(step_f), rlang::f_env(out_f))
            return(rlang::f_rhs(out_f))
        },
        # stock_ss subsets stock data var, overriding any set expressions
        stock_ss = function (x) { # Arguments: stock data variable (i.e. stock__num), [dim_name = override expr, ...]
            stock_inst <- x[[2]]  # NB: A "stock__num", not "stock"
            stock_var <- gsub('__.*$', '', stock_inst)
            stock <- get(stock_var, envir = orig_env)
            ss_overrides <- as.list(tail(x, -2))

            # Create initial subset of dim -> default
            ss <- sapply(names(stock$iter_ss), function(x) quote(default))

            # Decide what sort of vector we're generating
            if ("vec" %in% names(ss_overrides)) {
                vec <- as.character(ss_overrides[['vec']])
                ss_overrides <- ss_overrides[names(ss_overrides) != 'vec']
            } else if ("length" %in% names(ss)) {
                vec <- "length"
            } else {
                vec <- "single"
            }

            if (vec %in% names(ss)) {
                # Set all dimensions up until vec to missing
                ss[seq_len(min(which( names(ss) == vec )))] <- list( quote(missing) )
            } else if (vec == "full") {
                # Clear all subset parts
                ss[] <- list( quote(missing) )
            } else if (vec == "single") {
                # Leave all as default
            } else {
                stop("Unknown vec argument: ", deparse1(vec))
            }

            # Remove overrides that don't exist in stock_var
            ss_overrides[setdiff(names(ss_overrides), names(stock$dim))] <- NULL

            # Apply overrides
            ss[names(ss_overrides)] <- ss_overrides

            # Substitute "default" with real value. missing with magic missing constant
            ss <- sapply(names(ss),
                # NB: Use do.call() so we substitute the contents
                function(n) do.call(substitute, list(ss[[n]], list(
                    missing = quote(.[])[[3]],
                    default = stock$iter_ss[[n]] ))))

            # No dimensions mean a 1-entry array (see g3_stock_instance)
            if (length(ss) == 0) ss <- list(quote(g3_idx(1)))

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
        stock_prepend = function (x) { # Arguments: stock variable, param, name_part = NULL
            stock_var <- x[[2]]
            stock <- if (is.symbol(stock_var)) get(as.character(stock_var), envir = orig_env) else NULL

            if (g3_is_stock(stock)) {
                if (!is.null(x$name_part)) {
                    name_extra <- paste(stock$name_part[eval(x$name_part, envir = baseenv())], collapse = '_')
                } else {
                    name_extra <- stock$name
                }
            } else if (!is.null(stock)) {
                # stock is a constant to add
                name_extra <- as.character(stock)
            } else {
                # stock_var isn't a symbol, so must be a constant to add
                name_extra <- as.character(stock_var)
            }

            # Inner code is first item in arguments that doesn't have a name
            if (length(names(x)) > 0) {
                inner <- tail(x, -2)
                inner <- inner[!nzchar(names(inner))][[1]]
            } else {
                inner <- x[[3]]
            }

            # Apply stock rename to the rest of the call, to translate any stock__minage references.
            # NB: Recurse first to resolve any nested stock_prepend()
            if (g3_is_stock(stock)) {
                inner <- call("stock_with", stock_var, inner)  # stock_with(stock, ...) is implicit
            }
            inner <- rlang::f_rhs(g3_step(
                call_to_formula(inner, env = as.environment(list())),
                recursing = TRUE, orig_env = orig_env))

            # Add name_extra to the first parameter of inner (i.e. the parameter name)
            if (!is.character(inner[[2]])) {
                stop("First parameter isn't a string, not prepending to : ", deparse1(x))
            }
            inner[[2]] <- paste(c(name_extra, inner[[2]]), collapse = ".")

            return(inner)
        },
        stock_hasdim = function (x) {  # Arguments: stock variable, dimension name
            stock_var <- x[[2]]
            stock <- if (is.symbol(stock_var)) get(as.character(stock_var), envir = orig_env) else NULL
            dim_name <- x[[3]]

            return(dim_name %in% names(stock$dim))
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
            # Resolve stock_isdefined() calls to TRUE / FALSE
            rv <- resolve_stock_isdefined(rv)

            # Add anything that's not a global_formula to this level
            rv <- add_dependent_formula(rv, TRUE, add_dependent_formula_filter)

            # Run g3_step again to fix up dependents that got added
            rv <- g3_step(rv, recursing = TRUE, orig_env = orig_env)
            # Neaten output code by collapsing the stack of g3_with()s we made
            rv <- collapse_g3_with(rv)
            rv <- f_optimize(rv)
        }

        return(rv)
}

# Turn number/stock/string params into a single string indentifying that step
step_id <- function (...) {
    parts <- vapply(list(...), function (part) {
        if (is.null(part)) {
            return("")
        }
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
        if (!is.call(l)) {
            # Input isn't code, so no point wrapping with stock_with()
            return(f_substitute(quote(x), list(x = l)))
        }
        # Choosing one item is just stock_with()
        return(f_substitute(quote(
            stock_with(stock_var_sym, l)
        ), list(stock_var_sym = as.symbol(stock_var), l = l)))
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

# Resolve stock_list (e.g. suitability) ahead of time, unlike list_to_stock_switch
resolve_stock_list <- function (l, stock) {
    # Only one option, return it
    if (!is.list(l)) return(l)

    # If the one we want is present, return that
    if (stock$name %in% names(l)) return(l[[stock$name]])

    # Find a default and return it
    defaults <- l[!nzchar(names(l))]
    if (length(defaults) == 0) stop("Missing option for ", stock$name)
    if (length(defaults) > 1) stop("Only one default option allowed")
    return(defaults[[1]])
}
