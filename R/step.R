# Adapt a formulae into one that will iterate over stocks. Will replace the following functions:
# - debug_label(...) - Replace any stock arguments with the stock name
# - debug_trace(...) - Replace any stock arguments with the stock name
# - stock_iterate(stock, block) - wrap (block) with code to iterate over (stock) lengthgroup vectors
# - stock_intersect(stock, block) - wrap (block) with code to intersect with an outer stock we're iterating over
# - stock_with(stock, block) - Make sure any references to (stock) in (block) uses the right name
# References to the stock will also be renamed to their final name
g3_step <- function(step_f) {
    # For formula (f), rename all (old_name)__var variables to (new_name)__var, mangling environment to match
    stock_rename <- function(f, old_name, new_name) {
        old_name <- as.character(old_name)
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

        return(f)
    }

    repl_stock_fn <- function (x, to_replace) {
        stock_var <- x[[2]]
        stock <- get(as.character(stock_var), envir = rlang::f_env(step_f))

        # Recurse first, filling out any inner functions
        inner_f <- call_to_formula(x[[3]], rlang::f_env(step_f))
        inner_f <- g3_step(inner_f)

        # Wrap with stock's code
        out_f <- f_substitute(stock_rename(stock[[to_replace]], "stock",  stock_var), list(
            extension_point = inner_f))
        out_f <- stock_rename(out_f, stock_var, stock$name)

        # Add environment to formulae's environment, return inner call
        environment_merge(rlang::f_env(step_f), rlang::f_env(out_f))
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

    return(f_optimize(call_replace(step_f,
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

            return(call('assert_msg', g3_step(as.formula(call("~", x[[2]]), env = environment(step_f))), comment_str))
        },
        stock_reshape = function (x) {  # Arguments: dest_stock, source expression, will use the first variable we come across
            stock_var <- x[[2]]
            stock <- get(as.character(stock_var), envir = rlang::f_env(step_f))
            dest_lg <- stock_definition(stock, 'stock__minlen')

            # Recurse first, letting renames happen
            inner_f <- call_to_formula(x[[3]], rlang::f_env(step_f))
            inner_f <- g3_step(inner_f)

            # Assume source stock is the first in inner_f, probably true(?)
            source_stock_var <- sub('__.*$', '', all.vars(inner_f)[[1]])
            source_stock <- get(source_stock_var, envir = rlang::f_env(step_f))
            source_lg <- stock_definition(source_stock, 'stock__minlen')

            # Get upper bound for length groups, if infinite guess something that should work
            # NB: Assuming plus-group is one-long is cheating, but probably no easy general answers
            source_upperlen <- stock_definition(source_stock, 'stock__upperlen')
            if (is.infinite(source_upperlen)) source_upperlen <- tail(source_lg, 1) + 1
            dest_upperlen <- stock_definition(stock, 'stock__upperlen')
            if (is.infinite(dest_upperlen)) dest_upperlen <- max(tail(source_lg, 1), tail(dest_lg, 1)) + 1

            if (isTRUE(all.equal(source_lg, dest_lg))) {
                # Source == dest, so no point doing a transform
                out_f <- inner_f
            } else {
                # Generate a matrix to transform one to the other
                matrix_name <- paste0(source_stock$name, '_', stock$name, '_lgmatrix')
                assign(matrix_name, do.call('rbind', lapply(seq_along(dest_lg), function (dest_idx) vapply(seq_along(source_lg), function (source_idx) {
                    lower_s <- source_lg[[source_idx]]
                    upper_s <- if (source_idx >= length(source_lg)) source_upperlen else source_lg[[source_idx + 1]]

                    lower_d <- dest_lg[[dest_idx]]
                    upper_d <- if (dest_idx >= length(dest_lg)) dest_upperlen else dest_lg[[dest_idx + 1]]

                    intersect_size <- min(upper_s, upper_d) - max(lower_s, lower_d)
                    return(if (intersect_size > 0) intersect_size / (upper_s - lower_s) else 0)
                }, numeric(1)))))

                # Formulae to apply matrix
                out_f <- f_substitute(~g3_matrix_vec(lg_matrix, inner_f), list(
                    lg_matrix = as.symbol(matrix_name),
                    inner_f = inner_f))
            }

            # Add environment to formulae's environment, return inner call
            environment_merge(rlang::f_env(step_f), rlang::f_env(out_f))
            return(rlang::f_rhs(out_f))
        },
        # stock_ss subsets stock data var, Removing the specified dimensions (i.e. blanking it's part in the normal subset)
        stock_ss = function (x) { # Arguments: stock data variable (i.e. stock__num), dimension names.
            stock_instance <- x[[2]]  # NB: A "stock__num", not "stock"
            stock_var <- gsub('__.*$', '', stock_instance)
            stock <- get(stock_var, envir = rlang::f_env(step_f))
            wanted_dims <- as.character(tail(x, -2))

            # Get subset arguments
            ss <- stock$iter_ss

            # No dimensions mean a 1-entry array (see stock_instance)
            if (length(ss) == 0) ss <- list(quote(g3_idx(1)))

            # Replace unwanted dimensions with missing symbol
            ss[names(stock$dimnames) %in% wanted_dims] <- list(quote(x[])[[3]])
            return(stock_rename(as.call(c(list(as.symbol("["), stock_instance), ss)), "stock", stock_var))
        },
        # stock_ssinv subsets stock data var, keeping the specified dimensions (i.e. blanking it's part in the normal subset)
        stock_ssinv = function (x) { # Arguments: stock data variable (i.e. stock__num), dimension names.
            stock_instance <- x[[2]]  # NB: A "stock__num", not "stock"
            stock_var <- gsub('__.*$', '', stock_instance)
            stock <- get(stock_var, envir = rlang::f_env(step_f))
            wanted_dims <- as.character(tail(x, -2))

            # Get subset arguments
            ss <- stock$iter_ss

            # No dimensions mean a 1-entry array (see stock_instance)
            if (length(ss) == 0) ss <- list(quote(g3_idx(1)))

            # Replace unwanted dimensions with missing symbol
            ss[!(names(stock$dimnames) %in% wanted_dims)] <- list(quote(x[])[[3]])
            return(stock_rename(as.call(c(list(as.symbol("["), stock_instance), ss)), "stock", stock_var))
        },
        stock_with = function (x) {  # Arguments: stock variable, inner code block
            return(repl_stock_fn(x, 'rename'))
        },
        stock_iterate = function (x) {  # Arguments: stock variable, inner code block
            return(repl_stock_fn(x, 'iterate'))
        },
        stock_intersect = function (x) {  # Arguments: stock variable, inner code block
            return(repl_stock_fn(x, 'intersect'))
        })))
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
