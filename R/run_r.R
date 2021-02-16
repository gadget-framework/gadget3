open_curly_bracket <- intToUtf8(123) # Don't mention the bracket, so code editors don't get confused

# Compile actions together into a single R function, the attached environment contains:
# - model_data: Fixed values refered to within function
g3_to_r <- function(actions, trace = FALSE, strict = FALSE) {
    all_actions <- f_concatenate(g3_collate(actions), parent = g3_global_env, wrap_call = call("while", TRUE))
    model_data <- new.env(parent = emptyenv())
    scope <- list()
    param_lines <- list()

    # Enable / disable strict mode & trace mode
    all_actions <- call_replace(all_actions,
        strict_mode = function (x) { !isFALSE(strict) },
        trace_mode = function (x) { !isFALSE(trace) },
        debug_label = function (x) {
            if (trace) call("Rprintf", paste0(x[[2]], "\n")) else call("comment", x[[2]])
        },
        debug_trace = function (x) {
            if (identical(trace, 'full')) call("Rprintf", paste0(x[[2]], "\n")) else call("comment", x[[2]])
        })

    var_defns <- function (code, env) {
        # Find all things that have definitions in our environment
        all_defns <- mget(all.names(code, unique = TRUE), envir = env, inherits = TRUE, ifnotfound = list(NA))
        all_defns <- all_defns[!is.na(all_defns)]

        # Find any g3_native functions used, and add them
        for (var_name in names(all_defns)) {
            if ('g3_native' %in% class(all_defns[[var_name]])
                    && !(var_name %in% names(scope))) {
                if (is.function(all_defns[[var_name]])) {
                    scope[[var_name]] <<- call("<-", as.symbol(var_name), all_defns[[var_name]])
                } else if (is.character(all_defns[[var_name]]) && all_defns[[var_name]] != var_name) {
                    # Native function with a different name
                    scope[[var_name]] <<- call("<-", as.symbol(var_name), as.symbol(all_defns[[var_name]]))
                }
            }
        }

        # Find any g3_param stash it in param_lines
        call_replace(code,
            g3_param_table = function (x) {
                # Find data.frame value in environment
                # NB: We eval, so they can be defined in-formulae
                df <- eval(x[[3]], envir = env)

                # Add individual param lines for table contents
                for (i in seq_len(nrow(df))) {
                    param_name <- paste0(c(as.character(x[[2]]), df[i,]), collapse = ".")
                    param_lines[[param_name]] <<- NA
                }
            },
            g3_param_array = function (x) param_lines[[x[[2]]]] <<- 0,
            g3_param_matrix = function (x) param_lines[[x[[2]]]] <<- 0,
            g3_param_vector = function (x) param_lines[[x[[2]]]] <<- 0,
            g3_param = function (x) param_lines[[x[[2]]]] <<- 0)

        # Find with variables / iterators to ignore
        ignore_vars <- c(
            lapply(f_find(code, as.symbol("for")), function (x) { x[[2]] }),
            lapply(f_find(code, as.symbol("g3_with")), function (x) { x[[2]] }),
            list())

        # TODO: Should this loop be combined with the above?
        for (var_name in all.vars(code)) {
            if (var_name %in% names(scope)) {
                # Already init'ed this, ignore it.
                next
            }
            if (var_name %in% ignore_vars) {
                # It's an iterator
                next
            }
            if (var_name %in% ignore_vars) {
                # It's a with variable
                next
            }
            tryCatch({
                var_val <- get(var_name, envir = env, inherits = TRUE)
            }, error = function (e) {
                lines <- trimws(grep(var_name, deparse(code, width.cutoff = 500), fixed = TRUE, value = TRUE))
                stop(paste(trimws(e), "Used in expression(s):", lines, sep = "\n", collapse = "\n"))
            })

            if (rlang::is_formula(var_val)) {
                # Recurse, get definitions for formula, considering it's environment as well as the outer one
                var_defns(rlang::f_rhs(var_val), rlang::env_clone(rlang::f_env(var_val), parent = env))
                defn <- call("<-", as.symbol(var_name), rlang::f_rhs(var_val))
            } else if (is.call(var_val)) {
                defn <- call("<-", as.symbol(var_name), var_val)
            } else if (is(var_val, 'sparseMatrix') && Matrix::nnzero(var_val) == 0) {
                # Define empty sparseMatrix
                defn <- call(
                    "<-",
                    as.symbol(var_name),
                    substitute(Matrix::sparseMatrix(dims = x, x=numeric(0), i={}, j={}), list(x = dim(var_val))))
            } else if (is.array(var_val) && all(is.na(var_val))) {
                # Define dimensions for empty matrix
                defn <- call("<-", as.symbol(var_name), substitute(array(dim = x, dimnames = y), list(
                    x = dim(var_val),
                    y = dimnames(var_val))))
            } else if (is.array(var_val) && all(var_val == 0)) {
                # Define dimensions for zero array

                # Make sure everything within the dynamic dim is defined first
                var_defns(as.call(c(as.symbol(open_curly_bracket), attr(var_val, 'dynamic_dim'))), env)
                var_defns(as.call(c(as.symbol(open_curly_bracket), attr(var_val, 'dynamic_dimnames'))), env)

                defn <- call("<-", as.symbol(var_name), substitute(array(v, dim = x, dimnames = y), list(
                    v = var_val[[1]],  # NB: Might be 0 or FALSE, either way all values are the same
                    x = if (!is.null(attr(var_val, 'dynamic_dim'))) as.call(c(as.symbol("c"), attr(var_val, 'dynamic_dim'))) else dim(var_val),
                    y = if (!is.null(attr(var_val, 'dynamic_dimnames'))) as.call(c(as.symbol("list"), attr(var_val, 'dynamic_dimnames'))) else dimnames(var_val))))
            } else if ((is.numeric(var_val) || is.character(var_val) || is.logical(var_val)) && length(var_val) == 1) {
                # Add single-value literal to code
                defn <- call("<-", as.symbol(var_name), parse(text = deparse(var_val))[[1]])
            } else {
                # Bung in model_data
                defn <- call("<-", as.symbol(var_name), call("$", as.symbol("model_data"), as.symbol(var_name)))
                assign(var_name, var_val, envir = model_data)
            }
            scope[[var_name]] <<- defn
        }
    }

    g3_functions <- function (in_code) {
        # Replace any in-line g3 calls that may have been in formulae
        repl_fn <- function(sym_name) {
            return(function (x) {
                if (length(x) == 2) {
                    # Can lookup argument directly
                    item_name <- x[[2]]
                } else {
                    # Add a paste call to work the required argument
                    item_name <- as.call(c(list(as.symbol('paste0')), as.list(x[2:length(x)])))
                }
                # Lookup item_name in whatever sym_name is called
                return(call('[[', as.symbol(sym_name), item_name))
            })
        }

        call_replace(in_code,
            g3_idx = function (x) if (is.call(x[[2]])) g3_functions(x[[2]]) else call("(", g3_functions(x[[2]])),  # R indices are 1-based, so just strip off call
            g3_report = function (x) substitute(attr(nll, var_name) <- var, list(
                var_name = as.character(x[[2]]),
                var = as.symbol(x[[2]]))),
            g3_with = function (x) call(
                open_curly_bracket,
                call("<-", x[[2]], g3_functions(x[[3]])),
                g3_functions(x[[4]])),
            g3_param_table = function (x) {
                # NB: We eval, so they can be defined in-formulae
                df <- eval(x[[3]], envir = rlang::f_env(all_actions))

                # Generate a param[["lookup.cur_year.cur_step"]] call
                return(call('[[', as.symbol("param"), as.call(c(
                    list(as.symbol("paste"), as.character(x[[2]])),
                    lapply(names(df), as.symbol),
                    list(sep = ".")))))
            },
            g3_param_array = repl_fn("param"),  # TODO: Remove?
            g3_param_matrix = repl_fn("param"),  # TODO: Remove?
            g3_param_vector = repl_fn("param"),
            g3_param = repl_fn("param"))
    }
    # Populate scope for code block
    var_defns(rlang::f_rhs(all_actions), rlang::f_env(all_actions))

    # Wrap all steps in a function call
    out <- call("function", pairlist(param = alist(y=)$y), as.call(c(
        list(as.symbol(open_curly_bracket)),
        scope,
        lapply(names(param_lines), function (p) substitute(stopifnot(p %in% names(param)), list(p = p))),
        rlang::f_rhs(all_actions),
        quote(stop("Should have return()ed somewhere in the loop")))))

    out <- g3_functions(out)
    out <- eval(out)

    # Attach data to model as closure
    # NB: Needs to be globalenv() to evaluate core R
    environment(out) <- new.env(parent = globalenv())
    assign("model_data", model_data, envir = environment(out))
    class(out) <- c("g3_r", class(out))
    attr(out, 'parameter_template') <- param_lines
    return(out)
}

# Set a much higher width-cutoff for R source
edit.g3_r <- function(name = NULL, file = "", title = NULL, editor = getOption("editor"), ...) {
    if (file == "") {
        file <- tempfile(fileext = ".R")
        on.exit(unlink(file))
    }
    writeLines(deparse(name, width.cutoff = 500L), con = file)
    utils::file.edit(file, title = title, editor = editor)
    out <- eval(parse(file = file))  # NB: Evaluate returned expression, that contains function / structure call
    environment(out) <- environment(name)
    return(out)
}
