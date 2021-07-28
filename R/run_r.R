open_curly_bracket <- intToUtf8(123) # Don't mention the bracket, so code editors don't get confused

# Compile actions together into a single R function, the attached environment contains:
# - model_data: Fixed values refered to within function
g3_to_r <- function(actions, trace = FALSE, strict = FALSE) {
    all_actions <- f_concatenate(g3_collate(actions), parent = g3_global_env, wrap_call = call("while", TRUE))
    model_data <- new.env(parent = emptyenv())
    scope <- list()

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
        # Convert a g3_param* call into a reference, move it's definition to the environment
        # Replace any in-line g3 calls that may have been in formulae
        repl_fn <- function(x) {
            # NB: eval() because -1 won't be a symbol
            find_arg <- function (arg_name, def) if (arg_name %in% names(x)) eval(x[[arg_name]]) else def
            df_template <- function (name, dims = c(1)) {
                # Extract named args from g3_param() call
                value <- find_arg('value', 0)

                structure(list(value), names = name)
            }
            if (length(x) < 2 || !is.character(x[[2]])) stop("You must supply a name for the g3_param in ", deparse(x))
            if (x[[1]] == 'g3_param_table') {
                ifmissing <- as.numeric(find_arg('ifmissing', NULL))
                # NB: We eval, so they can be defined in-formulae
                df <- eval(x[[3]], envir = env)

                # Add stopifnot for each row in table
                for (i in seq_len(nrow(df))) {
                    param_name <- paste0(c(as.character(x[[2]]), df[i,]), collapse = ".")
                    scope[[paste0("..param:", param_name)]] <<- structure(
                        substitute(stopifnot(p %in% names(param)), list(p = param_name)),
                        param_template = df_template(param_name))
                }

                # Replace with a  param[["lookup.cur_year.cur_step"]] call
                return(call('nvl',
                    call('[[', as.symbol("param"), as.call(c(
                        list(as.symbol("paste"), as.character(x[[2]])),
                        lapply(names(df), as.symbol),
                        list(sep = ".")))),
                    if (length(ifmissing) == 1) ifmissing else call('stop', 'Out of range: ', as.character(x[[2]]))))
            }

            # Default for g3_param / g3_param_vector
            # NB: We haven't actually used ..param:thing, but will end up in top of function anyway
            scope[[paste0("..param:", x[[2]])]] <<- structure(
                substitute(stopifnot(p %in% names(param)), list(p = x[[2]])),
                param_template = df_template(x[[2]]))
            return(call('[[', as.symbol("param"), x[[2]]))
        }
        code <- call_replace(code,
            g3_param_table = repl_fn,
            g3_param_array = repl_fn,
            g3_param_vector = repl_fn,
            g3_param = repl_fn)

        # Find all things that have definitions in our environment
        all_defns <- mget(all.names(code, unique = TRUE), envir = env, inherits = TRUE, ifnotfound = list(NA))
        all_defns <- all_defns[!is.na(all_defns)]

        # Find any g3_native functions used, and add them
        for (var_name in names(all_defns)) {
            if ('g3_native' %in% class(all_defns[[var_name]])
                    && !(var_name %in% names(scope))) {
                var_defns(attr(all_defns[[var_name]], 'g3_native_depends'), env)
                if (is.function(all_defns[[var_name]])) {
                    fn_defn <- all_defns[[var_name]]
                    environment(fn_defn) <- env  # TODO: This should be the output function scope, not env.
                    scope[[var_name]] <<- call("<-", as.symbol(var_name), fn_defn)
                } else if (is.character(all_defns[[var_name]]) && all_defns[[var_name]] != var_name) {
                    # Native function with a different name
                    scope[[var_name]] <<- call("<-", as.symbol(var_name), as.symbol(all_defns[[var_name]]))
                }
            }
        }

        # Find with variables / iterators to ignore
        ignore_vars <- c(
            lapply(f_find(code, as.symbol("for")), function (x) { x[[2]] }),
            do.call(c, lapply(f_find(code, as.symbol("g3_with")), g3_with_extract_term_syms)),
            list('param'))

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
                var_val_code <- var_defns(rlang::f_rhs(var_val), rlang::env_clone(rlang::f_env(var_val), parent = env))
                defn <- call("<-", as.symbol(var_name), var_val_code)
            } else if (is.call(var_val)) {
                defn <- call("<-", as.symbol(var_name), var_val)
            } else if (is(var_val, 'sparseMatrix') && Matrix::nnzero(var_val) == 0) {
                # Define empty sparseMatrix
                defn <- call(
                    "<-",
                    as.symbol(var_name),
                    substitute(Matrix::sparseMatrix(dims = x, x=numeric(0), i={}, j={}), list(x = dim(var_val))))
            } else if (is.array(var_val) && all(is.na(var_val))) {
                # Make sure everything within the dynamic dim is defined first
                var_defns(as.call(c(as.symbol(open_curly_bracket), attr(var_val, 'dynamic_dim'))), env)
                var_defns(as.call(c(as.symbol(open_curly_bracket), attr(var_val, 'dynamic_dimnames'))), env)

                # Define dimensions for empty matrix
                defn <- call("<-", as.symbol(var_name), substitute(array(dim = x, dimnames = y), list(
                    x = if (!is.null(attr(var_val, 'dynamic_dim'))) as.call(c(as.symbol("c"), attr(var_val, 'dynamic_dim'))) else dim(var_val),
                    y = if (!is.null(attr(var_val, 'dynamic_dimnames'))) as.call(c(as.symbol("list"), attr(var_val, 'dynamic_dimnames'))) else dimnames(var_val))))
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
        return(code)
    }  # End of var_defns

    # Define all vars, populating scope as side effect
    all_actions_code <- var_defns(rlang::f_rhs(all_actions), rlang::f_env(all_actions))

    # Wrap all steps in a function call
    out <- call("function", pairlist(param = alist(y=)$y), as.call(c(
        list(as.symbol(open_curly_bracket)),
        scope,
        all_actions_code,
        quote(stop("Should have return()ed somewhere in the loop")))))

    # Rework any g3_* function calls into the code we expect
    g3_functions <- function (in_code) {
        call_replace(in_code,
            g3_idx = function (x) if (is.call(x[[2]])) g3_functions(x[[2]]) else call("(", g3_functions(x[[2]])),  # R indices are 1-based, so just strip off call
            g3_report = function (x) substitute(attr(nll, var_name) <- var, list(
                var_name = as.character(x[[2]]),
                var = as.symbol(x[[2]]))),
            g3_with = function (x) as.call(c(
                list(as.symbol(open_curly_bracket)),
                lapply(g3_with_extract_terms(x), function (c) { c[[3]] <- g3_functions(c[[3]]) ; c }),
                list(g3_functions(x[[length(x)]])))))
    }
    out <- g3_functions(out)

    # Turn call structure into an actual function
    out <- eval(out)

    # Attach data to model as closure
    # NB: Needs to be globalenv() to evaluate core R
    environment(out) <- new.env(parent = globalenv())
    assign("model_data", model_data, envir = environment(out))
    class(out) <- c("g3_r", class(out))
    attr(out, 'parameter_template') <- scope_to_parameter_template(scope, 'list')
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
