open_curly_bracket <- intToUtf8(123) # Don't mention the bracket, so code editors don't get confused

g3_compile_r <- function(steps) {
    all_steps <- g3_collate(steps)
    model_data <- new.env(parent = emptyenv())

    var_defns <- function (code, env) {
        scope <- list()

        # Find all things that have definitions in our environment
        all_defns <- mget(all.names(code, unique = TRUE), envir = env, inherits = TRUE, ifnotfound = list(NA))
        all_defns <- all_defns[!is.na(all_defns)]

        # Find any native functions used, and add them
        for (var_name in names(all_defns)) {
            if ('g3_native' %in% class(all_defns[[var_name]])) {
                scope[[var_name]] <- call("<-", as.symbol(var_name), all_defns[[var_name]]$r)
            }
        }

        # TODO: Should this loop be combined with the above?
        for (var_name in all.vars(code)) {
            if (var_name %in% scope) {
                # Already init'ed this, ignore it.
                next
            }
            if (var_name %in% lapply(f_find(code, as.symbol("for")), function (x) { x[[2]] }) ) {
                # It's an iterator
                next
            }
            var_val <- get(var_name, env = env, inherits = TRUE)

            if (is.formula(var_val)) {
                scope <- c(scope, var_defns(f_rhs(var_val), env))
                defn <- call("<-", as.symbol(var_name), f_rhs(var_val))
            } else if (is.call(var_val)) {
                defn <- call("<-", as.symbol(var_name), var_val)
            } else if (is.array(var_val) && all(is.na(var_val))) {
                # Just define dimensions
                defn <- call("<-", as.symbol(var_name), substitute(array(dim = x), list(x = dim(var_val))))
            } else if (is.array(var_val)) {
                # Generate code to define matrix
                defn <- call("<-", as.symbol(var_name), parse(text = deparse(var_val))[[1]])
            } else if (is.numeric(var_val) || is.character(var_val) || is.logical(var_val)) {
                # Set literal in data
                defn <- call("<-", as.symbol(var_name), call("$", as.symbol("model_data"), as.symbol(var_name)))
                assign(var_name, var_val, envir = model_data)
            } else {
                stop("Don't know how to define ", var_name, " = ", paste(capture.output(str(var_val)), collapse = "\n    "))
            }
            scope[[var_name]] <- defn
        }
        return(scope)
    }


    # Wrap all steps in a function call
    out <- call("function", pairlist(param = alist(y=)$y), as.call(c(
        list(as.symbol(open_curly_bracket)),
        var_defns(f_rhs(all_steps), rlang::f_env(all_steps)),
        f_rhs(all_steps),
        NULL)))

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
    out <- call_replace(out,
        g3_idx = function (x) if (is.call(x[[2]])) x[[2]] else call("(", x[[2]]),  # R indices are 1-based, so just strip off call
        g3_param = repl_fn("param"))
    out <- eval(out)

    # Attach data to model as closure
    # NB: Needs to be globalenv() to evaluate core R
    environment(out) <- new.env(parent = globalenv())
    assign("model_data", model_data, envir = environment(out))
    return(out)
}
