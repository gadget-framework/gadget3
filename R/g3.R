source('utils.R')
source('g3_stock.R')
source('g3_action.R')

# This is a function with separate equivalent R and C++ implementations
g3_native <- function(r, cpp) {
    return(structure(list(r = r, cpp = cpp), class = "g3_native"))
}

# This is something that should be pulled out of data
g3_data <- function(data_name) {
    # TODO: Doesn't acknowledge auto-paste0-ing, needs combining with
    # 2 messes below
    return(structure(data_name, class = "g3_data"))
}

# This is something that should be pulled out of param
g3_param <- function(param_name) {
    return(structure(param_name, class = "g3_param"))
}

g3_compile <- function(steps) {
    f_combine <- function (list_of_f) {
        e <- emptyenv()
        # Stack environments together
        for (f in list_of_f) {
            # NB: Actions producing multiple steps will share environments. We
            # have to clone environments so they have separate parents.
            e <- rlang::env_clone(f_envir(f), parent = e)
        }
        # Combine all functions into one expression
        out_call <- as.call(c(list(as.symbol("{")), lapply(unname(list_of_f), f_rhs)))
        formula(call("~", out_call), env = e)
    }

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
            } else if ('g3_data' %in% class(var_val)) {
                defn <- call("<-", as.symbol(var_name), call("g3_data", var_val))
            } else if ('g3_param' %in% class(var_val)) {
                defn <- call("<-", as.symbol(var_name), call("g3_param", var_val))
            } else if (is.array(var_val) && all(is.na(var_val))) {
                # Just define dimensions
                defn <- call("<-", as.symbol(var_name), substitute(array(dim = x), list(x = dim(var_val))))
            } else if (is.array(var_val)) {
                # Generate code to define matrix
                defn <- call("<-", as.symbol(var_name), parse(text = deparse(var_val))[[1]])
            } else if (is.numeric(var_val) || is.character(var_val) || is.logical(var_val)) {
                # Defined as a literal
                defn <- call("<-", as.symbol(var_name), var_val)
            } else {
                print(code)  # TODO: Better debug
                stop("Don't know how to define ", var_name)
            }
            scope[[var_name]] <- defn
        }
        return(scope)
    }

    steps <- steps[order(names(steps))]  # Steps should be in alphanumeric order
    all_steps <- f_combine(steps)

    # Wrap all steps in a function call
    out <- call("function", pairlist(data = alist(y=)$y, param = alist(y=)$y), as.call(c(
        list(as.symbol("{")),
        var_defns(f_rhs(all_steps), f_envir(all_steps)),
        as.call(c(list(as.symbol("while"), TRUE), f_rhs(all_steps))),
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
    out <- call_replace(out, g3_data = repl_fn("data"), g3_param = repl_fn("param"))
    return(eval(out))
}

g3_run <- function (g3m, data, param) {
    g3m(data, param)
}
