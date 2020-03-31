# Add missing is.formula
is.formula <- function (o) {
    is.call(o) && 'formula' %in% class(o)
}

# Turn a call into a formula, with environment env
call_to_formula <- function (c, env = parent.frame()) {
    formula(call("~", c), env = env)
}

# Replace the target of this formulae with what we really want.
f_lhs <- function (name, f) {
    if (length(f) < 3) {
        # Shunt RHS over one so there's space for LHS
        f[[3]] <- f[[2]]
    }
    f[[2]] <- as.symbol(name)
    return(f)
}

f_rhs <- function (f) f[[length(f)]]

environment_merge <- function (env, additions) {
    for (n in ls(envir = additions)) {
        if (!exists(n, envir = env)) {
            assign(n, get(n, envir = additions), envir = env)
        }
    }
    return(NULL)
}

# Substitute within formulae, merging all environments together
f_substitute <- function (f, env) {
    env <- as.environment(env)
    # Copy f's environment to a new environment, ignore it's parent
    combined_env <- new.env(parent = emptyenv())
    environment_merge(combined_env, rlang::f_env(f))

    # For all formula substitutions...
    for (n in all.vars(f)) {
        o <- mget(n, envir = env, ifnotfound = list(NULL))[[1]]
        if (!is.formula(o)) next

        # Replace formulae with the inner expression
        if (length(o) == 3) {
            assign(n, call('<-', o[[2]], o[[3]]), envir = env)
        } else {
            assign(n, o[[2]], envir = env)
        }

        # Combine it's environment with ours
        environment_merge(combined_env, rlang::f_env(o))
    }

    # Make a substitute call out of our unevaluated formulae
    out <- eval(call("substitute", f, env))
    as.formula(out, env = combined_env)
}
# f_a <- (function () { t <- 3 ; y ~ {x + 2 ; parp} })()
# f_b <- (function () { q <- 2 ; z ~ q * 2 })()
# parse_tree(f_substitute(f_a, list(parp = f_b)))

f_find <- function (f, target_symbol) {
    if (is.call(f)) {
        return(c(
             (if (f[[1]] == target_symbol) list(f) else list()),
             do.call(c, lapply(f, function(x) f_find(x, target_symbol)))))
    }
    return(list())
}
# str(f_find(~ (2+(3+1)) * (4+4), as.symbol("+")))

# Descend through call f, when a symbol like key appears, call it's function to modify the call
call_replace <- function (f, ...) {
    modify_call_fns <- list(...)

    if (!is.call(f)) return(f)

    # If there's a modify_fn that matches the symbol of this call, call it.
    modify_fn <- modify_call_fns[[as.character(f[[1]])]]
    if (length(modify_fn) > 0) {
        f <- modify_fn(f)
    }

    # Recurse through all elements of this call
    out <- as.call(lapply(f, function (x) call_replace(x, ...)))

    # Put back all attributes (i.e. keep formula-ness)
    attributes(out) <- attributes(f)
    return(out)
}
# call_replace(~ 2 + g3_param("woo"), g3_param = function (x) call('$', as.symbol("data"), x[[2]]))
