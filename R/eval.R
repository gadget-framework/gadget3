# Evaluate (f) in a g3-ish fashion, inserting (...) into the environment
g3_eval <- function (f, ...) {
    # Create environment from remaining args
    env <- list(...)
    if (length(env) == 1 && is.null(names(env))) {
        # Single unnamed argument, assume it's an environment (or convertable to one)
        env <- as.environment(env[[1]])
    } else {
        env <- as.environment(env)
    }

    if (rlang::is_formula(f)) {
        f_code <- rlang::f_rhs(f)
        # Environment: (...) -> copy of formula env -> g3_env
        parent.env(env) <- rlang::env_clone(environment(f))
        parent.env(parent.env(env)) <- g3_env
    } else if (is.call(f)) {
        f_code <- f
        # Environment: (...) -> g3_env
        parent.env(env) <- g3_env
    }

    # Wrap code in stock_with() for all stocks in environment, so stock substitution works
    for (var_name in ls(env)) {
        if (g3_is_stock(env[[var_name]])) {
            f_code <- substitute(stock_with(var, f_code), list(
                var = as.symbol(var_name),
                f_code = f_code))
        }
    }

    # Apply g3_step to evaluate stock_*()
    f <- g3_step(call_to_formula(f_code, env))

    # Fake g3_param lifts param.name from main formula environment (i.e. arguments to g3_eval)
    environment(f)$g3_param <- function (name, value = 0, ...) {
        out <- mget(
            paste0('param.', name),
            envir = parent.frame(),
            ifnotfound = list(value))[[1]]
        return(out)
    }

    # Evaluate RHS with it's environment
    tryCatch(eval(rlang::f_rhs(f), environment(f)), error = function (e) {
        stop(paste(c(
            e,
            "Whilst evaluating expression:",
            utils::capture.output(print( rlang::f_rhs(f) )),
            "With environment:",
            utils::capture.output(print( as.list(environment(f)) )),
            NULL), collapse = "\n"))
    })
}
