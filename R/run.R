# run: Common utilities from turning a set of steps into code

# Define a scalar parameter, e.g. g3_param("woo")
g3_param <- function(...) {
    # NB: g3_param calls shouldn't be directly evaluated, they're markers for
    #     the g3_compile_* functions
    match.call()
}

# Collate: Combine list of (steps) into a single while loop formula, with a combined environment
g3_collate <- function(steps) {
    f_combine <- function (list_of_f) {
        e <- g3_global_env
        # Stack environments together
        for (f in list_of_f) {
            # NB: Actions producing multiple steps will share environments. We
            # have to clone environments so they have separate parents.
            e <- rlang::env_clone(rlang::f_env(f), parent = e)
        }
        # Combine all functions into one expression
        out_call <- as.call(c(list(as.symbol("{")), lapply(unname(list_of_f), rlang::f_rhs)))
        formula(call("~", call("while", TRUE, out_call)), env = e)
    }

    steps <- steps[order(names(steps))]  # Steps should be in alphanumeric order
    return(f_combine(steps))
}
