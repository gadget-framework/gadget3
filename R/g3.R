# This is a function with separate equivalent R and C++ implementations
g3_native <- function(r, cpp) {
    return(structure(list(r = r, cpp = cpp), class = "g3_native"))
}

# g3_data / param calls shouldn't be directly evaluated, they're markers for the
# g3_compile_* functions
g3_data <- function(...) match.call()
g3_param <- function(...) match.call()

g3_collate <- function(steps) {
    f_combine <- function (list_of_f) {
        e <- g3_global_env
        # Stack environments together
        for (f in list_of_f) {
            # NB: Actions producing multiple steps will share environments. We
            # have to clone environments so they have separate parents.
            e <- rlang::env_clone(f_envir(f), parent = e)
        }
        # Combine all functions into one expression
        out_call <- as.call(c(list(as.symbol("{")), lapply(unname(list_of_f), f_rhs)))
        formula(call("~", call("while", TRUE, out_call)), env = e)
    }

    steps <- steps[order(names(steps))]  # Steps should be in alphanumeric order
    return(f_combine(steps))
}

g3_run <- function (g3m, data, param) {
    g3m(data, param)
}
