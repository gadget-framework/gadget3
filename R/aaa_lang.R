# Define a function with separate equivalent R and C++ implementations
# - r: R function object, or name of base function
# - cpp: C++ lambda function, as a string vector or list('Type', 'Type', NULL) to add casts to native fn or NULL if natively supported
# - depends: List of other things in the environment this depends on, e.g. extra functions
g3_native <- function(r, cpp, depends = c()) {
    # NB: We get away with using "as.numeric" for native functions with native
    #     evaluation since R will be looking for a function, not value.
    #     Ideally we'd be using a reference to base::as.numeric, but that's causing
    #     unfathomable problems in to_tmb land.
    out <- r
    attr(out, "g3_native_cpp") <- cpp
    # Turn depends vector into something that calls each item, to work with var_defns
    attr(out, "g3_native_depends") <- as.call(c(as.symbol("{"), lapply(depends, as.symbol)))  # }
    class(out) <- c("g3_native", class(out))
    return(out)
}

# A global formula is one that has an interative formula and an initial value
# (f) will set the value within the current step, (init_val) will initialize the variable
# outside the loop
g3_global_formula <- function(f = quote(noop), init_val = NULL) {
    if (!rlang::is_formula(f)) f <- call_to_formula(f, new.env(parent = emptyenv()))
    attr(f, "g3_global_init_val") <- init_val
    return(f)
}

