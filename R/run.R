# run: Common utilities from turning a set of steps into code

# Define a scalar parameter, e.g. g3_param("woo")
g3_param <- function(...) {
    # NB: g3_param calls shouldn't be directly evaluated, they're markers for
    #     the g3_compile_* functions
    match.call()
}

# Combine all provided action lists into one action list, throwing away duplicates
g3_collate <- function(...) {
    # Combine all lists into an environment, later lists win over previous
    # TODO: Just concatenate the lists backwards?
    actions <- new.env(parent = emptyenv())
    for (l in list(...)) {
        for (n in names(l)) {
            actions[[n]] <- l[[n]]
        }
    }
    actions <- as.list(actions)

    # Order items in alphanumeric order
    return(actions[order(names(actions))])
}
