# Pull the definition of the stock variable out of the stock object
stock_definition <- function(stock, var_name) {
    get(var_name, envir = rlang::f_env(stock$iterate))
}

# Define an array that matches stock
stock_instance <- function (stock, init_value = NA) {
    # TODO: Don't make it yet, defer until g3 needs it
    return(array(init_value, dim = stock$dim, dimnames = stock$dimnames))
}

g3_stock <- function(var_name, lengthgroups, open_ended = TRUE) {
    # See LengthGroupDivision::LengthGroupDivision
    stopifnot(length(lengthgroups) > 0)

    # If no names given, make some up
    if (is.null(names(lengthgroups))) {
        names(lengthgroups) <- paste0('len', lengthgroups)
    }

    # stock__dl is size of each lengthgroup
    # stock__upperlen is the final upper bound of the groups
    if (isTRUE(open_ended)) {
        # plus-group infinite, guess next value in sequence for semi-sane __dl
        stock__dl <- unname(diff(c(lengthgroups,
            tail(lengthgroups, 1) + (if (length(lengthgroups) > 1) mean(diff(lengthgroups)) else 1))))
        stock__upperlen <- Inf
    } else {
        # Split off final value for absolute max
        stock__dl <- unname(diff(lengthgroups))
        stock__upperlen <- unname(tail(lengthgroups, 1))
        lengthgroups <- head(lengthgroups, -1)
    }

    # Force array so type is stable in TMB
    stock__minlen <- as.array(lengthgroups)
    stock__midlen <- as.array(lengthgroups + (stock__dl / 2))

    structure(list(
        dim = c(
            length = length(lengthgroups)),
        dimnames = list(
            length = names(lengthgroups)),
        iterate = ~extension_point,
        iter_ss = quote(.[]),  # NB: This includes a missing parameter for the length dimension
        intersect = ~extension_point,
        rename = ~extension_point,
        name = var_name), class = c("g3_stock", "list"))
}

g3_fleet <- function(var_name) {
    structure(list(
        dim = c(),
        dimnames = list(),
        iterate = ~extension_point,
        iter_ss = quote(`[`(.)),  # NB: No dimensions yet
        intersect = ~extension_point,
        rename = ~extension_point,
        name = var_name), class = c("g3_stock", "list"))
}

g3s_prey <- function(inner_stock, energycontent) {
    inner_stock  # TODO:
}

g3s_clone <- function(inner_stock, var_name) {
    new_stock <- inner_stock
    new_stock$name <- var_name
    return(new_stock)
}
