# Pull the definition of the stock variable out of the stock object
stock_definition <- function(stock, var_name) {
    get(var_name, envir = rlang::f_env(stock$iterate))
}

# Define an array that matches stock
stock_instance <- function (stock) {
    # TODO: Don't make it yet, defer until g3 needs it
    return(array(dim = stock$dim, dimnames = stock$dimnames))
}

g3_stock <- function(var_name, lengthgroups) {
    # See LengthGroupDivision::LengthGroupDivision
    stopifnot(length(lengthgroups) > 0)

    # stock__dl is size of each lengthgroup
    plus_group_max <- tail(lengthgroups, 1) + (if (length(lengthgroups) > 1) mean(diff(lengthgroups)) else 1)
    stock__dl <- diff(c(lengthgroups, plus_group_max))

    # Force array so type is stable in TMB
    stock__minlen <- as.array(lengthgroups)
    stock__midlen <- as.array(lengthgroups + (stock__dl / 2))

    list(
        dim = c(
            length = length(lengthgroups)),
        dimnames = list(
            length = paste0('len', lengthgroups)),
        iterate = ~extension_point,
        iter_ss = quote(.[]),  # NB: This includes a missing parameter for the length dimension
        intersect = ~extension_point,
        rename = ~extension_point,
        name = var_name)
}

g3_fleet <- function(var_name) {
    list(
        dim = c(),
        dimnames = list(),
        iterate = ~extension_point,
        iter_ss = quote(`[`(.)),  # NB: No dimensions yet
        intersect = ~extension_point,
        rename = ~extension_point,
        name = var_name)
}

g3s_prey <- function(inner_stock, energycontent) {
    inner_stock  # TODO:
}
