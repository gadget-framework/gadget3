# Pull the definition of the stock variable out of the stock object
stock_definition <- function(stock, var_name) {
    get(var_name, envir = rlang::f_env(stock$iterate))
}

# Copy a stock definition, change the name.
stock_clone <- function(stock, name) {
    new_stock <- stock
    new_stock$name <- name
    # NB: new_stock will share stock's environment, but (probably?) okay,
    # since these will get used with f_substitute which will create a new one
    # anyway.
    return(new_stock)
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

    stock__num <- array(dim = length(lengthgroups))  # Number of individuals
    stock__wgt <- array(dim = length(lengthgroups))  # Mean weight of individuals

    list(
        iterate = ~extension_point,
        iter_ss = quote(.[]),  # NB: This includes a missing parameter for the length dimension
        intersect = ~extension_point,
        rename = ~extension_point,
        name = var_name)
}

g3_fleet <- function(var_name) {
    stock__catch <- 0.0

    list(
        iterate = ~extension_point,
        iter_ss = quote(`[`(.)),  # NB: No dimensions yet
        intersect = ~extension_point,
        rename = ~extension_point,
        name = var_name)
}

g3s_prey <- function(inner_stock, energycontent) {
    inner_stock  # TODO:
}
