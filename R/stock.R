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

g3_stock <- function(var_name, minlength, maxlength, dl) {
    # If these are literals, they should be integers
    stock__minlength <- if(is.numeric(minlength)) as.integer(minlength) else minlength
    stock__maxlength <- if(is.numeric(maxlength)) as.integer(maxlength) else maxlength
    stock__dl <- if(is.numeric(dl)) as.integer(dl) else dl

    # See LengthGroupDivision::LengthGroupDivision
    stock__countlen <- (stock__maxlength - stock__minlength) %/% stock__dl
    # TODO: These can't be formulae, since then we stop substituting stock name
    # Force array so type is stable in TMB
    stock__minlen <- as.array(stock__minlength + stock__dl * (seq_len(stock__countlen) - 1))
    stock__meanlen <- as.array(stock__minlen + (stock__dl / 2))

    stock__num <- array(dim = c(stock__countlen))  # Number of individuals
    stock__wgt <- array(dim = c(stock__countlen))  # Mean weight of individuals

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
