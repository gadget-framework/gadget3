stock_extend <- function(inner, ...) {
    out <- as.environment(inner)
    additions <- list(...)

    for (n in names(additions)) {
        new_val <- additions[[n]]

        assign(n, new_val, envir = out)
    }

    as.list(out)
}

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
        name = var_name)
}

g3_fleet <- function(var_name) {
    stock__catch <- 0.0

    list(
        iterate = ~extension_point,
        iter_ss = quote(`[`(.)),  # NB: No dimensions yet
        intersect = ~extension_point,
        name = var_name)
}

g3s_age <- function(inner_stock, minage, maxage) {
    # If these are literals, they should be integers
    stock__minage <- if(is.numeric(minage)) as.integer(minage) else minage
    stock__maxage <- if(is.numeric(minage)) as.integer(maxage) else minage
    stock__num <- array(dim = c(dim(stock_definition(inner_stock, 'stock__num')), stock__maxage - stock__minage + 1))
    stock__wgt <- array(dim = c(dim(stock_definition(inner_stock, 'stock__wgt')), stock__maxage - stock__minage + 1))
    stock__age_idx <- 0L

    list(
        iterate = f_substitute(~for (age in seq(stock__minage, stock__maxage)) {
            stock__age_idx <- g3_idx(age - stock__minage + 1)
            extension_point
        }, list(
            extension_point = inner_stock$iterate)),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__age_idx"))),
        intersect = f_substitute(~if (age <= stock__maxage) {
            stock__age_idx <- g3_idx(age - stock__minage + 1)
            extension_point
        }, list(
            extension_point = inner_stock$intersect)),
        name = inner_stock$name)
}

g3s_prey <- function(inner_stock, energycontent) {
    inner_stock  # TODO:
}
