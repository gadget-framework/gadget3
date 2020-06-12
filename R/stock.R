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

# A vector of areas, subsetting preserves the full list
g3_areas <- function (...) {
    structure(
        seq_along(c(...)),
        names = c(...),
        all_areas = c(...),
        class = 'g3_areas')
}

`[.g3_areas` <- function (x, ..., drop = FALSE) {
    y <- NextMethod("[")
    attr(y, "all_areas") <- attr(x, "all_areas")
    class(y) <- oldClass(x)
    return(y)
}

`[[.g3_areas` <- function (x, ..., drop = FALSE) {
    y <- NextMethod("[[")
    attr(y, "all_areas") <- attr(x, "all_areas")
    class(y) <- oldClass(x)
    return(y)
}

# TODO: Using this directly on top of others won't produce valid code. Should they be collapsed together?
g3_stock <- function(var_name, minlength, maxlength, dl) {
    # If these are literals, they should be integers
    stock__minlength <- if(is.numeric(minlength)) as.integer(minlength) else minlength
    stock__maxlength <- if(is.numeric(maxlength)) as.integer(maxlength) else maxlength
    stock__dl <- if(is.numeric(dl)) as.integer(dl) else dl

    # See LengthGroupDivision::LengthGroupDivision
    stock__countlen <- (stock__maxlength - stock__minlength) %/% stock__dl
    # TODO: These can't be formulae, since then we stop substituting stock name
    stock__minlen <- stock__minlength + stock__dl * (seq_len(stock__countlen) - 1)
    stock__meanlen <- stock__minlen + (stock__dl / 2)

    stock__num <- array(dim = c(stock__countlen))
    stock__wgt <- array(dim = c(stock__countlen))

    list(
        iterate = ~extension_point,
        iter_ss = quote(.[]),  # NB: This includes a missing parameter for the length dimension
        translate = ~extension_point,
        name = var_name)
}

g3_fleet <- function(var_name) {
    list(
        iterate = ~extension_point,
        iter_ss = quote(`[`(.)),  # NB: No dimensions yet
        translate = ~extension_point,
        name = var_name)
}

g3s_livesonareas <- function(inner_stock, areas) {
    stopifnot('g3_areas' %in% class(areas))
    stock_env <- rlang::f_env(inner_stock$iterate)

    stock__areas <- as.array(as.integer(areas))  # NB: Force stock__areas to be an array
    area <- areas[[1]]
    if (exists("stock__num", envir = stock_env)) {
        stock__num <- array(dim = c(dim(stock_env[['stock__num']]), length(stock__areas)))
    }
    if (exists("stock__wgt", envir = stock_env)) {
        stock__wgt <- array(dim = c(dim(stock_env[['stock__wgt']]), length(stock__areas)))
    }
    stock__area_idx <- 0L

    list(
        iterate = f_substitute(~for (stock__area_idx in seq_along(stock__areas)) {
            area <- area_lookup
            extension_point
        }, list(
            area_lookup = if (length(stock__areas) > 1) quote(stock__areas[[stock__area_idx]]) else stock__areas,
            extension_point = inner_stock$iterate)),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__area_idx"))),
        translate = f_substitute(~{
            for (possible_area in seq_along(stock__areas)) {
                if (stock__areas[[possible_area]] == area) {
                    stock__area_idx <- possible_area
                    break
                }
            }
            extension_point
            # TODO: What if we fall off the end?
        }, list(
            extension_point = inner_stock$translate)),
        name = inner_stock$name)
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
        translate = f_substitute(~if (age <= stock__maxage) {
            stock__age_idx <- g3_idx(age - stock__minage + 1)
            extension_point
        }, list(
            extension_point = inner_stock$translate)),
        name = inner_stock$name)
}

g3s_prey <- function(inner_stock, energycontent) {
    inner_stock  # TODO:
}
