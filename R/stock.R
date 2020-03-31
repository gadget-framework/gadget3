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
    get(var_name, envir = f_envir(stock$iterate))
}

# TODO: Using this directly on top of others won't produce valid code. Should they be collapsed together?
g3_stock <- function(stock_name, minlength, maxlength, dl) {
    # If these are literals, they should be integers
    stock_minlength <- if(is.numeric(minlength)) as.integer(minlength) else minlength
    stock_maxlength <- if(is.numeric(maxlength)) as.integer(maxlength) else maxlength
    stock_dl <- if(is.numeric(dl)) as.integer(dl) else dl

    # See LengthGroupDivision::LengthGroupDivision
    stock_countlen <- (stock_maxlength - stock_minlength) %/% stock_dl
    # TODO: These can't be formulae, since then we stop substituting stock name
    stock_minlen <- stock_minlength + stock_dl * (seq_len(stock_countlen) - 1)
    stock_meanlen <- stock_minlen + (stock_dl / 2)

    stock_num <- array(dim = c(stock_countlen))
    stock_wgt <- array(dim = c(stock_countlen))

    list(
        name = stock_name,
        stock_num = parse(text = "stock_num[]")[[1]],
        stock_wgt = parse(text = "stock_wgt[]")[[1]],
        stock_name = stock_name,
        capture = ~{stock_lengths <- length_agg},
        iterate = ~extension_point)
}

g3s_fleet <- function(stock_name) {
    extension_point <- c()
    assign(paste0(stock_name, '_state'), 0)
    list(
        iterate = ~extension_point)
}

g3s_livesonareas <- function(inner_stock, areas) {
    stopifnot('g3_areas' %in% class(areas))

    stock_areas <- as.array(as.integer(areas))  # NB: Force stock_areas to be an array
    area <- areas[[1]]
    area_idx <- 0
    stock_num <- array(dim = c(dim(stock_definition(inner_stock, 'stock_num')), length(stock_areas)))
    stock_wgt <- array(dim = c(dim(stock_definition(inner_stock, 'stock_wgt')), length(stock_areas)))
    stock_extend(inner_stock,
        stock_num = as.call(c(as.list(inner_stock$stock_num), as.symbol("area_idx"))),
        stock_wgt = as.call(c(as.list(inner_stock$stock_wgt), as.symbol("area_idx"))),
        capture = f_substitute(~if (area %in% stock_areas) extension_point, list()),
        iterate = f_substitute(~for (area_idx in seq_along(stock_areas)) {
            area <- area_lookup
            extension_point
        }, list(
            area_lookup = if (length(stock_areas) > 1) quote(stock_areas[[g3_idx(area)]]) else stock_areas,
            extension_point = inner_stock$iterate)))
}

g3s_age <- function(inner_stock, minage, maxage) {
    # If these are literals, they should be integers
    stock_minage <- if(is.numeric(minage)) as.integer(minage) else minage
    stock_maxage <- if(is.numeric(minage)) as.integer(maxage) else minage
    stock_num <- array(dim = c(dim(stock_definition(inner_stock, 'stock_num')), stock_maxage - stock_minage + 1))
    stock_wgt <- array(dim = c(dim(stock_definition(inner_stock, 'stock_wgt')), stock_maxage - stock_minage + 1))
    age_idx <- 0L
    inner_stock <- stock_extend(inner_stock,
        stock_num = as.call(c(as.list(inner_stock$stock_num), quote(age_idx))),
        stock_wgt = as.call(c(as.list(inner_stock$stock_wgt), quote(age_idx))),
        capture = f_substitute(~for (age in seq(stock_minage, stock_maxage)) extension_point, list()),
        iterate = f_substitute(~for (age in seq(stock_minage, stock_maxage)) {
            age_idx <- g3_idx(age - stock_minage + 1)
            extension_point
        }, list(extension_point = inner_stock$iterate)))
}

g3s_prey <- function(inner_stock, energycontent) {
    inner_stock  # TODO:
}
