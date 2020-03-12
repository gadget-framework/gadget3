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
g3_stock <- function(stock_name, stock_lengths) {
    stock_num <- array(dim = c(length(stock_lengths)))
    stock_wgt <- array(dim = c(length(stock_lengths)))
    stock_lenmid <- vapply(seq_along(stock_lengths), function (i) {
        if (i < length(stock_lengths)) {
            mean(c(stock_lengths[[i]], stock_lengths[[i+1]]))
        } else {
            stock_lengths[[i]]  # TODO: What is the midpoint of the plus-group?
        }
    }, 0)

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

g3s_livesonareas <- function(inner_stock, stock_areas) {
    stock_num <- array(dim = c(dim(stock_definition(inner_stock, 'stock_num')), length(stock_areas)))
    stock_wgt <- array(dim = c(dim(stock_definition(inner_stock, 'stock_wgt')), length(stock_areas)))
    stock_extend(inner_stock,
        stock_num = as.call(c(as.list(inner_stock$stock_num), as.symbol("area"))),
        stock_wgt = as.call(c(as.list(inner_stock$stock_wgt), as.symbol("area"))),
        capture = f_substitute(~if (area %in% stock_areas) extension_point, list()),
        iterate = f_substitute(~for (area in stock_areas) extension_point, list(extension_point = inner_stock$iterate))
    )
}

g3s_age <- function(inner_stock, stock_ages) {
    stock_num <- array(dim = c(dim(stock_definition(inner_stock, 'stock_num')), length(stock_ages)))
    stock_wgt <- array(dim = c(dim(stock_definition(inner_stock, 'stock_wgt')), length(stock_ages)))
    inner_stock <- stock_extend(inner_stock,
        stock_num = as.call(c(as.list(inner_stock$stock_num), as.symbol("age"))),
        stock_wgt = as.call(c(as.list(inner_stock$stock_wgt), as.symbol("age"))),
        capture = f_substitute(~for (age in stock_ages) extension_point, list()),
        iterate = f_substitute(~for (age in stock_ages) extension_point, list(extension_point = inner_stock$iterate)))
}

g3s_prey <- function(inner_stock, energycontent) {
    inner_stock  # TODO:
}
