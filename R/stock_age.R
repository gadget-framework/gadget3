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
        intersect = f_substitute(~if (age >= stock__minage && age <= stock__maxage) {
            stock__age_idx <- g3_idx(age - stock__minage + 1)
            extension_point
        }, list(
            extension_point = inner_stock$intersect)),
        name = inner_stock$name)
}
