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

as.data.frame.g3_areas <- function(x, ...) {
    # Just store numeric area values in data frames
    return(as.data.frame(as.numeric(x), ...))
}

g3s_livesonareas <- function(inner_stock, areas) {
    stopifnot('g3_areas' %in% class(areas))

    stock__areas <- as.array(as.integer(areas))  # NB: Force stock__areas to be an array
    stock__totalareas <- length(stock__areas)
    area <- as.integer(areas[[1]])  # Don't leak g3_areas into model

    # Expand all storage with extra dimension
    stock_env <- rlang::f_env(inner_stock$iterate)
    for (var_name in c("stock__num", "stock__wgt", "stock__catch")) {
        if (exists(var_name, envir = stock_env)) {
            assign(var_name, array(dim = c(dim(stock_env[[var_name]]), stock__totalareas)))
        }
    }

    if (stock__totalareas == 1) {
        # Stock only in one area, so simplify
        stock__area <- as.integer(areas[[1]])  # Don't leak g3_areas into model
        stock__area_idx <- ~g3_idx(1)
        list(
            iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__area_idx"))),
            iterate = f_substitute(~{
                area <- stock__area
                extension_point
            }, list(extension_point = inner_stock$iterate)),
            intersect = f_substitute(~if (area == stock__area) {
                extension_point
            }, list(extension_point = inner_stock$intersect)),
            rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
            name = inner_stock$name)
    } else {
        stock__area_idx <- ~g3_idx(1)
        list(
            iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__area_idx"))),
            iterate = f_substitute(~for (stock__area_idx in seq_along(stock__areas)) {
                area <- stock__areas[[stock__area_idx]]
                extension_point
            }, list(extension_point = inner_stock$iterate)),
            intersect = f_substitute(~for (stock__area_idx in seq_along(stock__areas)) {
                if (stock__areas[[stock__area_idx]] == area) {
                    extension_point
                    break
                }
            }, list(extension_point = inner_stock$intersect)),
            rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
            name = inner_stock$name)
    }
}
