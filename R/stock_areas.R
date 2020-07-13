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
    stock_env <- rlang::f_env(inner_stock$iterate)

    stock__areas <- as.array(as.integer(areas))  # NB: Force stock__areas to be an array
    area <- areas[[1]]
    possible_area <- areas[[1]]
    if (exists("stock__num", envir = stock_env)) {
        stock__num <- array(dim = c(dim(stock_env[['stock__num']]), length(stock__areas)))
    }
    if (exists("stock__wgt", envir = stock_env)) {
        stock__wgt <- array(dim = c(dim(stock_env[['stock__wgt']]), length(stock__areas)))
    }
    if (exists("stock__catch", envir = stock_env)) {
        stock__catch <- array(dim = c(dim(stock_env[['stock__catch']]), length(stock__areas)))
    }
    stock__area_idx <- 0L

    list(
        iterate = f_substitute(~for (stock__area_idx in seq_along(stock__areas)) {
            area <- area_lookup
            extension_point
        }, list(
            # TODO: I think this was supposed to optimise away the loop for single area, got lost somewhere
            area_lookup = if (length(stock__areas) > 1) quote(stock__areas[[stock__area_idx]]) else quote(stock__areas[[g3_idx(1)]]),
            extension_point = inner_stock$iterate)),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__area_idx"))),
        intersect = f_substitute(~for (possible_area in seq_along(stock__areas)) {
            if (stock__areas[[possible_area]] == area) {
                stock__area_idx <- possible_area

                extension_point
                break
            }
        }, list(
            extension_point = inner_stock$intersect)),
        name = inner_stock$name)
}
