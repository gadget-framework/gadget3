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
