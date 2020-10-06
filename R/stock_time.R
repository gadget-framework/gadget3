g3s_time_convert <- function (year, step = NULL) {
    if (is.null(step)) as.integer(year) else as.integer(year) * 1000 + as.integer(step)
}

# Time dimension, useful for data objects
# - times: Vector of g3s_time_convert(year, step) for each year/step data applies to
g3s_time <- function(inner_stock, times, year = NULL, step = NULL) {
    # If year/step provided, populate times
    if (!is.null(year)) {
        if (is.null(step)) {
            times <- g3s_time_convert(year)
        } else {
            # Generate all combinations of year/step, turn into times
            times <- expand.grid(year = year, step = step)
            times <- g3s_time_convert(times$year, times$step)
        }
    }

    # time -> index lookup
    timelookup <- g3_intlookup(
        paste0('times_', inner_stock$name),
        as.integer(times),
        seq_along(times))

    if (any(times > 9999)) {
        # Year + step
        idx_f <- timelookup('getdefault', ~cur_year * 1000 + cur_step, -1)
    } else {
        # Just year
        idx_f <- timelookup('getdefault', ~cur_year, -1)
    }

    structure(list(
        dim = c(inner_stock$dim,
            time = length(times)),
        dimnames = c(inner_stock$dimnames, list(
            time = times)),
        iterate = ~stop("Not implemented"),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__time_idx"))),
        intersect = f_substitute(~g3_with(stock__time_idx, g3_idx(idx_f), if (stock__time_idx >= g3_idx(1)) extension_point), list(
                idx_f = idx_f,
                extension_point = inner_stock$intersect)),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
        name = inner_stock$name), class = c("g3_stock", "list"))
}
