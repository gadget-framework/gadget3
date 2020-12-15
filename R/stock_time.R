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
            times <- expand.grid(step = step, year = year)
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
        idx_f <- timelookup('getdefault', ~cur_year * 1000L + cur_step, -1L)
    } else {
        # Just year
        idx_f <- timelookup('getdefault', ~cur_year, -1L)
    }

    structure(list(
        dim = c(inner_stock$dim,
            time = length(times)),
        dimnames = c(inner_stock$dimnames, list(
            time = times)),
        # NB: iterate same as intersect, iterating over all time won't make sense in ~all cases
        iterate = f_substitute(~g3_with(stock__time_idx, g3_idx(idx_f), if (stock__time_idx >= g3_idx(1L)) extension_point), list(
                idx_f = idx_f,
                extension_point = inner_stock$iterate)),
        iter_ss = c(inner_stock$iter_ss, as.symbol("stock__time_idx")),
        intersect = f_substitute(~g3_with(stock__time_idx, g3_idx(idx_f), if (stock__time_idx >= g3_idx(1L)) extension_point), list(
                idx_f = idx_f,
                extension_point = inner_stock$intersect), copy_all_env = TRUE),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
        name = inner_stock$name), class = c("g3_stock", "list"))
}

# Add dimension for model time (i.e. every year/step)
g3s_modeltime <- function (inner_stock) {
    structure(list(
        dim = c(inner_stock$dim, list(
            # NB: Quoted so this is defined at run-time (by g3a_time)
            time = quote(total_steps + 1))),
        dimnames = c(inner_stock$dimnames, list(
            # NB: Quoted so this is defined at run-time (by g3a_time)
            time = quote(sprintf("%d-%02d",
                rep(seq(start_year, end_year), each = length(step_lengths)),
                rep(seq_along(step_lengths), times = end_year - start_year + 1))))),
        iterate = f_substitute(~extension_point, list(
                extension_point = inner_stock$iterate), copy_all_env = TRUE),
        iter_ss = c(inner_stock$iter_ss, quote(g3_idx(cur_time+1L))),
        intersect = f_substitute(~extension_point, list(
                extension_point = inner_stock$intersect), copy_all_env = TRUE),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename), copy_all_env = TRUE),
        name = inner_stock$name), class = c("g3_stock", "list"))
}
