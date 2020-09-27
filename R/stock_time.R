# Time dimension, useful for data objects
# - steps: Which steps get added to the data, e.g. c(4) for final step in quarter
g3s_time <- function(inner_stock, minyear, maxyear, steps = c()) {
    stock__totalsteps <- max(length(steps), 1L)  # NB: If no steps, the dimension should still be #years long
    # i.e. for all steps, return array lookup of cur_step -> stock__time_idx
    stock__steplookup <- vapply(1:12, function (x) { out <- which(steps <= x); if (length(out) > 0) max(out) else NA}, integer(1))

    if (length(steps) == 0) {
        # Year-based
        idx_f <- f_substitute(~g3_idx(cur_year - minyear + 1), list(
            minyear = minyear,
            maxyear = maxyear))
    } else {
        # Year & step
        idx_f <- f_substitute(~g3_idx(((cur_year - minyear) * stock__totalsteps) + stock__steplookup[[cur_step]]), list(
            minyear = as.integer(minyear),
            maxyear = as.integer(maxyear)))
    }

    list(
        dim = c(inner_stock$dim,
            time = (maxyear - minyear + 1) * stock__totalsteps),
        dimnames = c(inner_stock$dimnames, list(
            time = paste(
                rep(seq(minyear, maxyear), each = max(length(steps), 1)),
                rep(steps, times = maxyear - minyear + 1),
                sep = '.'))),
        iterate = ~stop("Not implemented"),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__time_idx"))),
        intersect = f_substitute(~g3_with(stock__time_idx, idx_f, if (stock__time_idx >= g3_idx(1) && stock__time_idx <= g3_idx(dimsize)) extension_point), list(
                dimsize = (maxyear - minyear + 1) * stock__totalsteps,
                idx_f = idx_f,
                extension_point = inner_stock$intersect)),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
        name = inner_stock$name)
}
