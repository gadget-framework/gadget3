# Time dimension, useful for data objects
# - steps: Which steps get added to the data, e.g. c(4) for final step in quarter
g3s_time <- function(inner_stock, minyear, maxyear, steps = c()) {
    stock__totalsteps <- max(length(steps), 1L)  # NB: If no steps, the dimension should still be #years long
    # i.e. for all steps, return array lookup of cur_step -> stock__time_idx
    stock__steplookup <- vapply(1:12, function (x) { out <- which(steps <= x); if (length(out) > 0) max(out) else NA}, integer(1))

    # Expand all storage with extra dimension
    stock_env <- rlang::f_env(inner_stock$iterate)
    for (var_name in c("stock__num", "stock__wgt", "stock__catch")) {
        if (exists(var_name, envir = stock_env)) {
            assign(var_name, array(
                dim = c(dim(stock_env[[var_name]]), (maxyear - minyear + 1) * stock__totalsteps)))
        }
    }

    if (length(steps) == 0) {
        # Year-based
        idx_f <- f_substitute(~g3_idx(cur_year - minyear + 1), list(
            minyear = minyear,
            maxyear = maxyear))
    } else {
        # Year & step
        idx_f <- f_substitute(~g3_idx(((cur_year - minyear) * stock__totalsteps) + stock__steplookup[[g3_idx(cur_step)]]), list(
            minyear = as.integer(minyear),
            maxyear = as.integer(maxyear)))
    }

    list(
        iterate = ~stop("Not implemented"),
        iter_ss = as.call(c(as.list(inner_stock$iter_ss), as.symbol("stock__time_idx"))),
        iter_ss_names = c(inner_stock$iter_ss_names, 'time'),
        intersect = f_substitute(~g3_with(stock__time_idx, idx_f, if (stock__time_idx >= g3_idx(1) && stock__time_idx <= g3_idx(dimsize)) extension_point), list(
                dimsize = (maxyear - minyear + 1) * stock__totalsteps,
                idx_f = idx_f,
                extension_point = inner_stock$intersect)),
        rename = f_substitute(~extension_point, list(extension_point = inner_stock$rename)),
        name = inner_stock$name)
}
