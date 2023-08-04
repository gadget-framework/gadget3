g3_parameterized <- function(
        name,
        by_stock = FALSE,
        by_year = FALSE,
        by_step = FALSE,
        by_age = FALSE,
        exponentiate = FALSE,
        avoid_zero = FALSE,
        scale = 1,
        offset = 0,
        ...) {
    stopifnot(is.character(name))
    stopifnot(is.logical(by_age))
    stopifnot(is.logical(by_year))
    stopifnot(is.logical(by_step))
    stopifnot(is.logical(avoid_zero))

    if (exponentiate) name <- paste0(name, '_exp')

    table_defn <- list()

    if (by_year) {
        table_defn <- c(table_defn, list( cur_year = quote( seq(start_year, end_year) ) ))
    }
    if (by_step) {
        table_defn <- c(table_defn, list( cur_step = quote( seq_along(step_lengths) ) ))
    }

    if (isFALSE(by_stock)) {  # No grouping by stock
        if (by_age) stop("!by_stock && by_age doesn't make sense")
    } else if (isTRUE(by_stock) || is.character(by_stock)) {  # Group by default "stock", with an optional name_part
        if (by_age) {
            table_defn <- c(table_defn, list(age = quote(seq(stock__minage, stock__maxage))))
        }

        # Define name_part based on input by_stock
        if (isTRUE(by_stock)) {
            name_part <- NULL
        } else if (length(by_stock) > 1) {
            # Turn a vector into it's c(x, y, ...) language expression
            name_part <- as.call(c(as.symbol("c"), by_stock))
        } else {
            name_part <- by_stock
        }
    } else if (g3_is_stock(by_stock) || (is.list(by_stock) && all(sapply(by_stock, g3_is_stock)))) {  # Group by explicit stocks
        if (g3_is_stock(by_stock)) by_stock <- list(by_stock)
        names(by_stock) <- vapply(by_stock, function(s) s$name, character(1))

        # by_age is union of all stock ages
        # NB: We're not, ensuring s__minage are available. This is a bit naughty, but it's very improbable it's a problem
        if (by_age) {
            for (s in by_stock) {
                if (!("age" %in% names(s$dim))) stop(s$name, " has no age dimension, by_age = TRUE doesn't make sense")
            }
            min_age <- as.call(c(as.symbol('min'), lapply(
                unname(by_stock),
                function (s) as.symbol(paste0(s$name, '__minage')))))
            max_age <- as.call(c(as.symbol('max'), lapply(
                unname(by_stock),
                function (s) as.symbol(paste0(s$name, '__maxage')))))
            table_defn <- c(table_defn, list(age = substitute(seq(min_age, max_age), list(min_age = min_age, max_age = max_age))))
        }

        # Find common name_part, add that to our name
        common_part <- rep(TRUE, length(by_stock[[1]]$name_parts))
        for (i in seq_along(by_stock)) if (i > 1) {
            common_part <- common_part & (by_stock[[1]]$name_parts == by_stock[[i]]$name_parts)
        }
        name <- paste(c(by_stock[[1]]$name_parts[common_part], name), collapse = ".")
    } else stop('Unknown by_stock parameter, should be FALSE, TRUE, a name_part or list of stocks')

    # Generate core call
    if (length(table_defn) > 0) {
        table_defn <- as.call(c(as.symbol('expand.grid'), table_defn))
        out <- substitute(g3_param_table(x, table_defn), list(x = name, table_defn = table_defn))
    } else {
        out <- substitute(g3_param(x), list(x = name))
    }

    # Pass through standard g3_param arguments
    out <- as.call(c(as.list(out), list(...)))

    if (isTRUE(by_stock) || is.character(by_stock)) {
        # Use stock_prepend() to do stock substitutions
        out <- substitute(stock_prepend(stock, out, name_part = name_part), list(out = out, name_part = name_part))
    }

    # Turn character scale/offset into parameter code
    if (is.character(scale)) scale <- g3_parameterized(scale, by_stock = by_stock)
    if (is.character(offset)) offset <- g3_parameterized(offset, by_stock = by_stock)

    # Modify value if asked
    if (exponentiate) out <- substitute(exp(x), list(x = out))
    if (scale != 1) out <- substitute(scale * x, list(x = out, scale = scale))
    if (avoid_zero != 0) out <- substitute(avoid_zero(x), list(x = out))
    if (offset != 0) out <- substitute(x + offset, list(x = out, offset = offset))
    return(out)
}
