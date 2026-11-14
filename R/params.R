stock_common_part <- function (stocks, collapse = "_") {
    # Find common name_part, add that to our name
    for (i in seq_along(stocks)) {
        if (i == 1) {
            common_part <- stocks[[i]]$name_parts
        } else {
            common_part <- intersect(common_part, stocks[[i]]$name_parts)
        }
    }

    if (length(common_part) == 0) {
        # No common parts, concatenate full names of both
        common_part <- sort(vapply(stocks, function (s) s$name, character(1)))
    }

    # If collapse given, collapse parts together
    if (!is.null(collapse)) common_part <- paste(common_part, collapse = collapse)
    return(common_part)
}

# Reverse g3_parameterized(), find out how parameter would be broken down
g3_parameterized_breakdown <- function (in_c) {
    if (!is.call(in_c)) stop("Expect a g3_parameterized() call")
    fn_name <- deparse1(in_c[[1]])

    # Ignore value modifiers
    if (fn_name == '~') return(g3_parameterized_breakdown(in_c[[2]]))
    if (fn_name == 'exp') return(g3_parameterized_breakdown(in_c[[2]]))
    if (fn_name == '*') return(g3_parameterized_breakdown(in_c[[2]]))
    if (fn_name == 'avoid_zero') return(g3_parameterized_breakdown(in_c[[2]]))
    if (fn_name == '+') return(g3_parameterized_breakdown(in_c[[2]]))

    if (fn_name == 'stock_prepend') return(c("stock", g3_parameterized_breakdown(in_c[[3]])))
    if (fn_name == 'g3_param') return(as.character(c()))
    if (fn_name == 'g3_param_table') {
        if (deparse1(in_c[[3]][[1]]) != "expand.grid") stop("Expect g3_param_table() to use expand.grid()")
        col_names <- names(in_c[[3]])
        return(col_names[nzchar(col_names)])
    }
    stop("Unknown parameter definition: ", deparse1(in_c))
}

g3_parameterized <- function(
        name,
        by_stock = FALSE,
        by_predator = FALSE,
        by_year = FALSE,
        by_step = FALSE,
        by_age = FALSE,
        by_area = FALSE,
        prepend_extra = list(),
        exponentiate = FALSE,
        avoid_zero = FALSE,
        scale = 1,
        offset = 0,
        ifmissing = NULL,
        ...) {
    stopifnot(is.character(name))
    stopifnot(is.logical(by_age))
    stopifnot(is.logical(by_year) || is.numeric(by_year))
    stopifnot(is.logical(by_step))
    stopifnot(is.logical(by_area))
    stopifnot(is.logical(avoid_zero))
    extra_params <- list(...)

    find_public_call <- function (calls) {
        for (in_c in rev(calls)) {
            if (is.symbol(in_c)) {
                # Raw symbol
                in_c <- as.character(in_c)
            } else if (is.call(in_c) && length(in_c[[1]]) == 3 && as.character(in_c[[1]][[1]]) == "::") {
                # Ignore explicit non-gadget imports
                if (!identical(in_c[[1]][[2]], "gadget3")) next

                in_c <- as.character(in_c[[1]][[3]])
            } else if (is.call(in_c) && is.symbol(in_c[[1]])) {
                # Unprefixed function call
                in_c <- as.character(in_c[[1]])
            } else {
                # Probably an anonymous function, ignore
                next
            }

            # Ignore anything not starting with g3
            if (!startsWith(in_c, "g3")) next

            # Ignore ourself & ~internal g3 commands
            if (in_c %in% c("g3_parameterized", "g3_formula", "g3_step", "g3_is_stock", "g3l_distribution")) next

            # Suitability functions are used everywhere, not very meaningful
            if (startsWith(in_c, "g3_suitability_")) next

            # Ignore anything that isn't publicly exported by ourself
            if (!exists(in_c, envir = asNamespace("gadget3"), inherits = FALSE)) next

            return(in_c)
        }
        return(NULL)
    }

    # If provided multiple parts to a name, concatenate them together
    name <- paste(name, collapse = ".")

    # Define name_part based on input arg
    name_part <- function (arg) {
        if (isTRUE(arg)) {
            name_part <- NULL
        } else if (length(arg) > 1) {
            # Turn a vector into it's c(x, y, ...) language expression
            name_part <- as.call(c(as.symbol("c"), arg))
        } else {
            name_part <- arg
        }
        return(name_part)
    }

    table_defn <- list()
    stock_extra <- NULL

    if (isTRUE(by_year)) {
        table_defn <- c(table_defn, list( cur_year = quote( seq(start_year, end_year) ) ))
    } else if (is.numeric(by_year)) {
        table_defn <- c(table_defn, list( cur_year = by_year ))
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

        stock_extra <- stock_common_part(by_stock)
    } else stop('Unknown by_stock parameter, should be FALSE, TRUE, a name_part or list of stocks')

    if (isTRUE(by_area) && (isTRUE(by_stock) || is.character(by_stock))) {
        # Area by stock
        table_defn <- c(table_defn, list(area = quote(named_vec_to_factor(stock__areas))))
    } else if (isTRUE(by_area) && (isTRUE(by_predator) || is.character(by_predator))) {
        # Area by predator
        table_defn <- c(table_defn, list(area = quote(named_vec_to_factor(predstock__areas))))
    } else if (isTRUE(by_area)) {
        stop("One of by_stock/by_predator should be set if by_area is set")
    }

    # Generate core call
    if (length(table_defn) > 0) {
        table_defn <- as.call(c(as.symbol('expand.grid'), table_defn))
        # NB: select isn't used (yet), but give something for code-analysis to find
        select_c <- as.call(c(
            list(as.symbol("list")),
            lapply(tail(names(table_defn), -1), as.symbol) ))
        out <- substitute(
            g3_param_table(x, table_defn, select = select_c), list(x = name, table_defn = table_defn, select_c = select_c))
    } else {
        out <- substitute(g3_param(x), list(x = name))
    }

    # log() parameters going into model (we'll exp them further down)
    if (exponentiate) out$type <- "LOG"

    # Add ifmissing to output, turning strings into parameters
    if (!is.null(ifmissing)) {
        if (is.character(ifmissing)) ifmissing <- g3_parameterized(ifmissing, by_stock = by_stock)
        out$ifmissing <- ifmissing
    }

    # Pass through standard g3_param arguments
    out <- as.call(c(as.list(out), extra_params))

    # Add source if we found one
    source <- find_public_call(sys.calls())
    if (length(source) > 0) out$source <- source

    if (isTRUE(by_predator) || is.character(by_predator)) {
        # Use stock_prepend() to do stock substitutions
        out <- substitute(stock_prepend(predstock, out, name_part = name_part), list(out = out, name_part = name_part(by_predator)))
    }

    if (!is.null(stock_extra)) {
        out <- substitute(stock_prepend(stock_extra, out), list(out = out, stock_extra = stock_extra))
    } else if (isTRUE(by_stock) || is.character(by_stock)) {
        # Use stock_prepend() to do stock substitutions
        out <- substitute(stock_prepend(stock, out, name_part = name_part), list(out = out, name_part = name_part(by_stock)))
    }

    # Append any extra items using stock_prepend
    for (ex in (if (is.list(prepend_extra)) prepend_extra else list(prepend_extra))) {
        out <- substitute(stock_prepend(ex, out), list(out = out, ex = ex))
    }

    # Turn character scale/offset into parameter code
    if (is.character(scale)) scale <- g3_parameterized(scale, by_stock = by_stock)
    if (is.character(offset)) offset <- g3_parameterized(offset, by_stock = by_stock)

    # Modify value if asked
    if (exponentiate) out <- substitute(exp(x), list(x = out))
    if (scale != 1) out <- substitute(x * scale, list(x = out, scale = scale))
    if (avoid_zero != 0) out <- substitute(avoid_zero(x), list(x = out))
    if (offset != 0) out <- substitute(x + offset, list(x = out, offset = offset))

    return(out)
}
