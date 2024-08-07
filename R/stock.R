# Pull the definition of the stock variable out of the stock object
g3_stock_def <- function(stock, name) {
    # If a list is given, sapply each member
    if (!g3_is_stock(stock) && is.list(stock)) return(lapply(stock, function (s) g3_stock_def(s, name)))
    if (!startsWith(name, 'stock__')) name <- paste0('stock__', name)
    get(name, envir = stock$env)
}
# Deprecated internal alias
stock_definition <- function(...) {
    warning("Use g3_stock_def() instead of gadget3:::stock_definition()")
    g3_stock_def(...)
}

# Define an array that matches stock
g3_stock_instance <- function (stock, init_value = NA, desc = "") {
    if (length(stock$dim) == 0) {
        # No dimensions mean a 1-entry array
        x <- array(init_value, dim = (1))
    } else if (all(is.numeric(stock$dim))) {
        # TODO: Don't make it yet, defer until g3 needs it
        x <- array(init_value, dim = stock$dim, dimnames = stock$dimnames)
    } else {
        # Filter any dynamic dimensions, and set them to 1 initially
        use_dynamic_dim <- FALSE
        init_dim <- vapply(stock$dim, function (x) {
            if (is.call(x) || is.symbol(x)) { use_dynamic_dim <<- TRUE; 1L } else as.integer(x)
        }, integer(1))
        init_dimnames <- lapply(stock$dimnames, function (x) {
            if (is.call(x) || is.symbol(x)) { use_dynamic_dim <<- TRUE; NULL } else x
        })
        x <- array(init_value, dim = init_dim, dimnames = init_dimnames)
        if (use_dynamic_dim) {
            attr(x, 'dynamic_dim') <- stock$dim
            attr(x, 'dynamic_dimnames') <- stock$dimnames
        }
    }
    if (nzchar(desc)) {
        attr(x, 'desc') <- desc
    }
    return(x)
}

g3_storage <- function(var_name) {
    stopifnot(is.character(var_name))

    structure(list(
        dim = list(),
        dimnames = list(),
        iterate = list(),
        iter_ss = list(),  # NB: No dimensions yet
        intersect = list(),
        interact = list(),
        with = list(),
        env = as.environment(list()),
        name_parts = var_name,
        name = paste(var_name, collapse = "_")), class = c("g3_stock", "list"))
}

# Print method, only really interested in dimensions
print.g3_stock <- function (x, ...) {
    cat("g3_stock", x$name, "with dimensions:\n")
    print(x$dimnames)
}

# True iff (x) is a stock object
g3_is_stock <- function (stock) inherits(stock, "g3_stock")

g3s_length <- function(inner_stock, lengthgroups, open_ended = TRUE, plus_dl = NULL) {
    # See LengthGroupDivision::LengthGroupDivision
    stopifnot(length(lengthgroups) > 0)

    if (!is.null(plus_dl)) {
        stock__plusdl <- as.numeric(plus_dl)
    } else if (length(lengthgroups) == 1) {
        # Only one lengthgroup, no sensible value to use
        stock__plusdl <- 1
    } else {
        # No plusdl provided, find the mode of dls and use that
        # (assuming core of lengthgroups will be regular, with a plus/minus group)
        stat_mode <- function(vec) {
            r <- rle(vec)
            r$values[[which.max(r$lengths)]]
        }
        stock__plusdl <- as.numeric(stat_mode(diff(lengthgroups)))
    }

    # stock__dl is size of each lengthgroup
    # stock__upperlen is the final upper bound of the groups
    if (isTRUE(open_ended)) {
        # plus-group infinite, use plusdl to guess a semi-sane __dl
        stock__dl <- unname(diff(c(lengthgroups, tail(lengthgroups, 1) + stock__plusdl)))
        stock__upperlen <- Inf
    } else {
        # Split off final value for absolute max
        stock__dl <- unname(diff(lengthgroups))
        stock__upperlen <- unname(tail(lengthgroups, 1))
        lengthgroups <- head(lengthgroups, -1)
    }

    # If no names given, make some up
    if (is.null(names(lengthgroups))) {
        names(lengthgroups) <- paste0(lengthgroups, ':', c(tail(lengthgroups, -1), stock__upperlen))
    }

    stock__minlen <- as_force_vector(lengthgroups)
    # NB: __midlen must be a vector not array, so we can do "stock__growth_l * maturity_bygrowth"
    # i.e. multiply growth matrix by ~midlen
    stock__midlen <- as_force_vector(lengthgroups + (stock__dl / 2))
    stock__maxmidlen <- as.numeric(tail(stock__midlen, 1))
    stock__minmidlen <- as.numeric(head(stock__midlen, 1))
    stock__maxlen <- as_force_vector(c(tail(lengthgroups, -1), stock__upperlen))
    names(stock__maxlen) <- names(stock__midlen)

    structure(list(
        dim = c(inner_stock$dim,
            length = length(lengthgroups)),
        dimnames = c(inner_stock$dimnames, list(
            length = names(lengthgroups))),
        iterate = c(inner_stock$iterate, length = quote(
                for (stock__length_idx in seq_along(stock__midlen)) g3_with(
                    length := stock__midlen[[stock__length_idx]], extension_point)
            )),
        iter_ss = c(inner_stock$iter_ss, length = as.symbol("stock__length_idx")),
        intersect = c(inner_stock$intersect, length = quote(
                for (stock__length_idx in seq_along(stock__minlen)) {
                    if (stock_isdefined(length)) {
                        if (stock__minlen[[stock__length_idx]] <= length &&
                                length < stock__maxlen[[stock__length_idx]]) {
                            extension_point
                            break
                        }
                    } else {
                        g3_with(length := stock__midlen[[stock__length_idx]], extension_point)
                    }
                }
            )),
        interact = c(inner_stock$interact, length = quote(
                for (stock__length_idx in seq_along(stock__midlen)) g3_with(
                    interactvar_length := stock__midlen[[stock__length_idx]], extension_point)
            )),
        with = c(inner_stock$with, length = quote(extension_point)),
        env = as.environment(c(as.list(inner_stock$env), list(
            stock__plusdl = stock__plusdl,
            stock__dl = stock__dl,
            stock__upperlen = stock__upperlen,
            stock__minlen = stock__minlen,
            stock__midlen = stock__midlen,
            stock__maxmidlen = stock__maxmidlen,
            stock__minmidlen = stock__minmidlen,
            stock__maxlen = stock__maxlen))),
        name_parts = inner_stock$name_parts,
        name = inner_stock$name), class = c("g3_stock", "list"))
}

g3_fleet <- function(var_name) g3_storage(var_name)
g3_stock <- function(var_name, lengthgroups, open_ended = TRUE) g3s_length(g3_storage(var_name), lengthgroups, open_ended)

g3s_clone <- function(inner_stock, var_name) {
    new_stock <- inner_stock
    new_stock$name <- var_name
    return(new_stock)
}
