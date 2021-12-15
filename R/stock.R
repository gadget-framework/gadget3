# Pull the definition of the stock variable out of the stock object
stock_definition <- function(stock, var_name) {
    get(var_name, envir = rlang::f_env(stock$iterate))
}

# Define an array that matches stock
stock_instance <- function (stock, init_value = NA, desc = "") {
    if (length(stock$dim) == 0) {
        # No dimensions mean a 1-entry array
        return(array(init_value, dim = (1)))
    }
    # TODO: Don't make it yet, defer until g3 needs it
    if (all(is.numeric(stock$dim))) {
        return(array(init_value, dim = stock$dim, dimnames = stock$dimnames))
    }

    # Filter any dynamic dimensions, and set them to 1 initially
    use_dynamic_dim <- FALSE
    init_dim <- vapply(stock$dim, function (x) if (is.call(x) || is.symbol(x)) {use_dynamic_dim <<- TRUE; 1L} else as.integer(x), integer(1))
    init_dimnames <- lapply(stock$dimnames, function (x) if (is.call(x) || is.symbol(x)) {use_dynamic_dim <<- TRUE; NULL} else x)
    x <- array(init_value, dim = init_dim, dimnames = init_dimnames)
    if (use_dynamic_dim) {
        attr(x, 'dynamic_dim') <- stock$dim
        attr(x, 'dynamic_dimnames') <- stock$dimnames
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
        iterate = ~extension_point,
        iter_ss = list(),  # NB: No dimensions yet
        intersect = ~extension_point,
        interact = ~extension_point,
        rename = ~extension_point,
        name_parts = var_name,
        name = paste(var_name, collapse = "_")), class = c("g3_stock", "list"))
}

# Print method, only really interested in dimensions
print.g3_stock <- function (x, ...) {
    cat("g3_stock", x$name, "with dimensions:\n")
    print(x$dimnames)
}

# True iff (x) is a stock object
g3_is_stock <- function (x) "g3_stock" %in% class(x)

g3s_length <- function(inner_stock, lengthgroups, open_ended = TRUE, plus_dl = NULL) {
    # See LengthGroupDivision::LengthGroupDivision
    stopifnot(length(lengthgroups) > 0)

    if (!is.null(plus_dl)) {
        stock__plusdl <- as.integer(plus_dl)
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
        stock__plusdl <- stat_mode(diff(lengthgroups))
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

    # Force array so type is stable in TMB
    stock__minlen <- as.array(lengthgroups)
    stock__midlen <- as.array(lengthgroups + (stock__dl / 2))

    structure(list(
        dim = c(inner_stock$dim,
            length = length(lengthgroups)),
        dimnames = c(inner_stock$dimnames, list(
            length = names(lengthgroups))),
        iterate = f_substitute(~extension_point, list(
            extension_point = inner_stock$iterate), copy_all_env = TRUE),
        # NB: Length iterator is always "missing", so we get a lengthgroup vector
        iter_ss = c(inner_stock$iter_ss, length = list(quote(.[])[[3]])),
        intersect = f_substitute(~extension_point, list(
            extension_point = inner_stock$intersect), copy_all_env = TRUE),
        interact = f_substitute(~extension_point, list(
            extension_point = inner_stock$interact), copy_all_env = TRUE),
        rename = f_substitute(~extension_point, list(
            extension_point = inner_stock$rename), copy_all_env = TRUE),
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
