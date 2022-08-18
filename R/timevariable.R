g3_timevariable <- function (lookup_name, fs) {
    stopifnot(is.character(lookup_name) && length(lookup_name) == 1)
    stopifnot(is.list(fs))
    lookup_name <- paste0("tv_", lookup_name)

    # Pick off first item as initial value
    # NB: gadget2 insisted that first item's timestep matched start year
    if (names(fs)[[1]] != 'init') stop("First formula should be named 'init'")
    first_f <- fs[[1]]
    fs <- tail(fs, -1)

    # years/steps from label: Either 4 digits (year, step = 1) or 1234-56 (year & step)
    m <- regmatches(names(fs), regexec('^([0-9]{4})-?([0-9]{1,2})?$', names(fs), perl = TRUE))
    invalid_matches <- vapply(m, length, integer(1)) != 3
    if (any(invalid_matches)) stop("Invalid formula identifier(s): ", names(fs)[invalid_matches])
    years <- vapply(m, function(x) as.integer(x[[2]]), integer(1))
    steps <- vapply(m, function(x) as.integer(x[[3]]), integer(1))
    steps[is.na(steps)] <- 1  # Default to step 1

    # Collapse formulas into long conditional
    iter_f <- f_chain_conditional(
        fs,
        cur_year = years,
        cur_step = steps,
        default_f = as.symbol(lookup_name))

    # Return formula that references a global formula for the time variable
    out <- call_to_formula(as.symbol(lookup_name))
    environment(out)[[lookup_name]] <- g3_global_formula(iter_f, init_val = first_f)
    return(out)
}
