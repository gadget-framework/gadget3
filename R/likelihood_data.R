g3l_likelihood_data <- function (nll_name, data, missing_val = 0) {
    grid_args <- list()

    # Turn incoming data into stocks with correct dimensions
    if (!is.null(attr(data, 'length', exact = TRUE))) {
        length_groups <- attr(data, 'length', exact = TRUE)
        # Ditch upper bound from length groups
        # TODO: Should be filling in any gaps
        length_groups <- vapply(length_groups, function (x) x[[1]], numeric(1))
        grid_args$length <- names(length_groups)
        modelstock <- g3_stock(paste("cdist", nll_name, "model", sep = "_"), length_groups)
        obsstock <- g3_stock(paste("cdist", nll_name, "obs", sep = "_"), length_groups)
    } else if ('length' %in% names(data)) {
        grid_args$length <- sort(unique(data$length))
        modelstock <- g3_stock(paste("cdist", nll_name, "model", sep = "_"), grid_args$length)
        obsstock <- g3_stock(paste("cdist", nll_name, "obs", sep = "_"), grid_args$length)
    } else {
        # Stocks currently have to have a length vector, even if it only has one element
        modelstock <- g3_stock(paste("cdist", nll_name, "model", sep = "_"), c(0))
        obsstock <- g3_stock(paste("cdist", nll_name, "obs", sep = "_"), c(0))
    }

    # TODO: Stock dimensions should be managing their parts
    if (!is.null(attr(data, 'age', exact = TRUE))) {
        age_groups <- attr(data, 'age', exact = TRUE)
        grid_args$age <- names(age_groups)
        modelstock <- g3s_agegroup(modelstock, age_groups)
        obsstock <- g3s_agegroup(obsstock, age_groups)
    } else if ('age' %in% names(data)) {
        grid_args$age <- sort(unique(data$age))
        modelstock <- g3s_agegroup(modelstock, grid_args$age)
        obsstock <- g3s_agegroup(obsstock, grid_args$age)
    }

    if ('stock' %in% names(data)) {
        grid_args$stock <- levels(as.factor(data$stock))
        stock_map <- structure(as.list(seq_along(grid_args$stock)), names = grid_args$stock)
        # NB: We have to replace stockidx_f later whenever we intersect over these
        modelstock <- g3s_manual(modelstock, 'stock', grid_args$stock, ~stockidx_f)
        obsstock <- g3s_manual(obsstock, 'stock', grid_args$stock, ~stockidx_f)
    } else {
        stock_map <- NULL
    }

    # NB: area has to be last, so we can sum for the entire area/time
    if (!is.null(attr(data, 'area', exact = TRUE))) {
        # Do area via. MFDB-style attributes
        area_group <- attr(data, 'area', exact = TRUE)
        area_group <- area_group[order(names(area_group))]  # Dimension order should match data
        grid_args$area <- names(area_group)
        modelstock <- g3s_areagroup(modelstock, area_group)
        obsstock <- g3s_areagroup(obsstock, area_group)
    }

    if ('year' %in% names(data)) {
        grid_args$year <- seq(min(data$year), max(data$year))
    } else {
        stop("Data must contain a year column")
    }
    if ('step' %in% names(data)) {
        grid_args$step <- sort(unique(data$step))
    }
    obsstock <- g3s_time(obsstock, min(data$year), max(data$year), if (is.null(grid_args$step)) c() else grid_args$step)

    # Regenerate table, making sure any gaps are filled (with NA)
    full_table <- do.call('expand.grid', c(grid_args, list(stringsAsFactors = FALSE)))
    # NB: We want to sort by final dimension first for array, so specfiy by to get the order right
    full_table <- merge(full_table, data, all.x = TRUE, sort = TRUE, by = names(grid_args))

    # TODO: More fancy NA-handling (i.e. random effects) goes here
    if (missing_val == 'stop') {
        if (any(is.na(full_table$number))) stop("Missing values in data")
    } else {
        # Fill in missing values with given value
        full_table$number[is.na(full_table$number)] <- missing_val
    }
    
    return(list(
        modelstock = modelstock,
        obsstock = obsstock,
        done_aggregating_f = if ('step' %in% names(data)) ~TRUE else ~cur_step_final,
        stock_map = stock_map,
        number = array(full_table$number,
            dim = obsstock$dim,
            dimnames = obsstock$dimnames),
        nll_name = nll_name))
}

