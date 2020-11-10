g3l_likelihood_data <- function (nll_name, data, missing_val = 0, area_group = NULL) {
    mfdb_min_bound <- function (x) { if (is.null(attr(x, 'min'))) x[[1]] else attr(x, 'min') }
    mfdb_max_bound <- function (x) { if (is.null(attr(x, 'max'))) tail(x, 1) else attr(x, 'max') }
    mfdb_eval <- function (x) { if (is.call(x)) eval(x) else x }

    # Turn incoming data into stocks with correct dimensions
    if ('length' %in% names(data)) {
        if (!is.null(attr(data, 'length', exact = TRUE))) {
            length_groups <- attr(data, 'length', exact = TRUE)
            # Ditch upper bound from length groups
            # TODO: Enforce continous groups, we don't support gaps
            length_groups <- c(
                # Lower bound of all groups
                vapply(length_groups, mfdb_min_bound, numeric(1)),
                # Upper bound
                mfdb_max_bound(length_groups[[length(length_groups)]]))

            modelstock <- g3_stock(paste("cdist", nll_name, "model", sep = "_"), length_groups, open_ended = FALSE)
        } else {
            length_groups <- sort(unique(data$length))

            # Default to open-ended, as there's no way to specify the maximum
            modelstock <- g3_stock(paste("cdist", nll_name, "model", sep = "_"), length_groups, open_ended = TRUE)
            data$length <- paste0('len', data$length)  # Make data match autoset groups
        }
    } else {
        # Stocks currently have to have a length vector, even if it only has one element
        modelstock <- g3_stock(paste("cdist", nll_name, "model", sep = "_"), c(0))
        data$length <- 'len0'
    }

    # TODO: Stock dimensions should be managing their parts
    if ('age' %in% names(data)) {
        if (!is.null(attr(data, 'age', exact = TRUE))) {
            age_groups <- attr(data, 'age', exact = TRUE)
            age_groups <- lapply(age_groups, mfdb_eval)  # Convert seq(2, 4) back to 2,3,4
            modelstock <- g3s_agegroup(modelstock, age_groups)
        } else {
            age_groups <- seq(min(data$age), max(data$age))
            modelstock <- g3s_agegroup(modelstock, age_groups)
            data$age <- paste0('age', data$age)  # Make data match autoset groups
        }
    }

    if ('stock' %in% names(data)) {
        stock_groups <- levels(as.factor(data$stock))
        stock_map <- structure(as.list(seq_along(stock_groups)), names = stock_groups)
        # NB: We have to replace stockidx_f later whenever we intersect over these
        modelstock <- g3s_manual(modelstock, 'stock', stock_groups, ~stockidx_f)
    } else {
        stock_map <- NULL
    }

    # NB: area has to be last, so we can sum for the entire area/time
    if ('area' %in% names(data)) {
        # NB: Ignore MFDB attributes on purpose, we're interested in the aggregated areas here
        used_areas <- as.character(unique(data$area))
        if (is.null(area_group)) {
            # If no area grouping provided, assume area_group are integers already
            if (suppressWarnings(anyNA(as.integer(used_areas)))) {
                stop("Areas in data don't have integer names, but areas not provided")
            }
            area_group <- structure(
                as.integer(used_areas),
                names = used_areas)
        } else {
            # Filter area_group by what we actually need
            area_group <- area_group[names(area_group) %in% used_areas]
        }
        area_group <- area_group[order(names(area_group))]  # Dimension order should match data
        modelstock <- g3s_areagroup(modelstock, area_group)
    }

    # Work out time dimension, create an obsstock using it
    if (!('year' %in% names(data))) {
        stop("Data must contain a year column")
    }
    # NB: Let g3s_time_convert() worry about if the step column is there or not
    data$time <- g3s_time_convert(data$year, data$step)
    obsstock <- g3s_time(
        g3s_clone(modelstock, paste("cdist", nll_name, "obs", sep = "_")),
        sort(unique(data$time)))

    # Generate full table based on stock
    full_table <- as.data.frame.table(
        stock_instance(obsstock, 0),
        stringsAsFactors = TRUE)
    # Use freq column to preserve ordering of output
    full_table$Freq <- seq_len(nrow(full_table))
    full_table <- merge(full_table, data, all.x = TRUE)

    if ('number' %in% names(full_table)) {
        # TODO: More fancy NA-handling (i.e. random effects) goes here
        if (missing_val == 'stop') {
            if (any(is.na(full_table$number))) stop("Missing values in data")
        } else {
            # Fill in missing values with given value
            full_table$number[is.na(full_table$number)] <- missing_val
        }
        # TODO: Stock_instance instead?
        number_array <- array(full_table$number[order(full_table$Freq)],
            dim = obsstock$dim,
            dimnames = obsstock$dimnames)
    } else {
        number_array <- NULL
    }

    if ('weight' %in% names(full_table)) {
        # TODO: More fancy NA-handling (i.e. random effects) goes here
        if (missing_val == 'stop') {
            if (any(is.na(full_table$weight))) stop("Missing values in data")
        } else {
            # Fill in missing values with given value
            full_table$weight[is.na(full_table$weight)] <- missing_val
        }
        # TODO: Stock_instance instead?
        weight_array <- array(full_table$weight[order(full_table$Freq)],
            dim = obsstock$dim,
            dimnames = obsstock$dimnames)
    } else {
        weight_array <- NULL
    }
    
    return(list(
        modelstock = modelstock,
        obsstock = obsstock,
        done_aggregating_f = if ('step' %in% names(data)) ~TRUE else ~cur_step_final,
        stock_map = stock_map,
        number = number_array,
        weight = weight_array,
        nll_name = nll_name))
}

