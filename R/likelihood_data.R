g3l_likelihood_data <- function (nll_name, data, missing_val = 0, area_group = NULL, model_history = FALSE) {
    mfdb_min_bound <- function (x) { if (is.null(attr(x, 'min'))) x[[1]] else attr(x, 'min') }
    mfdb_max_bound <- function (x) { if (is.null(attr(x, 'max'))) tail(x, 1) else attr(x, 'max') }
    mfdb_eval <- function (x) { if (is.call(x)) eval(x) else x }

    # vector of col names, will cross them off as we go
    handled_columns <- structure(as.list(seq_along(names(data))), names = names(data))

    # Turn incoming data into stocks with correct dimensions
    if ('length' %in% names(data)) {
        if (!is.null(attr(data, 'length', exact = TRUE))) {
            length_groups <- attr(data, 'length', exact = TRUE)

            # Make sure length groups are contiguous
            if (!isTRUE(all.equal(
                    unname(head(vapply(length_groups, mfdb_max_bound, numeric(1)), -1)),
                    unname(tail(vapply(length_groups, mfdb_min_bound, numeric(1)), -1))))) {
                stop("Gaps in length groups are not supported")
            }

            # Form length groups using lower bound from all groups
            length_vec <- vapply(length_groups, mfdb_min_bound, numeric(1))

            open_ended_upper <- isTRUE(attr(length_groups[[length(length_groups)]], 'max_open_ended'))
            if (!open_ended_upper) {
                # Not open ended, so final bound should be max of last item
                length_vec <- c(
                    length_vec,
                    mfdb_max_bound(length_groups[[length(length_groups)]]))
            }

            if (isTRUE(attr(length_groups[[1]], 'min_open_ended'))) {
                # Lower bound open-ended, so set first lengthgroup to start at 0
                length_vec[[1]] <- 0
            }

            # We want to use our own names, so remove MFDB's
            names(length_vec) <- NULL

            modelstock <- g3_stock(paste(nll_name, "model", sep = "_"), length_vec, open_ended = open_ended_upper)

            # Convert data$length to use our naming
            data$length <- factor(data$length, levels = names(length_groups))
            levels(data$length) <- modelstock$dimnames$length
        } else {
            length_groups <- sort(unique(data$length))

            # Default to open-ended, as there's no way to specify the maximum
            modelstock <- g3_stock(paste(nll_name, "model", sep = "_"), length_groups, open_ended = TRUE)
            # Convert length data to use our naming
            data$length <- factor(
                data$length,
                levels = length_groups,
                labels = modelstock$dimnames$length)
        }
        handled_columns$length <- NULL
    } else {
        # Stocks currently have to have a length vector, even if it only has one element
        modelstock <- g3_stock(paste(nll_name, "model", sep = "_"), c(0))
        data$length <- modelstock$dimnames$length
    }

    if ('age' %in% names(data)) {
        if (!is.null(attr(data, 'age', exact = TRUE))) {
            age_groups <- attr(data, 'age', exact = TRUE)
            age_groups <- lapply(age_groups, mfdb_eval)  # Convert seq(2, 4) back to 2,3,4

            # We want to use our own names, so remove MFDB's
            modelstock <- g3s_agegroup(modelstock, unname(age_groups))

            # Convert data$age to use our naming
            data$age <- factor(data$age, levels = names(age_groups))
            levels(data$age) <- modelstock$dimnames$age
        } else {
            age_groups <- seq(min(data$age), max(data$age))

            modelstock <- g3s_age(modelstock, min(data$age), max(data$age))
            # Convert age data to use our naming
            data$age <- factor(
                data$age,
                levels = age_groups,
                labels = modelstock$dimnames$age)
        }
        handled_columns$age <- NULL
    }

    if ('tag' %in% names(data)) {
        if (is.factor(data$tag)) {
            tag_ids <- structure(
                seq_along(levels(data$tag)),
                names = levels(data$tag))
        } else {
            tag_ids <- as.integer(unique(data$tag))
        }
        modelstock <- g3s_tag(modelstock, tag_ids, force_untagged = FALSE)
        handled_columns$tag <- NULL
    }

    if ('stock' %in% names(data)) {
        stock_groups <- levels(as.factor(data$stock))
        stock_map <- structure(as.list(seq_along(stock_groups)), names = stock_groups)
        # NB: We have to replace stockidx_f later whenever we intersect over these
        modelstock <- g3s_manual(modelstock, 'stock', stock_groups, ~stockidx_f)
        handled_columns$stock <- NULL
    } else {
        stock_map <- NULL
    }

    # Work out time dimension, create an obsstock using it
    if (!('year' %in% names(data))) {
        stop("Data must contain a year column")
    }
    # NB: Let g3s_time_convert() worry about if the step column is there or not
    data$time <- g3s_time_convert(data$year, data$step)
    obsstock <- g3s_time(
        g3s_clone(modelstock, paste(nll_name, "obs", sep = "_")),
        sort(unique(data$time)))
    if (model_history) {
        modelstock <- g3s_time(
            modelstock,
            sort(unique(data$time)))
    }
    data$time <- g3s_time_labels(data$time)
    handled_columns$year <- NULL
    handled_columns$step <- NULL

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
        obsstock <- g3s_areagroup(obsstock, area_group)
        handled_columns$area <- NULL
    }

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
        handled_columns$number <- NULL
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
        handled_columns$weight <- NULL
    } else {
        weight_array <- NULL
    }

    if (length(handled_columns) > 0) {
        stop("Unrecognised columns in likelihood data: ", paste(names(handled_columns), collapse = ", "))
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

