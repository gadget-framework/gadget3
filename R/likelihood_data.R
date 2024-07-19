parse_levels <- function (lvls, var_name) {
    m <- suppressWarnings(as.numeric(lvls))
    if (!anyNA(m)) return(data.frame(
        names = lvls,
        lower_incl = TRUE,
        lower_bound = m,
        upper_bound = c(tail(m, -1), Inf),  # NB: No data about final bound, assume open-ended
        upper_incl = FALSE,
        open_ended_upper = TRUE,
        stringsAsFactors = FALSE))

    m <- regmatches(lvls, regexec('^(\\[|\\()(.*),(.*)(\\]|\\))', lvls))
    if (all(vapply(m, length, numeric(1)) == 5)) return(data.frame(
        names = lvls,
        lower_incl = vapply(m, function (mm) identical(mm[[2]], '['), logical(1)),
        lower_bound = vapply(m, function (mm) as.numeric(mm[[3]]), numeric(1)),
        upper_bound = vapply(m, function (mm) as.numeric(mm[[4]]), numeric(1)),
        upper_incl = vapply(m, function (mm) identical(mm[[5]], ']'), logical(1)),
        open_ended_upper = is.infinite(as.numeric(tail(m, 1)[[1]][[4]])),
        stringsAsFactors = FALSE))

    m <- regmatches(lvls, regexec('^(.+):(.+)$', lvls))
    if (all(vapply(m, length, numeric(1)) == 3)) return(data.frame(
        names = lvls,
        lower_incl = TRUE,
        lower_bound = vapply(m, function (mm) as.numeric(mm[[2]]), numeric(1)),
        upper_bound = vapply(m, function (mm) as.numeric(mm[[3]]), numeric(1)),
        upper_incl = FALSE,
        open_ended_upper = is.infinite(as.numeric(tail(m, 1)[[1]][[3]])),
        stringsAsFactors = FALSE))

    stop("Unknown form of ", var_name, " levels, see ?cut for formatting: ", paste(lvls, collapse = ", "))
}

g3l_likelihood_data <- function (nll_name, data, missing_val = 0, area_group = NULL, model_history = "", all_stocks = list(), all_fleets = list()) {
    # vector of col names, will cross them off as we go
    handled_columns <- structure(as.list(seq_along(names(data))), names = names(data))

    # Work out time dimension, but don't add it just yet
    if ('year' %in% names(data)) {
        # NB: Let g3s_time_convert() worry about if the step column is there or not
        #     Suppress warnings from tibbles that it might be missing
        data$time <- g3s_time_convert(data$year, suppressWarnings(data$step))
        handled_columns$year <- NULL
        handled_columns$step <- NULL
    } else if ('time' %in% names(data)) {  # Convert our time=1999-01 strings back
        data$time <- g3s_time_convert(data$time)
        handled_columns$time <- NULL
    } else {
        stop("Data must contain a year column")
    }

    modelstock <- g3_storage(paste(nll_name, "model", sep = "_"))
    obsstock <- g3_storage(paste(nll_name, "obs", sep = "_"))
    maps <- list()

    # Turn incoming data into stocks with correct dimensions
    d <- ld_dim_length(data)
    if (!is.null(d[[1]])) {
        modelstock <- copydim(modelstock, d[[1]])
        obsstock <- copydim(obsstock, d[[1]])
        data$length <- d[[2]]
        handled_columns$length <- NULL
    } else {
        # Stocks currently have to have a length vector, even if it only has one element
        d[[1]] <- g3s_length(g3_storage("x"), c(0))
        modelstock <- copydim(modelstock, d[[1]])
        obsstock <- copydim(obsstock, d[[1]])
    }

    # Add early time dimension for surveyindices
    if (identical(model_history, 'early')) {
        modelstock <- g3s_time(
            modelstock,
            sort(unique(data$time)))
        obsstock <- g3s_time(
            obsstock,
            sort(unique(data$time)))
        data$time <- g3s_time_labels(data$time)
    }

    d <- ld_dim_age(data)
    if (!is.null(d[[1]])) {
        modelstock <- copydim(modelstock, d[[1]])
        obsstock <- copydim(obsstock, d[[1]])
        data$age <- d[[2]]
        handled_columns$age <- NULL
    }

    d <- ld_dim_tag(data)
    if (!is.null(d[[1]])) {
        modelstock <- copydim(modelstock, d[[1]])
        obsstock <- copydim(obsstock, d[[1]])
        data$tag <- d[[2]]
        handled_columns$tag <- NULL
    }

    d <- ld_dim_g3stock(data, "stock", all_stocks)
    if (!is.null(d[[1]])) {
        modelstock <- copydim(modelstock, d[[1]])
        # NB: We intersect obbstock onto modelstock, so we should copy that rather than the current stock
        obsstock <- g3s_manual(obsstock, names(d[[1]]$dim), d$groups, as.symbol( paste0(modelstock$name, "__stock_idx") ))
        # NB: g3stock doesn't modify data, so don't bother with d[[2]]
        handled_columns[[names(d[[1]]$dim)]] <- NULL
        maps[["stock"]] <- d$map
    }

    d <- ld_dim_length(data, col_name = 'predator_length')
    if (!is.null(d[[1]])) {
        modelstock <- copydim(modelstock, d[[1]], prefix = 'predator_')
        obsstock <- copydim(obsstock, d[[1]], prefix = 'predator_')
        data$predator_length <- d[[2]]
        handled_columns$predator_length <- NULL
    }

    d <- ld_dim_age(data, col_name = 'predator_age')
    if (!is.null(d[[1]])) {
        modelstock <- copydim(modelstock, d[[1]], prefix = 'predator_')
        obsstock <- copydim(obsstock, d[[1]], prefix = 'predator_')
        data$predator_age <- d[[2]]
        handled_columns$predator_age <- NULL
    }

    d <- ld_dim_tag(data, col_name = 'predator_tag')
    if (!is.null(d[[1]])) {
        modelstock <- copydim(modelstock, d[[1]], prefix = 'predator_')
        obsstock <- copydim(obsstock, d[[1]], prefix = 'predator_')
        data$predator_tag <- d[[2]]
        handled_columns$predator_tag <- NULL
    }

    d <- ld_dim_g3stock(data, "fleet", all_fleets)
    if (!is.null(d[[1]])) {
        modelstock <- copydim(modelstock, d[[1]])
        # NB: We intersect obbstock onto modelstock, so we should copy that rather than the current stock
        obsstock <- g3s_manual(obsstock, names(d[[1]]$dim), d$groups, as.symbol( paste0(modelstock$name, "__fleet_idx") ))
        # NB: g3stock doesn't modify data, so don't bother with d[[2]]
        handled_columns[[names(d[[1]]$dim)]] <- NULL
        maps[["fleet"]] <- d$map
    }

    # Add time dimension if it's supposed to be last
    if (!identical(model_history, 'early')) {
        # NB: We always add time to observations, whereas only when explicitly requested for model
        if (identical(model_history, 'late')) {
            modelstock <- g3s_time(
                modelstock,
                sort(unique(data$time)))
        }
        obsstock <- g3s_time(
            obsstock,
            sort(unique(data$time)))
        data$time <- g3s_time_labels(data$time)
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
        obsstock <- g3s_areagroup(obsstock, area_group)
        handled_columns$area <- NULL
    }

    # Generate full table based on stock
    full_table <- as.data.frame.table(
        g3_stock_instance(obsstock, 0),
        stringsAsFactors = TRUE)
    # Use freq column to preserve ordering of output
    full_table$Freq <- seq_len(nrow(full_table))
    full_table <- merge(full_table, data, all.x = TRUE)

    if ('number' %in% names(full_table)) {
        # TODO: More fancy NA-handling (i.e. random effects) goes here
        if (identical(missing_val, 'stop')) {
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
        if (identical(missing_val, 'stop')) {
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
        stock_map = maps[["stock"]],
        fleet_map = maps[["fleet"]],
        number = number_array,
        weight = weight_array,
        nll_name = nll_name))
}

ld_dim_length <- function(data, col_name = 'length') {
    mfdb_min_bound <- function (x) { if (is.null(attr(x, 'min'))) x[[1]] else attr(x, 'min') }
    mfdb_max_bound <- function (x) { if (is.null(attr(x, 'max'))) tail(x, 1) else attr(x, 'max') }
    data_col <- data[[col_name]]

    if (is.null(data_col)) {
        # No length dimension
        stock <- NULL
    } else if (!is.null(attr(data, col_name, exact = TRUE))) {
        length_groups <- attr(data, col_name, exact = TRUE)

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

        # Convert data$length to use our naming
        data_col <- factor(data_col, levels = names(length_groups))

        stock <- g3s_length(g3_storage("x"), length_vec, open_ended = open_ended_upper)
    } else {
        # Force length to be a factor if not already
        if (!is.factor(data_col)) {
            # Make sure levels are ordered according to cut strings
            lvls <- parse_levels(unique(data_col), col_name)
            lvls <- lvls[with(lvls, order(lower_bound, upper_bound)), 'names']
            data_col <- factor(data_col, levels = lvls)
        }

        lvls <- parse_levels(levels(data_col), col_name)
        open_ended_upper <- lvls$open_ended_upper[[1]]

        length_vec <- if (open_ended_upper) lvls$lower_bound else c(lvls$lower_bound, tail(lvls$upper_bound, 1))

        if (any(!lvls$lower_incl) || any(lvls$upper_incl)) {
            stop("length intervals should be inclusive-lower, i.e. cut(..., right=FALSE): ", paste(lvls$names, collapse = ", "))
        }
        if (!isTRUE(all.equal(tail(lvls$lower_bound, -1), head(lvls$upper_bound, -1)))) {
            stop("Gaps in length groups are not supported: ", paste(lvls$names, collapse = ", "))
        }

        stock <- g3s_length(g3_storage("x"), length_vec, open_ended = open_ended_upper)
    }

    if (!is.null(data_col)) levels(data_col) <- stock$dimnames$length
    return(list(stock, data_col))
}

ld_dim_age <- function(data, col_name = 'age') {
    mfdb_eval <- function (x) { if (is.call(x)) eval(x) else x }
    data_col <- data[[col_name]]

    if (is.null(data_col)) {
        # No age dimension
        stock <- NULL
    } else if (!is.null(attr(data, col_name, exact = TRUE))) {
        age_groups <- attr(data, col_name, exact = TRUE)
        age_groups <- lapply(age_groups, mfdb_eval)  # Convert seq(2, 4) back to 2,3,4

        # We want to use our own names, so remove MFDB's
        stock <- g3s_agegroup(g3_storage("x"), unname(age_groups))

        # Convert data_col to use our naming
        data_col <- factor(data_col, levels = names(age_groups))
        levels(data_col) <- stock$dimnames[[col_name]]
    } else if (is.numeric(data_col)) {
        # Numeric age columns don't need grouping
        age_groups <- seq(min(data_col), max(data_col))

        stock <- g3s_age(g3_storage("x"), min(data_col), max(data_col))
        # Convert age data to use our naming
        data_col <- factor(
            data_col,
            levels = age_groups,
            labels = stock$dimnames[[col_name]])
    } else {
        if (!is.factor(data_col)) {
            # Make sure levels are ordered according to cut strings
            lvls <- parse_levels(unique(data_col))
            lvls <- lvls[with(lvls, order(lower_bound, upper_bound)), 'names']
            data_col <- factor(data_col, levels = lvls)
        }
        lvls <- parse_levels(levels(data_col), "age")

        if (is.infinite(tail(lvls$upper_bound, 1))) {
            # No support for infinite upper bound, bodge
            lvls$upper_bound[[length(lvls$upper_bound)]] <-
                lvls$lower_bound[[length(lvls$lower_bound)]] +
                1  # NB: It's not going to be upper-inclusive, so will subtract one at next step
        }

        # Account for lower_incl / upper_incl
        lvls$lower_bound <- ifelse(!lvls$lower_incl, lvls$lower_bound + 1, lvls$lower_bound)
        lvls$upper_bound <- ifelse(!lvls$upper_incl, lvls$upper_bound - 1, lvls$upper_bound)
        age_groups <- lapply(seq_len(nrow(lvls)), function (i) seq(lvls[i, "lower_bound"], lvls[i, "upper_bound"]))
        # NB: We never set the original names on age_groups

        stock <- g3s_agegroup(g3_storage("x"), age_groups)
        levels(data_col) <- stock$dimnames[["age"]]  # NB: [[col_name]] isn't the right choice here, as it won't be prefixed
    }
    return(list( stock, data_col ))
}

ld_dim_tag <- function(data, col_name = 'tag') {
    data_col <- data[[col_name]]

    if (is.null(data_col)) {
        # No tag dimension
        stock <- NULL
    } else {
        if (is.factor(data_col)) {
            tag_ids <- structure(
                seq_along(levels(data_col)),
                names = levels(data_col))
        } else {
            tag_ids <- as.integer(unique(data_col))
        }
        stock <- g3s_tag(g3_storage("x"), tag_ids, force_untagged = FALSE)
    }
    return(list( stock, data_col ))
}

ld_dim_g3stock <- function(data, col_name = "stock", all_stocks) {
    col_name_re <- paste0(col_name, "_re")
    if (col_name %in% names(data) && col_name_re %in% names(data)) {
        stop("Don't support both ", col_name, " and ", col_name_re)
    }

    if (col_name %in% names(data)) {
        actual_col <- col_name
        # Unique stock string groupings, in order
        stock_groups <- as.character(data[[col_name]][!duplicated(data[[col_name]])])

        # stock_map: list of stock$name --> index of stock_groups it should be added to
        # Start off with everything mapping to NULL
        stock_map <- structure(
            rep(list(NULL), length(all_stocks)),
            names = vapply(all_stocks, function (s) s$name, character(1)))

        # For 1..(max name parts) and all stocks...
        for (n in seq_len(max(vapply(all_stocks, function (s) length(s$name_parts), integer(1))))) {
            for (i in seq_along(all_stocks)) {
                s <- all_stocks[[i]]

                # Get all (n)-long combinations of (s)' name parts. fish_imm_f --> c("fish_f", "imm_f", ...)
                if (n > length(s$name_parts)) next
                name_combn <- apply(utils::combn(s$name_parts, n), 2, function (x) paste(x, collapse = "_"))

                # If any one of these matches a stock_group, assign this stock to that string
                # NB: We do shortest first, so longer matches will override shorter ones
                matches <- which(stock_groups %in% name_combn)
                if (length(matches) > 0) stock_map[[i]] <- head(matches, 1)
            }
        }

        unused_groups <- setdiff(
            seq_along(stock_groups),
            unique(unlist(stock_map)) )
        if (length(unused_groups) > 0) {
            stop(col_name, " groups matched no ", col_name, " in likelihood data: ", paste(stock_groups[unused_groups], collapse = ", "))
        }
    } else if (col_name_re %in% names(data)) {
        actual_col <- col_name_re
        # Start off with everything mapping to NULL
        stock_map <- structure(
            rep(list(NULL), length(all_stocks)),
            names = vapply(all_stocks, function (s) s$name, character(1)))

        # For each regex, find all matches and map to that index
        stock_groups <- as.character(data[[col_name_re]][!duplicated(data[[col_name_re]])])
        for (i in rev(seq_along(stock_groups))) {  # NB: Reverse so first ones have precedence
            stock_map[grep(stock_groups[[i]], names(stock_map))] <- i
        }

        unused_regexes <- setdiff(
            seq_along(stock_groups),
            unique(unlist(stock_map)) )
        if (length(unused_regexes) > 0) {
            stop("stock_re regexes matched no stocks in likelihood data: ", paste(stock_groups[unused_regexes], collapse = ", "))
        }
    } else {
        # No relevant column --> nothing to do
        return(list(NULL))
    }

    # Turn stock_map mapping into formula
    intersect_idx_f <- list_to_stock_switch(
        # Wrap each value of stock_map with g3_idx(i)
        # NB: -1 will cause g3s_manual() to ignore this intersect
        sapply(stock_map, function (i) substitute(g3_idx(i), list(i = if (is.null(i)) -1L else i)) ),
        # Variable in g3l_distribution() this should be looking for
       if (col_name == "stock") "stock" else "predstock" )

    return(list(
        g3s_manual(g3_storage("x"), actual_col, stock_groups, intersect_idx_f),
        NULL,  # This is data_col in other methods
        groups = stock_groups,
        map = stock_map ))
}

# Copy a single dimension from (new_stock) atop (old_stock), renaming dimension by adding (prefix)
copydim <- function (inner_stock, new_stock, prefix = '') {
     old_dim <- names(new_stock$dim)[[1]]
     new_dim <- paste0(prefix, old_dim)

     sym_list <- list()
     # Prefix any instance of symbols (sl) in code / character vector (in_c) with (prefix)
     add_prefix <- function (in_c, sl) {
         repl_fn <- function (x) {
             sym <- as.character(if (is.symbol(x)) x else x[[1]])
             # Add our prefix to stock__ names
             sym <- gsub('^stock__', paste0('stock__', prefix), sym)
             # If something is already renamed, (e.g. x__agegroup), swap in new name
             sym <- gsub(paste0('^\\Q', new_stock$name, '\\E__'), paste0(inner_stock$name, '__', prefix), sym)
             # If "old_dim", add to the beginning
             if (sym == old_dim) sym <- paste0(prefix, sym)

             if (is.character(x)) x[[1]] <- sym  # NB: When renaming environment names
             else if (is.symbol(x)) x <- as.symbol(sym)
             else if (is.call(x)) {
                 for (i in seq_along(x)) {
                     # Replace function name, recurse into arguments
                     x[[i]] <- if (i == 1) as.symbol(sym) else add_prefix(x[[i]], sl)
                 }
             }

             return(x)
         }

         if (is.character(in_c)) return(vapply(in_c, repl_fn, character(1)))

         # Form call_replace(in_c, sl[[1]] = repl_fn, sl[[2]] = repl_fn, ...)
         args <- c(
             list(in_c),
             rep(list(repl_fn), length(sl)) )
         names(args) <- c("", sl)
         do.call(call_replace, args, quote = TRUE)
     }

     # Arguments to call_replace to add prefix to everything in repl_fn
     sym_list <- c(
         names(new_stock$env),
         if (nzchar(prefix)) old_dim else NULL,
         new_stock$iter_ss[[old_dim]])

     # Add prefix to references to symbols in (sym_list)
     repl_env_fn <- function (env) {
         out <- as.list(env)
         names(out) <- add_prefix(names(out), sym_list)

         # Recurse into any environments contained by this one
         for (n in names(out)) {
             if (rlang::is_formula(attr(out[[n]], "g3_global_init_val"))) {
                 # Add things defined here to the list of things we should be prefixing
                 sym_list <- c(
                     sym_list,
                     names(environment(out[[n]])),
                     names(environment(attr(out[[n]], "g3_global_init_val"))) )
                 out[[n]] <- structure(
                     call_to_formula(
                         add_prefix(rlang::f_rhs(out[[n]]), sym_list),
                         repl_env_fn(environment(out[[n]])) ),
                     g3_global_init_val = call_to_formula(
                         add_prefix(rlang::f_rhs( attr(out[[n]], "g3_global_init_val") ), sym_list),
                         repl_env_fn(environment(attr(out[[n]], "g3_global_init_val"))) ))
             } else if (rlang::is_formula(out[[n]])) {
                 sym_list <- c(sym_list, names(environment(out[[n]])))
                 out[[n]] <- call_to_formula(
                     add_prefix(rlang::f_rhs(out[[n]]), sym_list),
                     repl_env_fn(environment(out[[n]])) )
             }
         }
         return(out)
     }
     new_env <- repl_env_fn(new_stock$env)

     inner_stock$dim[[new_dim]] <- new_stock$dim[[old_dim]]
     inner_stock$dimnames[[new_dim]] <- new_stock$dimnames[[old_dim]]
     inner_stock$iterate[[new_dim]] <- add_prefix(new_stock$iterate[[old_dim]], sym_list)
     inner_stock$iter_ss[[new_dim]] <- add_prefix(new_stock$iter_ss[[old_dim]], sym_list)
     inner_stock$intersect[[new_dim]] <- add_prefix(new_stock$intersect[[old_dim]], sym_list)
     inner_stock$interact[[new_dim]] <- add_prefix(new_stock$interact[[old_dim]], sym_list)
     inner_stock$with[[new_dim]] <- add_prefix(new_stock$with[[old_dim]], sym_list)
     inner_stock$env <- as.environment(c(as.list(inner_stock$env), new_env))
     # NB: Leave name_parts, name as-is
     return(inner_stock)
}
