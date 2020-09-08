g3l_catchdistribution_sumofsquares <- function (over = c('area')) {
    call_append <- function (call, extra) as.call(c(as.list(sys.call(0)[[2]]), extra))

    f_substitute(~sum((
        modelstock__num[modelstock__iter] / max(sum(modelstock_total_f), 0.00001) -
        obsstock__num[obsstock__iter] / max(sum(obsstock_total_f), 0.00001)) ** 2), list(
            modelstock_total_f = call_append(stock_ssinv(modelstock__num), over),
            obsstock_total_f = call_append(stock_ssinv(obsstock__num, 'time'), over)))
}

g3l_catchdistribution_multinomial <- function () {
    ~2 * (lgamma(1 + sum(obsstock__num[obsstock__iter])) -
        sum(lgamma_vec(1 + obsstock__num[obsstock__iter])) +
        sum(obsstock__num[obsstock__iter] * log(
            modelstock__num[modelstock__iter] / sum(modelstock__num[modelstock__iter])))
    )
}

g3l_likelihood_data <- function (nll_name, data, missing = 'stop') {
    grid_args <- list()

    # Turn incoming data into stocks with correct dimensions
    grid_args$length = sort(unique(data$length))
    modelstock <- g3_stock(paste("cdist", nll_name, "model", sep = "_"), unique(data$length))
    obsstock <- g3_stock(paste("cdist", nll_name, "obs", sep = "_"), unique(data$length))

    # TODO: Stock dimensions should be managing their parts
    if ('age' %in% names(data)) {
        # TODO: MFDB-style attribute
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
    if ('area' %in% names(data)) {
        # Do area via. MFDB-style attributes
        area_group <- attr(data, 'area')
        area_group <- area_group[order(names(area_group))]  # Dimension order should match data
        grid_args$area <- names(area_group)
        modelstock <- g3s_areagroup(modelstock, area_group)
        obsstock <- g3s_areagroup(obsstock, area_group)
    }

    if ('step' %in% names(data)) {
        grid_args$step <- sort(unique(data$step))
    }
    grid_args$year <- seq(min(data$year), max(data$year))
    obsstock <- g3s_time(obsstock, min(data$year), max(data$year), if (is.null(grid_args$step)) c() else grid_args$step)

    # Regenerate table, making sure any gaps are filled (with NA)
    full_table <- do.call('expand.grid', c(grid_args, list(stringsAsFactors = FALSE)))
    # NB: We want to sort by final dimension first for array, so specfiy by to get the order right
    full_table <- merge(full_table, data, all.x = TRUE, sort = TRUE, by = names(grid_args))

    # TODO: More fancy NA-handling (i.e. random effects) goes here
    if (missing == 'stop') {
        if (any(is.na(full_table$number))) stop("Missing values in data")
    } else {
        # Fill in missing values with given value
        full_table$number[is.na(full_table$number)] <- missing
    }
    
    return(list(
        modelstock = modelstock,
        obsstock = obsstock,
        done_aggregating_f = if ('step' %in% names(data)) ~TRUE else ~cur_step_final,
        stock_map = stock_map,
        number = array(full_table$number,
            dim = dim(stock_definition(obsstock, 'stock__num')),
            dimnames = dimnames(stock_definition(obsstock, 'stock__num'))),
        nll_name = nll_name))
}

# Compare numbers caught by fleets to observation data
# - obs_data: Real-world observations. data.frame containing colums:
#    - year
#    - (optional) step (otherwise data summed over the current year)
#    - (optional) area (otherwise all areas summed)
#    - (optional) age (otherwise all ages summed)
#    - length
#    - number - expected number for given combination
# - fleets: Gather catch from these fleets
# - stocks: Gather catch (by fleets) for these stocks
# - function_f: Comparison function to compare modelstock__num[modelstock__iter] & obsstock__num[obsstock__iter]
# - weight: Weighting of parameter in final nll
g3l_catchdistribution <- function (nll_name, obs_data, fleets, stocks, function_f, missing = 'stop', weight = 1.0, run_at = 10) {
    out <- new.env(parent = emptyenv())

    # Convert data to stocks
    ld <- g3l_likelihood_data(nll_name, obs_data, missing = missing)
    modelstock <- ld$modelstock
    obsstock <- ld$obsstock
    assign(paste0(nll_name, '_number'), ld$number)

    out[[step_id(run_at, nll_name, 0)]] <- stock_step(f_substitute(~{
        comment(step_comment)
        if (cur_time == 0) {
            # Populate stock with data
            stock_with(obsstock, obsstock__num[] <- obsnumber)
            stock_with(modelstock, modelstock__num[] <- 0)
        }
    }, list(
        step_comment = paste0("Initial data / reset observations for ", nll_name),
        obsnumber = as.symbol(paste0(nll_name, '_number')))))
    
    for (fleet_stock in fleets) for (prey_stock in stocks) {
        # Work out stock index for obs/model variables
        if (!is.null(ld$stock_map)) {
            # Skip over stocks not part of the observation data, map to an index
            # NB: This is what stock_iterate() would do for us
            if (is.null(ld$stock_map[[prey_stock$name]])) next
            stockidx_f <- f_substitute(~g3_idx(x), list(x = ld$stock_map[[prey_stock$name]]))
        } else {
            # Not using stock grouping, __stock_idx variable not needed
            stockidx_f <- ~-1
        }

        # Collect all of fleet's sampling of prey and dump it in modelstock
        out[[step_id(run_at, nll_name, 1, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("Collect catch from ", fleet_stock, "/", prey_stock, " for ", modelstock)
            stock_iterate(prey_stock, stock_intersect(modelstock, {
                # Take prey_stock__fleet_stock weight, convert to individuals, add to our count
                modelstock__num[modelstock__iter] <- modelstock__num[modelstock__iter] +
                    stock_reshape(modelstock, prey_stock__fleet_stock[prey_stock__iter] / pmax(prey_stock__wgt[prey_stock__iter], 0.00001))
            }))
        }, list(
            # Find catch from predation step
            prey_stock__fleet_stock = as.symbol(paste0('prey_stock__', fleet_stock$name)))))

        # Fix-up stock intersection, add in stockidx_f
        out[[step_id(run_at, nll_name, 1, prey_stock)]] <- f_substitute(out[[step_id(run_at, nll_name, 1, prey_stock)]], list(
            stockidx_f = stockidx_f))
    }

    out[[step_id(run_at, nll_name, 2)]] <- stock_step(f_substitute(~{
        if (done_aggregating_f) {
            stock_comment("Compare ", modelstock, " to ", obsstock)
            stock_iterate(modelstock, stock_intersect(obsstock, {
                nll <- nll + weight * function_f
            }))
            stock_with(modelstock, modelstock__num[] <- 0)
        }
    }, list(
        done_aggregating_f = ld$done_aggregating_f,
        weight = weight,
        function_f = function_f)))
    # Fix-up stock intersection: index should be the same as observation
    out[[step_id(run_at, nll_name, 2)]] <- f_substitute(out[[step_id(run_at, nll_name, 2)]], list(
        stockidx_f = as.symbol(paste0(modelstock$name, "__stock_idx"))))

    return(out)
}
