g3l_catchdistribution_sumofsquares <- function () {
    ~sum((
        predstock__num[predstock__iter] / sum(stock_ssinv(predstock__num, 'area')) -
        obsstock__num[obsstock__iter] / sum(stock_ssinv(obsstock__num, 'time', 'area'))) ** 2)
}

g3l_likelihood_data <- function (nll_name, data) {
    grid_args <- list()

    # Turn incoming data into stocks with correct dimensions
    grid_args$length = sort(unique(data$length))
    predstock <- g3_stock(paste("cdist", nll_name, "pred", sep = "_"), unique(data$length))
    obsstock <- g3_stock(paste("cdist", nll_name, "obs", sep = "_"), unique(data$length))

    # TODO: Stock dimensions should be managing their parts
    if ('age' %in% names(data)) {
        # TODO: MFDB-style attribute
        grid_args$age <- sort(unique(data$age))
        predstock <- g3s_agegroup(predstock, grid_args$age)
        obsstock <- g3s_agegroup(obsstock, grid_args$age)
    }

    if ('area' %in% names(data)) {
        # Do area via. MFDB-style attributes
        area_group <- attr(data, 'area')
        area_group <- area_group[order(names(area_group))]  # Dimension order should match data
        grid_args$area <- names(area_group)
        predstock <- g3s_areagroup(predstock, area_group)
        obsstock <- g3s_areagroup(obsstock, area_group)
    }

    grid_args$step <- sort(unique(data$step))
    grid_args$year <- seq(min(data$year), max(data$year))
    obsstock <- g3s_time(obsstock, min(data$year), max(data$year), grid_args$step)

    # Regenerate table, making sure any gaps are filled (with NA)
    full_table <- do.call('expand.grid', c(grid_args, list(stringsAsFactors = FALSE)))
    # NB: We want to sort by final dimension first for array, so specfiy by to get the order right
    full_table <- merge(full_table, data, all.x = TRUE, sort = TRUE, by = names(grid_args))

    # TODO: More fancy NA-handling (i.e. random effects) goes here
    if (any(is.na(full_table$number))) stop("Missing values in data")
    
    return(list(
        predstock = predstock,
        obsstock = obsstock,
        data = array(full_table$number,
            dim = dim(stock_definition(obsstock, 'stock__num'))),
        nll_name = nll_name))
}

# Compare numbers caught by fleets to observation data
# - obs_data: Real-world observations. data.frame containing:
#    - year & step columns
#    - (optional) area (otherwise all areas summed)
#    - (optional) age (otherwise all ages summed)
#    - length
#    - number - expected number for given combination
# - fleets: Gather catch from these fleets
# - stocks: Gather catch (by fleets) for these stocks
# - function_f: Comparison function to compare predstock__num[predstock__iter] & obsstock__num[obsstock__iter]
# - weight: Weighting of parameter in final nll
g3l_catchdistribution <- function (nll_name, obs_data, fleets, stocks, function_f, weight = 1.0, run_f = ~TRUE, run_at = 10) {
    out <- new.env(parent = emptyenv())

    # Convert data to stocks
    ld <- g3l_likelihood_data(nll_name, obs_data)
    predstock <- ld$predstock
    obsstock <- ld$obsstock
    assign(paste0(nll_name, '_data'), ld$data)

    out[[step_id(run_at, nll_name, 0)]] <- stock_step(f_substitute(~{
        comment(step_comment)
        if (cur_time == 0) {
            # Populate stock with data
            stock_rename(obsstock, obsstock__num[] <- obsdata)
            stock_rename(predstock, predstock__num[] <- 0)
        }
        stock_rename(predstock, predstock__num[] <- 0)
    }, list(
        step_comment = paste0("Initial data / reset observations for ", nll_name),
        obsdata = as.symbol(paste0(nll_name, '_data')))))
    
    for (fleet_stock in fleets) for (prey_stock in stocks) {
        # Collect all of fleet's sampling of prey and dump it in predstock
        out[[step_id(run_at, nll_name, 1, prey_stock)]] <- stock_step(f_substitute(~{
            stock_comment("Collect catch from", fleet_stock, "/", prey_stock, " for ", predstock)
            stock_iterate(prey_stock, stock_intersect(predstock, {
                # Take prey_stock__fleet_stock weight, convert to individuals, add to our count
                predstock__num[predstock__iter] <- predstock__num[predstock__iter] + stock_reshape(predstock, prey_stock__fleet_stock[prey_stock__iter]) / stock_reshape(predstock, prey_stock__wgt[prey_stock__iter])
            }))
        }, list(
            # Find catch from predation step
            prey_stock__fleet_stock = as.symbol(paste0('prey_stock__', fleet_stock$name)))))
    }

    out[[step_id(run_at, nll_name, 2)]] <- stock_step(f_substitute(~{
        stock_comment("Collect catchdistribution nll")
        stock_iterate(predstock, stock_intersect(obsstock, {
            nll <- nll + weight * function_f
        }))
    }, list(
        weight = weight,
        function_f = function_f)))

    return(out)
}
