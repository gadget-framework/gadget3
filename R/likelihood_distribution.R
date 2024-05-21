call_append <- function (call, extra) as.call(c(as.list(call), extra))

dist_prop <- function (var_name, over) {
    var_sym <- as.symbol(var_name)
    length_idx_sym <- as.symbol("default")
    total_var_name <- gsub('__x$', '__sstotal', var_name)

    if ('length' %in% over) {
        # Stratified sums of squares
        out <- substitute(x / total[[idx]], list(
            x = call("stock_ss", var_sym, length = length_idx_sym),
            total = as.symbol(total_var_name),
            idx = as.symbol(gsub('__x$', '__length_idx', var_name))))
        total_call <- substitute(avoid_zero_vec(rowSums(x)), list(
            x = call_append(call("stock_ssinv", var_sym), over[over != 'length'])))
    } else {
        out <- substitute(x / total, list(
            x = call("stock_ss", var_sym),
            total = as.symbol(total_var_name)))
        total_call <- substitute(avoid_zero(sum(x)), list(
            x = call_append(call("stock_ssinv", var_sym), over)))
    }
    out <- call_to_formula(out, as.environment(list()))
    assign(total_var_name, total_call, envir = environment(out))
    return(out)
}

g3l_distribution_sumofsquares <- function (over = c('area')) {
    out <- f_substitute( quote((modelstock_prop - obsstock_prop) ** 2), list(
        modelstock_prop = dist_prop("modelstock__x", c('time', over)),
        obsstock_prop = dist_prop("obsstock__x", c('time', over))))
    # NB: Avoid the final sum() for stratified sum of squares, as we'll (probably)
    #     produce a scalar, which TMB can't sum. Ideally I think we remove the
    #     auto-drop-to-scalar from convert_subset(), but that's going to require a
    #     lot of debugging
    if (!('length' %in% over)) out <- f_substitute(quote(sum(out)), list(out = out))
    return(out)
}

g3l_distribution_multinomial <- function (epsilon = 10) {
    # data == obs, dist == model
    # multinomial.cc defines sumlog/likely as below
    sumlog <- substitute(
        sum(lgamma_vec(1 + data)) - lgamma(1 + sum(data)), list(
            data = quote(stock_ss(obsstock__x))))

    likely <- substitute(
        -sum(data * log(logspace_add_vec(
            dist / avoid_zero(sum(dist)) * 10000,
            (1 / (length(data) * epsilon)) * 10000) / 10000)), list(
                data = quote(stock_ss(obsstock__x)),
                dist = quote(stock_ss(modelstock__x)),
                epsilon = epsilon))

    f_substitute(~2 * (likely + sumlog), list(
        likely = likely,
        sumlog = sumlog))
}

g3l_distribution_multivariate <- function (rho_f, sigma_f, over = c('area')) {
    multivariate_fn <- g3_native(r = function (x, rho, sigma) {
        sum(dnorm(rho * lag(x, 1), sqrt(1 - rho**2) * sigma))
    }, cpp = '[](auto x, Type rho, Type sigma) -> Type { // NB: "auto" because it could be vector<Type> or array<Type>
       // http://kaskr.github.io/adcomp/_book/Densities.html#autoregressive-processes
       using namespace density;

       return SCALE(AR1(rho), sigma)(x);
    }')

    f_substitute(~multivariate_fn(
            modelstock_prop - obsstock_prop,
            rho_f,
            sigma_f), list(
                modelstock_prop = dist_prop("modelstock__x", c('time', over)),
                obsstock_prop = dist_prop("obsstock__x", c('time', over)),
                rho_f = rho_f,
                sigma_f = sigma_f))
}

g3l_distribution_surveyindices <- function (fit = 'log', alpha = NULL, beta = NULL) {
    stopifnot(fit == 'linear' || fit == 'log')

    N <- if(fit == 'log') ~log(avoid_zero_vec(stock_ss(modelstock__x, time =))) else ~stock_ss(modelstock__x, time = )
    I <- if(fit == 'log') ~log(avoid_zero_vec(stock_ss(obsstock__x, time =))) else ~stock_ss(obsstock__x, time = )

    surveyindices_linreg <- g3_native(r = function (N, I, fixed_alpha, fixed_beta) {
        meanI <- mean(I)
        meanN <- mean(N)
        beta <- if (is.nan(fixed_beta)) sum((I - meanI) * (N - meanN)) / avoid_zero(sum((N - meanN)**2)) else fixed_beta
        alpha <- if (is.nan(fixed_alpha)) meanI - beta * meanN else fixed_alpha
        return(c(alpha, beta))
    }, cpp = '[&avoid_zero](vector<Type> N, vector<Type> I, Type fixed_alpha, Type fixed_beta) -> vector<Type> {
        vector<Type> out(2);

        auto meanI = I.mean();
        auto meanN = N.mean();
        auto beta = std::isnan(asDouble(fixed_beta)) ? ((I - meanI) * (N - meanN)).sum() / avoid_zero((pow(N - meanN, (Type)2)).sum()) : fixed_beta;
        auto alpha = std::isnan(asDouble(fixed_alpha)) ? meanI - beta * meanN : fixed_alpha;
        out(0) = alpha;
        out(1) = beta;
        return out;
    }', depends = c('avoid_zero'))

    modelstock__params <- g3_global_formula(f_substitute(
        # NB: If we're not going to compare, don't bother recalculating linear regression
        ~if (modelstock__time_idx != modelstock__max_time_idx) modelstock__params else surveyindices_linreg(N, I, intercept_f, slope_f),
        list(
            N = N, I = I,
            # NB: Can't use NULL in C++, Use NaN instead
            intercept_f = if (is.null(alpha)) NaN else alpha,
            slope_f = if (is.null(beta)) NaN else beta)), init_val = c(0.0, 0.0))

    # NB: stock_ss(..., time = ) means we keep the time dimension, and have a vector of model time for each area,
    #     checking max_time_idx means we only do this once all data is collected.
    # NB: Outer sum sums the vector of timepoints, this formula is called for each area separately.
    out <- f_substitute(
        ~if (modelstock__time_idx != modelstock__max_time_idx) 0 else sum((modelstock__params[[1]] + modelstock__params[[2]] * N - I)**2),
        list(N = N, I = I))
    return(out)
}
g3l_distribution_surveyindices_log <- function (alpha = NULL, beta = 1) g3l_distribution_surveyindices('log', alpha, beta)
g3l_distribution_surveyindices_linear <- function (alpha = NULL, beta = 1) g3l_distribution_surveyindices('linear', alpha, beta)

# i.e. catchinkilo's sumofsquares
g3l_distribution_sumofsquaredlogratios <- function (epsilon = 10) {
    f_substitute(quote(
        sum((
            log(stock_ss(obsstock__x) + epsilon) -
            log(stock_ss(modelstock__x) + epsilon)
        ) ** 2)
    ), list(
        epsilon = epsilon))
}

g3_distribution_preview <- function (
        obs_data,
        fleets = list(),
        stocks = list(),
        area_group = NULL) {
    ld <- g3l_likelihood_data(
        'preview',
        obs_data,
        missing_val = NA,
        all_stocks = stocks,
        all_fleets = fleets,
        area_group = area_group,
        model_history = "" )
    if (!is.null(ld$number)) return(ld$number)
    if (!is.null(ld$weight)) return(ld$weight)
    stop('obs_data should contain either a number column or weight column')
}

# Compare model state to observation data
# - obs_data: Real-world observations. data.frame containing colums:
#    - year
#    - (optional) step (otherwise data summed over the current year)
#    - (optional) area (otherwise all areas summed)
#    - (optional) age (otherwise all ages summed)
#    - length
#    - number - expected number for given combination (optional, but must provide one)
#    - weight - expected total biomass for given combination (optional, but must provide one)
# - fleets: Gather catch from these fleets (or abundance, if no fleets)
# - stocks: Gather catch/abundance from these stocks
# - function_f: Comparison function to compare stock_ss(modelstock__x) & stock_ss(obsstock__x)
# - weight: Weighting of parameter in final nll
g3l_distribution <- function (
        nll_name,
        obs_data,
        fleets = list(),
        stocks,
        function_f,
        transform_fs = list(),
        missing_val = 0,
        area_group = NULL,
        report = FALSE,
        nll_breakdown = FALSE,
        weight = substitute(
            g3_param(n, optimise = FALSE, value = 1),
            list(n = paste0(nll_name, "_weight"))),
        run_at = g3_action_order$likelihood) {
    stopifnot(is.character(nll_name) && length(nll_name) == 1)
    stopifnot(is.data.frame(obs_data))
    stopifnot(is.list(fleets) && all(sapply(fleets, g3_is_stock)))
    stopifnot(is.list(stocks) && all(sapply(stocks, g3_is_stock)))
    stopifnot(rlang::is_formula(function_f))
    stopifnot(is.list(transform_fs) && all(sapply(transform_fs, rlang::is_formula)))

    if ("modelstock__time_idx" %in% all.vars(function_f)) {
        # g3l_distribution_surveyindices needs to generate time vectors, so needs time early in dimension list
        model_history <- 'early'
    } else if (report) {
        model_history <- 'late'
    } else {
        model_history <- ''
    }

    # Replace any __x vars in (f) with (repl_postfix), e.g. "num"
    generic_var_replace <- function (f, repl_postfix) {
        GENERIC_VAR_RE <- '__x(_pretransform)?$'

        # Find all vars ending in __x
        replace_vars <- all.vars(f)
        replace_vars <- replace_vars[grepl(GENERIC_VAR_RE, replace_vars)]

        # Turn into a list of __x = __postfix
        names(replace_vars) <- replace_vars
        replace_vars <- gsub(GENERIC_VAR_RE, paste0("__", repl_postfix, "\\1"), replace_vars)

        # Apply to formula
        out_f <- f_substitute(f, lapply(replace_vars, as.symbol))

        # step.R isn't doing environment for us, so do it manually
        for (real_name in replace_vars) {
            # Work backwards to guess the actual var name
            if (grepl("^nll_.dist_", real_name)) {
                var_name <- gsub("^.*__", "nllstock__", real_name)
            } else if (grepl("^.dist_.*_model__", real_name)) {
                var_name <- gsub("^.*__", "modelstock__", real_name)
            } else if (grepl("^.dist_.*_obs__", real_name)) {
                var_name <- gsub("^.*__", "obsstock__", real_name)
            } else {
                next
            }
            assign(real_name, get(var_name), envir = environment(out_f))
        }

        return(out_f)
    }

    out <- new.env(parent = emptyenv())

    # Find name of function user called, error if it was catchdistribution but with missing fleets
    this_name <- as.character(sys.call()[[1]])
    if (this_name == "g3l_catchdistribution" && length(fleets) == 0) stop("Fleets must be supplied for g3l_catchdistribution")
    if (this_name == "g3l_abundancedistribution" && length(fleets) > 0) stop("Fleets must not be supplied for g3l_abundancedistribution")

    # Find name of function user called, and g3l_substitution function used
    function_f_name <- if (is.call(substitute(function_f))) as.character(substitute(function_f)[[1]]) else "custom"
    function_f_name <- gsub("^g3l_distribution_", "", function_f_name)

    # Add our called name / function name to labels & nll_name
    prefix <- paste0(this_name, "_", function_f_name, ": ")
    nll_name <- paste(
        if (length(fleets) > 0) 'cdist' else 'adist',
        function_f_name,
        nll_name,
        sep = "_")

    # Convert data to stocks
    ld <- g3l_likelihood_data(nll_name, obs_data, missing_val = missing_val, area_group = area_group, model_history = model_history, all_stocks = stocks, all_fleets = fleets)
    modelstock <- ld$modelstock
    obsstock <- ld$obsstock
    if (!is.null(ld$number)) {
        modelstock__num <- g3_stock_instance(modelstock, 0)
        obsstock__num <- g3_stock_instance(obsstock, ld$number)
    }
    if (!is.null(ld$weight)) {
        modelstock__wgt <- g3_stock_instance(modelstock, 0)
        obsstock__wgt <- g3_stock_instance(obsstock, ld$weight)
    }

    # If no fleets, set predstock = NULL, otherwise iterate over fleets
    for (predstock in (if (length(fleets) > 0) fleets else list(NULL))) for (prey_stock in stocks) {
        stock <- prey_stock  # Alias stock == prey_stock

        # NB: In lockstep with action_predate()
        predprey <- g3s_stockproduct(prey_stock, pred = predstock)
        predprey__cons <- g3_stock_instance(predprey, desc = paste0("Total biomass consumption of ", predprey$name))

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

        # Work out fleet index for obs/model variables
        if (!is.null(ld$fleet_map)) {
            # Skip over fleets not part of the observation data, map to an index
            # NB: This is what stock_iterate() would do for us
            if (is.null(ld$fleet_map[[predstock$name]])) next
            fleetidx_f <- f_substitute(~g3_idx(x), list(x = ld$fleet_map[[predstock$name]]))
        } else {
            # Not using fleet grouping, fleetidx_f not used
            fleetidx_f <- ~-1
        }

        # Inner formula to do collection
        if (is.null(predstock)) {
            # No fleet -> Compare to abundance
            collect_f <- f_substitute(~{
                if (compare_num) {
                    debug_trace("Add ", prey_stock, " individuals to our count")
                    stock_ss(modelstock__num) <- stock_ss(modelstock__num) + stock_reshape(modelstock,
                        transform_f * tform_stock_ss(prey_stock__num))
                }
                if (compare_wgt) {
                    debug_trace("Take ", prey_stock, " total biomass to our count")
                    stock_ss(modelstock__wgt) <- stock_ss(modelstock__wgt) + stock_reshape(modelstock,
                        transform_f * tform_stock_ss(prey_stock__num) * tform_stock_ss(prey_stock__wgt))
                }
            }, list(
                compare_num = !is.null(ld$number),
                compare_wgt = !is.null(ld$weight)))
        } else {
            collect_f <- f_substitute(~{
                if (compare_num) {
                    debug_trace("Take predprey__cons weight, convert to individuals, add to our count")
                    stock_ss(modelstock__num) <- stock_ss(modelstock__num) + stock_reshape(modelstock,
                        transform_f * tform_stock_ss(predprey__cons) / avoid_zero_vec(tform_stock_ss(prey_stock__wgt)))
                }
                if (compare_wgt) {
                    debug_trace("Take predprey__cons weight, add to our count")
                    stock_ss(modelstock__wgt) <- stock_ss(modelstock__wgt) + stock_reshape(modelstock,
                        transform_f * tform_stock_ss(predprey__cons))
                }
            }, list(
                compare_num = !is.null(ld$number),
                compare_wgt = !is.null(ld$weight) ))
        }

        # Wrap with any iteration for transformations
        transform_f <- quote(1)
        tform_ss <- list()
        for (tf_index in seq_along(transform_fs)) {
            tf_name <- names(transform_fs)[[tf_index]]
            transform_f <- f_substitute(quote(transform_f * extra_tf), list(
                transform_f = transform_f,
                extra_tf = transform_fs[[tf_index]]))

            if (tf_name == 'age') {
                # NB: This is nearly interact$age, but not quite. We need a different _idx name
                collect_f <- f_substitute(quote({
                    # NB: By setting the iterator to preage, we may remove mentions of __age_idx,
                    #     which will cause stock_iterate() to not loop over age. Force this by
                    #     explicitly mentioning the index.
                    comment(prey_stock__age_idx)
                    for (preage in seq(prey_stock__minage, prey_stock__maxage, by = 1)) g3_with(
                        prey_stock__preage_idx := g3_idx(preage - prey_stock__minage + 1L),
                        collect_f)
                }), list(collect_f = collect_f))
                # Add extra subset for injection below
                tform_ss$age <- quote( stock__preage_idx )
            } else {
                stop("Transforms for dimensions other than age not supported yet")
            }
        }

        # Replace tform_stock_ss, adding in the transform subsets required
        collect_f <- call_replace(collect_f, tform_stock_ss = function (x) {
            x[[1]] <- as.symbol("stock_ss")
            x[names(tform_ss)] <- tform_ss
            return(x)
        })
        # Add in transform_f now we're done adding bits to it.
        collect_f <- f_substitute(collect_f, list(transform_f = transform_f))

        # Finally iterate/intersect over stock in question
        out[[step_id(run_at, 'g3l_distribution', nll_name, 1, predstock, prey_stock)]] <- f_optimize(f_substitute(~{
            if (compare_fleet) {
                debug_label(prefix, "Collect catch from ", predstock, "/", prey_stock, " for ", nll_name)
                stock_iterate(prey_stock, stock_interact(predstock, stock_with(predprey, stock_intersect(modelstock, collect_f)), prefix = "predator"))
            } else {
                debug_label(prefix, "Collect abundance from ", stock, " for ", nll_name)
                stock_iterate(prey_stock, stock_intersect(modelstock, collect_f))
            }
        }, list(compare_fleet = !is.null(predstock), collect_f = collect_f)))

        # Fix-up stock intersection, add in stockidx_f
        out[[step_id(run_at, 'g3l_distribution', nll_name, 1, predstock, prey_stock)]] <- f_substitute(out[[step_id(run_at, 'g3l_distribution', nll_name, 1, predstock, prey_stock)]], list(
            fleetidx_f = fleetidx_f,
            stockidx_f = stockidx_f))
        out[[step_id(run_at, 'g3l_distribution', nll_name, 1, predstock, prey_stock)]] <- g3_step(out[[step_id(run_at, 'g3l_distribution', nll_name, 1, predstock, prey_stock)]])
    }

    nllstock <- g3_storage(paste("nll", nll_name, sep = "_"))
    if (nll_breakdown) nllstock <- g3s_modeltime(nllstock)
    if (!is.null(ld$number)) nllstock__num <- g3_stock_instance(nllstock, 0)
    if (!is.null(ld$weight)) nllstock__wgt <- g3_stock_instance(nllstock, 0)
    nllstock__weight <- g3_stock_instance(nllstock, 0)
    nll <- 0.0

    # Generic comparison step with __x instead of __num or __wgt
    compare_f <- g3_step(f_substitute(~{
        debug_label(prefix, "Compare ", modelstock, " to ", obsstock)
        if (done_aggregating_f) {
            stock_iterate(modelstock, stock_intersect(obsstock, stock_intersect(nllstock, if (function_comare_f) g3_with(
                cur_cdist_nll := function_f, {
                nll <- nll + (weight) * cur_cdist_nll
                stock_ss(nllstock__x) <- stock_ss(nllstock__x) + cur_cdist_nll
                stock_ss(nllstock__weight) <- weight
            }))))
            if (!report) {
                debug_trace("Zero counters for next reporting period")
                stock_with(modelstock, modelstock__x[] <- 0)
            }
        }
    }, list(
        done_aggregating_f = ld$done_aggregating_f,
        function_comare_f = if (is.null(attr(function_f, 'do_compare_f'))) TRUE else attr(function_f, 'do_compare_f'),
        function_f = function_f,
        report = report,
        weight = weight)))
    compare_f <- f_substitute(compare_f, list(stockidx_f = as.symbol(paste0(modelstock$name, "__stock_idx"))))

    if (!is.null(ld$number)) {
        out[[step_id(run_at, 'g3l_distribution', nll_name, 3, 'num')]] <-
            generic_var_replace(compare_f, 'num')
    }

    if (!is.null(ld$weight)) {
        out[[step_id(run_at, 'g3l_distribution', nll_name, 3, 'wgt')]] <-
            generic_var_replace(compare_f, 'wgt')
    }

    if (!report) return(as.list(out))

    return(c(as.list(out),
        if (!('modelstock__params' %in% names(environment(function_f)))) NULL else g3a_report_var(
            "modelstock__params",
            environment(function_f)$modelstock__params,
            stock = modelstock,
            out_prefix = NULL ),
        if (is.null(ld$number)) NULL else g3a_report_var(
            "obsstock__num",
            obsstock__num,
            stock = obsstock,
            out_prefix = NULL ),
        if (is.null(ld$number)) NULL else g3a_report_var(
            "modelstock__num",
            modelstock__num,
            stock = modelstock,
            out_prefix = NULL ),
        if (is.null(ld$number)) NULL else g3a_report_var(
            "nllstock__num",
            nllstock__num,
            stock = nllstock,
            out_prefix = NULL ),
        if (is.null(ld$weight)) NULL else g3a_report_var(
            "obsstock__wgt",
            obsstock__wgt,
            stock = obsstock,
            out_prefix = NULL ),
        if (is.null(ld$weight)) NULL else g3a_report_var(
            "modelstock__wgt",
            modelstock__wgt,
            stock = modelstock,
            out_prefix = NULL ),
        if (is.null(ld$weight)) NULL else g3a_report_var(
            "nllstock__wgt",
            nllstock__wgt,
            stock = nllstock,
            out_prefix = NULL ),
        g3a_report_var(
            "nllstock__weight",
            nllstock__weight,
            stock = nllstock,
            out_prefix = NULL ),
        NULL))
}
g3l_catchdistribution <- g3l_distribution
g3l_abundancedistribution <- g3l_distribution
