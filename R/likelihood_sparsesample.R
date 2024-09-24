# df-like storage for table entries
g3s_sparsedata <- function(var_name, in_df, area_group = NULL) {
    stopifnot(is.character(var_name))

    iterate <- quote( extension_point )
    env <- as.environment(list())
    env$stock__cols <- c()

    for (n in names(in_df)) {
        if (n %in% c("year", "step")) next

        if (n == "area" && !is.null(area_group)) {
            # Use area_group to resolve names of items
            env[[paste0("stock__", n)]] <- area_group[in_df[[n]]]
            names(env[[paste0("stock__", n)]]) <- in_df[[n]]
        } else if (n == "length") {
            env[[paste0("stock__", n)]] <- as.numeric(in_df[[n]])
        } else {
            env[[paste0("stock__", n)]] <- as.integer(in_df[[n]])
        }

        iterate <- substitute(
            g3_with(n := stock__n[[stock__row_idx]], extension_point), list(
                n = as.symbol(n),
                stock__n = as.symbol(paste0("stock__", n)),
                extension_point = iterate ))
        env$stock__cols <- c(env$stock__cols, n)
    }

    if ("step" %in% names(in_df)) {
        iterate <- substitute({ if (stock__step[[stock__row_idx]] != cur_step) next; extension_point }, list(extension_point = iterate))
        env$stock__step <- as.integer(in_df$step)
    }

    if ("year" %in% names(in_df)) {
        in_df <- in_df[order(in_df$year),,drop = F]  # Data has to be sorted by year for this to work
        iterate <- substitute({
            if (stock__year[[stock__row_idx]] < cur_year) next
            if (stock__year[[stock__row_idx]] > cur_year) break
            extension_point
        }, list(extension_point = iterate))
        env$stock__year <- as.integer(in_df$year)
    } else {
        stop("Data must at least contain a 'year' column")
    }

    iterate <- substitute( for (stock__row_idx in seq_along(stock__year)) extension_point , list(extension_point = iterate))

    structure(list(
        dim = list(row = nrow(in_df)),
        dimnames = list(row = NULL),
        iterate = list(row = iterate),
        iter_ss = list(row = as.symbol("stock__row_idx")),
        intersect = list(row = quote(stop("stock_intersect not supported for g3_sparsedata"))),
        interact = list(row = quote(stop("stock_interact not supported for g3_sparsedata"))),
        with = list(row = quote(extension_point) ),
        env = env,
        name_parts = var_name,
        name = paste(var_name, collapse = "_") ), class = c("g3_sparsedata", "g3_stock", "list"))
}

g3_sparsedata_instance <- function (stock, init_value = NA, desc = "") {
    stopifnot(inherits(stock, "g3_sparsedata"))

    if (length(init_value) == 1) {
        x <- as_force_vector(rep(init_value, stock$dim$row))
    } else if (length(init_value) == stock$dim$row) {
        x <- as_force_vector(init_value)
    } else {
        stop("Length of init_value ", length(init_value), " doesn't match stock rows ", stock$dim$row)
    }

    if (nzchar(desc)) {
        attr(x, 'desc') <- desc
    }
    return(x)
}

g3l_sparsesample_linreg <- function (
        fit = c('log', 'linear'),
        slope = 1,
        intercept = NULL ) {
    fit <- match.arg(fit)

    N <- if(fit == 'log') ~log(avoid_zero_vec(nllstock__model_sum/nllstock__model_n)) else ~nllstock__model_sum/nllstock__model_n
    I <- if(fit == 'log') ~log(avoid_zero_vec(nllstock__obs_mean)) else ~nllstock__obs_mean

    nllstock__nll <- c(nll = 0.0, intercept = 0.0, slope = 0.0)
    regression_linear <- regression_linear  # Make sure regression_linear is in environment
    return(f_substitute(
        ~(nllstock__nll[] <- regression_linear(N, I, intercept_f, slope_f))[[1]],
        list(
            N = N,
            I = I,
            # NB: Can't use NULL in C++, Use NaN instead
            intercept_f = if (is.null(intercept)) NaN else intercept,
            slope_f = if (is.null(slope)) NaN else slope)) )
}

g3l_sparsesample_sumsquares <- function (
        weighting = "model_stddev" ) {
    if (identical(weighting, "model_stddev")) {
        weighting_f <- quote( 1 / (nllstock__model_sqsum/nllstock__model_n - (nllstock__model_sum/nllstock__model_n)**2) )
    } else if (identical(weighting, "obs_stddev")) {
        weighting_f <- quote( 1 / nllstock__obs_stddev**2 )
    } else {
        weighting_f <- weighting
    }

    # Sumofsquares, picking out NaN values (i.e. projections)
    g3l_sparsesample_sumsquares_stddev <- g3_native(r = function(model_mean, obs_mean, weighting) {
        finites <- is.finite(model_mean) & is.finite(obs_mean)

        if (any(!is.finite(weighting[finites]))) warning("g3l_sparsesample_sumsquares_stddev: 1/variance is NaN, use a custom weighing")
        return( ifelse(finites, weighting, 0) * (ifelse(finites, model_mean, 0) - ifelse(finites, obs_mean, 0))^2 )
    }, cpp = '[](vector<Type> model_mean, vector<Type> obs_mean, vector<Type> weighting) -> vector<Type> {
        Eigen::Matrix<bool, Eigen::Dynamic, 1> finites = model_mean.isFinite() && obs_mean.isFinite() && weighting.isFinite();

        if ((!weighting.isFinite()).any()) Rf_warning("g3l_sparsesample_sumsquares_stddev: 1/variance is NaN, use a custom weighing");
        return(finites.select(weighting, 0) * (finites.select(model_mean, 1) - finites.select(obs_mean, 1)).pow(2) );
    }', depends = c('avoid_zero_vec'))

    # NB: We don't define nllstock__nll, so g3l_sparsesample will define a g3_stock_instance()
    return(f_substitute(
        ~sum(nllstock__nll[] <- g3l_sparsesample_sumsquares_stddev(
            nllstock__model_sum/nllstock__model_n,
            nllstock__obs_mean,
            weighting_f * nllstock__obs_n )),
        list(
            weighting_f = weighting_f,
            end = NULL )))
}

g3l_sparsesample <- function (
        nll_name,
        obs_df,
        stocks,
        measurement_f = quote( wgt ),
        function_f = g3l_sparsesample_linreg(),
        predstocks = list(),
        area_group = NULL,
        weight = g3_parameterized(paste0(nll_name, "_weight"),
            optimise = FALSE, value = 1),
        run_at = g3_action_order$likelihood ) {
    stopifnot(is.list(stocks) && all(sapply(stocks, g3_is_stock)))
    stopifnot(is.list(predstocks) && all(sapply(predstocks, g3_is_stock)))
    out <- new.env(parent = emptyenv())

    nllstock <- g3s_sparsedata(c("nll", type = if (length(predstocks) > 0 ) "spcatch" else "spabund", name = nll_name), obs_df[,-ncol(obs_df), drop = FALSE], area_group = area_group)
    nllstock__obs_mean <- g3_sparsedata_instance(nllstock, as.numeric(obs_df[, "mean"]), desc = paste0(nll_name, " observations"))
    nllstock__obs_stddev <- if ("stddev" %in% names(obs_df)) g3_sparsedata_instance(nllstock, as.numeric(obs_df[, "stddev"]), desc = paste0(nll_name, " observation stddev")) else quote( stop("No observation stddev column") )
    nllstock__obs_n <- g3_sparsedata_instance(nllstock, as.numeric(if ("number" %in% names(obs_df)) obs_df[, "number"] else 1), desc = paste0(nll_name, " observation number"))
    nllstock__model_sum <- g3_sparsedata_instance(nllstock, 0, desc = paste0(nll_name, " prediction total"))
    nllstock__model_sqsum <- g3_sparsedata_instance(nllstock, 0, desc = paste0(nll_name, " prediction squared-sum"))
    nllstock__model_n <- g3_sparsedata_instance(nllstock, 0, desc = paste0(nll_name, " datapoint count"))

    nllstock__weight <- 0

    g3_step_foreach_stock <- function (stocks, inner, outer_f, predstocks = list()) {
        if (length(predstocks) == 0) predstocks <- list(NULL)
        inner_f <- f_concatenate(do.call(c, lapply(predstocks, function (predstock) lapply(stocks, function (stock) {
            f <- inner(stock, predstock)
            environment(f) <- rlang::env_clone(environment(f), parent = environment(outer_f))

            g3_step(f, recursing = TRUE)
        }))))
        outer_f <- g3_step(f_substitute(outer_f, list(inner_f = inner_f)))
        return(outer_f)
    }

    # For each stock, collect current values for table
    out[[step_id(run_at, 'g3l_sparsesample', nll_name, 1)]] <- g3_step_foreach_stock(stocks, predstocks = predstocks, function (stock, predstock) {
        # Find the "smallest" dimension that either the observations or measurement formula considers
        dims_measured <- intersect(
            union(g3_stock_def(nllstock, "cols"), all.vars(measurement_f)),
            names(stock$dim) )
        smallest_dim <- suppressWarnings(min(which(names(stock$dim) %in% dims_measured))) - 1
        if (smallest_dim == 0) {
            # Considering full stock breakdown
            vec_sym <- as.symbol("single")
            meas_sum <- as.symbol("(")  # Nothing to sum, replace with bracket )
        } else if (is.infinite(smallest_dim)) {
            # Aggregating everything for this timestep
            vec_sym <- as.symbol("full")
            meas_sum <- quote(sum)
        } else {
            # Considering up to age/length/whatever
            vec_sym <- as.symbol(names(stock$dim)[[smallest_dim]])
            meas_sum <- quote(sum)
        }

        # Substitute terms in measurement_f
        st_measurement_f <- f_substitute(measurement_f, list(
            wgt = substitute(stock_ss(stock__wgt, vec = vec_sym) , list(vec_sym = vec_sym)),
            # NB: "length" will be the observed length, we should be using the midlen value
            length = quote( stock__midlen[[stock__length_idx]] ),
            end = NULL ))

        if (!is.null(predstock)) {
            # NB: In lockstep with action_predate()
            predprey <- g3s_stockproduct(stock, predator = predstock, ignore_dims = c('predator_area'))

            # Quantity is number of fish present in catch
            st_quantity_f <- substitute(
                stock_ss(predprey__cons, vec = vec_sym) / avoid_zero(stock_ss(stock__wgt, vec = vec_sym)),
                list(vec_sym = vec_sym) )
        } else {
            # Dummy predator to intersect with
            predstock <- g3_storage("NULL")
            predprey <- g3_storage("NULL")

            # Multiply quantity by number present in this cell
            st_quantity_f <- substitute(
                stock_ss(stock__num, vec = vec_sym),
                list(vec_sym = vec_sym) )
        }

        # NB: Manually g3_with()ing to work around lost stock-variables, should always be inside intersect anyway
        f_substitute(~stock_intersect(predstock, stock_intersect(stock, stock_with(predprey, g3_with(
                measurement := st_measurement_f,
                quantity := st_quantity_f, {
            stock_ss(nllstock__model_sum, vec = single) <- stock_ss(nllstock__model_sum, vec = single) + meas_sum(measurement * quantity)
            stock_ss(nllstock__model_sqsum, vec = single) <- stock_ss(nllstock__model_sqsum, vec = single) + meas_sum((measurement)^2 * quantity)
            stock_ss(nllstock__model_n, vec = single) <- stock_ss(nllstock__model_n, vec = single) + meas_sum(quantity)
        })))), list(
            st_measurement_f = st_measurement_f,
            st_quantity_f = st_quantity_f,
            meas_sum = meas_sum ))
    }, outer_f = ~{
        debug_label("Gather model predictions matching individuals in ", nllstock, "__obs")
        stock_iterate(nllstock, inner_f)
    })

    # If nll formula doesn't define __nll, generate a stock instance for it to use
    if (!exists("nllstock__nll", envir = environment(function_f))) {
        environment(function_f)$nllstock__nll <- g3_stock_instance(nllstock, NaN, desc = paste0("nll for ", nll_name, " at each step"))
    }

    # Finally, apply function_f to results
    out[[step_id(g3_action_order$initial, 'g3l_sparsesample', nll_name, 2)]] <- g3_step(f_substitute(~stock_with(nllstock, if (cur_time > total_steps) {
        debug_label("Add nll for ", nllstock, "__obs vs ", nllstock, "__model")
        nllstock__weight <- weight
        nll <- nll + nllstock__weight * (function_f)
        # TODO: Generate stddev ready for reporting?
    }), list(
        function_f = function_f,
        weight = weight )))

    return(as.list(out))
}
