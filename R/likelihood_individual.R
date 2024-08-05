# df-like storage for table entries
g3s_sparsedata <- function(var_name, in_df, area_group = NULL) {
    stopifnot(is.character(var_name))

    iterate <- quote( extension_point )
    env <- as.environment(list())

    for (n in names(in_df)) {
        if (n %in% c("year", "step")) next

        if (n == "area") {
            # TODO: Make sure area column is a factor with integer-area levels
        }

        iterate <- substitute(
            g3_with(n := stock__n[[stock__row_idx]], extension_point), list(
                n = as.symbol(n),
                stock__n = as.symbol(paste0("stock__", n)),
                extension_point = iterate ))
        env[[paste0("stock__", n)]] <- if (n == "length") in_df$length else as.integer(in_df[[n]])
    }

    if ("step" %in% names(in_df)) {
        iterate <- substitute({ if (stock__step[[stock__row_idx]] != cur_step) next; extension_point }, list(extension_point = iterate))
        env$stock__step <- as.integer(in_df$step)
    }

    if ("year" %in% names(in_df)) {
        # TODO: Check sorting, stop when over
        iterate <- substitute({ if (stock__year[[stock__row_idx]] != cur_year) next; extension_point }, list(extension_point = iterate))
        env$stock__year <- as.integer(in_df$year)
    } else {
        stop("Data must at least contain a 'year' column")
    }

    iterate <- substitute( for (stock__row_idx in seq_along(stock__year)) extension_point , list(extension_point = iterate))

    structure(list(
        dim = list(row = nrow(in_df)),
        dimnames = list(),
        iterate = list(row = iterate),
        iter_ss = list(row = as.symbol("stock__row_idx")),
        intersect = list(row = quote(stop("stock_intersect not supported for g3_sparsedata"))),
        interact = list(row = quote(stop("stock_interact not supported for g3_sparsedata"))),
        with = list(row = quote(extension_point) ),
        env = env,
        name_parts = var_name,
        name = paste(var_name, collapse = "_") ), class = c("g3_sparsedata", "g3_stock", "list"))
}

g3l_individual_linreg <- function (
        fit = c('log', 'linear'),
        slope = 1,
        intercept = NULL ) {
    fit <- match.arg(fit)

    N <- if(fit == 'log') ~log(avoid_zero_vec(nllstock__model/nllstock__model_n)) else ~nllstock__model/nllstock__model_n
    I <- if(fit == 'log') ~log(avoid_zero_vec(nllstock__obs)) else ~nllstock__obs

    nllstock__nll <- g3_global_formula(
        f_substitute(
            # NB: If we're not going to compare, don't bother recalculating linear regression
            ~stock_with(nllstock, if (cur_time != total_steps) nllstock__nll else regression_linear(N, I, intercept_f, slope_f)),
            list(
                N = N,
                I = I,
                # NB: Can't use NULL in C++, Use NaN instead
                intercept_f = if (is.null(intercept)) NaN else intercept,
                slope_f = if (is.null(slope)) NaN else slope)),
        init_val = as.array(c(nll = 0.0, intercept = 0.0, slope = 0.0)) )
    environment(nllstock__nll)$regression_linear <- regression_linear
    return(~nllstock__nll[[1]])
}

g3l_individual <- function (
        nll_name,  # TODO: Can we autogenerate?
        obs_df,
        stocks,
        transform_f = quote( stock_ss(stock__wgt, vec = single) ),
        function_f = g3l_individual_linreg(),
        area_group = NULL,
        weight = g3_parameterized(paste0(nll_name, "_weight"),
            optimise = FALSE, value = 1),
        run_at = g3_action_order$likelihood ) {
    stopifnot(is.list(stocks) && all(sapply(stocks, g3_is_stock)))
    out <- new.env(parent = emptyenv())

    nllstock <- g3s_sparsedata(c("nll", type = "ind", name = nll_name), obs_df[,-ncol(obs_df), drop = FALSE], area_group = area_group)
    nllstock__obs <- g3_stock_instance(nllstock, obs_df[,ncol(obs_df)], desc = paste0(nll_name, " observations"))
    nllstock__model <- g3_stock_instance(nllstock, 0, desc = paste0(nll_name, " predictions"))
    nllstock__model_n <- g3_stock_instance(nllstock, 0, desc = paste0(nll_name, " datapoint count"))

    nllstock__weight <- 0

    g3_step_foreach_stock <- function (stocks, inner_f, outer_f) {
        inner_f <- f_concatenate(lapply(stocks, function (stock) {
            environment(inner_f)$stock <- stock  # TODO: Eugh
            g3_step(inner_f, recursing = TRUE)
        }))
        outer_f <- g3_step(f_substitute(outer_f, list(inner_f = inner_f)))
        return(outer_f)
    }
    # For each stock, collect current values for table
    out[[step_id(run_at, 'g3l_individual', nll_name, 1)]] <- g3_step_foreach_stock(
        stocks,
        inner_f = f_substitute(~stock_intersect(stock, {
            stock_ss(nllstock__model, vec = single) <- stock_ss(nllstock__model, vec = single) + transform_f
            stock_ss(nllstock__model_n, vec = single) <- stock_ss(nllstock__model_n, vec = single) + 1
        }), list(transform_f = transform_f)),
        outer_f = ~{
            debug_label("Gather model predictions matching individuals in ", nllstock, "__obs")
            stock_iterate(nllstock, inner_f)
        })

    # Finally, apply function_f to results
    ind__nll <- function_f
    # TODO: Working around something broken in add_dependent_formula?
    environment(environment(ind__nll)$nllstock__nll)$nllstock__obs <- nllstock__obs
    environment(environment(ind__nll)$nllstock__nll)$nllstock__model <- nllstock__model
    environment(environment(ind__nll)$nllstock__nll)$nllstock__model_n <- nllstock__model_n
    out[[step_id(run_at, 'g3l_individual', nll_name, 2)]] <- g3_step(f_substitute(~stock_with(nllstock, {
        debug_label("Add nll for ", nllstock, "__obs vs ", nllstock, "__model")
        nllstock__weight <- weight
        nll <- nll + nllstock__weight * ind__nll
    }), list(
        weight = weight )))

    return(as.list(out))
}
