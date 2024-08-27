g3_param_project_dnorm <- function (
        mean_f = 0,
        stddev_f = 0 ) {
    # https://eigen.tuxfamily.org/dox/group__TutorialSlicingIndexing.html

    # NB: Only real purpose is to cast the var to .vec()
    g3_param_project_nll_dnorm <- g3_native(r = function (var, mean, stddev) {
        return(dnorm(var, mean = mean, sd = stddev))
    }, cpp = '[](array<Type> var, Type mean, Type stddev) -> vector<Type> {
        return(dnorm(var.vec(), mean, stddev, 0));
    }')
    g3_param_project_dnorm <- g3_native(r = function (var, mean, stddev) {
        var[!is.finite(var)] <- rnorm(length(var[!is.finite(var)]), mean = mean, sd = stddev)
        return(var)
    }, cpp = '[](array<Type> var, Type mean, Type stddev) {
        int count = var.size() - var.isFinite().count();

        vector<Type> rn = rnorm(count, mean, stddev);
        var.tail(count) = rn;

        return var;
    }')

    list(
        nll = f_substitute(
            ~sum(g3_param_project_nll_dnorm(proj_var, mean_f, stddev_f)),
            list(mean_f = mean_f, stddev_f = stddev_f) ),
        project = f_substitute(
            ~g3_param_project_dnorm(proj_var, mean_f, stddev_f),
            list(mean_f = mean_f, stddev_f = stddev_f) ))
}

g3_param_project_rwalk <- function (
        mean_f = 0,
        stddev_f = 0 ) {
    g3_param_project_nll_rwalk <- g3_native(r = function (var, mean, stddev) {
        d <- c(0, diff(var))
        return(dnorm(d, mean = mean, sd = stddev))
    }, cpp = '[](array<Type> var, Type mean, Type stddev) -> vector<Type> {
        array<Type> d(var.size());
        std::adjacent_difference(var.begin(), var.end(), d.begin());
        d(0) = 0;  // NB: Starting difference should be 0, not first value
        return(dnorm(d.vec(), mean, stddev, 0));
    }')
    g3_param_project_rwalk <- g3_native(r = function (var, mean, stddev) {
        var[!is.finite(var)] <- as.vector(tail(var[is.finite(var)], 1)) +
            cumsum(rnorm(length(var[!is.finite(var)]), mean = mean, sd = stddev))
        return(var)
    }, cpp = '[](array<Type> var, Type mean, Type stddev) -> array<Type> {
        int count = var.size() - var.isFinite().count();

        vector<Type> rn = rnorm(count, mean, stddev);
        std::partial_sum(rn.begin(), rn.end(), rn.begin());
        var.tail(count) = rn;

        return var;
    }')

    list(
        nll = f_substitute(
            ~sum(g3_param_project_nll_rwalk(proj_var, mean_f, stddev_f)),
            list(mean_f = mean_f, stddev_f = stddev_f) ),
        project = f_substitute(
            ~g3_param_project_rwalk(proj_var, mean_f, stddev_f),
            list(mean_f = mean_f, stddev_f = stddev_f) ))
}

g3_param_project <- function (
        param_name,
        project_fs = g3_param_project_rwalk(mean_f = 0, stddev_f = 1),
        # TODO: Can we actually by_stock this? Would need a new mechanism to prepend to param_name
        by_step = TRUE,
        weight = g3_parameterized(paste0(param_name, "_weight"),
            optimise = FALSE, value = 1),
        random = TRUE ) {
    projstock <- g3_storage(c('proj', 'dnorm'))
    projstock <- g3s_modeltime(projstock, by_year = isFALSE(by_step))

    param_tbl <- g3_parameterized(param_name, by_year = TRUE, by_step = isTRUE(by_step), random = TRUE, ifmissing = NaN)
    projstock_var_sym <- as.symbol(paste0("projstock__", param_name))
    # Add to local environment so ~ picks it up
    assign(as.character(projstock_var_sym), g3_stock_instance(projstock, NaN))

    out <- g3_step(f_substitute(~(
        stock_with(projstock, stock_ss(projstock_var_sym, vec = single))
    ), list(
        projstock_var_sym = projstock_var_sym,
        end = NULL )), recursing = TRUE)

    environment(out)[[step_id(g3_action_order$initial, "g3a_project_param", param_name)]] <- g3_step(f_substitute(~stock_with(projstock, {
        if (cur_time > total_steps) {
            # TODO: Separate nll report?
            nll <- nll + weight * nll_f
        } else if (cur_year_projection) {
            if (is.nan(stock_ss(projstock_var_sym, vec = single))) {
                projstock_var_sym <- project_f
            }
        } else {
            stock_ss(projstock_var_sym, vec = single) <- param_tbl
        }
    }), list(
        projstock_var_sym = projstock_var_sym,
        # Add projstock_var_sym into the projection method
        project_f = f_substitute(project_fs$project, list(proj_var = projstock_var_sym)),
        nll_f = f_substitute(project_fs$nll, list(proj_var = projstock_var_sym)),
        param_tbl = param_tbl,
        weight = weight,
        end = NULL )))
    return(out)
}
