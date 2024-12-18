g3_param_project_dlnorm <- function (
        lmean_f = g3_parameterized("proj.dlnorm.lmean",
            value = 0, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        lstddev_f = g3_parameterized("proj.dlnorm.lstddev",
            value = 1e5, optimise = FALSE,
            prepend_extra = quote(param_name) )) {
    # https://eigen.tuxfamily.org/dox/group__TutorialSlicingIndexing.html
    # lmean_f = log(mean(var))
    # lstddev_f = log(sd(log(var)))

    # NB: Only real purpose is to cast the var to .vec()
    g3_param_project_nll_dlnorm <- g3_native(r = function (var, lmean, lstddev) {
        return(-dnorm(log(var), mean = lmean - exp(2*lstddev) / 2, sd = exp(lstddev), log = TRUE))
    }, cpp = '[](array<Type> var, Type lmean, Type lstddev) -> vector<Type> {
        return(-dnorm((vector<Type>)(var.vec().log()), lmean - exp(2*lstddev) / 2, exp(lstddev), 1));
    }')
    g3_param_project_dlnorm <- g3_native(r = function (var, lmean, lstddev) {
        count <- length(var[!is.finite(var)])

        var[!is.finite(var)] <- exp(rnorm(count, mean = lmean - exp(2*lstddev) / 2, sd = exp(lstddev)))
        return(var)
    }, cpp = '[](array<Type> var, Type lmean, Type lstddev) {
        int count = var.size() - var.isFinite().count();

        vector<Type> rn = exp(rnorm(count, lmean - exp(2*lstddev) / 2, exp(lstddev)));
        var.tail(count) = rn;

        return var;
    }')

    list(
        name = "dlnorm",
        # NB: We don't define projstock__nll, so g3_param_project will define a g3_stock_instance()
        nll = f_substitute(
            ~sum(projstock__nll[] <- g3_param_project_nll_dlnorm(projstock__var, lmean_f, lstddev_f)),
            list(lmean_f = lmean_f, lstddev_f = lstddev_f) ),
        project = f_substitute(
            ~g3_param_project_dlnorm(projstock__var, lmean_f, lstddev_f),
            list(lmean_f = lmean_f, lstddev_f = lstddev_f) ))
}

g3_param_project_dnorm <- function (
        mean_f = g3_parameterized("proj.dnorm.mean",
            value = 0, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        stddev_f = g3_parameterized("proj.dnorm.stddev",
            value = 1, optimise = FALSE,
            prepend_extra = quote(param_name) )) {
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
        name = "dnorm",
        # NB: We don't define projstock__nll, so g3_param_project will define a g3_stock_instance()
        nll = f_substitute(
            ~sum(projstock__nll[] <- g3_param_project_nll_dnorm(projstock__var, mean_f, stddev_f)),
            list(mean_f = mean_f, stddev_f = stddev_f) ),
        project = f_substitute(
            ~g3_param_project_dnorm(projstock__var, mean_f, stddev_f),
            list(mean_f = mean_f, stddev_f = stddev_f) ))
}

g3_param_project_rwalk <- function (
        mean_f = g3_parameterized("proj.rwalk.mean",
            value = 0, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        stddev_f = g3_parameterized("proj.rwalk.stddev",
            value = 1, optimise = FALSE,
            prepend_extra = quote(param_name) )) {
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
        name = "rwalk",
        # NB: We don't define projstock__nll, so g3_param_project will define a g3_stock_instance()
        nll = f_substitute(
            ~sum(projstock__nll[] <- g3_param_project_nll_rwalk(projstock__var, mean_f, stddev_f)),
            list(mean_f = mean_f, stddev_f = stddev_f) ),
        project = f_substitute(
            ~g3_param_project_rwalk(projstock__var, mean_f, stddev_f),
            list(mean_f = mean_f, stddev_f = stddev_f) ))
}

g3_param_project_logar1 <- function (
        logphi_f = g3_parameterized(
            "proj.logar1.logphi",
            value = 0.8, lower = 0, upper = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        lstddev_f = g3_parameterized(
            "proj.logar1.lstddev",
            value = 1, optimise = FALSE,
            prepend_extra = quote(param_name) )) {
    g3_param_project_nll_logar1 <- g3_native(r = function (var, logphi, lstddev) {
        logvar <- log(var)
        noisemean <- 0 - exp(2*lstddev) / 2
        noisestddev <- exp(lstddev)

        laglogvar <- c(0, head(logvar, -1))  # i.e. vector with the previous entry to subtract
        return(-dnorm(
            logvar - logphi * laglogvar,
            noisemean,
            noisestddev,
            log = TRUE ))
    }, cpp = '[](array<Type> var, Type logphi, Type lstddev) -> vector<Type> {
        array<Type> logvar(var.size());
        logvar = var.log();
        Type noisemean = 0 - exp(2*lstddev) / 2;
        Type noisestddev = exp(lstddev);

        array<Type> laglogvar(logvar.size());
        laglogvar(0) = 0;
        laglogvar.tail(laglogvar.size() - 1) = logvar.head(logvar.size() - 1);
        return -dnorm(
            (vector<Type>)(logvar - logphi * laglogvar),
            noisemean,
            noisestddev,
            1 );
    }')
    g3_param_project_logar1 <- g3_native(r = function (var, logphi, lstddev) {
        if (all(is.finite(var))) return(var)
        logvar <- log(var)
        noisemean <- 0 - exp(2*lstddev) / 2
        noisestddev <- exp(lstddev)

        lastlogvar <- 0
        loglevel <- 0
        for (i in seq_along(logvar)) {
            if (is.finite(logvar[[i]])) {
                # Use final non-projection value as loglevel when available
                loglevel <- logvar[[i]]
            } else {
                logvar[[i]] <- logphi * (lastlogvar - loglevel) + rnorm(1, noisemean, noisestddev) + loglevel
            }
            lastlogvar <- logvar[[i]]
        }
        return(exp(logvar))
    }, cpp = '[](array<Type> var, Type logphi, Type lstddev) -> vector<Type> {
        if (var.allFinite()) return var;
        array<Type> logvar(var.size());
        logvar = var.log();
        Type noisemean = 0 - exp(2*lstddev) / 2;
        Type noisestddev = exp(lstddev);

        Type lastlogvar = 0;
        Type loglevel = 0;
        for (int i = 0; i < logvar.size(); i++) {
            if (logvar.segment(i, 1).allFinite()) {
                // Use final non-projection value as loglevel when available
                loglevel = logvar(i);
            } else {
                logvar(i) = (Type)(logphi * (lastlogvar - loglevel) + rnorm(1, noisemean, noisestddev)(0) + loglevel);
            }
            lastlogvar = logvar(i);
        }
        return (logvar).exp();
    }')

    list(
        name = "logar1",
        # NB: We don't define projstock__nll, so g3_param_project will define a g3_stock_instance()
        nll = f_substitute(
            ~sum(projstock__nll[] <- g3_param_project_nll_logar1(projstock__var, logphi_f, lstddev_f)),
            list(logphi_f = logphi_f, lstddev_f = lstddev_f) ),
        project = f_substitute(
            ~g3_param_project_logar1(projstock__var, logphi_f, lstddev_f),
            list(logphi_f = logphi_f, lstddev_f = lstddev_f) ))
}

g3_param_project <- function (
        param_name,
        project_fs = g3_param_project_rwalk(),
        by_step = TRUE,
        by_stock = FALSE,
        weight = g3_parameterized(
            paste("proj", project_fs$name, param_name, "weight", sep = "_"),
            optimise = FALSE, value = 1),
        scale = 1,
        offset = 0,
        random = TRUE ) {

    # Convert by_stock into a character vector
    if (isTRUE(by_stock)) {
        stop("by_stock = TRUE not supported for g3_param_project(), must be explicit stock objects")
    } else if (isFALSE(by_stock)) {
        by_stock <- c()
    } else if (g3_is_stock(by_stock)) {
        by_stock <- by_stock$name
    } else if (is.list(by_stock) && all(sapply(by_stock, g3_is_stock))) {
        by_stock <- stock_common_part(by_stock)
    } else {
        stop("Unknown by_stock argument. Should be a g3_stock or list of stocks")
    }
    param_name <- c(by_stock, param_name)

    # Turn character scale/offset into parameter code, prepending name
    if (is.character(scale)) scale <- g3_parameterized(c(param_name, scale), value = 1, optimise = FALSE)
    if (is.character(offset)) offset <- g3_parameterized(c(param_name, offset), optimise = FALSE)

    # Create projstock storage for projection values
    projstock <- g3_storage(c(
        "proj",
        project_fs$name,
        param_name ))
    projstock <- g3s_modeltime(projstock, by_year = isFALSE(by_step))

    param_tbl <- g3_parameterized(
        param_name,
        by_year = TRUE,
        by_step = isTRUE(by_step),
        random = random,
        ifmissing = NaN )
    projstock__var <- g3_stock_instance(projstock, NaN, desc = paste0("Projected values for ", projstock$name))

    out <- g3_step(f_substitute(~(
        stock_with(projstock, stock_ss(projstock__var, vec = single))
    ), list(
        end = NULL )), recursing = TRUE)

    # If nll formula doesn't define projstock__nll, generate a stock instance for it to use
    if (!exists("projstock__nll", envir = environment(project_fs$nll))) {
        environment(project_fs$nll)$projstock__nll <- g3_stock_instance(projstock, NaN, desc = paste0("nll for ", projstock$name, " deviants"))
    }

    environment(out)[[step_id(g3_action_order$initial, "g3a_project_param", param_name)]] <- g3_step(f_substitute(~{
        debug_label("g3_param_project: generate projections for ", projstock)
        stock_with(projstock, {
            if (cur_time > total_steps) {
                nll <- if (weight > 0) nll + weight * nll_f else 0
            } else if (cur_year_projection) {
                if (is.nan(stock_ss(projstock__var, vec = single))) {
                    projstock__var <- (projstock__var - offset) / scale  # Unapply scale/offset from below
                    projstock__var <- project_f * scale + offset
                }
            } else {
                stock_ss(projstock__var, vec = single) <- param_tbl * scale + offset
            }
        })
    }, list(
        # Add projstock_var_sym into the projection method
        project_f = project_fs$project,
        nll_f = project_fs$nll,
        param_tbl = param_tbl,
        weight = weight,
        scale = scale,
        offset = offset,
        end = NULL )))
    return(out)
}
