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

g3_param_project_ar1 <- function (
        phi_f = g3_parameterized(
            "proj.ar1.phi",
            value = 0.8, lower = 0, upper = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        stddev_f = g3_parameterized(
            "proj.ar1.stddev",
            value = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        level_f = g3_parameterized(
            "proj.ar1.level",
            value = -1, optimise = FALSE,
            prepend_extra = quote(param_name) )) {
    g3_param_project_nll_ar1 <- g3_native(r = function (var, phi, stddev) {
        noisemean <- 0
        noisestddev <- stddev

        # If noise disabled, just set nll 0
        if (noisestddev == 0) return(rep(0, length(var)))

        lastvar <- head(var, -1)
        curvar <- tail(var, -1)
        c(0, -dnorm(
            curvar - phi * lastvar,
            noisemean,
            noisestddev,
            log = TRUE ))
    }, cpp = '[](array<Type> var, Type phi, Type stddev) -> vector<Type> {
        Type noisemean = 0;
        Type noisestddev = stddev;

        // If noise disabled, just set nll 0
        if (noisestddev == 0) {
            vector<Type> out(var.size());
            out.setConstant(0);
            return out;
        }

        array<Type> lastvar(var.size() - 1);
        array<Type> curvar(var.size() - 1);
        array<Type> nll(var.size());
        lastvar = var.head(var.size() - 1);
        curvar = var.tail(var.size() - 1);
        nll(0) = 0;
        nll.tail(nll.size() - 1) = -dnorm(
            (vector<Type>)(curvar - phi * lastvar),
            noisemean,
            noisestddev,
            1 );
        return(nll);
    }')
    g3_param_project_ar1 <- g3_native(r = function (var, phi, stddev, level) {
        if (all(is.finite(var))) return(var)
        noisemean <- 0
        noisestddev <- stddev

        lastvar <- 0
        for (i in seq_along(var)) {
            if (!is.finite(var[[i]])) {  # Ignore non-projection values
                if (level < 0) {
                    # level needs setting from previous values, if not enough assume 0
                    lastx <- -round(level)
                    level <- if ((i - lastx) >= 1) mean(var[(i - lastx):(i - 1)]) else 0
                }
                var[[i]] <- phi * lastvar + (1 - phi) * level + rnorm(1, noisemean, noisestddev)
            }
            lastvar <- var[[i]]
        }
        return(var)
    }, cpp = '[](array<Type> var, Type phi, Type stddev, Type level) -> vector<Type> {
        if (var.allFinite()) return var;
        Type noisemean = 0;
        Type noisestddev = stddev;

        Type lastvar = 0;
        for (int i = 0; i < var.size(); i++) {
            if (!var.segment(i, 1).allFinite()) {  // Ignore non-projection values
                if (level < 0) {
                    // level needs setting from previous values, if not enough assume 0
                    int lastx = -std::round(asDouble(level));
                    level = (i - lastx) >= 0 ? var.segment(i - lastx, lastx).mean() : 0;
                }
                var(i) = (Type)(phi * lastvar + (1 - phi) * level + rnorm(1, noisemean, noisestddev)(0));
            }
            lastvar = var(i);
        }
        return (var);
    }')

    list(
        name = "ar1",
        # NB: We don't define projstock__nll, so g3_param_project will define a g3_stock_instance()
        nll = f_substitute(
            ~sum(projstock__nll[] <- g3_param_project_nll_ar1(projstock__var, phi_f, stddev_f)),
            list(phi_f = phi_f, stddev_f = stddev_f) ),
        project = f_substitute(
            ~g3_param_project_ar1(projstock__var, phi_f, stddev_f, level_f),
            list(phi_f = phi_f, stddev_f = stddev_f, level_f = level_f) ))
}

g3_param_project_logar1 <- function (
        logphi_f = g3_parameterized(
            "proj.logar1.logphi",
            value = 0.8, lower = 0, upper = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        lstddev_f = g3_parameterized(
            "proj.logar1.lstddev",
            value = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        loglevel_f = g3_parameterized(
            "proj.logar1.loglevel",
            value = -1, optimise = FALSE,
            prepend_extra = quote(param_name) )) {
    g3_param_project_nll_logar1 <- g3_native(r = function (var, logphi, lstddev) {
        logvar <- log(var)
        noisemean <- 0 - exp(2*lstddev) / 2
        noisestddev <- exp(lstddev)

        # If noise disabled, just set nll 0
        if (noisestddev < 1e-7) return(rep(0, length(logvar)))

        lastlogvar <- head(logvar, -1)
        curlogvar <- tail(logvar, -1)
        c(0, -dnorm(
            curlogvar - logphi * lastlogvar,
            noisemean,
            noisestddev,
            log = TRUE ))
    }, cpp = '[](array<Type> var, Type logphi, Type lstddev) -> vector<Type> {
        array<Type> logvar(var.size());
        logvar = var.log();
        Type noisemean = 0 - exp(2*lstddev) / 2;
        Type noisestddev = exp(lstddev);

        // If noise disabled, just set nll 0
        if (noisestddev < 1e-7) {
            vector<Type> out(logvar.size());
            out.setConstant(0);
            return out;
        }

        array<Type> lastlogvar(logvar.size() - 1);
        array<Type> curlogvar(logvar.size() - 1);
        array<Type> nll(logvar.size());
        lastlogvar = logvar.head(logvar.size() - 1);
        curlogvar = logvar.tail(logvar.size() - 1);
        nll(0) = 0;
        nll.tail(nll.size() - 1) = -dnorm(
            (vector<Type>)(curlogvar - logphi * lastlogvar),
            noisemean,
            noisestddev,
            1 );
        return(nll);
    }')
    g3_param_project_logar1 <- g3_native(r = function (var, logphi, lstddev, loglevel) {
        if (all(is.finite(var))) return(var)
        logvar <- log(var)
        noisemean <- 0 - exp(2*lstddev) / 2
        noisestddev <- exp(lstddev)

        lastlogvar <- 0
        for (i in seq_along(logvar)) {
            if (!is.finite(logvar[[i]])) {  # Ignore non-projection values
                if (loglevel < 0) {
                    # Loglevel needs setting from previous values, if not enough assume 0
                    lastx <- -round(loglevel)
                    loglevel <- if ((i - lastx) >= 1) mean(logvar[(i - lastx):(i - 1)]) else 0
                }
                logvar[[i]] <- logphi * lastlogvar + (1 - logphi) * loglevel + rnorm(1, noisemean, noisestddev)
            }
            lastlogvar <- logvar[[i]]
        }
        return(exp(logvar))
    }, cpp = '[](array<Type> var, Type logphi, Type lstddev, Type loglevel) -> vector<Type> {
        if (var.allFinite()) return var;
        array<Type> logvar(var.size());
        logvar = var.log();
        Type noisemean = 0 - exp(2*lstddev) / 2;
        Type noisestddev = exp(lstddev);

        Type lastlogvar = 0;
        for (int i = 0; i < logvar.size(); i++) {
            if (!logvar.segment(i, 1).allFinite()) {  // Ignore non-projection values
                if (loglevel < 0) {
                    // Loglevel needs setting from previous values, if not enough assume 0
                    int lastx = -std::round(asDouble(loglevel));
                    loglevel = (i - lastx) >= 0 ? logvar.segment(i - lastx, lastx).mean() : 0;
                }
                logvar(i) = (Type)(logphi * lastlogvar + (1 - logphi) * loglevel + rnorm(1, noisemean, noisestddev)(0));
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
            ~g3_param_project_logar1(projstock__var, logphi_f, lstddev_f, loglevel_f),
            list(logphi_f = logphi_f, lstddev_f = lstddev_f, loglevel_f = loglevel_f) ))
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
                if (weight > 0) nll <- nll + weight * nll_f
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
