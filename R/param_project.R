g3_param_project_dlnorm <- function (
        lmean_f = g3_parameterized("proj.dlnorm.mean",
            value = 1e-5, optimise = FALSE, type = "LOG",
            prepend_extra = quote(param_name) ),
        lstddev_f = g3_parameterized("proj.dlnorm.stddev",
            value = 0.2, optimise = FALSE, type = "LOG",
            prepend_extra = quote(param_name) )) {
    # https://eigen.tuxfamily.org/dox/group__TutorialSlicingIndexing.html
    # lmean_f = log(mean(exp(lvar)))
    # lstddev_f = log(sd(lvar))

    # NB: Only real purpose is to cast the var to .vec()
    g3_param_project_nll_dlnorm <- g3_native(r = function (lvar, lmean, lstddev) {
        return(-dnorm(lvar, mean = lmean - exp(2*lstddev) / 2, sd = exp(lstddev), log = TRUE))
    }, cpp = '[](array<Type> lvar, Type lmean, Type lstddev) -> vector<Type> {
        return(-dnorm((vector<Type>)(lvar.vec()), lmean - exp(2*lstddev) / 2, exp(lstddev), 1));
    }')
    g3_param_project_dlnorm <- g3_native(r = function (lvar, lmean, lstddev) {
        count <- length(lvar[!is.finite(lvar)])

        lvar[!is.finite(lvar)] <- rnorm(count, mean = lmean - exp(2*lstddev) / 2, sd = exp(lstddev))
        return(lvar)
    }, cpp = '[](array<Type> lvar, Type lmean, Type lstddev) {
        int count = lvar.size() - lvar.isFinite().count();

        vector<Type> rn = rnorm(count, lmean - exp(2*lstddev) / 2, exp(lstddev));
        lvar.tail(count) = rn;

        return lvar;
    }')

    list(
        name = "dlnorm",
        # NB: We don't define projstock__nll, so g3_param_project will define a g3_stock_instance()
        nll = f_substitute(
            ~sum(projstock__nll[] <- g3_param_project_nll_dlnorm(projstock__lvar, lmean_f, lstddev_f)),
            list(lmean_f = lmean_f, lstddev_f = lstddev_f) ),
        project = f_substitute(
            ~g3_param_project_dlnorm(projstock__lvar, lmean_f, lstddev_f),
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
        return(-dnorm(var, mean = mean, sd = stddev, log = TRUE))
    }, cpp = '[](array<Type> var, Type mean, Type stddev) -> vector<Type> {
        return(-dnorm(var.vec(), mean, stddev, 1));
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
        return(-dnorm(d, mean = mean, sd = stddev, log = TRUE))
    }, cpp = '[](array<Type> var, Type mean, Type stddev) -> vector<Type> {
        array<Type> d(var.size());
        std::adjacent_difference(var.begin(), var.end(), d.begin());
        d(0) = 0;  // NB: Starting difference should be 0, not first value
        return(-dnorm(d.vec(), mean, stddev, 1));
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
            value = 0,
            prepend_extra = quote(param_name) ),
        lastx_f = 0L ) {
    if (!identical(lastx_f, 0L)) level_f <- NaN  # Disable level parameter when lastx enabled

    g3_param_project_nll_ar1 <- g3_native(r = function (var, phi, stddev, level, lastx) {
        noisemean <- 0
        noisestddev <- stddev

        if (lastx > 0 && is.nan(level)) {
            # level needs setting from previous values, if not enough assume 0
            i <- length(var)  # NB: This should be the first projected value, but optimising a projection isnt a likely thing to want to do
            level <- if ((i - lastx) >= 1) mean(var[(i - lastx):(i - 1)]) else 0
        }

        lastvar <- head(var, -1)
        curvar <- tail(var, -1)
        c(0, -dnorm(
            # i.e. only try to account for level when it's not derived from previous values
            curvar - phi * lastvar - (1 - phi) * max(level, 0),
            noisemean,
            # noisestddev needs to be > 0, or nll is inf. Optimiser will need a curve of some kind to work with
            max(noisestddev, 1e-7),
            log = TRUE ))
    }, cpp = '[](array<Type> var, Type phi, Type stddev, Type level, int lastx) -> vector<Type> {
        Type noisemean = 0;
        Type noisestddev = stddev;

        if (lastx > 0 && std::isnan(asDouble(level))) {
            // level needs setting from previous values, if not enough assume 0
            int i = var.size();  // NB: This should be the first projected value, but optimising a projection isnt a likely thing to want to do
            level = (i - lastx) >= 0 ? var.segment(i - lastx, lastx).mean() : 0;
        }

        array<Type> lastvar(var.size() - 1);
        array<Type> curvar(var.size() - 1);
        array<Type> nll(var.size());
        lastvar = var.head(var.size() - 1);
        curvar = var.tail(var.size() - 1);
        nll(0) = 0;
        nll.tail(nll.size() - 1) = -dnorm(
            // i.e. only try to account for level when its not derived from previous values
            (vector<Type>)(curvar - phi * lastvar - (1 - phi) * std::max(level, (Type)0)),
            noisemean,
            // noisestddev needs to be > 0, or nll is inf. Optimiser will need a curve of some kind to work with
            std::max(noisestddev, (Type)1e-7),
            1 );
        return(nll);
    }')
    g3_param_project_ar1 <- g3_native(r = function (var, phi, stddev, level, lastx) {
        if (all(is.finite(var))) return(var)
        noisemean <- 0
        noisestddev <- stddev

        lastvar <- 0
        for (i in seq_along(var)) {
            if (!is.finite(var[[i]])) {  # Ignore non-projection values
                if (lastx > 0 && is.nan(level)) {
                    # level needs setting from previous values, if not enough assume 0
                    level <- if ((i - lastx) >= 1) mean(var[(i - lastx):(i - 1)]) else 0
                }
                var[[i]] <- phi * lastvar + (1 - phi) * level + rnorm(1, noisemean, noisestddev)
            }
            lastvar <- var[[i]]
        }
        return(var)
    }, cpp = '[](array<Type> var, Type phi, Type stddev, Type level, int lastx) -> vector<Type> {
        if (var.allFinite()) return var;
        Type noisemean = 0;
        Type noisestddev = stddev;

        Type lastvar = 0;
        for (int i = 0; i < var.size(); i++) {
            if (!var.segment(i, 1).allFinite()) {  // Ignore non-projection values
                if (lastx > 0 && std::isnan(asDouble(level))) {
                    // level needs setting from previous values, if not enough assume 0
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
            ~sum(projstock__nll[] <- g3_param_project_nll_ar1(projstock__var, phi_f, stddev_f, level_f, as_integer(lastx_f))),
            list(phi_f = phi_f, stddev_f = stddev_f, level_f = level_f, lastx_f = lastx_f) ),
        project = f_substitute(
            ~g3_param_project_ar1(projstock__var, phi_f, stddev_f, level_f, as_integer(lastx_f)),
            list(phi_f = phi_f, stddev_f = stddev_f, level_f = level_f, lastx_f = lastx_f) ))
}

g3_param_project_logar1 <- function (
        phi_f = g3_parameterized(
            "proj.logar1.phi",
            value = 0.8, lower = 0, upper = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        lstddev_f = g3_parameterized(
            "proj.logar1.stddev",
            value = 0.2, optimise = FALSE, type = "LOG",
            prepend_extra = quote(param_name) ),
        loglevel_f = g3_parameterized(
            "proj.logar1.level",
            value = 1, type = "LOG",
            prepend_extra = quote(param_name) ),
        lastx_f = 0L) {
    if (!identical(lastx_f, 0L)) loglevel_f <- NaN  # Disable loglevel parameter when lastx enabled

    g3_param_project_nll_logar1 <- g3_native(r = function (logvar, phi, lstddev, loglevel, lastx) {
        noisemean <- 0 - exp(2*lstddev) / 2
        noisestddev <- exp(lstddev)

        if (lastx > 0L && is.nan(loglevel)) {
            # Loglevel needs setting from previous values, if not enough assume 0
            i <- length(logvar)  # NB: This should be the first projected value, but optimising a projection isnt a likely thing to want to do
            loglevel <- if ((i - lastx) >= 1) mean(logvar[(i - lastx):(i - 1)]) else 0
        }

        lastlogvar <- head(logvar, -1)
        curlogvar <- tail(logvar, -1)
        c(0, -dnorm(
            curlogvar - phi * lastlogvar - (1 - phi) * loglevel,
            noisemean,
            noisestddev,
            log = TRUE ))
    }, cpp = '[](array<Type> logvar, Type phi, Type lstddev, Type loglevel, int lastx) -> vector<Type> {
        Type noisemean = 0 - exp(2*lstddev) / 2;
        Type noisestddev = exp(lstddev);

        if (lastx > 0 && std::isnan(asDouble(loglevel))) {
            // Loglevel needs setting from previous values, if not enough assume 0
            int i = logvar.size();  // NB: This should be the first projected value, but optimising a projection isnt a likely thing to want to do
            loglevel = (i - lastx) >= 0 ? logvar.segment(i - lastx, lastx).mean() : 0;
        }

        array<Type> lastlogvar(logvar.size() - 1);
        array<Type> curlogvar(logvar.size() - 1);
        array<Type> nll(logvar.size());
        lastlogvar = logvar.head(logvar.size() - 1);
        curlogvar = logvar.tail(logvar.size() - 1);
        nll(0) = 0;
        nll.tail(nll.size() - 1) = -dnorm(
            (vector<Type>)(curlogvar - phi * lastlogvar - (1 - phi) * loglevel),
            noisemean,
            noisestddev,
            1 );
        return(nll);
    }')
    g3_param_project_logar1 <- g3_native(r = function (logvar, phi, lstddev, loglevel, lastx) {
        if (all(is.finite(logvar))) return(logvar)
        noisemean <- 0 - exp(2*lstddev) / 2
        noisestddev <- exp(lstddev)

        lastlogvar <- 0
        for (i in seq_along(logvar)) {
            if (!is.finite(logvar[[i]])) {  # Ignore non-projection values
                if (lastx > 0L && is.nan(loglevel)) {
                    # Loglevel needs setting from previous values, if not enough assume 0
                    loglevel <- if ((i - lastx) >= 1) mean(logvar[(i - lastx):(i - 1)]) else 0
                }
                logvar[[i]] <- phi * lastlogvar + (1 - phi) * loglevel + rnorm(1, noisemean, noisestddev)
            }
            lastlogvar <- logvar[[i]]
        }
        return(logvar)
    }, cpp = '[](array<Type> logvar, Type phi, Type lstddev, Type loglevel, int lastx) -> vector<Type> {
        if (logvar.allFinite()) return logvar;
        Type noisemean = 0 - exp(2*lstddev) / 2;
        Type noisestddev = exp(lstddev);

        Type lastlogvar = 0;
        for (int i = 0; i < logvar.size(); i++) {
            if (!logvar.segment(i, 1).allFinite()) {  // Ignore non-projection values
                if (lastx > 0 && std::isnan(asDouble(loglevel))) {
                    // Loglevel needs setting from previous values, if not enough assume 0
                    loglevel = (i - lastx) >= 0 ? logvar.segment(i - lastx, lastx).mean() : 0;
                }
                logvar(i) = (Type)(phi * lastlogvar + (1 - phi) * loglevel + rnorm(1, noisemean, noisestddev)(0));
            }
            lastlogvar = logvar(i);
        }
        return logvar;
    }')

    list(
        name = "logar1",
        # NB: We don't define projstock__nll, so g3_param_project will define a g3_stock_instance()
        nll = f_substitute(
            ~sum(projstock__nll[] <- g3_param_project_nll_logar1(projstock__lvar, phi_f, lstddev_f, loglevel_f, as_integer(lastx_f))),
            list(phi_f = phi_f, lstddev_f = lstddev_f, loglevel_f = loglevel_f, lastx_f = lastx_f) ),
        project = f_substitute(
            ~g3_param_project_logar1(projstock__lvar, phi_f, lstddev_f, loglevel_f, as_integer(lastx_f)),
            list(phi_f = phi_f, lstddev_f = lstddev_f, loglevel_f = loglevel_f, lastx_f = lastx_f) ))
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
        type = (if ("projstock__lvar" %in% all.names(project_fs$project)) "LOG" else ""),
        ifmissing = NaN )
    projstock__var <- g3_stock_instance(projstock, NaN, desc = paste0("Projected values for ", projstock$name))
    projstock__lvar <- g3_stock_instance(projstock, NaN, desc = paste0("Projected values for ", projstock$name, " in logspace"))
    uses_var <- "projstock__var" %in% all.names(project_fs$project)
    uses_lvar <- "projstock__lvar" %in% all.names(project_fs$project)

    out <- g3_step(f_substitute(~(
        stock_with(projstock, (if (uses_var) stock_ss(projstock__var, vec = single) * scale else 0) +
                              (if (uses_lvar) exp(stock_ss(projstock__lvar, vec = single)) * scale else 0) +
                              offset)
    ), list(
        uses_var = uses_var,
        uses_lvar = uses_lvar,
        scale = scale,
        offset = offset,
        end = NULL )), recursing = TRUE)

    # If nll formula doesn't define projstock__nll, generate a stock instance for it to use
    if (!exists("projstock__nll", envir = environment(project_fs$nll))) {
        environment(project_fs$nll)$projstock__nll <- g3_stock_instance(projstock, NaN, desc = paste0("nll for ", projstock$name, " deviants"))
    }

    environment(out)[[step_id(g3_action_order$initial, "g3a_project_param", project_fs$name, paste(param_name, collapse = "_"))]] <- g3_step(f_substitute(~{
        debug_label("g3_param_project: generate projections for ", projstock)
        stock_with(projstock, {
            if (cur_time > total_steps) {
                if (weight > 0) nll <- nll + weight * nll_f

                if (uses_var) stock_with(projstock, ADREPORT(sum(projstock__var)))
                if (uses_lvar) stock_with(projstock, ADREPORT(sum(exp(projstock__lvar))))
            } else if (cur_year_projection) {
                if (uses_var) if (is.nan(stock_ss(projstock__var, vec = single))) {
                    projstock__var <- project_f
                }
                if (uses_lvar) if (is.nan(stock_ss(projstock__lvar, vec = single))) {
                    projstock__lvar <- project_f
                }
            } else {
                if (uses_var) { stock_ss(projstock__var, vec = single) <- param_tbl }
                if (uses_lvar) { stock_ss(projstock__lvar, vec = single) <- param_tbl }
            }
        })
    }, list(
        # Add projstock_var_sym into the projection method
        project_f = project_fs$project,
        nll_f = project_fs$nll,
        param_tbl = param_tbl,
        uses_var = uses_var,
        uses_lvar = uses_lvar,
        weight = weight,
        end = NULL )))
    return(out)
}
