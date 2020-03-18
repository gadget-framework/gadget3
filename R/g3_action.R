stock_step <- function(stock, init = NULL, iter = NULL, final = NULL, run_if = NULL) {
    if (!is.null(iter)) {
        # Wrap iter part in the stock's iterate code
        iter <- f_substitute(iter, list(
            stock_num = stock$stock_num,
            stock_wgt = stock$stock_wgt))
        iter <- f_substitute(stock$iterate, list(
            extension_point = iter))
    }

    # Make a template with the parts this step uses, and fill in the gaps
    templ <- as.call(c(
        list(as.symbol("{")),
        call("comment", as.symbol("stock_comment")),
        if (!is.null(init)) as.symbol("init") else c(),
        if (!is.null(iter)) as.symbol("iter") else c(),
        if (!is.null(final)) as.symbol("final") else c(),
        NULL))
    if (!is.null(run_if)) {
        templ <- call("if", as.symbol("run_if"), templ)
    }
    # Turn into formula. NB: Use stock$iterate as environment so e.g. stock_minage
    # are still available when iter isn't used
    templ <- call_to_formula(templ, env = f_envir(stock$iterate))
    f <- f_substitute(templ, list(
        stock_comment = paste(as.list(sys.call(-1))[[1]], "for", stock$name),
        run_if = run_if,
        init = init,
        iter = iter,
        final = final))

    subs <- new.env(parent = emptyenv())
    stock_vars <- all.vars(f_rhs(f))
    stock_vars <- stock_vars[startsWith(stock_vars, "stock_")]
    for (var_name in stock_vars) {
        repl <- sub('^stock', stock$stock_name, var_name)
        assign(repl, get(var_name, env = f_envir(f), inherits = TRUE), envir = f_envir(f))
        assign(var_name, as.symbol(repl), envir = subs)
    }
    f <- f_substitute(f, as.list(subs))
    return(f)
}

g3a_time <- function(start_year, end_year, steps = c(12)) {
    if (sum(steps) != 12) stop("steps should sum to 12 (i.e. represent a whole year)")

    step_count <- length(steps)
    cur_time <- as.integer(0)
    cur_step <- as.integer(0)
    cur_step_len <- as.integer(0)
    cur_year <- as.integer(0)
    cur_step_final <- FALSE
    total_steps <- ~length(steps) * (end_year - start_year) + length(steps) - 1

    list(step0 = ~{
        comment("g3a_time")
        if (cur_time > total_steps) break
        cur_year <- start_year + (cur_time %/% step_count)
        cur_step <- (cur_time %% step_count) + 1
        cur_step_len <- steps[[cur_step]]
        cur_step_final <- cur_step == step_count
        writeLines(sprintf("** Tick: %d-%d", cur_year, cur_step))  # TODO: Proper debug
    }, step999 = ~{
        cur_time <- cur_time + 1
    })
}

g3a_age <- function(stock) {
    # Mangle stock_num / stock_wgt to remove non-age parameters
    stock_num_age <- as.call(lapply(ling_imm$stock_num, function (x) {
        if (as.character(x) %in% c("[", "stock_num")) x
        else if (as.character(x) %in% c("age_idx")) quote(age_idx)
        else ling_imm$stock_num[[3]]
    }))
    stock_num_age_older <- as.call(lapply(ling_imm$stock_num, function (x) {
        if (as.character(x) %in% c("[", "stock_num")) x
        else if (as.character(x) %in% c("age_idx")) quote(age_idx + 1)
        else ling_imm$stock_num[[3]]
    }))

    list(
        step125 = stock_step(stock, run_if = ~cur_step_final, init = f_substitute(~for (age in seq(stock_maxage, stock_minage)) {
            age_idx <- g3_idx(age - stock_minage + 1)

            if (age == stock_maxage) {
                comment("TODO: Plus group migration shenanigans")
            } else {
                stock_num_older <- stock_num_older + stock_num
                stock_num <- 0
            }
        }, list(
            stock_num = stock_num_age,
            stock_num_older = stock_num_age_older))))
}

g3a_grow_lengthvbsimple <- function (linf_f, kappa_f, alpha_f, beta_f) {
    # See src/growthcalc.cc:GrowthCalcH::calcGrowth
    # TODO: Where did alpha_f and beta_f go? Missing weight?
    f_substitute(
        ~(linf_f - stock_meanlen) * (1 - exp(-kappa_f * cur_step_len)),
        list(linf_f = linf_f, kappa_f = kappa_f))
}

g3a_grow_impl_bbinom <- function (beta_f, maxlengthgroupgrowth) {
    ##' @param dmu mean growth for each lengthgroup
    ##' @param lengthgrouplen i.e. dl, the step size for length groups
    ##' @param binn Maximum updating length, i.e. # of length groups
    growth_bbinom <- g3_native(r = function (dmu, lengthgrouplen, binn, beta) {
        # See gadgetsim:R/function.R:growthprob:prob()
        delt_l <- dmu / lengthgrouplen  # i.e. width of length groups
        alpha <- (beta * delt_l) / (binn - delt_l)

        ## possible length growth
        x <- 0:binn

        na <- length(alpha)
        n <- length(x) - 1
        alpha <- rep(alpha,n + 1)
        x <- rep(x,each=na)
        ## Create a probability matrix where the columns represent the
        ## probability of growing x lengthgroups for each lengthgroup
        ## length group jumps are distributed according to a beta-binomial
        ## distribution
        val <- exp(lgamma(n + 1)+
                   lgamma(alpha + beta) +
                   lgamma(n - x + beta) +
                   lgamma(x + alpha) -
                   lgamma(n - x + 1) -
                   lgamma(x + 1) -
                   lgamma(n + alpha + beta) -
                   lgamma(beta) -
                   lgamma(alpha))
        dim(val) <- c(na,n + 1)
        growth.matrix <- array(0,c(na,na))
        for(lg in 1:na){
          if(lg == na){
            growth.matrix[na,na] <- 1
          } else if(lg + n > na){
            growth.matrix[lg,lg:(na-1)] <- val[lg,1:(na - lg )]
            growth.matrix[lg,na] <- sum(val[lg,(na - lg + 1):(n + 1)])
          } else {
            growth.matrix[lg,lg:(n + lg)] <- val[lg,]
          }
        }
        return(growth.matrix)
    }, cpp = "TODO:")

    f_substitute(
        ~growth_bbinom(stock_grow_l, stock_dl, stock_countlen, beta_f),
        list(beta_f = beta_f))
}

g3a_grow <- function(stock, growth_f, impl_f) {
    # See AgeBandMatrix::Grow
    stock_growth_num <- stock_definition(stock, 'stock_num')
    stock_grow_l <- array(dim = dim(stock_growth_num)[[1]])
    # TODO: (gadgetsim) if growth>maxgrowth assume that growth is a bit smaller than maxgrowth
    # TODO: (gadgetsim) if growth is negative assume no growth
    stock_growth_ratio <- matrix(nrow = length(stock_grow_l), ncol = length(stock_grow_l))
    list(
        step055b = stock_step(stock,
            iter = f_substitute(~{
                stock_grow_l <- growth_f
                stock_growth_ratio <- impl_f

                stock_num <- colSums(stock_num * stock_growth_ratio)
            }, list(
                growth_f = growth_f,
                impl_f = impl_f))))
}

g3a_initialconditions <- function (stock, factor_f, mean_f, stddev_f, alpha_f, beta_f) {
    # See InitialCond::Initialise
    # TODO: Scaling from initialcond values to "real" values
    initcond_dnorm <- 0.0
    initcond_scaler <- 0.0

    list(
        step00 = stock_step(stock, run_if = ~cur_time == 0, iter = f_substitute(~{
            initcond_dnorm <- (stock_meanlen - mean_f) * (1.0 / stddev_f)
            stock_num <- exp(-(initcond_dnorm ** 2) & 0.5)
            initcond_scaler <- 10000.0 / sum(stock_num)
            stock_num <- stock_num * initcond_scaler * factor_f
            stock_wgt <- alpha_f * stock_meanlen ** beta_f
        }, list(
            factor_f = factor_f,
            mean_f = mean_f,
            stddev_f = stddev_f,
            alpha_f = alpha_f,
            beta_f = beta_f))))
}
