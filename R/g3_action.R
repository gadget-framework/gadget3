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
    # Turn into formula. NB: Use stock$iterate as environment so e.g. stock_ages
    # are still available when iter isn't used
    templ <- formula(call("~", templ), env = f_envir(stock$iterate))
    f <- f_substitute(templ, list(
        stock_comment = paste(as.list(sys.call(-1))[[1]], "for", stock$name),
        run_if = run_if,
        init = init,
        iter = iter,
        final = final))

    subs <- new.env(emptyenv())
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
    steps <- steps
    step_count <- length(steps)
    cur_time <- as.integer(0)
    cur_step_len <- as.integer(0)
    cur_step_final <- FALSE
    total_steps <- ~length(steps) * (end_year - start_year)

    list(step0 = ~{
        comment("g3a_time")
        cur_time <- cur_time + 1
        cur_step_len <- 55 # TODO: More realistic definition, can we remove if not required?
        cur_step_final <- (cur_time %% step_count) == step_count - 1
        if (cur_time < total_steps) break
    })
}

g3a_age <- function(stock) {
    # Mangle stock_num / stock_wgt to remove non-age parameters
    stock_num_age <- as.call(lapply(ling_imm$stock_num, function (x) {
        if (as.character(x) %in% c("[", "stock_num", "age")) x else ling_imm$stock_num[[3]]
    }))
    stock_num_age_older <- as.call(lapply(ling_imm$stock_num, function (x) {
        if (as.character(x) %in% c("[", "stock_num")) x
        else if (as.character(x) %in% c("age")) quote(age + 1)
        else ling_imm$stock_num[[3]]
    }))
    stock_wgt_age <- as.call(lapply(ling_imm$stock_wgt, function (x) {
        if (as.character(x) %in% c("[", "stock_wgt", "age")) x else ling_imm$stock_wgt[[3]]
    }))
    stock_wgt_age_older <- as.call(lapply(ling_imm$stock_wgt, function (x) {
        if (as.character(x) %in% c("[", "stock_wgt")) x
        else if (as.character(x) %in% c("age")) quote(age + 1)
        else ling_imm$stock_wgt[[3]]
    }))

    list(
        step125 = stock_step(stock, run_if = ~cur_step_final, init = f_substitute(~for (age in rev(stock_ages)) {
            if (age == stock_ages[length(stock_ages)]) {
                comment("TODO: Plus group migration shenanigans")
            } else {
                stock_num_older <- stock_num_older + stock_num
                stock_num <- 0
            }
        }, list(
            stock_num = stock_num_age,
            stock_num_older = stock_num_age_older))))
}

g3a_grow_lengthvbsimple <- function (linf, kappa, alpha, beta) {
    f_substitute(
        ~(linf - stock_lenmid) * (1 - exp(-kappa * cur_step_len)),
        list(linf = linf, kappa = kappa, alpha = alpha, beta = beta))
}

g3a_grow_impl_bbinom <- function (beta, maxlengthgroupgrowth) {
    f_substitute(
        ~growth_bbinom(beta, maxlengthgroupgrowth),
        list(beta = beta, maxlengthgroupgrowth = maxlengthgroupgrowth))
}

g3a_grow <- function(stock, growth_fn, impl_fn) {
    stock_growth_num <- stock_definition(stock, 'stock_num')
    stock_delt_l <- array(dim = dim(stock_growth_num)[[1]])
    stock_growth_ratio <- matrix(nrow = length(stock_delt_l), ncol = length(stock_delt_l))
    list(
        step055b = stock_step(stock,
            init = stock_growth_num ~ 0,
            iter = f_substitute(~{
                stock_delt_l <- growth_fn
                stock_growth_ratio <- impl_fn

                stock_growth_num <- stock_num * stock_growth_ratio
            }, list(
                growth_fn = growth_fn,
                impl_fn = impl_fn)),
            final = stock_num ~ stock_growth_num))
}
