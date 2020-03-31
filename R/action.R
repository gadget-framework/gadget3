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
    templ <- call_to_formula(templ, env = rlang::f_env(stock$iterate))
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
        assign(repl, get(var_name, env = rlang::f_env(f), inherits = TRUE), envir = rlang::f_env(f))
        assign(var_name, as.symbol(repl), envir = subs)
    }
    f <- f_substitute(f, as.list(subs))
    return(f)
}

g3a_time <- function(start_year, end_year, steps = c(12)) {
    if (sum(steps) != 12) stop("steps should sum to 12 (i.e. represent a whole year)")

    # If these are literals, they should be integers
    if (is.numeric(start_year)) start_year <- as.integer(start_year)
    if (is.numeric(end_year)) end_year <- as.integer(end_year)
    if (is.numeric(steps)) steps <- as.integer(steps)

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
        cur_step_len <- steps[[g3_idx(cur_step)]]
        cur_step_final <- cur_step == step_count
        debugf("** Tick: %d-%d\n", cur_year, cur_step)
    }, step999 = ~{
        cur_time <- cur_time + 1
    })
}
