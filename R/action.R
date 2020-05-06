# Turn a formulae into one that works on a given stock by...
# - replacing all "stock_*" variables with a matching name for the stock, e.g. "ling_imm_wgt"
# - wrap iter formula with loops to iterate over all lengthgroups for that stock
#   and add array lookups to stock_num/stock_wgt, e.g. ing_mat_num[, stock_area_idx, stock_age_idx]
# - init: formula code to run before iter loops
# - final: formula code to run after iter loops
# - run_if: Wrap entire step with if statement, e.g. run_if = ~cur_time == 0 to only run on the first step of the model
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
    stock_vars <- all.vars(rlang::f_rhs(f))
    stock_vars <- stock_vars[startsWith(stock_vars, "stock_")]
    for (var_name in stock_vars) {
        repl <- sub('^stock', stock$stock_name, var_name)
        assign(repl, get(var_name, env = rlang::f_env(f), inherits = TRUE), envir = rlang::f_env(f))
        assign(var_name, as.symbol(repl), envir = subs)
    }
    f <- f_substitute(f, as.list(subs))
    return(f)
}
