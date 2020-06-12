# Turn a formulae into one that works on a given stock by...
# - replacing all "stock__*" variables with a matching name for the stock, e.g. "ling_imm__wgt"
# - replace (stock variable)__iter with apprpriate subsets so stock arrays can be treated as 1 dimensional lengthgroups
# - wrap iter_f formula with loops to iterate over all lengthgroups for that stock
# - Add code to translate indices for main stock to extra_stock
# - init_f: formula code to run before iter_f loops
# - final_f: formula code to run after iter_f loops
# - run_if: Wrap entire step with if statement, e.g. run_if = ~cur_time == 0 to only run on the first step of the model
stock_step <- function(stock, init_f = NULL, iter_f = NULL, final_f = NULL, run_if = NULL, extra_stock = NULL) {
    # Get names of arguents
    our_args <- as.list(sys.call())

    # Build loop, translating "stock" to caller's names for things
    loop_f <- stock_rename(stock$iterate, "stock",  our_args[[2]])
    if (!is.null(extra_stock)) {
        # Loop over first stock, then translate indices to second
        loop_f <- f_substitute(loop_f, list(
            extension_point = stock_rename(extra_stock$translate, "stock", our_args$extra_stock)))
    }

    # Replace __iter with correct code, wrap iter_f part in the stock's iterate code
    if (!is.null(iter_f)) {
        subs <- list()
        subs[[paste0(our_args[[2]], "__iter")]] <- stock_rename(stock$iter_ss, "stock", our_args[[2]])
        if (!is.null(extra_stock)) {
            subs[[paste0(our_args$extra_stock, "__iter")]] <- stock_rename(extra_stock$iter_ss, "stock", our_args$extra_stock)
        }
        iter_f <- fix_subsets(f_substitute(iter_f, subs))
        iter_f <- f_substitute(loop_f, list(extension_point = iter_f))
    }

    # Make a template with the parts this step uses, and fill in the gaps
    templ <- as.call(c(
        list(as.symbol("{")),
        call("comment", as.symbol("stock_comment")),
        if (!is.null(init_f)) as.symbol("init_f") else c(),
        if (!is.null(iter_f)) as.symbol("iter_f") else c(),
        if (!is.null(final_f)) as.symbol("final_f") else c(),
        NULL))
    if (!is.null(run_if)) {
        templ <- call("if", as.symbol("run_if"), templ)
    }
    # Turn into formula. NB: Use stock$iterate as environment so e.g. stock__minage
    # are still available when iter_f isn't used
    templ <- call_to_formula(templ, env = rlang::f_env(stock$iterate))
    f <- f_substitute(templ, list(
        stock_comment = paste(as.list(sys.call(-1))[[1]], "for", stock$name),
        run_if = run_if,
        init_f = init_f,
        iter_f = iter_f,
        final_f = final_f))
    f <- stock_rename(f, our_args[[2]], stock$name)
    if (!is.null(extra_stock)) {
        f <- stock_rename(f, our_args$extra_stock, extra_stock$name)
    }
    return(f)
}

# For formula (f), rename all (old_name)__var variables to (new_name)__var, mangling environment to match
stock_rename <- function(f, old_name, new_name) {
   old_name <- as.character(old_name)
   if (length(old_name) != 1) stop("Stocks should be variable references")
   f_vars <- all.vars(f)
   if (rlang::is_formula(f)) {
       f_vars <- union(f_vars, ls(rlang::f_env(f)))
   }
   stock_re <- paste0("^", old_name, "__")

   # Find all vars matching stock_re
   subs <- new.env(parent = emptyenv())
   for (old_var in f_vars[grepl(stock_re, f_vars)]) {
       new_var <- sub(stock_re, paste0(new_name, "__"), old_var)
       assign(old_var, as.symbol(new_var), envir = subs)
   }

   # Update environment with new names of variables
   if (rlang::is_formula(f)) {
       new_env <- rlang::env_clone(rlang::f_env(f))
       for (old_var in f_vars[grepl(stock_re, f_vars)]) {
           if (exists(old_var, envir = rlang::f_env(f), inherits = FALSE)) {
               new_var <- sub(stock_re, paste0(new_name, "__"), old_var)
               remove(list = old_var, envir = new_env)
               assign(new_var, get(old_var, env = rlang::f_env(f), inherits = FALSE), envir = new_env)
           }
       }
       f <- f_substitute(f, as.list(subs))
       rlang::f_env(f) <- new_env
   } else {
       # Just use regular substitute
       f <- eval(call("substitute", f, as.list(subs)))
   }

   return(f)
}
