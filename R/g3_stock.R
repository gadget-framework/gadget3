stock_extend <- function(inner, ...) {
    out <- as.environment(inner)
    additions <- list(...)

    for (n in names(additions)) {
        new_val <- additions[[n]]

        assign(n, new_val, envir = out)
    }

    as.list(out)
}

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

# Pull the definition of the stock variable out of the stock object
stock_definition <- function(stock, var_name) {
    get(var_name, envir = f_envir(stock$iterate))
}

# TODO: Using this directly on top of others won't produce valid code. Should they be collapsed together?
g3_stock <- function(stock_name, stock_lengths) {
    stock_num <- array(dim = c(length(stock_lengths)))
    stock_wgt <- array(dim = c(length(stock_lengths)))
    stock_lenmid <- vapply(seq_along(stock_lengths), function (i) {
        if (i < length(stock_lengths)) {
            mean(c(stock_lengths[[i]], stock_lengths[[i+1]]))
        } else {
            stock_lengths[[i]]  # TODO: What is the midpoint of the plus-group?
        }
    }, 0)

    list(
        name = stock_name,
        stock_num = parse(text = "stock_num[]")[[1]],
        stock_wgt = parse(text = "stock_wgt[]")[[1]],
        stock_name = stock_name,
        capture = ~{stock_lengths <- length_agg},
        iterate = ~extension_point)
}

g3s_fleet <- function(stock_name) {
    extension_point <- c()
    assign(paste0(stock_name, '_state'), 0)
    list(
        iterate = ~extension_point)
}

g3s_livesonareas <- function(inner_stock, stock_areas) {
    stock_num <- array(dim = c(dim(stock_definition(inner_stock, 'stock_num')), length(stock_areas)))
    stock_wgt <- array(dim = c(dim(stock_definition(inner_stock, 'stock_wgt')), length(stock_areas)))
    stock_extend(inner_stock,
        stock_num = as.call(c(as.list(inner_stock$stock_num), as.symbol("area"))),
        stock_wgt = as.call(c(as.list(inner_stock$stock_wgt), as.symbol("area"))),
        capture = f_substitute(~if (area %in% stock_areas) extension_point, list()),
        iterate = f_substitute(~for (area in stock_areas) extension_point, list(extension_point = inner_stock$iterate))
    )
}

g3s_age <- function(inner_stock, stock_ages) {
    stock_num <- array(dim = c(dim(stock_definition(inner_stock, 'stock_num')), length(stock_ages)))
    stock_wgt <- array(dim = c(dim(stock_definition(inner_stock, 'stock_wgt')), length(stock_ages)))
    inner_stock <- stock_extend(inner_stock,
        stock_num = as.call(c(as.list(inner_stock$stock_num), as.symbol("age"))),
        stock_wgt = as.call(c(as.list(inner_stock$stock_wgt), as.symbol("age"))),
        capture = f_substitute(~for (age in stock_ages) extension_point, list()),
        iterate = f_substitute(~for (age in stock_ages) extension_point, list(extension_point = inner_stock$iterate)))
}

g3s_prey <- function(inner_stock, energycontent) {
    inner_stock  # TODO:
}
