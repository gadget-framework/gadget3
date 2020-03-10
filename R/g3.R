source('utils.R')

g3a_time <- function(start_year, end_year, steps = c(12)) {
    steps <- steps
    step_count <- length(steps)
    cur_time <- as.integer(0)
    cur_step_len <- as.integer(0)
    total_steps <- ~length(steps) * (end_year - start_year)

    list(step0 = ~{
        comment("g3a_time")
        cur_time <- cur_time + 1
        cur_step_len <- 55 # TODO: More realistic definition, can we remove if not required?
        if (cur_time < total_steps) break
    })
}

stock_extend <- function(inner, ...) {
    out <- as.environment(inner)
    additions <- list(...)

    for (n in names(additions)) {
        new_val <- additions[[n]]

        assign(n, new_val, envir = out)
    }

    as.list(out)
}

stock_step <- function(stock, f, init = quote({}), final = quote({})) {
    # Wrap f in the stock's iterate code
    f <- f_substitute(f, list(
        stock_num = stock$stock_num,
        stock_wgt = stock$stock_wgt))
    f <- f_substitute(stock$iterate, list(
        extension_point = f))
    f <- f_substitute(~{
        comment(stock_comment)
        init
        middle
        final
    }, list(stock_comment = paste(as.list(sys.call(-1))[[1]], "for", stock$name),
        init = init, middle = f, final = final))

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

g3a_age <- function(stock) {
    prev_age <- "TODO:"
    tmp <- "TODO:"
    list(
        step125 = stock_step(stock, ~{
            if ((cur_time %% step_count) == step_count - 1) {  # TODO: Can g3a_time define this for us? Or do we nest the loop?
                if (prev_age) {
                    # Swap this age for previous entry
                    tmp <- stock_num
                    stock_num <- prev_age
                    prev_age <- tmp
                    # TODO: Final age bracket?
                } else {
                    # Note first age group
                    prev_age <- stock_num
                }
            }
        }))
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
            final = stock_num ~ stock_growth_num,
            f_substitute(~{
                stock_delt_l <- growth_fn
                stock_growth_ratio <- impl_fn

                stock_growth_num <- stock_num * stock_growth_ratio
            }, list(
                growth_fn = growth_fn,
                impl_fn = impl_fn))))
}

# This is something that should be pulled out of data
g3_data <- function(data_name) {
    return(structure(data_name, class = "g3_data"))
}

# This is something that should be pulled out of params
g3_param <- function(param_name) {
    return(structure(param_name, class = "g3_param"))
}

g3_compile <- function(steps) {
    f_combine <- function (list_of_f) {
        out_env <- emptyenv()
        # Stack environments together
        for (f in list_of_f) {
            e <- f_envir(f)
            parent.env(e) <- out_env
            out_env <- e
        }
        # Combine all functions into one expression
        out_call <- as.call(c(list(as.symbol("{")), lapply(unname(list_of_f), f_rhs)))
        formula(call("~", out_call), env = out_env)
    }

    var_defns <- function (code, env) {
        scope <- list()
        for (var_name in all.vars(code)) {
            if (var_name %in% scope) {
                # Already init'ed this, ignore it.
                next
            }
            if (var_name %in% lapply(f_find(code, as.symbol("for")), function (x) { x[[2]] }) ) {
                # It's an iterator
                next
            }
            var_val <- get(var_name, env = env, inherits = TRUE)
    
            if (is.formula(var_val)) {
                scope <- c(scope, var_defns(f_rhs(var_val), env))
                defn <- call("<-", as.symbol(var_name), f_rhs(var_val))
            } else if ('g3_data' %in% class(var_val)) {
                defn <- call("<-", as.symbol(var_name), call('$', as.symbol("data"), var_val))
            } else if ('g3_param' %in% class(var_val)) {
                defn <- call("<-", as.symbol(var_name), call('$', as.symbol("param"), var_val))
            } else if (is.array(var_val) && all(is.na(var_val))) {
                # Just define dimensions
                defn <- call("<-", as.symbol(var_name), substitute(array(dim = x), list(x = dim(var_val))))
            } else if (is.array(var_val)) {
                # Generate code to define matrix
                defn <- call("<-", as.symbol(var_name), parse(text = deparse(var_val))[[1]])
            } else if (is.numeric(var_val) || is.character(var_val)) {
                # Defined as a literal
                defn <- call("<-", as.symbol(var_name), var_val)
            } else {
                print(code)  # TODO: Better debug
                stop("Don't know how to define ", var_name)
            }
            scope[[var_name]] <- defn
        }
        return(scope)
    }

    steps <- steps[order(names(steps))]  # Steps should be in alphanumeric order
    all_steps <- f_combine(steps)

    out <- substitute(function (data, params) x, list(x = as.call(c(
        list(as.symbol("{")),
        var_defns(f_rhs(all_steps), f_envir(all_steps)),
        as.call(c(list(as.symbol("while"), TRUE), f_rhs(all_steps))),
        NULL))))

    # Replace any in-line g3 calls that may have been in formulae
    # TODO: A bit ugly doing this twice
    out <- call_replace(out,
        g3_data = function (x) call('$', as.symbol("data"), x[[2]]),
        g3_param = function (x) call('$', as.symbol("param"), x[[2]]))
    return(out)
}
