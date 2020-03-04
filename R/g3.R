source('utils.R')

g3_time <- function(start_year, end_year, steps = c(12)) {
    steps <- steps
    step_count <- length(steps)
    cur_time <- as.integer(0)
    total_steps <- total_steps ~ step_count * (end_year - start_year)
    list(
        step = list(
            cur_time ~ cur_time + 1,
            ~if (cur_time < total_steps) break,
            NULL))
}

# Merge stock with it's parent, insert code at extension_point, replace stock with proper stock variable
stock_extend <- function(inner_stock, ...) {
    out <- inner_stock
    additions <- list(...)

    for (n in names(additions)) {
        if (is.formula(additions[[n]])) {
            out[[n]] <- f_substitute(out[[n]], list(
                extension_point = f_substitute(additions[[n]], list(stock = inner_stock$stock))))
        } else {
            out[[n]] <- additions[[n]]
        }
    }
    return(out)
}

g3_stock <- function(stock_name, length_agg) {
    extension_point <- c()
    assign(paste0(stock_name, '_state'), rep.int(0, length(length_agg)))
    list(
        stock = as.symbol(paste0(stock_name, '_state')),
        harvest = ~{stock_lengths <- length_agg; extension_point},
        step = ~{stock_lengths <- length_agg; extension_point})
}

g3s_fleet <- function(stock_name) {
    extension_point <- c()
    assign(paste0(stock_name, '_state'), 0)
    list(
        stock = as.symbol(paste0(stock_name, '_state')),
        step = ~{extension_point})
}

g3s_livesonareas <- function(inner_stock, areas) {
    # TODO: An implementation will need to use reference classes in R
    inner_stock %>% stock_extend(
        stock = call("[[", inner_stock$stock, as.symbol("area")),
        harvest = f_substitute(~if (area %in% areas) extension_point, list(areas = areas)),
        step = f_substitute(~for (area in areas) extension_point, list(areas = areas))
    )
}

g3s_growth <- function(inner_stock, ages, delt_l) {
    inner_stock %>% stock_extend(
        stock = call("[[", inner_stock$stock, as.symbol("age")),
        harvest = f_substitute(~for (age in ages) extension_point, list(ages = ages)),
        step = f_substitute(~{
            delt_l <- delt_l_defn
            stock <- stock + delt_l
            growth_ratio <- matrix(0, length(stock_lengths), length(stock_lengths))
            for (l in stock_lengths) {
                growth_ratio[l+1, l] <- 1 / delt_l
            }
            for (age in rev(ages)) {
                # TODO: Plus group doesn't zero, bottom group doesn't have a lower group feeding in
                stock[[age]] <- rep(0, length(stock_lengths))
                for (target_l in stock_lengths) {
                    stock[[age]] <- stock[[age]] + stock[[age - 1]] * growth_ratio[[target_l]]
                }
            }
            for (age in ages) extension_point
        }, list(delt_l_defn = delt_l))
    )
}

# ... is list of formula (prey_stock) ~ (selectivity)
g3s_predator_number <- function(inner_stock, ...) {
    out <- inner_stock

    for (prey_f in list(...)) {
        # Get stock from LHS of formula
        prey_stock <- get(as.character(prey_f[[2]]), envir = f_envir(prey_f))
        
        prey_harvest <- f_substitute(prey_stock$harvest, list(
            extension_point = f_substitute(~{
                suitable_catch <- suitable_catch + suit * stock
            }, list(
                suit = f_substitute(prey_f, list(preylen = 9999)),  # TODO:
                stock = prey_stock$stock
            )), harvest_areas = as.symbol("areas")))

        str(prey_harvest)
        out <- stock_extend(out, step = f_substitute(~{
            suitable_catch <- c();
            prey_harvest;
            suitable_catch <- E * suitable_catch;
        }, list(
            prey_harvest = prey_harvest,
            E = 4 # TODO:
        )))
    }
    return(out)
}

g3_model <- function(...) {
    # Combine all steps into one list
    list(step = do.call(c, lapply(list(...), function (x) x$step)))
}

# This is something that should be pulled out of data
g3_data <- function(data_name) {
    return(structure(data_name, class = "g3_data"))
}

# This is something that should be pulled out of params
g3_param <- function(param_name) {
    return(structure(param_name, class = "g3_param"))
}

g3_run <- function(g3m, data, param) {
    init_con <- textConnection("init_str", "w", local = TRUE)
    step_con <- textConnection("step_str", "w", local = TRUE)
    
    scope <- new.env()
    parse_line <- function (l, out_con = step_con) {
        # Parse formulae for any variables that need to be defined
        for (var_name in if (is.formula(l)) all.vars(l[[length(l)]]) else c()) {
            if (exists(var_name, envir = scope, inherits = FALSE)) {
                # Already init'ed this, ignore it.
                next
            }
            if (var_name %in% lapply(f_find(l, as.symbol("<-")), function (x) { x[[2]] }) ) {
                next
            }
            if (var_name %in% lapply(f_find(l, as.symbol("for")), function (x) { x[[2]] }) ) {
                next
            }

            var <- get(var_name, env = f_envir(l), inherits = TRUE)
            if ('g3_data' %in% class(var)) {
                cat(var_name, '<-', paste0('data$', var), '\n', file = init_con)
            } else if (is.numeric(var) || is.character(var)) {
                # Defined as a literal
                cat(var_name, '<-', deparse(var), '\n', file = init_con)
            } else {
                parse_line(var, out_con = init_con)
            }
            assign(var_name, TRUE, envir = scope)
        }

        if (is.formula(l) && length(l) == 3) {
            cat(l[[2]], '<-', deparse(l[[3]]), "\n", file = out_con)
        } else if (is.formula(l)) {
            l <- f_substitute(l, list(extension_point = quote({})))  # Remove any final extension_point
            cat(paste(deparse(l[[2]]), collapse = "\n"), "\n", file = out_con)
        } else if (is.language(l)) {
            writeLines(paste(deparse(l), collapse = "\n"), con = out_con)
        } else if (is.null(l)) {
            # Do nothing
        } else {
            stop("Don't know how to output ", capture.output(str(l)))
        }
    }
    for (l in g3m$step) parse_line(l)

    close(init_con)
    close(step_con)
    c(
        "function (data, params) {",
        paste("  ", init_str),
        '  while (TRUE) {',
        paste("    ", step_str),
        '  }',
        '}')
}
