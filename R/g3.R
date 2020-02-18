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

g3_stock <- function(stock_name, length_agg) {
    assign(stock_name, rep.int(0, length(length_agg)))
    stockextend <- c()
    list(
        stock_name = stock_name,
        length_agg = length_agg,
        step = as.formula(paste("~{stock <-", stock_name, "; stockextend}")))
}

stock_extend <- function(inner_stock, ...) {
    out <- inner_stock
    additions <- list(...)

    for (n in names(additions)) {
        if (is.formula(additions[[n]])) {
            out[[n]] <- f_substitute(out[[n]], list(stockextend = additions[[n]]))
        } else {
            out[[n]] <- additions[[n]]
        }
    }
    return(out)
}

g3s_age <- function(inner_stock, ages) {
    # TODO: An implementation will need to use reference classes in R
    inner_stock %>% stock_extend(
        step = ~for (a in ages) { stock <- stock[[a]] ; stockextend }
    )
}

g3s_growth <- function(inner_stock, delt_l) {
    inner_stock %>% stock_extend(
        step = f_substitute(~{
            delt_l <- delt_l_defn
            stock <- stock + delt_l
        }, list(delt_l_defn = delt_l))
    )

# for (cur_area in inner_stock$areas) {
#     delt_l = ...
#     growth_ratio = [zero-matrix of lxl]
#     for (l in inner_stock$lengths) {
#         growth_ratio(l+1)(l) = 1 / delt_l  # NB: target to source
#     }
# 
#     for (cur_age_stock in inner_stock$ages) {
#         tmp = [0-vector length-group long]
#         for (target_l in inner_stock$lengths) {
#             tmp(target_l) += cur_age_stock * growth_ratio(target);
#         }
#         cur_age_stock = tmp(l)
#     }
# }
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

            var <- get(var_name, env = f_envir(l), inherit = TRUE)
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
