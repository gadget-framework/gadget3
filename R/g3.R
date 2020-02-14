##############################

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

g3s_growth <- function(inner_stock, delt_l) {
    extend_stock(inner_stock, step = list(
        f_set_var(inner_stock$name, 'delt_l', delt_l),
        f_set_var(inner_stock$name, 'growth_ratio', ~moo),
        delt_l
    ))

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

g3_model <- function(time) {
    return(list(init = time$init, step = time$step))
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
        for (var_name in if (class(l) == 'formula') all.vars(l[[length(l)]]) else c()) {
            var <- get(var_name, env = attr(l, '.Environment'))

            if (exists(var_name, envir = scope)) {
                # Already init'ed this, ignore it.
            } else if ('g3_data' %in% class(var)) {
                cat(var_name, '<-', paste0('data$', var), '\n', file = init_con)
            } else if (is.numeric(var) || is.character(var)) {
                # Defined as a literal
                cat(var_name, '<-', deparse(var), '\n', file = init_con)
            } else {
                parse_line(var, out_con = init_con)
            }
            assign(var_name, TRUE, envir = scope)
        }

        if (class(l) == 'formula' && length(l) == 3) {
            cat(l[[2]], '<-', deparse(l[[3]]), "\n", file = out_con)
        } else if (class(l) == 'formula') {
            cat(deparse(l[[2]]), "\n", file = out_con)
        } else if (is.language(l)) {
            writeLines(deparse(l), con = out_con)
        }
    }
    for (l in g3m$step) parse_line(l)

    close(init_con)
    close(step_con)
    cat(paste(init_str, collapse = "\n"), '\nwhile (TRUE) {    \n', paste(step_str, collapse = "   \n"), '\n}\n')
}


time <- g3_time(g3_data("strtyr"), g3_data("endyr"), 4)
g3m <- g3_model(time)
g3_run(g3m)

###########

# time <- g3_time(2000, 2005, 4)
# area <- g3_area(list(a1=5, a2=5))
# 
# ling_imm <- g3s_stock('ling_imm', 10, 100) %>% g3s_growth(
#     delt_l =~ time$step_length * ling_imm$length * area$temperature,
#     )
# ling <- g3s_maturation(ling_imm, ling_mat)
# 
# g3_run(ling)
# 

# ling_imm <- g3s_stock('ling_imm', 10, 100) %>% g3s_growth()
# ling_mat <- g3s_stock('ling_mat', 10, 100)
# ling <- g3s_maturation(ling_imm, ling_mat, selectivity = ...)
# 
# survey.si <- g3s_fleet('si') %>% g3s_on_step(1, g3s_predates(ling, selectivity = ...))
# 
# g3_model(om = list(
#     ling,  # NB: We're not including ling_imm, ling_mat. That's automatic
#     survey.si,
# ), likelihood = list(
#     tgl_penalty(),
#     tgl_catchstatistics(survey.si, ling, ...),
# ))
# 
# #########
# 
# 
# 
# g3s_stock <- function (stock_name, max_age, max_len) {
#     list(
#         # TODO: Define stock type for a state, use that.
#         init = c('array<Type> {stock_name}State({max_age}, {max_len})'),
#         step = c(),
#         select_from = function (matrix, into = NULL) {
#             '{stock_name}State * {matrix}'
#         })
# }
# 
# g3s_growth <- function (inner_stock, selectivity) {
#     s <- inner_stock
#     s$step <- c(
#         s$step,
#         '// Growth',
#         '{select_from(inner_stock, "tmp_growth")}'
#         '{arr} -= migrate;',
#         '// TODO: shift migrate by one',
#         '{arr} += migrate;'
#         NULL)
# }
# 
# 
# 
# # Convert formulae arguments to list, named by their LHS
# list_by_lhs <- function(expected = c(), ...) {
#     out <- list(...)
#     names(out) <- vapply(out, function (x) as.character(x[[2]]), character(1))
#     for (n in expected) {
#         if (!(n %in% names(out))) stop("Missing formulae argument ", n)
#     }
#     out
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# om_init.stock <- function (self, base_name) {
#     self$name <- base_name
# 
#     glue::glue_data(list(
#         name = self$name,
#     ), 'array<Type>  {base_name}Arr(%d, %d, %d);')
# }
# 
# om_init.maturation <- function (self, base_name) {
#     om_init(self$stock_imm, base_name + '_imm')
#     om_init(self$stock_mat, base_name + '_mat')
# }
# 
# om_init.fleet <- function () {
#     'array<Type>  %sArr(%d, %d)' % (
#         name,
#         self$max_age, . . .
#     )
# 
#     'Array<type> %s'
#     array<Type>  $(fleet_name)Arr;
# }
# 
# om_init.predates <- function () {
#     '
#     function select_from(x, curve) {
#         ...
#     }
#     '
# }
# 
# ####
# 
# om_init.growth <- function (self, base_name) {
#     om_init(self$stock, base_name)
#     # TODO: Init growthprob matrix for this stock
#     # TODO: migrate arr
# }
# 
# om_step.growth <- function () {
#     # TODO: What if it turns out in evaluating our code we find a parameter? Need to init and step in one go.
#     # TODO: Really worth storing each type at this stage? growth generates a code object to evaluate instead?
#     # TODO: 
#     glue::glue_data(list(
#         arr_n = self$arr_n
#     ), c(
#         'migrate = {arr} * {growthMatrix};',
#         '{arr} -= migrate;',
#         '// TODO: shift migrate by one',
#         '{arr} += migrate;'
#         NULL))
# }
# 
# ###
# 
# om_step.predates <- function () {
#     '
#     survArr(...) = %s
#     ' % (
#         om_select(self$prey, curve, remove = FALSE)
#     )
# }
# 
# om_select.maturation <- function (self) {
#     '
#     %s * %s + %s * %s
#     ' % (
#         curve,
#         self$stock_imm,
#         curve,
#         self$stock_mat,
#     )
# }
# 
# om_select.stock <- function (self) {
#     '%sArr(...)' % (self$name)
# }
# 
# ##########################
# 
# 
#     array<Type> ling_immArr(maxage,maxlength,numyears,4);
#     array<Type> ling_matArr(maxage,maxlength,numyears,4);
#     
#     array<Type> stkArr(maxage,maxlength,numyears,4);
#     array<Type> survArr(maxage,maxlength,numyears,4);
