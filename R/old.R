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
