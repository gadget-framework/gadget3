## -----------------------------------------------------------------------------
##
## SETUP MODEL
##
## -----------------------------------------------------------------------------

## Age bounds as symbols
imm_minage <- as.symbol(paste0(imm_stock, "__minage"))
imm_maxage <- as.symbol(paste0(imm_stock, "__maxage"))
mat_minage <- as.symbol(paste0(mat_stock, "__minage"))
mat_maxage <- as.symbol(paste0(mat_stock, "__maxage"))

## Stock names without space
imm_name <- paste0(species_name, "imm")
mat_name <- paste0(species_name, "mat")

# -----------------------------
# Define bounded parameters
# -----------------------------

## MODEL PARAMETERS
# List containing references to parameters in the model
# function "g3_stock_param" is a wrapper for g3_param that inserts the species/stock name into the parameter reference
# if lower and/or upper are NULL, then the parameter will be unbounded e.g. ~g3_param("ling.k")
# if the lower and upper parameters are integers a reference to a bounded parameter will be created e.g. ~bounded(g3_param(ling.k), lower, upper)
# if a vector is provided to lower or upper, the minimum and maximum will be used as bounds respectively
# "g3_stock_param_table" is an equivalent function that creates a reference to a table of parameters e.g. g3_param_table(ling.init, minage:maxage)

model_params <- list(
  
  ## Initial conditions:
  walpha.imm = g3_stock_param(imm_name, param = "walpha", lower = NULL, upper = NULL),
  wbeta.imm = g3_stock_param(imm_name, param = "wbeta", lower = NULL, upper = NULL),
  init.sd.imm = g3_stock_param_table(imm_name, param = "init.sd", 
                                     lowage = imm_minage, highage = imm_maxage,
                                     lower = NULL, upper = NULL),
  
  walpha.mat = g3_stock_param(mat_name, param = "walpha", lower = NULL, upper = NULL),
  wbeta.mat = g3_stock_param(mat_name, param = "wbeta", lower = NULL, upper = NULL),
  init.sd.mat = g3_stock_param_table(mat_name, param = "init.sd", 
                                     lowage = mat_minage, highage = mat_maxage,
                                     lower = NULL, upper = NULL),
  
  # Scalar for initial abundance and recruitment (all stocks)
  scalar = g3_stock_param(species_name, param = "scalar", lower = 1, upper = 100),
  
  ## Renewal:
  rec.sd = g3_stock_param(species_name, param = "rec.sd", lower = 1, upper = 5),
  recl = g3_stock_param(species_name, param = "recl", lower = -5, upper = 30),
  
  ## Growth:
  linf = g3_stock_param(species_name, param = "Linf", lower = 150, upper = 200),
  K = g3_stock_param(species_name, param = "k", lower = 40, upper = 120, xby = 0.001),
  bbin = g3_stock_param(species_name, param = "bbin", lower = 1, upper = 1000),
  
  ## Maturity:
  mat1 = gadget3:::f_substitute(~(0.001 * exp(g3_param(x))), 
                                list(x=paste(species_name, "mat1", sep="."))),
  mat2 = g3_stock_param(species_name, param = "mat2", lower = 20, upper = 120),
  mat.a = g3_stock_param(species_name, param = "mat.a", lower = 0.5, upper = 2),  ## For initial abundance
  
  ## Mortality:
  init_F = g3_stock_param(species_name, param = "init.F", lower = 0.2, upper = 0.8),
  M.imm = g3_stock_param_table(imm_name, param = "M", lowage = imm_minage, highage = imm_maxage,
                               lower = NULL, upper = NULL),
  M.mat = g3_stock_param_table(mat_name, param = "M", lowage = mat_minage, highage = mat_maxage,
                               lower = NULL, upper = NULL)
  
)

## OTHER
# INITIAL ABUNDANCE AND RECRUITMENT
# recruitment and initial numbers at age come from the same distribution..

ling_init_abund <- gadget3:::f_substitute(~scalar * bounded(
  if (cur_time == 0)
    g3_param_table(p1, expand.grid(
      age = seq(
        min(x1, y1),
        max(x2, y2))))
  else
    g3_param_table(p2, expand.grid(
      cur_year = seq(start_year, end_year)))
  , 0.001, 200),
  list(scalar = model_params[['scalar']],
       p1 = paste0(species_name, ".init"),
       p2 = paste0(species_name, ".renew"),
       x1 = imm_minage, x2 = imm_maxage,
       y1 = mat_minage, y2 = mat_maxage))


# ling_init_abund <-
#   ~bounded(g3_param("ling.scalar"), 1, 100) *
#   bounded(g3_param_table("ling.init",
#                          expand.grid(cur_year = seq(start_year, end_year),
#                                      age = seq(
#                                        min(ling_imm__minage, ling_mat__minage),
#                                        max(ling_imm__maxage, ling_mat__maxage)))),
#           0.001,200)
#
# #

## main a50 between bounds
a50 <- gadget3:::f_substitute(~bounded(g3_param(mata), min(x, y), max(x2, y2)), 
                              list(mata=sprintf("%s.mat.a50", species_name), 
                                   x = imm_minage, x2 = imm_maxage,
                                   y = mat_minage, y2 = mat_maxage))

## ensure that old fish are not immature
prop_m_age <-
  gadget3:::f_substitute(~ 1/(1 + exp(-mata*(age - a50))),
                         list(a50 = a50, mata = model_params[['mat.a']]))

## mean length is estimated based on a Von B relationship used for immature and mature
mean_l <-
  gadget3:::f_substitute(~linf * (1 - exp(-1 * K *
                                            (age - (1 + log(1 - recl/linf)/K)))),
                         list(recl = model_params[['recl']], linf = model_params[['linf']], K = model_params[['K']]))

ling_imm_actions <- list(
  
  g3a_initialconditions_normalparam(stock_list[["imm_stock"]],
                                    # NB: area & age factor together (gadget2 just multiplied them)
                                    # initial abundance at age is 1e4 * q
                                    factor_f =
                                      gadget3:::f_substitute(
                                        ~ling_init_abund * exp(-1 * (immM + init_F) * (age - x1)) * (1 - prop_m_age ),
                                        list(ling_init_abund = ling_init_abund,
                                             x1 = imm_minage,
                                             prop_m_age = prop_m_age,
                                             init_F = model_params[['init_F']],
                                             immM = model_params[['M.imm']])),
                                    mean_f = mean_l,
                                    stddev_f = model_params[['init.sd.imm']],
                                    alpha_f = model_params[['walpha.imm']], #g3_stock_param(imm_name, "walpha"),
                                    beta_f = model_params[['wbeta.imm']]),  #g3_stock_param(imm_name, "wbeta")),
  
  g3a_renewal_normalparam(stock_list[["imm_stock"]],
                          factor_f = ling_init_abund,
                          # gadget3:::f_substitute(~rec*exp(-g3_param_table("lingimm.M",
                          #                                                 data.frame(age = seq(ling_imm__minage, ling_imm__maxage)))),
                          #                        list(rec=ling_init_abund)),
                          mean_f = mean_l,
                          stddev_f = model_params[['rec.sd']],
                          alpha_f = model_params[['walpha.imm']],
                          beta_f = model_params[['wbeta.imm']],
                          run_f = gadget3:::f_substitute(~cur_step == 1 && age == x1 && cur_time > 0,
                                                         list(x1 = imm_minage))),
  
  g3a_growmature(ling_imm,
                 impl_f = g3a_grow_impl_bbinom(
                   g3a_grow_lengthvbsimple(model_params[['linf']], model_params[['K']]),
                   g3a_grow_weightsimple(model_params[['walpha.imm']],
                                         model_params[['wbeta.imm']]),
                   beta_f = model_params[['bbin']],
                   maxlengthgroupgrowth = mlgg),
                 maturity_f = g3a_mature_continuous(
                   alpha = model_params[['mat1']],
                   l50 = model_params[['mat2']]),
                 output_stocks = list(stock_list[["mat_stock"]])),
  
  g3a_naturalmortality(stock_list[["imm_stock"]],
                       g3a_naturalmortality_exp(model_params[['M.imm']])),
  
  g3a_age(stock_list[["imm_stock"]],
          output_stocks = list(stock_list[["mat_stock"]])),
  list())


## MATURE STOCK
ling_mat_actions <- list(
  g3a_initialconditions_normalparam(stock_list[["mat_stock"]],
                                    # NB: area & age factor together (gadget2 just multiplied them)
                                    factor_f =
                                      gadget3:::f_substitute(~ling_init_abund *
                                                               exp(-1 * (g3_param_table(nm, data.frame(age = seq(x1, y2))) +
                                                                           init_F) * (age - x1)) *prop_m_age,
                                                             list(ling_init_abund = ling_init_abund,
                                                                  prop_m_age = prop_m_age,
                                                                  init_F = model_params[['init_F']],
                                                                  nm = paste0(species_name, "mat.M"),
                                                                  x1 = imm_minage,
                                                                  y2 = mat_maxage)),
                                    
                                    mean_f = mean_l,
                                    stddev_f = model_params[['init.sd.mat']],
                                    alpha_f = model_params[['walpha.mat']],
                                    beta_f = model_params[['wbeta.mat']]),
  
  g3a_growmature(stock_list[["mat_stock"]],
                 impl_f = g3a_grow_impl_bbinom(
                   g3a_grow_lengthvbsimple(model_params[['linf']], model_params[['K']]),
                   g3a_grow_weightsimple(model_params[['walpha.mat']],
                                         model_params[['wbeta.mat']]),
                   beta_f = model_params[['bbin']],
                   maxlengthgroupgrowth = mlgg)),
  
  g3a_naturalmortality(stock_list[["mat_stock"]],
                       g3a_naturalmortality_exp(model_params[['M.mat']])),
  
  g3a_age(stock_list[["mat_stock"]]),
  list())



