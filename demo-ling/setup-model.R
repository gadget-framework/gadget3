## -----------------------------------------------------------------------------
##
## SETUP MODEL
##
## -----------------------------------------------------------------------------

# -----------------------------
# Define bounded parameters
# -----------------------------

## MODEL PARAMETERS
# List containing the parameter name, its lower and upper bounds.
# The function "g3_stock_param" is a wrapper for g3_param that inserts the species/stock name into the parameter reference
# if lower and upper are NULL, then the parameter will be unbounded e.g. ~g3_param("ling.k")
# if the lower and upper parameters are integers a reference to a bounded parameter will be created e.g. ~bounded(g3_param(ling.k), lower, upper)
# "g3_stock_param_table" is an equivalent function that creates a reference to a table of parameters e.g. g3_param_table(ling.init, minage:maxage)


model_params <- list(
  
  ## Initial conditions:
  'walpha' = list(lower = NULL, upper = NULL),
  'wbeta' = list(lower = NULL, upper = NULL),
  'init.F' = list(lower = 0.2, upper = 0.8),
  'init.sd' = list(lower = NULL, upper = NULL),
  'scalar' = list(lower = 1, upper = 100),      ## Scalar for initial abundance and recruitment (all stocks)
  'init' = list(lower = 0.001, upper = 200),
  'renew' = list(lower = 0.001, upper = 200),
  
  ## Renewal:
  'rec.sd' = list(lower = 1, upper = 5),
  'recl' = list(lower = -5, upper = 30),
  ## Growth:
  'Linf' = list(lower = 150, upper = 200),          
  'K' = list(lower = 40, upper = 120),             
  'bbin' = list(lower = 1, upper = 1000),          
  
  ## Maturity:
  'mat1' = list(lower = NULL, upper = NULL),
  'mat2' = list(lower = 20, upper = 120),                 
  'mat.a' = list(lower = 0.5, upper = 2), 
  'mat.a50' = list(lower = min(gadget3:::stock_definition(ling_imm, 'minage'),
                                gadget3:::stock_definition(ling_mat, 'minage')),
                    upper = max(gadget3:::stock_definition(ling_imm, 'maxage'),
                                gadget3:::stock_definition(ling_mat, 'maxage'))),
  
  ## Mortality
  'M' = list(lower = NULL, upper = NULL),
  
  ## Age bounds as symbols
  imm_minage = as.symbol(paste0(ling_imm$name, "__minage")),
  imm_maxage = as.symbol(paste0(ling_imm$name, "__maxage")),
  mat_minage = as.symbol(paste0(ling_mat$name, "__minage")),
  mat_maxage = as.symbol(paste0(ling_mat$name, "__maxage"))
  
)

## OTHER
# INITIAL ABUNDANCE
init_abund <- gadget3:::f_substitute(~scalar * bounded(
  g3_param_table(init, expand.grid(age = seq(min(imm_minage, mat_minage),
                                          max(imm_maxage, mat_maxage)))), lower, upper),
  c(model_params,
    model_params$init,
    scalar = bounded_param(stock_species(ling_imm), "scalar", model_params),
    init = paste(stock_species(ling_imm), "init", sep=".")))

# RENEWAL
renewal <- gadget3:::f_substitute(~scalar * bounded(
  g3_param_table(renew, expand.grid(cur_year = seq(start_year, end_year))), lower, upper),
  c(model_params,
    model_params$renew,
    scalar = bounded_param(stock_species(ling_imm), "scalar", model_params),
    renew = paste(stock_species(ling_imm), "renew", sep=".")))

## Ensure that old fish are not immature
# a50 is bounded
prop_m_age <- 
  gadget3:::f_substitute(~ 1/(1 + exp(-mat.a*(age - a50))), 
                         list('a50' = bounded_param(stock_species(ling_imm), "mat.a50", model_params),
                              'mat.a' = bounded_param(stock_species(ling_imm), "mat.a", model_params)))

## mean length is estimated based on a Von B relationship used for immature and mature
mean_l <-
  gadget3:::f_substitute(~Linf * (1 - exp(-1 * K * (age - (1 + log(1 - recl/Linf)/K)))),
                         list('recl' = bounded_param(stock_species(ling_imm), "recl", model_params), 
                              'Linf' = bounded_param(stock_species(ling_imm), "Linf", model_params), 
                              'K' = gadget3:::f_substitute(~0.001 * K, 
                                                           list(K = bounded_param(stock_species(ling_mat), "K", model_params)))))

ling_imm_actions <- list(
  
  g3a_initialconditions_normalparam(ling_imm,
                                    # NB: area & age factor together (gadget2 just multiplied them)
                                    # initial abundance at age is 1e4 * q
                                    factor_f =
                                      gadget3:::f_substitute(
                                        ~init_abund * exp(-1 * (M + init.F) * (age - imm_minage)) * (1 - prop_m_age),
                                        c(model_params,
                                          init_abund = init_abund,
                                          prop_m_age = prop_m_age,
                                          init.F = bounded_param(stock_species(ling_imm), "init.F", model_params),
                                          M = bounded_table(ling_imm, "M", model_params))),
                                    mean_f = mean_l,
                                    stddev_f = bounded_table(ling_imm, "init.sd", model_params),
                                    alpha_f = bounded_param(ling_imm$name, "walpha", model_params),
                                    beta_f = bounded_param(ling_imm$name, "wbeta", model_params)),
  
  g3a_renewal_normalparam(ling_imm,
                          factor_f = renewal,
                          # gadget3:::f_substitute(~rec*exp(-g3_param_table("lingimm.M",
                          #                                                 data.frame(age = seq(ling_imm__minage, ling_imm__maxage)))),
                          #                        list(rec=ling_init_abund)),
                          mean_f = mean_l,
                          stddev_f = bounded_param(stock_species(ling_imm), "rec.sd", model_params),
                          alpha_f = bounded_param(ling_imm$name, "walpha", model_params),
                          beta_f = bounded_param(ling_imm$name, "wbeta", model_params),
                          run_f = gadget3:::f_substitute(~cur_step == 1 && age == imm_minage && cur_time > 0, model_params)),
                                                    
  g3a_growmature(ling_imm,
                 impl_f = g3a_grow_impl_bbinom(
                   g3a_grow_lengthvbsimple(bounded_param(stock_species(ling_imm), "Linf", model_params),
                                           gadget3:::f_substitute(~0.001 * K, 
                                                                  list(K = bounded_param(stock_species(ling_imm), "K", model_params)))),      
                   g3a_grow_weightsimple(bounded_param(ling_imm$name, "walpha", model_params),  
                                         bounded_param(ling_imm$name, "wbeta", model_params)),   
                   beta_f = bounded_param(stock_species(ling_imm), "bbin", model_params),
                   maxlengthgroupgrowth = mlgg),
                 maturity_f = g3a_mature_continuous(
                   alpha = gadget3:::f_substitute(~(0.001 * exp(mat1)), 
                                                  list(mat1 = bounded_param(stock_species(ling_imm), "mat1", model_params))),
                   l50 = bounded_param(stock_species(ling_imm), "mat2", model_params)),
                 output_stocks = list(ling_mat)),
  
  g3a_naturalmortality(ling_imm,
                       g3a_naturalmortality_exp(bounded_table(ling_imm, "M", model_params))), 
  
  g3a_age(ling_imm,
          output_stocks = list(ling_mat)),
  list())

## MATURE STOCK
ling_mat_actions <- list(
  g3a_initialconditions_normalparam(ling_mat,
                                    # NB: area & age factor together (gadget2 just multiplied them)
                                    factor_f =
                                      gadget3:::f_substitute(~init_abund *
                                                               exp(-1 * (g3_param_table(M, data.frame(age = seq(imm_minage, mat_maxage))) +
                                                                           init.F) * (age - imm_minage)) * prop_m_age,
                                                             c(model_params,
                                                               init_abund = init_abund,
                                                               prop_m_age = prop_m_age,
                                                               init.F = bounded_param(stock_species(ling_mat), "init.F", model_params),
                                                               M = paste(ling_mat$name, "M", sep="."))),
                                    mean_f = mean_l,
                                    stddev_f = bounded_table(ling_mat, "init.sd", model_params),
                                    alpha_f = bounded_param(ling_mat$name, "walpha", model_params),
                                    beta_f = bounded_param(ling_mat$name, "wbeta", model_params)),
  
  g3a_growmature(ling_mat,
                 impl_f = g3a_grow_impl_bbinom(
                   g3a_grow_lengthvbsimple(bounded_param(stock_species(ling_mat), "Linf", model_params),
                                           gadget3:::f_substitute(~0.001 * K, 
                                                                  list(K = bounded_param(stock_species(ling_mat), "K", model_params)))),  
                   g3a_grow_weightsimple(bounded_param(ling_mat$name, "walpha", model_params),  
                                         bounded_param(ling_mat$name, "wbeta", model_params)),   
                   beta_f = bounded_param(stock_species(ling_mat), "bbin", model_params),
                   maxlengthgroupgrowth = mlgg)),
  
  g3a_naturalmortality(ling_mat,
                       g3a_naturalmortality_exp(bounded_table(ling_mat, "M", model_params))),
  
  g3a_age(ling_mat),
  list())



