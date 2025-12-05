if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

# Core years the model will run for
year_range <- 1975:1990

areas <- g3_areas(c('1', '2', '3'))

actions_time <- list(
  g3a_time(start_year = min(year_range), end_year = max(year_range))
)

### Configure stocks & stock dynamics
#########################################

stocks <- list(
  imm = g3_stock(c(species = 'bli', maturity = 'imm'), 1:100) |>
    g3s_age(minage = 0, maxage = 8) |>
    g3s_livesonareas(areas[c('1', '2', '3')])
)

actions_time <- list(
  g3a_time(
    start_year = min(year_range),
    end_year = max(year_range),
    step_lengths = rep(3L, 4)
  )
)

# Define actions for immature stocks
actions_imm <- list(
  g3a_initialconditions_normalcv(
    stocks$imm,
    cv_f = g3_parameterized('lencv', by_stock = stocks, by_age = TRUE),
    mean_f = g3a_renewal_vonb_t0(
        Linf = g3_parameterized('Linf', value = 1, by_stock = TRUE, by_area = TRUE),
        by_stock = stocks ),
    by_stock = stocks
  ),
  g3a_naturalmortality(stocks$imm, g3a_naturalmortality_exp(by_stock = stocks)),
  g3a_age(stocks$imm),
  g3a_growmature(
    stocks$imm,
    impl_f = g3a_grow_impl_bbinom(
      maxlengthgroupgrowth = 10,
      delta_len_f = g3a_grow_lengthvbsimple(
          linf_f = g3_parameterized('Linf', by_stock = TRUE, by_area = TRUE),
          by_stock = stocks ),
      by_stock = stocks
    )
  ),
  g3a_renewal_normalcv(stocks$imm,
    mean_f = g3a_renewal_vonb_t0(
        Linf = g3_parameterized('Linf', value = 1, by_stock = TRUE, by_area = TRUE),
        by_stock = stocks ),
     by_stock = stocks)
)

actions <- c(
  actions_time,
  actions_imm,
  # NB: Only required for testing
  gadget3:::g3l_test_dummy_likelihood()
)
model_fn <- g3_to_r(c(
  actions,
  list(
    g3a_report_detail(actions),
    g3l_bounds_penalty(actions)
  )
))
model_cpp <- g3_to_tmb(c(
  actions,
  list(
    g3a_report_detail(actions),
    g3l_bounds_penalty(actions)
  )
))

params.in.base <-
  attr(model_fn, 'parameter_template') |>

  # Recruitment and initial conditions
  g3_init_val("recage", 2) |>
  g3_init_val('*.rec.#', 2) |>
  g3_init_val('*.init.#', 0.01) |>
  g3_init_val('*.rec.scalar', 50) |>
  g3_init_val('*.init.scalar', 0) |>
  g3_init_val('*.lencv.#', 0.01) |>

  # Growth
  g3_init_val('*.K', 0.05) |>
  g3_init_val('*.t0', -.5) |>
  g3_init_val('*.bbin', 100) |>
  g3_init_val('*.M.#', 0.15) |>

  # Weight-length relationship
  g3_init_val('*.walpha', 8.511e-07) |>
  g3_init_val('*.wbeta', 3.3264) |>

  identity()

params.in <-
  params.in.base |>
  # TODO: We're muddling integer area numbers and names for the parameter table, this needs fixing
  g3_init_val('*.Linf.1', 40) |>
  g3_init_val('*.Linf.2', 60) |>
  g3_init_val('*.Linf.3', 100) |>
  identity()
r <- attributes(model_fn(params.in))
ok(ut_cmp_equal(
  as.vector(g3_array_agg(r$dstart_bli_imm__num, c("area"), agg = "length_mean", year = 1990, step = 2, age = 8)),
  c(17.35107, 25.58539, 42.64214),
  tolerance = 1e-6), "Longest fish in area 3")
gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)
  
params.in <-
  params.in.base |>
  # TODO: We're muddling integer area numbers and names for the parameter table, this needs fixing
  g3_init_val('*.Linf.1', 40) |>
  g3_init_val('*.Linf.2', 100) |>
  g3_init_val('*.Linf.3', 40) |>
  identity()
r <- attributes(model_fn(params.in))
ok(ut_cmp_equal(
  as.vector(g3_array_agg(r$dstart_bli_imm__num, c("area"), agg = "length_mean", year = 1990, step = 2, age = 8)),
  c(17.35107, 42.64214, 17.35107),
  tolerance = 1e-6 ), "Longest fish in area 2")
gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)
  
