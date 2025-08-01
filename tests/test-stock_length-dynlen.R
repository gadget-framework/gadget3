library(gadget3)
library(dplyr)

actions <- list()
area_names <- g3_areas(c('IXa', 'IXb'))

# Create time definitions ####################

actions_time <- list(
  g3a_time(
    1990L, 2023L,
    step_lengths = c(3L, 3L, 3L, 3L)),
  NULL)

actions <- c(actions, actions_time)

# Create stock definition for fish ####################
st_imm <- gadget3:::g3_storage(c(species = "fish", 'imm')) |> gadget3:::g3s_dynlen() |>
  g3s_age(1L, 5L) |>
  g3s_livesonareas(area_names["IXa"])

st_mat <- gadget3:::g3_storage(c(species = "fish", 'mat')) |> gadget3:::g3s_dynlen() |>
  g3s_age(3L, 10L) |>
  g3s_livesonareas(area_names["IXa"])
stocks = list(imm = st_imm, mat = st_mat)

actions_st_imm <- list(
  g3a_growmature(st_imm,
    impl_f = gadget3:::g3a_growmature_impl_dynlen(),
    maturity_f = g3a_mature_constant(),
    output_stocks = list(st_mat),
    transition_f = ~TRUE ),
  g3a_naturalmortality(st_imm),
  g3a_initialconditions_normalcv(st_imm),
  g3a_renewal_normalcv(st_imm),
  g3a_age(st_imm, output_stocks = list(st_mat)),
  NULL)

actions_st_mat <- list(
  g3a_growmature(st_mat, impl_f = gadget3:::g3a_growmature_impl_dynlen()),
  g3a_naturalmortality(st_mat),
  g3a_initialconditions_normalcv(st_mat),
  g3a_age(st_mat),
  NULL)

actions_likelihood_st <- list(
  g3l_understocking(stocks, nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_st_imm, actions_st_mat, actions_likelihood_st)

# Fleet data for f_surv #################################

# Landings data: For each year/step/area
expand.grid(year = 1990:2023, step = 2, area = 'IXa') |>
    # Generate a random total landings by weight
    mutate(weight = rnorm(n(), mean = 1e5, sd = 10)) |>
    # Assign result to landings_f_surv
    identity() -> landings_f_surv

# Length distribution data: Generate 1000 random samples in each year/step/area
expand.grid(year = 1990:2023, step = 2, area = 'IXa', length = rep(NA, 1000)) |>
  # Generate random lengths for these samples
  mutate(length = rnorm(n(), mean = 50, sd = 20)) |>
  # Save unagggregated data into ldist_f_surv.raw
  identity() -> ldist_f_surv.raw

# Aggregate .raw data
ldist_f_surv.raw |>
  # Group into length bins
  group_by(
      year = year,
      step = step,
      length = cut(length, breaks = c(seq(0, 110, 5), Inf), right = FALSE) ) |>
  # Report count in each length bin
  summarise(number = n(), .groups = 'keep') |>
  # Save into ldist_f_surv
  identity() -> ldist_f_surv

# Assume 100 * 100 samples in each year/step/area
expand.grid(year = 1990:2023, step = 2, area = 'IXa', age = rep(NA, 100), length = rep(NA, 100)) |>
  # Generate random whole numbers for age
  mutate(age = floor(pmin(rnorm(n(), mean = 6, sd = 1), 10))) |>
  # Generate random lengths for these samples
  mutate(length = rnorm(n(), mean = 30 + age * 2, sd = 20)) |>
  # Group into length/age bins
  group_by(
      year = year,
      step = step,
      age = age,
      length = cut(length, breaks = c(seq(0, 110, 5), Inf), right = FALSE) ) |>
  # Report count in each length bin
  summarise(number = n(), .groups = 'keep') ->
  aldist_f_surv

# Map maturity stage data to stocks
as.data.frame(aldist_f_surv) |>
  # Generate random maturity stage data from age data, our stock matures between 3..5
  mutate(maturity = age > runif(n(), 3, 5)) |>
  # Map maturity stage to the stock name: Note we don't have to use the full stock name
  mutate(stock = ifelse(maturity, "mat", "imm")) |>
  # Remove redundant columns
  mutate(age = NULL, maturity = NULL) ->
  matp_f_surv

# Create fleet definition for f_surv ####################
f_surv <- g3_fleet("f_surv") |> g3s_livesonareas(area_names["IXa"])

actions_f_surv <- list(
  g3a_predate_fleet(
    f_surv,
    stocks,
    suitabilities = g3_suitability_exponentiall50(by_stock = 'species'),
    catchability_f = g3a_predate_catchability_totalfleet(
      g3_timeareadata("landings_f_surv", landings_f_surv, "weight", areas = area_names))),
  NULL)
actions_likelihood_f_surv <- list(
  g3l_catchdistribution(
    "ldist_f_surv",
    obs_data = ldist_f_surv,
    fleets = list(f_surv),
    stocks = stocks,
    function_f = g3l_distribution_sumofsquares(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  g3l_catchdistribution(
    "aldist_f_surv",
    obs_data = aldist_f_surv,
    fleets = list(f_surv),
    stocks = stocks,
    function_f = g3l_distribution_sumofsquares(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  g3l_catchdistribution(
    "matp_f_surv",
    obs_data = matp_f_surv,
    fleets = list(f_surv),
    stocks = stocks,
    function_f = g3l_distribution_sumofsquares(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_f_surv, actions_likelihood_f_surv)

# Create abundance index for si_cpue ########################

# Generate random data
expand.grid(year = 1990:2023, step = 3, area = 'IXa') |>
    # Fill in a weight column with total biomass for the year/step/area combination
    mutate(weight = runif(n(), min = 10000, max = 100000)) ->
    dist_si_cpue

actions_likelihood_si_cpue <- list(

  g3l_abundancedistribution(
    "dist_si_cpue",
    dist_si_cpue,

    stocks = stocks,
    function_f = g3l_distribution_surveyindices_log(alpha = NULL, beta = 1),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_likelihood_si_cpue)

# Create model objective function ####################

model_code <- g3_to_tmb(c(actions, list(
    g3a_trace_var(actions, var_re = "."),
    g3a_report_detail(actions),
    g3l_bounds_penalty(actions) )))

load("/tmp/params.out.Rdata", verbose = T)

attr(model_code, "parameter_template") |>
  identity() -> params.in
params.in$value <- params.out[params.in$switch, "value"]

fn <- g3_tmb_fn(model_code)
r <- fn(params.in)


## Guess l50 / linf based on stock sizes
#estimate_l50 <- g3_stock_def(st_imm, "midlen")[[length(g3_stock_def(st_imm, "midlen")) / 2]]
#estimate_linf <- tail(g3_stock_def(st_imm, "midlen"), 3)[[1]]
#estimate_t0 <- g3_stock_def(st_imm, "minage") - 0.8
#
#attr(model_code, "parameter_template") |>
#  g3_init_val("*.rec|init.scalar", 1000, optimise = FALSE) |>
#  g3_init_val("*.init.#", 10, lower = 0.001, upper = 10) |>
#  g3_init_val("*.rec.#", 100, lower = 1e-6, upper = 1000) |>
#  g3_init_val("*.rec.proj", 0.002) |>
#  g3_init_val("*.M.#", 0.15, lower = 0.001, upper = 10) |>
#  g3_init_val("init.F", 0.5, lower = 0.1, upper = 10) |>
#  g3_init_val("*.Linf", estimate_linf, spread = 2) |>
#  g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
#  g3_init_val("*.t0", estimate_t0, optimise = FALSE) |>
#  g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
#  g3_init_val("*.wbeta", 3, optimise = FALSE) |>
#  g3_init_val("*.*.alpha", 0.07, lower = 0.01, upper = 1.8) |>
#  g3_init_val("*.*.l50", estimate_l50, spread = 1.5) |>
#  # Treat maturity alpha/l50 separately
#  g3_init_val("*.mat.alpha", 0.07, lower = 0.0001, upper = 1.8) |>
#  g3_init_val("*.mat.l50", estimate_l50, spread = 3.5) |>
#  g3_init_val("*.bbin", 100, lower = 1e-05, upper = 1500) |>
#  identity() -> params.in

# Optimise model ################################
#obj.fn <- g3_tmb_adfun(model_code, params.in)
