library(unittest)
if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

library(gadget3)

actions <- list()
area_names <- g3_areas(c('AA', 'BB', 'CC'))

st_imm_f <- g3_stock(c(species = "fish", maturity = 'imm', sex = 'f'), seq(5L, 25L, 5)) |>
  g3s_livesonareas(area_names["AA"]) |>
  g3s_age(1L, 5L)
st_imm_m <- g3_stock(c(species = "fish", maturity = 'imm', sex = 'm'), seq(5L, 25L, 5)) |>
  g3s_livesonareas(area_names["AA"]) |>
  g3s_age(1L, 5L)
st_mat <- g3_stock(c(species = "fish", maturity = 'mat'), seq(5L, 25L, 5)) |>
  g3s_livesonareas(area_names["AA"]) |>
  g3s_age(3L, 10L)

actions <- list(
  g3a_time(
    1980, 1995,
    step_lengths = c(6L, 6L)),
  g3a_initialconditions(st_imm_f, quote( 0 * stock__midlen ), quote( 0 * stock__midlen )),
  g3a_initialconditions(st_imm_m, quote( 0 * stock__midlen ), quote( 0 * stock__midlen )),
  g3a_initialconditions_normalcv(st_mat),
  g3a_spawn(
    st_mat,
    recruitment_f = g3a_spawn_recruitment_bevertonholt(
      mu = g3_parameterized('spawn_mu', value = 5, by_year = TRUE),
      lambda = g3_parameterized("spawn_lambda", value = 1, by_stock = TRUE) ),
    proportion_f = g3_suitability_exponentiall50(),
    output_stocks = list(st_imm_f, st_imm_m),
    output_ratios = list(
      st_imm_f = quote( g3_param('spawn_ratio', value = 0.5) ),
      st_imm_m = quote( 1 - g3_param('spawn_ratio', value = 0.5) )),
    run_f = quote( cur_step == 1 ) ),
  g3a_age(st_imm_f),
  g3a_age(st_imm_m),
  g3a_age(st_mat) )

# Compile model
model_fn <- g3_to_r(c(actions, list(
  g3a_report_history(actions, c(
      '__offspringnum$',
      '__num$',
      '__wgt$' )))))
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
  model_cpp <- g3_to_tmb(c(actions, list(
    g3a_report_history(actions, c(
        '__offspringnum$',
        '__num$',
        '__wgt$' )))))
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

estimate_l50 <- g3_stock_def(st_mat, "midlen")[[length(g3_stock_def(st_mat, "midlen")) / 2]]
estimate_linf <- max(g3_stock_def(st_mat, "midlen"))

for (spawn_ratio in runif(5)) ok_group(paste0("spawn_ratio: ", spawn_ratio), {
  attr(model_fn, 'parameter_template') |>
    g3_init_val("*.Linf", estimate_linf, spread = 0.2) |>
    g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
    g3_init_val("*.wbeta", 3, optimise = FALSE) |>

    g3_init_val("*.*.l50", estimate_l50, spread = 0.25) |>
    g3_init_val("spawn_ratio", spawn_ratio) |>

    identity() -> params

  r <- attributes(model_fn(params))
  num_spawn <- colSums(r$hist_fish_mat__offspringnum, dims = 3)
  num_m <- colSums(r$hist_fish_imm_m__num, dims = 3)
  num_f <- colSums(r$hist_fish_imm_f__num, dims = 3)

  ok(ut_cmp_equal(
    cumsum(num_spawn * (1 - spawn_ratio)),
    num_m,
    tolerance = 1e-8), "hist_fish_imm_m__num: Cumulative proportion of __offspringnum")
  ok(ut_cmp_equal(
    cumsum(num_spawn * spawn_ratio),
    num_f,
    tolerance = 1e-8), "hist_fish_imm_f__num: Cumulatve proportion of __offspringnum")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})
