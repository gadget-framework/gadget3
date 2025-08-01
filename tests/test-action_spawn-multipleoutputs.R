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
  gadget3:::g3a_initialconditions_manual(st_imm_f, quote( 0 * stock__midlen ), quote( 0 * stock__midlen )),
  gadget3:::g3a_initialconditions_manual(st_imm_m, quote( 0 * stock__midlen ), quote( 0 * stock__midlen )),
  g3a_initialconditions_normalcv(st_mat),
  g3a_spawn(
    st_mat,
    recruitment_f = g3a_spawn_recruitment_bevertonholt(
      mu = g3_parameterized('spawn_mu', value = 5, by_year = TRUE),
      lambda = g3_parameterized("spawn_lambda", value = 1, by_stock = TRUE) ),
    proportion_f = g3_suitability_exponentiall50(),
    weightloss_args = list(
        abs_loss = g3_parameterized("spawn.weightabsloss", value = 0),
        rel_loss = g3_parameterized("spawn.weightrelloss", value = 0) ),
    output_stocks = list(st_imm_f, st_imm_m),
    output_ratios = list(
      st_imm_f = quote( g3_param('spawn_ratio', value = 0.5) ),
      st_imm_m = quote( 1 - g3_param('spawn_ratio', value = 0.5) )),
    run_f = quote( cur_step == 1 ) ),
  g3a_age(st_imm_f),
  g3a_age(st_imm_m),
  g3a_age(st_mat),
  # NB: Only required for testing
  gadget3:::g3l_test_dummy_likelihood() )

# Compile model
model_fn <- g3_to_r(c(actions, list(
  g3a_report_history(actions, c(
      '__offspringnum$',
      '__num$',
      '__wgt$' )))))
model_cpp <- g3_to_tmb(c(actions, list(
  g3a_report_history(actions, c(
      '__offspringnum$',
      '__num$',
      '__wgt$' )))))
  # model_cpp <- edit(model_cpp)

estimate_l50 <- g3_stock_def(st_mat, "midlen")[[length(g3_stock_def(st_mat, "midlen")) / 2]]

for (spawn_ratio in runif(5)) ok_group(paste0("spawn_ratio: ", spawn_ratio), {
  attr(model_fn, 'parameter_template') |>
    g3_init_val("fish_imm_m.Linf", g3_stock_def(st_mat, "midlen")[[1]]) |>
    g3_init_val("fish_imm_f.Linf", g3_stock_def(st_mat, "midlen")[[3]]) |>
    g3_init_val("fish_mat.Linf", g3_stock_def(st_mat, "midlen")[[5]]) |>
    g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
    g3_init_val("*.wbeta", 3, optimise = FALSE) |>

    g3_init_val("*.*.l50", estimate_l50, spread = 0.25) |>
    g3_init_val("spawn_ratio", spawn_ratio) |>

    identity() -> params

  r <- attributes(model_fn(params))
  num_spawn <- colSums(r$hist_fish_mat__offspringnum, dims = 3)
  num_m <- colSums(r$hist_fish_imm_m__num, dims = 3)
  num_f <- colSums(r$hist_fish_imm_f__num, dims = 3)

  # Make sure lengthgroup structure varies between m & f (i.e. used appropriate Linfs)
  for (t in dimnames(r$hist_fish_imm_m__num)$time) {
    ok(ut_cmp_identical(
        names(which.max(r$hist_fish_imm_m__num[,1,1,time = t])),
        "5:10"), paste0("r$hist_fish_imm_m__num[,1,1,time = ", t, "]: Shortest lengthgroup most populated"))
    ok(ut_cmp_identical(
        names(which.max(r$hist_fish_imm_f__num[,1,1,time = t])),
        "10:15"), paste0("r$hist_fish_imm_f__num[,1,1,time = ", t, "]: Second lengthgroup most populated"))
  }

  ok(ut_cmp_equal(
    cumsum(num_spawn * (1 - spawn_ratio)),
    num_m,
    tolerance = 1e-8), "hist_fish_imm_m__num: Cumulative proportion of __offspringnum")
  ok(ut_cmp_equal(
    cumsum(num_spawn * spawn_ratio),
    num_f,
    tolerance = 1e-8), "hist_fish_imm_f__num: Cumulatve proportion of __offspringnum")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})

ok_group("weightloss") ################
attr(model_fn, 'parameter_template') |>
  g3_init_val("fish_imm_m.Linf", g3_stock_def(st_mat, "midlen")[[1]]) |>
  g3_init_val("fish_imm_f.Linf", g3_stock_def(st_mat, "midlen")[[3]]) |>
  g3_init_val("fish_mat.Linf", g3_stock_def(st_mat, "midlen")[[5]]) |>
  g3_init_val("*.walpha", 1, optimise = FALSE) |>
  g3_init_val("*.wbeta", 1, optimise = FALSE) |>

  g3_init_val("*.*.l50", estimate_l50, spread = 0.25) |>
  g3_init_val("spawn_ratio", spawn_ratio) |>
  g3_init_val('spawn.weightabsloss', 0.2) |>

  identity() -> params
r <- sapply(attributes(model_fn(params)), drop)

# NB: We can't check spawn.weightabsloss = 0, since inaccuracy in ratio_log_vec() makes a mess

ok(ut_cmp_equal(round(diff(colSums(r$hist_fish_mat__wgt[,8,])), 1)[seq(1, 31, by = 2)], c(
    "1980-02" = 0,
    "1981-02" = 0,
    "1982-02" = 0,
    "1983-02" = 0,
    "1984-02" = 0,
    "1985-02" = 0,
    "1986-02" = 0,
    "1987-02" = 0,
    "1988-02" = 0,
    "1989-02" = 0,
    "1990-02" = 0,
    "1991-02" = 0,
    "1992-02" = 0,
    "1993-02" = 0,
    "1994-02" = 0,
    "1995-02" = 0 )), "r$hist_fish_mat__wgt[,8,]: No weightloss outside spawning steps")
ok(ut_cmp_equal(round(diff(colSums(r$hist_fish_mat__wgt[,8,])), 1)[seq(2, 30, by = 2)], c(
    "1981-01" = -0.4,
    "1982-01" = -0.5,
    "1983-01" = -0.5,
    "1984-01" = -0.5,
    "1985-01" = -0.4,
    "1986-01" = -0.3,
    "1987-01" = -0.2,
    "1988-01" = -0.4,
    "1989-01" = -0.4,
    "1990-01" = -0.4,
    "1991-01" = -0.4,
    "1992-01" = -0.4,
    "1993-01" = -0.4,
    "1994-01" = -0.4,
    "1995-01" = -0.4 )), "r$hist_fish_mat__wgt[,8,]: Total weight loss roughly 0.4 (first lengthgroup not spawning, half of remaining 4 spawning)")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
