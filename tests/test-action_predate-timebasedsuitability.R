if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

comven12 <- g3_fleet("comven12")
st_imm <- g3_stock(c(species = "st", maturity = "imm"), c(10, 20, 30)) |> g3s_age(1, 5)
st_mat <- g3_stock(c(species = "st", maturity = "mat"), c(10, 20, 30)) |> g3s_age(1, 5)
stocks_st <- list(st_imm, st_mat)

actions <- list(
    g3a_time(2000, 2005, step_lengths = c(6,6), project_years = 0),
    g3a_otherfood_normalcv(
        st_imm,
        factor_f = g3_parameterized(
            'of_abund', by_year = TRUE, by_stock = TRUE,
            ifmissing = "of_abund.proj" )),
    g3a_otherfood_normalcv(
        st_mat,
        factor_f = g3_parameterized(
            'of_abund', by_year = TRUE, by_stock = TRUE,
            ifmissing = "of_abund.proj" )),

    g3a_predate_fleet(
        fleet_stock = comven12,
        prey_stocks = stocks_st,
        suitabilities = g3_suitability_exponentiall50(
            l50 = g3_timevariable('com.l50', list(
                "init" = g3_parameterized('com.l50.early'),
                "2003" = g3_parameterized('com.l50.late') ))),
        catchability_f = g3a_predate_catchability_totalfleet(100) ),

    # NB: Dummy parameter so model will compile in TMB
    ~{nll <- nll + g3_param("x", value = 0)} )
full_actions <- c(actions, list(
    g3a_report_detail(actions) ))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_cpp, 'parameter_template') |>
    g3_init_val("st_*.of_abund.#", 1e6) |>

    g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
    g3_init_val("*.Linf", max(g3_stock_def(st_imm, "midlen")), spread = 0.2) |>
    g3_init_val("*.t0", g3_stock_def(st_imm, "minage") - 0.8, spread = 2) |>
    g3_init_val("*.lencv", 0.1, optimise = FALSE) |>
    g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
    g3_init_val("*.wbeta", 3, optimise = FALSE) |>

    g3_init_val("*.*.alpha", 1) |>
    g3_init_val("com.l50.early", 5) |>
    g3_init_val("com.l50.late", 25) |>
    identity() -> params.in
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(gadget3:::ut_cmp_df(as.data.frame(g3_array_agg(r$detail_st_imm_comven12__cons, c("year", "length"))), "
             2000     2001     2002       2003       2004       2005
  10:20  12.17300 12.17300 12.17300 4.7026e-04 4.7026e-04 4.7026e-04
  20:30  86.86000 86.86000 86.86000 9.7822e+01 9.7822e+01 9.7822e+01
  30:Inf  0.96607  0.96607  0.96607 2.1768e+00 2.1768e+00 2.1768e+00
", tolerance = 1e-3), "detail_st_imm_comven12__cons: Suitability changes after 2003")

ok(gadget3:::ut_cmp_df(as.data.frame(g3_array_agg(r$detail_st_mat_comven12__cons, c("year", "length"))), "
             2000     2001     2002       2003       2004       2005
  10:20  12.17300 12.17300 12.17300 4.7026e-04 4.7026e-04 4.7026e-04
  20:30  86.86000 86.86000 86.86000 9.7822e+01 9.7822e+01 9.7822e+01
  30:Inf  0.96607  0.96607  0.96607 2.1768e+00 2.1768e+00 2.1768e+00
", tolerance = 1e-3), "detail_st_mat_comven12__cons: Suitability changes after 2003")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)
