if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st_imm <- g3_stock(c(species = "st", maturity = "imm"), c(10, 20, 30)) |> g3s_age(1, 5)
st_mat <- g3_stock(c(species = "st", maturity = "mat"), c(10, 20, 30)) |> g3s_age(1, 5)
stocks_st <- list(st_imm, st_mat)
fl1 <- g3_fleet(c(type = "surv", 1))
fl2 <- g3_fleet(c(type = "surv", 2))
stocks_fl <- list(fl1, fl2)

fl_quota <- gadget3:::g3_quota(
    # NB: Quota covers both surveys
    gadget3:::g3_quota_hockeyfleet(
        stocks_fl,
        list(st_mat) ),
    lag = 4,  # Skip 6 steps before applying
    run_step = 1 )

actions <- list(
    g3a_time(1990, 1994, c(3,3,3,3)),
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
    g3a_predate(
        fl1,
        stocks_st,
        catchability_f = gadget3:::g3a_predate_catchability_project(
            fl_quota,
            g3_parameterized("landings", value = 0, by_year = TRUE, by_predator = TRUE),
            active_at = 1),
        g3_suitability_exponentiall50(by_stock = 'species') ),
    g3a_predate(
        fl2,
        stocks_st,
        catchability_f = gadget3:::g3a_predate_catchability_project(
            fl_quota,
            g3_parameterized("landings", value = 0, by_year = TRUE, by_predator = TRUE),
            active_at = c(2,4)),
        g3_suitability_exponentiall50(by_stock = 'species') ),
    # NB: Dummy parameter so model will compile in TMB
    quote( nll <- nll + g3_param("x", value = 0) ) )
full_actions <- c(actions, list(
    g3a_report_detail(actions),
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    g3a_report_history(actions, 'quota_.*', out_prefix = NULL),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_cpp, 'parameter_template') |>
    g3_init_val("project_years", 10) |>
    g3_init_val("st_*.of_abund.#", runif(5 * 2, 1e5, 1e6)) |>
    g3_init_val("st_*.of_abund.proj", runif(2, 1e5, 1e6)) |>

    g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
    g3_init_val("*.Linf", max(g3_stock_def(st_imm, "midlen")), spread = 0.2) |>
    g3_init_val("*.t0", g3_stock_def(st_imm, "minage") - 0.8, spread = 2) |>
    g3_init_val("*.lencv", 0.1, optimise = FALSE) |>
    g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
    g3_init_val("*.wbeta", 3, optimise = FALSE) |>
    g3_init_val("st_*.of_abund.proj", runif(2, 1e5, 1e6)) |>

    g3_init_val("*.*.l50", g3_stock_def(st_imm, "midlen")[[length(g3_stock_def(st_imm, "midlen")) / 2]], spread = 0.25) |>
    # surv_1 works in step_1, surv_2 works in step_2
    g3_init_val("surv_1.landings.#", runif(5, 1e5, 1e6)) |>
    g3_init_val("surv_2.landings.#", runif(5, 1e5, 1e6)) |>
    g3_init_val("surv.hf.harvest_rate", runif(1, 1e5, 1e6)) |>
    g3_init_val("surv.hf.btrigger", runif(1, 1e5, 1e6)) |>

    identity() -> params.in
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)




gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)
