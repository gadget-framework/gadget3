if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st_imm <- g3_stock(c(species = "st", maturity = "imm"), c(10, 20, 30)) |> g3s_age(1, 5)
st_mat <- g3_stock(c(species = "st", maturity = "mat"), c(10, 20, 30)) |> g3s_age(3, 15)
stocks_st <- list(st_imm, st_mat)
fl1 <- g3_fleet(c(type = "surv", 1))
fl2 <- g3_fleet(c(type = "surv", 2))
stocks_fl <- list(fl1, fl2)

assess_outputs <- list()

# Assessment function, gets pulled into model by g3_formula
assess_fn <- function (
        # The start model year, as defined by g3a_time,
        start_year,
        # The current model year, as defined by g3a_time,
        cur_year,
        # Nested list of pred -> prey -> detail_prey_pred__cons
        cons,
        # List of prey -> dstart_prey__num
        abund,
        # List of prey -> dstart_prey__wgt
        meanwgt ) {
    years <- seq(start_year, cur_year)

    ## catch in numbers at age over all fleets
    cn <- g3_array_combine(lapply(names(cons), function (pred_n) lapply(names(cons[[pred_n]]), function (prey_n) {
        g3_array_agg(cons[[pred_n]][[prey_n]] / pmax(meanwgt[[prey_n]], 0.001), c("age", "year"), year = years)
    })))

    ## Abundance by age at step 1
    smb <- g3_array_combine(lapply(names(abund), function (prey_n) {
        g3_array_agg(abund[[prey_n]], c("age", "year"), year = years, step = 1)
    }))

    ## total abundance by maturity at step 1 by age
    immtotal <- g3_array_agg(abund$st_imm, c("age", "year"), year = years, step = 1)
    mattotal <- g3_array_agg(abund$st_mat, c("age", "year"), year = years, step = 1)

    ## Log outputs in globalenv
    assess_outputs[[as.character(cur_year)]] <<- list(
        cn = cn,
        smb = smb,
        immtotal = immtotal,
        mattotal = mattotal )

    ## Perform the assessment
    (sum(immtotal) + sum(mattotal)) / 1e10
}

fl_quota <- g3_quota(
    g3_quota_assess(stocks_fl, stocks_st, g3_formula(
        assess_fn(start_year, cur_year, cons, abund, meanwgt),
        assess_fn = assess_fn )),
    run_revstep = -3L,  # Run in spring
    start_step = 4L,  # Skip first year, so we run 2000.2005
    year_length = 5L )  # Run every years

actions <- list(
    g3a_time(1990, 1994, c(3,3,3,3)),
    g3a_otherfood_normalcv(
        st_imm,
        factor_f = g3_timeareadata('st_abund', data.frame(
            year = 1990:2050,
            abund = 1e6 - 1e4 * seq(0, 2050-1990)), "abund")),
    g3a_otherfood_normalcv(
        st_mat,
        factor_f = g3_timeareadata('st_abund', data.frame(
            year = 1990:2050,
            abund = 1e6 - 1e4 * seq(0, 2050-1990)), "abund")),
    g3a_predate(
        fl1,
        stocks_st,
        catchability_f = g3a_predate_catchability_project(
            fl_quota,
            g3_parameterized("landings", value = 0, by_year = TRUE, by_predator = TRUE) ),
        g3_suitability_exponentiall50(by_stock = 'species') ),
    g3a_predate(
        fl2,
        stocks_st,
        catchability_f = g3a_predate_catchability_project(
            fl_quota,
            g3_parameterized("landings", value = 0, by_year = TRUE, by_predator = TRUE) ),
        g3_suitability_exponentiall50(by_stock = 'species') ),
    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood() )
full_actions <- c(actions, list(
    g3a_report_detail(actions),
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    g3a_report_history(actions, 'quota_.*', out_prefix = NULL),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_cpp, 'parameter_template') |>
    # Project for 30 years
    g3_init_val("project_years", 30) |>

    g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
    g3_init_val("*.Linf", max(g3_stock_def(st_imm, "midlen")), spread = 0.2) |>
    g3_init_val("*.t0", g3_stock_def(st_imm, "minage") - 0.8, spread = 2) |>
    g3_init_val("*.lencv", 0.1, optimise = FALSE) |>
    g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
    g3_init_val("*.wbeta", 3, optimise = FALSE) |>
    g3_init_val("*.*.l50", g3_stock_def(st_imm, "midlen")[[length(g3_stock_def(st_imm, "midlen")) / 2]], spread = 0.25) |>

    # surv_1 takes 80% of the quota
    g3_init_val("surv_1.quota.prop", 0.8) |>
    g3_init_val("surv_2.quota.prop", 0.2) |>
    # surv_1 active at 1, surv_2 active at 2/3
    g3_init_val("surv_1.cons.step.#", c(1, 0, 0, 0)) |>
    g3_init_val("surv_2.cons.step.#", c(0, 0.6, 0.4, 0)) |>

    identity() -> params.in
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_identical(
    names(assess_outputs),
    c("1995", "2000", "2005", "2010", "2015", "2020") ), "assess_outputs: Ran assessment according to fishing year")

ok(ut_cmp_identical(sapply(assess_outputs, function (x) dimnames(x$mattotal)$year), list(
    "1995" = as.character(seq(1990, 1995)),
    "2000" = as.character(seq(1990, 2000)),
    "2005" = as.character(seq(1990, 2005)),
    "2010" = as.character(seq(1990, 2010)),
    "2015" = as.character(seq(1990, 2015)),
    "2020" = as.character(seq(1990, 2020)) )), "assess_outputs$mattotal: Correct years aggreagated")

for (yr in seq(2000, 2020, by = 5)) ok_group(paste0("Year = ", yr), {
    if (yr > 2000) {
        x <- assess_outputs[[as.character(yr - 5)]]$cn[,as.character(seq(1990, yr - 6)), drop = FALSE]
        y <- assess_outputs[[as.character(yr)]]$cn[,as.character(seq(1990, yr - 6)), drop = FALSE]
        ok(ut_cmp_equal(x, y), paste0("assess_outputs$cn$", yr, ": Matches previous year, bar final year"))
        ok(all(is.na(assess_outputs[[as.character(yr)]]$cn[,as.character(yr),drop = FALSE])), paste0("assess_outputs$cn$", yr, ": Final year NA"))
    }
    ok(ut_cmp_equal(
        assess_outputs[[as.character(yr - 5)]]$smb,
        assess_outputs[[as.character(yr)]]$smb[,as.character(seq(1990, yr - 5)), drop = FALSE] ), paste0("assess_outputs$smb$", yr, ": Matches previous year"))
    ok(ut_cmp_equal(
        assess_outputs[[as.character(yr - 5)]]$immtotal,
        assess_outputs[[as.character(yr)]]$immtotal[,as.character(seq(1990, yr - 5)), drop = FALSE] ), paste0("assess_outputs$immtotal$", yr, ": Matches previous year"))
    ok(ut_cmp_equal(
        assess_outputs[[as.character(yr - 5)]]$mattotal,
        assess_outputs[[as.character(yr)]]$mattotal[,as.character(seq(1990, yr - 5)), drop = FALSE] ), paste0("assess_outputs$mattotal$", yr, ": Matches previous year"))
})

ok(ut_cmp_equal(
    as.vector(g3_array_agg(r$detail_st_imm_surv_1__cons / r$dstart_st_imm__wgt, c("year"), age = 5, year = 1990:2019) +
    g3_array_agg(r$detail_st_mat_surv_1__cons / r$dstart_st_mat__wgt, c("year"), age = 5, year = 1990:2019) +
    g3_array_agg(r$detail_st_imm_surv_2__cons / r$dstart_st_imm__wgt, c("year"), age = 5, year = 1990:2019) +
    g3_array_agg(r$detail_st_mat_surv_2__cons / r$dstart_st_mat__wgt, c("year"), age = 5, year = 1990:2019) +
    0),
    as.vector(assess_outputs[["2020"]]$cn["age5", as.character(1990:2019)]) ), "assess_outputs$2020$cn: Matches reporting output")

# NB: Not testing TMB, doesn't make sense to
