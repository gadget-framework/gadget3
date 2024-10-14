if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st <- g3_stock("st", c(10))
fl <- g3_fleet('fl')

# Define quota for the fleet, with an assessment in spring, application in autumn
fl_quota <- g3_quota(
    g3_quota_hockeyfleet(list(fl), list(st), preyprop_fs = 1),
    start_step = 4L,
    run_revstep = -2L )

actions <- list(
    g3a_time(1990, 1995, c(3,3,3,3)),
    # Define st with steadily collapsing stock
    g3a_otherfood(st, num_f = g3_timeareadata('st_abund', data.frame(
        year = 1990:2050,
        abund = 1e6 - 1e4 * seq(0, 2050-1990)), "abund"), wgt_f = 10),
    # Fleet predates stock
    g3a_predate(
        fl,
        list(st),
        suitabilities = 0.8,
        catchability_f = g3a_predate_catchability_project(
            # Use the quota when projecting, otherwise use landings parameters
            fl_quota,
            g3_parameterized("landings", value = 0, by_year = TRUE, by_predator = TRUE) )),
    NULL )
full_actions <- c(actions, list(
    g3a_report_detail(actions),
    g3a_report_history(actions, "__num$|__wgt$", out_prefix="dend_"),  # NB: Late reporting
    g3a_report_history(actions, "quota_", out_prefix = NULL),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_fn, "parameter_template") |>
    # Project for 30 years
    g3_init_val("project_years", 30) |>
    # Fishing generally occurs in spring/summer, none in winter
    g3_init_val("fl.quota.step.#", c(0.0, 0.5, 0.4, 0.1)) |>
    # Initial landings fixed
    g3_init_val("fl.landings.#", 1e6) |>
    # Hockefleet: harvest rate & trigger biomass
    g3_init_val("fl.hf.harvest_rate", 0.2) |>
    g3_init_val("fl.hf.btrigger", 7.2e6) |>
    identity() -> params.in
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_identical(
    names(which((g3_array_agg(r$dend_st__num * r$dend_st__wgt, "time") < 7.2e6 )))[[1]],
    "2012-02"), "Fall below btrigger at 2012-02")
ok(ut_cmp_equal(as.vector(tail(0 - diff(r$quota_hockeyfleet_fl__var), -1)), as.vector(c(
    `1991:1992` = 16000, `1992:1993` = 16000, `1993:1994` = 16000,
    `1994:1995` = 16000, `1995:1996` = 16000, `1996:1997` = 16000,
    `1997:1998` = 16000, `1998:1999` = 16000, `1999:2000` = 16000,
    `2000:2001` = 16000, `2001:2002` = 16000, `2002:2003` = 16000,
    `2003:2004` = 16000, `2004:2005` = 16000, `2005:2006` = 16000,
    `2006:2007` = 16000, `2007:2008` = 16000, `2008:2009` = 16000,
    `2009:2010` = 16000, `2010:2011` = 16000, `2011:2012` = 16000,
    `2012:2013` = 21561, `2013:2014` = 31181, `2014:2015` = 29986,
    `2015:2016` = 29711, `2016:2017` = 29356, `2017:2018` = 29006,
    `2018:2019` = 28655, `2019:2020` = 28303, `2020:2021` = 27949,
    `2021:2022` = 27595, `2022:2023` = 27239, `2023:2024` = 26883,
    `2024:2025` = 26525, `2025:2026` = 26166
    )), tolerance = 1e-4), "quota_hockeyfleet_fl__var: Constant until btrigger, then starts dropping")

x <- g3_array_agg(r$detail_st_fl__cons, year = 1990:2024, "step")
ok(ut_cmp_equal(
    as.vector(x / sum(x)),
    c(0, 0.5, 0.4, 0.1),
    tolerance = 3e-3 ), "detail_st_fl__cons: Follows seasonal pattern")

fishingyear_cons <- c(0, tail(g3_array_agg(r$detail_st_fl__cons, year = 1990:2024, "year", step = 1:3), -1)) +
    g3_array_agg(r$detail_st_fl__cons, year = 1990:2024, "year", step = 4)
fishingyear_quota <- head(r$quota_hockeyfleet_fl__var, -1)
ok(ut_cmp_equal(
    fishingyear_cons[[1]],
    1e6 * 0.1), "fishingyear_cons: First year only contains an autumn")
ok(ut_cmp_equal(
    as.vector(fishingyear_cons[2:6]),
    rep(1e6, 5)), "fishingyear_cons: Outside projections we consume landings rate")
ok(ut_cmp_equal(
    as.vector( fishingyear_cons[7:length(fishingyear_cons)] ),
    as.vector( r$quota_hockeyfleet_fl__var[7:length(fishingyear_cons)] / 1.001 ),
    tolerance = 9e-3), "fishingyear_cons: Consume based on quota")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)
