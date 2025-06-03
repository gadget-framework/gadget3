if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st <- g3_stock("st", c(10))
stocks_fl <- list(
    a = g3_fleet(c(type = 'fl', 'a')),
    # NB: We don't use this one, just added to ensure we alter names accordingly
    b = g3_fleet(c(type = 'fl', 'b')) )

# Define quota for the fleet, with an assessment in spring, application in autumn
fl_quota <- g3_quota(
    g3_quota_hockeyfleet(stocks_fl, list(st), preyprop_fs = 1),
    start_step = 4L,
    run_revstep = -2L )

actions <- list(
    g3a_time(1990, 1995, c(3,3,3,3)),
    # Define st with steadily collapsing stock
    g3a_otherfood(st, num_f = g3_timeareadata('st_abund', data.frame(
        year = rep(1990:2050, each = 4),
        step = rep(1:4, times = length(1990:2050)),
        abund = 1e6 - 1e4 * seq(0, 2050-1990 + 0.75, by = 0.25)), "abund"), wgt_f = 10),
    # Fleet predates stock
    g3a_predate(
        stocks_fl$a,
        list(st),
        suitabilities = 0.8,
        catchability_f = g3a_predate_catchability_project(
            # Use the quota when projecting, otherwise use landings parameters
            fl_quota,
            g3_parameterized("landings", value = 0, by_year = TRUE, by_predator = TRUE) )),
     # NB: Only required for testing
     gadget3:::g3l_test_dummy_likelihood() )
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
    g3_init_val("fl_a.cons.step.#", c(0.0, 0.5, 0.4, 0.1)) |>
    # Initial landings fixed
    g3_init_val("fl_a.landings.#", 1e6) |>
    # Hockefleet: harvest rate & trigger biomass
    g3_init_val("fl.hf.harvest_rate", 0.2) |>
    g3_init_val("fl.hf.btrigger", 7.8e6) |>
    identity() -> params.in
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

to_vect <- function (x) structure(as.vector(x), names = dimnames(x)[[1]])

ok(ut_cmp_identical(
    # NB: dend because assessment comes after consumption
    # NB: step=2 comes from start_step = 4 / run_revstep = -2
    names(which((g3_array_agg(r$dend_st__num * r$dend_st__wgt, "year", step = 2) < 7.8e6 )))[[1]],
    "2005"), "Fall below btrigger at 2005 assessment step")

ok(ut_cmp_equal(
    to_vect(r$quota_hockeyfleet_fl__var), c(
    `1990:1990` = 0, `1990:1991` = 0.2, `1991:1992` = 0.2, `1992:1993` = 0.2,
    `1993:1994` = 0.2, `1994:1995` = 0.2, `1995:1996` = 0.2, `1996:1997` = 0.2,
    `1997:1998` = 0.2, `1998:1999` = 0.2, `1999:2000` = 0.2, `2000:2001` = 0.2,
    `2001:2002` = 0.2, `2002:2003` = 0.2, `2003:2004` = 0.2, `2004:2005` = 0.2,
    `2005:2006` = 0.199819, `2006:2007` = 0.19758, `2007:2008` = 0.195411,
    `2008:2009` = 0.193231, `2009:2010` = 0.191048, `2010:2011` = 0.18886,
    `2011:2012` = 0.186669, `2012:2013` = 0.184473, `2013:2014` = 0.182274,
    `2014:2015` = 0.18007, `2015:2016` = 0.177861, `2016:2017` = 0.175649,
    `2017:2018` = 0.173432, `2018:2019` = 0.171212, `2019:2020` = 0.168986,
    `2020:2021` = 0.166757, `2021:2022` = 0.164524, `2022:2023` = 0.162286,
    `2023:2024` = 0.160044, `2024:2025` = 0.157797, `2025:2026` = 0.155546
    ), tolerance = 5e-5), "quota_hockeyfleet_fl__var Constant until btrigger, then starts dropping")

x <- g3_array_agg(r$detail_st_fl_a__cons, year = 1990:1995, "step")
ok(ut_cmp_equal(
    as.vector(x),
    c(6e6, 6e6, 6e6, 6e6),
    tolerance = 3e-7 ), "detail_st_fl_a__cons: Specified by parameters (before projecting)")
x <- g3_array_agg(r$detail_st_fl_a__cons, year = 1996:2024, "step")
ok(ut_cmp_equal(
    as.vector(x / sum(x)),
    c(0, 0.5, 0.4, 0.1),
    tolerance = 4e-3 ), "detail_st_fl_a__cons: Follows seasonal pattern (whilst projecting)")

fishingyear_cons <- c(0, tail(g3_array_agg(r$detail_st_fl_a__cons, year = 1990:2024, "year", step = 1:3), -1)) +
    g3_array_agg(r$detail_st_fl_a__cons, year = 1990:2024, "year", step = 4)
ok(ut_cmp_equal(
    fishingyear_cons[[1]],
    1e6), "fishingyear_cons: First year only contains an autumn")
ok(ut_cmp_equal(
    as.vector(fishingyear_cons[2:6]),
    rep(4e6, 5)), "fishingyear_cons: Outside projections we consume landings rate")

ok(ut_cmp_equal(
    as.vector(tail(g3_array_agg(r$detail_st_fl_a__cons, "year", step = 1), -6)),
    as.vector(tail(to_vect(g3_array_agg(
        r$dstart_st__num * r$dstart_st__wgt * 0.8, "year", step = 1) * head(r$quota_hockeyfleet_fl__var * 0, -1)), -6)),
    tolerance = 1e-7), "detail_st_fl_a__cons[step = 1]: consumption based on quota")
ok(ut_cmp_equal(
    as.vector(tail(g3_array_agg(r$detail_st_fl_a__cons, "year", step = 2), -6)),
    as.vector(tail(to_vect(g3_array_agg(
        r$dstart_st__num * r$dstart_st__wgt * 0.8, "year", step = 2) * head(r$quota_hockeyfleet_fl__var * 0.5, -1)), -6)),
    tolerance = 1e-7), "detail_st_fl_a__cons[step = 2]: consumption based on quota")
ok(ut_cmp_equal(
    as.vector(tail(g3_array_agg(r$detail_st_fl_a__cons, "year", step = 3), -6)),
    as.vector(tail(to_vect(g3_array_agg(
        r$dstart_st__num * r$dstart_st__wgt * 0.8, "year", step = 3) * head(r$quota_hockeyfleet_fl__var * 0.4, -1)), -6)),
    tolerance = 1e-7), "detail_st_fl_a__cons[step = 3]: consumption based on quota")
ok(ut_cmp_equal(
    as.vector(tail(g3_array_agg(r$detail_st_fl_a__cons, "year", step = 4), -6)),
    as.vector(tail(to_vect(g3_array_agg(
        r$dstart_st__num * r$dstart_st__wgt * 0.8, "year", step = 4) * tail(r$quota_hockeyfleet_fl__var * 0.1, -1)), -6)),
    tolerance = 1e-7), "detail_st_fl_a__cons[step = 4]: consumption based on quota")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)

ok_group("g3_quota_hockeyfleet: selectivity function preyprop_fs", local({
    g3_suitability_blueling <- function(){
        ~1/(1+exp(-0.23*(stock__midlen - 91.5856)))
    }
    stocks <- list(
        g3_stock(c("st", "male"), 0:10 * 10) |> g3s_age(1, 3),
        g3_stock(c("st", "female"), 0:10 * 10) |> g3s_age(1, 3))

    q_hf <- g3_quota_hockeyfleet(
            stocks_fl,
            stocks,
            preyprop_fs = g3_suitability_blueling() )

    actions <- list(
        g3a_time(1990, 1991),
        lapply(stocks, g3a_initialconditions_normalcv),
        g3_step(g3_formula({
            REPORT(q_hf)
        }, q_hf = q_hf )),
        # NB: Only required for testing
        gadget3:::g3l_test_dummy_likelihood() )
    full_actions <- c(actions, list(
        g3a_report_detail(actions),
        NULL))
    model_fn <- g3_to_r(full_actions)
    model_cpp <- g3_to_tmb(full_actions)

    attr(model_fn, "parameter_template") |>
        g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
        g3_init_val("st_male.Linf", 80, spread = 0.2) |>
        g3_init_val("st_female.Linf", 105, spread = 0.2) |>
        g3_init_val("*.t0", g3_stock_def(stocks[[1]], "minage") - 0.8, spread = 2) |>
        g3_init_val("*.lencv", 0.1, optimise = FALSE) |>
        g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
        g3_init_val("*.wbeta", 3, optimise = FALSE) |>
        g3_init_val("fl.hf.btrigger", 1000, optimise = FALSE) |>
        g3_init_val("fl.hf.harvest_rate", 1000, optimise = FALSE) |>
        identity() -> params.in
    nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

    # Work out expected ssb, show we've used it to calculate results
    expectedssb <-
        sum(r$dstart_st_male__num[,,1] * r$dstart_st_male__wgt[,,1] * 1/(1+exp(-0.23*((0:10 * 10 + 5) - 91.5856)))) +
        sum(r$dstart_st_female__num[,,1] * r$dstart_st_female__wgt[,,1] * 1/(1+exp(-0.23*((0:10 * 10 + 5) - 91.5856))))
    ok(ut_cmp_equal(
        r$q_hf,
        expectedssb), "r$q_hf: Matches expectedssb, as fl.hf.btrigger is greater than ssb")
    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)

    params.in |>
        g3_init_val("fl.hf.btrigger", expectedssb + 50, optimise = FALSE) |>
        identity() -> params.in
    nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)
    ok(ut_cmp_equal(
        r$q_hf,
        1000 * (expectedssb / (expectedssb + 50)) ), "r$q_hf: expectedssb below btrigger, so scaling result")
    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)
}))
