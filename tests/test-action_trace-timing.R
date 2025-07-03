if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

comven12 <- g3_fleet("comven12")
st_imm <- g3_stock(c(species = "st", maturity = "imm"), 1:10 * 10) |> g3s_age(1, 20)
st_mat <- g3_stock(c(species = "st", maturity = "mat"), 1:10 * 10) |> g3s_age(1, 20)
stocks_st <- list(st_imm, st_mat)

actions <- list(
    g3a_time(2000, 2010, step_lengths = c(6,6), project_years = 0),
    g3a_growmature(st_imm,
        g3a_grow_impl_bbinom(
            maxlengthgroupgrowth = 4L ),
        maturity_f = g3a_mature_continuous(),
        output_stocks = list(st_mat),
        transition_f = ~TRUE ),
    g3a_naturalmortality(st_imm),
    g3a_initialconditions_normalcv(st_imm),
    g3a_renewal_normalcv(st_imm),
    g3a_age(st_imm, output_stocks = list(st_mat)),
    g3a_growmature(st_mat,
        g3a_grow_impl_bbinom(
            maxlengthgroupgrowth = 4L )),
    g3a_naturalmortality(st_mat),
    g3a_initialconditions_normalcv(st_mat),
    g3a_age(st_mat),

    g3a_predate_fleet(
        fleet_stock = comven12,
        prey_stocks = stocks_st,
        suitabilities = g3_suitability_exponentiall50(
            l50 = g3_timevariable('com.l50', list(
                "init" = g3_parameterized('com.l50.early'),
                "2003" = g3_parameterized('com.l50.late') ))),
        catchability_f = g3a_predate_catchability_totalfleet(100) ),
    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood() )
full_actions <- c(actions, list(
    gadget3:::g3a_trace_timings(actions),
    g3a_report_detail(actions) ))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_cpp, 'parameter_template') |>
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

ok(ut_cmp_identical(
    unique(strtrim(dimnames(r$trace_timings)[[1]], 4)),
    c("-01:", "000:", "001", "003:", "004:", "005:", "007:", "008:", "010:", "012:") ), "trace_timings: One dimension is action IDs")
ok(all(r$trace_timings[,"min"] <= r$trace_timings[,"mean"]), "trace_timings: min less than mean")
ok(any(r$trace_timings[,"min"] != r$trace_timings[,"mean"]), "trace_timings: some mean/mins don't match")
ok(all(r$trace_timings[,"mean"] <= r$trace_timings[,"max"]), "trace_timings: mean less than max")
ok(any(r$trace_timings[,"mean"] != r$trace_timings[,"max"]), "trace_timings: some mean/maxs don't match")
ok(ut_cmp_equal(
    r$trace_timings[,"total"] / 22,
    r$trace_timings[,"mean"]), "trace_timings: total & mean match")

# NB: Arrays only roughly match, obviously timing should be different
gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in, tolerance = 1e-2)
