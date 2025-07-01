library(magrittr)
library(unittest)

library(gadget3)

prey_a <- g3_stock('prey_a', seq(1, 10))
prey_b <- g3_stock('prey_b', seq(1, 10))
fleet_totalfleet <- g3_fleet('fleet_totalfleet')
fleet_numberfleet <- g3_fleet('fleet_numberfleet')
fleet_linearfleet <- g3_fleet('fleet_linearfleet')
fleet_effortfleet <- g3_fleet('fleet_effortfleet')
fleet_quotafleet <- g3_fleet('fleet_quotafleet')

actions <- list(
    g3a_time(2000, 2005, step_lengths = c(6,6), project_years = 0),
    g3a_initialconditions(prey_a, ~1e10 * prey_a__midlen, ~100 * prey_a__midlen),
    g3a_initialconditions(prey_b, ~2e10 * prey_b__midlen, ~200 * prey_b__midlen),
    g3a_predate_fleet(
        fleet_totalfleet,
        list(prey_a, prey_b),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1),
        catchability_f = g3a_predate_catchability_totalfleet(10)),
    g3a_predate_fleet(
        fleet_numberfleet,
        list(prey_a, prey_b),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1),
        catchability_f = g3a_predate_catchability_numberfleet(10)),
    g3a_predate_fleet(
        fleet_linearfleet,
        list(prey_a, prey_b),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1),
        catchability_f = g3a_predate_catchability_linearfleet(1e-5)),
    g3a_predate_fleet(
        fleet_effortfleet,
        list(prey_a, prey_b),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1),
        catchability_f = g3a_predate_catchability_effortfleet(list(prey_a = 1e-3, prey_b= 1e-4), 10)),
    g3a_predate_fleet(
        fleet_quotafleet,
        list(prey_a, prey_b),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1),
        catchability_f = g3a_predate_catchability_quotafleet(
            data.frame(
                biomass = c(100, Inf),
                quota = I(list(~g3_param('quota.low', value = 1e-9), ~g3_param('quota.high', value = 1e-5)))),
            E = 2,
            recalc_f = ~cur_step == 1)),
    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood() )
actions <- c(actions, list(
    g3a_report_detail(actions) ))

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
model_cpp <- g3_to_tmb(actions, trace = FALSE)

ok_group("Catchability", {
    params <- attr(model_fn, 'parameter_template')

    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    # Make sure everything is internally consistent
    ok(all(r$report_a__consratio > 0.9499), "report_a__consratio: No overconsumption")
    ok(all(r$report_b__consratio > 0.9499), "report_b__consratio: No overconsumption")
    for (time_idx in head(seq_along(dimnames(r$dstart_prey_b__num)$time), -1)) {
        time_lbl <- dimnames(r$dstart_prey_b__num)$time[[time_idx]]
        ok(ut_cmp_equal(
             r$dstart_prey_a__num[,time = time_idx] * r$dstart_prey_a__wgt[,time = time_idx] - 
                 r$detail_prey_a_fleet_totalfleet__cons[,time = time_idx] -
                 r$detail_prey_a_fleet_numberfleet__cons[,time = time_idx] -
                 r$detail_prey_a_fleet_linearfleet__cons[,time = time_idx] -
                 r$detail_prey_a_fleet_effortfleet__cons[,time = time_idx] -
                 r$detail_prey_a_fleet_quotafleet__cons[,time = time_idx],
             r$dstart_prey_a__num[,time = time_idx + 1] * r$dstart_prey_a__wgt[,time = time_idx + 1],
             tolerance = 1e-6), paste0(time_lbl, "/prey_a: Consumption and abundance consistent"))
        ok(ut_cmp_equal(
             r$dstart_prey_b__num[,time = time_idx] * r$dstart_prey_b__wgt[,time = time_idx] - 
                 r$detail_prey_b_fleet_totalfleet__cons[,time = time_idx] -
                 r$detail_prey_b_fleet_numberfleet__cons[,time = time_idx] -
                 r$detail_prey_b_fleet_linearfleet__cons[,time = time_idx] -
                 r$detail_prey_b_fleet_effortfleet__cons[,time = time_idx] -
                 r$detail_prey_b_fleet_quotafleet__cons[,time = time_idx],
             r$dstart_prey_b__num[,time = time_idx + 1] * r$dstart_prey_b__wgt[,time = time_idx + 1],
             tolerance = 1e-6), paste0(time_lbl, "/prey_b: Consumption and abundance consistent"))

        ok(ut_cmp_equal(
            colSums(r$detail_prey_a_fleet_totalfleet__cons + r$detail_prey_b_fleet_totalfleet__cons)[[time_idx]],
            10,
            tolerance = 1e6), paste0(time_lbl, "/totalfleet: Total consumption adds to 10"))
        ok(ut_cmp_equal(
            colSums(r$detail_prey_a_fleet_numberfleet__cons / r$dstart_prey_a__wgt + r$detail_prey_b_fleet_numberfleet__cons / r$dstart_prey_b__wgt)[[time_idx]],
            10,
            tolerance = 1e6), paste0(time_lbl, "/numberfleet: Total consumption in numbers adds to 10"))

        ok(ut_cmp_equal(
            r$detail_prey_a_fleet_linearfleet__cons[,time_idx],
            (r$dstart_prey_a__num * r$dstart_prey_a__wgt * 0.5 * 0.1 * 1e-5)[,time_idx],
            tolerance = 1e6), paste0(time_lbl, "/prey_a/linearfleet: Total consumption proportion of overall stock"))

        ok(ut_cmp_equal(
            r$detail_prey_b_fleet_linearfleet__cons[,time_idx],
            (r$dstart_prey_b__num * r$dstart_prey_b__wgt * 0.5 * 0.1 * 1e-5)[,time_idx],
            tolerance = 1e6), paste0(time_lbl, "/prey_b/linearfleet: Total consumption proportion of overall stock"))

        ok(ut_cmp_equal(
            r$detail_prey_a_fleet_effortfleet__cons[,time_idx],
            (r$dstart_prey_a__num * r$dstart_prey_a__wgt * 0.5 * 0.1 * 1e-3 * 10)[,time_idx],
            tolerance = 1e6), paste0(time_lbl, "/prey_a/effortfleet: Total consumption proportion of overall stock (using prey catchability)"))
        ok(ut_cmp_equal(
            r$detail_prey_b_fleet_effortfleet__cons[,time_idx],
            (r$dstart_prey_b__num * r$dstart_prey_b__wgt * 0.5 * 0.1 * 1e-4 * 10)[,time_idx],
            tolerance = 1e6), paste0(time_lbl, "/prey_b/effortfleet: Total consumption proportion of overall stock (using prey catchability)"))

        ok(ut_cmp_equal(
            r$detail_prey_a_fleet_quotafleet__cons[,time_idx],
            (r$dstart_prey_a__num * r$dstart_prey_a__wgt * 0.5 * 0.1 * 1e-5 * 2)[,time_idx],
            tolerance = 1e6), paste0(time_lbl, "/prey_a/quotafleet: Total consumption proportion of overall stock (high quota)"))
        ok(ut_cmp_equal(
            r$detail_prey_b_fleet_quotafleet__cons[,time_idx],
            (r$dstart_prey_b__num * r$dstart_prey_b__wgt * 0.5 * 0.1 * 1e-5 * 2)[,time_idx],
            tolerance = 1e6), paste0(time_lbl, "/prey_b/quotafleet: Total consumption proportion of overall stock (high quota)"))
    }
    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})
