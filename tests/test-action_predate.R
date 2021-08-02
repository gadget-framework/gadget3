library(magrittr)
library(unittest)

library(gadget3)

tmb_r_compare <- function (model_fn, model_tmb, params) {
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        # Reformat params into a single vector in expected order
        par <- unlist(params[attr(model_cpp, 'parameter_template')$switch])
        model_tmb_report <- model_tmb$report(par)
        r_result <- model_fn(params)
        for (n in names(attributes(r_result))) {
            ok(ut_cmp_equal(
                as.vector(model_tmb_report[[n]]),
                as.vector(attr(r_result, n)),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}

unattr <- function (x) {
    attributes(x) <- NULL
    return(x)
}

areas <- list(a=1, b=2, c=3)

prey_a <- g3_stock('prey_a', seq(1, 10)) %>% g3s_livesonareas(areas[c('a')])
prey_b <- g3_stock('prey_b', seq(1, 10)) %>% g3s_livesonareas(areas[c('b')])
prey_c <- g3_stock('prey_c', seq(1, 10)) %>% g3s_livesonareas(areas[c('c')])
fleet_ab <- g3_fleet('fleet_ab') %>% g3s_livesonareas(areas[c('a', 'b')])
fleet_bc <- g3_fleet('fleet_bc') %>% g3s_livesonareas(areas[c('b', 'c')])

ok_group("Detect missing suitabilities", {
    ok(ut_cmp_error(g3a_predate_totalfleet(
        fleet_bc,
        list(prey_a, prey_b, prey_c),
        suitabilities = list(
            prey_a = ~g3_param_vector("fleet_bc_a"),
            prey_c = ~g3_param_vector("fleet_bc_c")),
        amount_f = ~g3_param('amount_bc') * area), "prey_b"), "Threw an error on missing suitability function")
})

actions <- list(
    g3a_time(2000, 2000),
    g3a_initialconditions(prey_a, ~10 * prey_a__midlen, ~100 * prey_a__midlen),
    g3a_initialconditions(prey_b, ~10 * prey_b__midlen, ~100 * prey_b__midlen),
    g3a_initialconditions(prey_c, ~10 * prey_c__midlen, ~100 * prey_c__midlen),
    g3a_predate_totalfleet(
        fleet_ab,
        list(prey_a, prey_b, prey_c),
        suitabilities = list(
            # TODO: Should be testing we can use prey_l
            prey_a = ~g3_param_vector("fleet_ab_a"),
            prey_b = ~g3_param_vector("fleet_ab_b"),
            prey_c = ~g3_param_vector("fleet_ab_c")),
        amount_f = ~g3_param('amount_ab') * area),
    g3a_predate_fleet(
        fleet_bc,
        list(prey_a, prey_b, prey_c),
        suitabilities = list(
            # TODO: Should be testing we can use prey_l
            prey_a = ~g3_param_vector("fleet_bc_a"),
            prey_b = ~g3_param_vector("fleet_bc_b"),
            prey_c = ~g3_param_vector("fleet_bc_c")),
        catchability_f = g3a_predate_catchability_totalfleet(~g3_param('amount_bc') * area)),
    g3l_understocking(
        list(prey_a, prey_b, prey_c),
        power_f = ~g3_param('understocking_power'),
        weight = 2),
    list(
        '999' = ~{
            g3_report(prey_a__num)
            g3_report(prey_a__wgt)
            g3_report(prey_a__totalpredate)
            g3_report(prey_a__consratio)

            g3_report(prey_b__num)
            g3_report(prey_b__wgt)
            g3_report(prey_b__totalpredate)
            g3_report(prey_b__consratio)

            g3_report(prey_c__num)
            g3_report(prey_c__wgt)
            g3_report(prey_c__totalpredate)
            g3_report(prey_c__consratio)

            g3_report(prey_a__fleet_ab)
            g3_report(prey_b__fleet_ab)
            g3_report(prey_c__fleet_ab)
            g3_report(fleet_ab__catch)

            g3_report(prey_a__fleet_bc)
            g3_report(prey_b__fleet_bc)
            g3_report(prey_c__fleet_bc)
            g3_report(fleet_bc__catch)

            g3_report(nll)  # NB: This report triggers tmb_r_compare to compare nll
        }))
params <- list(
    fleet_ab_a = c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0),
    fleet_ab_b = c(0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0),
    fleet_ab_c = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
    amount_ab = 100,
    fleet_bc_a = c(0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0),
    fleet_bc_b = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
    fleet_bc_c = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
    amount_bc = 100,
    understocking_power = 2,
    x=1.0)

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

ok_group("No overconsumption", {
    params <- list(
        fleet_ab_a = c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0),
        fleet_ab_b = c(0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0),
        fleet_ab_c = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
        amount_ab = 100,
        fleet_bc_a = c(0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0),
        fleet_bc_b = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
        fleet_bc_c = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
        amount_bc = 50,
        understocking_power = 2,
        x=1.0)
    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_equal(unattr(result), 0), "nll: No overconsumption")
    ok(ut_cmp_equal(sum(r$nll_understocking__wgt), 0), "nll_understocking__wgt: Breakdown also 0")

    # Fleet_ab
    ok(ut_cmp_equal(
        as.vector(r$fleet_ab__catch),
        c(sum(r$prey_a__fleet_ab[,1]), sum(r$prey_b__fleet_ab[,1])),
        tolerance = 1e-7), "prey_ab__catch: Totals match prey_a__fleet_ab & prey_b__fleet_ab")
    prey_a_catch_45 <- 0.1 * 45 * 450  # NB: 0.1 = selectivity, 45 = __num, 450 = __wgt
    prey_a_catch_55 <- 0.2 * 55 * 550
    prey_a_catch_65 <- 0.1 * 65 * 650
    ok(ut_cmp_equal(
        as.vector(r$prey_a__fleet_ab),
        c(
            0, 0, 0,
            100 * prey_a_catch_45 / (prey_a_catch_45 + prey_a_catch_55 + prey_a_catch_65),
            100 * prey_a_catch_55 / (prey_a_catch_45 + prey_a_catch_55 + prey_a_catch_65),
            100 * prey_a_catch_65 / (prey_a_catch_45 + prey_a_catch_55 + prey_a_catch_65),
            0, 0, 0, 0)), "prey_a__fleet_ab: Scaled to match suitability")
    prey_b_catch_65 <- 0.1 * 65 * 650
    prey_b_catch_75 <- 0.2 * 75 * 750
    prey_b_catch_85 <- 0.1 * 85 * 850
    ok(ut_cmp_equal(
        as.vector(r$prey_b__fleet_ab),
        c(
            0, 0, 0, 0, 0,
            200 * prey_b_catch_65 / (prey_b_catch_65 + prey_b_catch_75 + prey_b_catch_85),
            200 * prey_b_catch_75 / (prey_b_catch_65 + prey_b_catch_75 + prey_b_catch_85),
            200 * prey_b_catch_85 / (prey_b_catch_65 + prey_b_catch_75 + prey_b_catch_85),
            0, 0)), "prey_b__fleet_ab: Scaled to match suitability")
    ok(ut_cmp_equal(
        as.vector(r$prey_c__fleet_ab),
        c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), "prey_c__fleet_ab: None, in wrong area")

    # Fleet_bc
    ok(ut_cmp_equal(
        as.vector(r$fleet_bc__catch),
        c(2 * 50, 3 * 50)), "fleet_bc__catch: 50 * (area) in total")
    ok(ut_cmp_equal(
        as.vector(r$prey_a__fleet_bc),
        c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), "prey_a__fleet_bc: None, in wrong area")
    prey_b_catch_1 <- 0.1 * 75 * 750
    prey_b_catch_2 <- 0.2 * 85 * 850
    prey_b_catch_3 <- 0.1 * 95 * 950
    ok(ut_cmp_equal(
        as.vector(r$prey_b__fleet_bc),
        c(
            0, 0, 0, 0, 0, 0,
            100 * prey_b_catch_1 / (prey_b_catch_1 + prey_b_catch_2 + prey_b_catch_3),
            100 * prey_b_catch_2 / (prey_b_catch_1 + prey_b_catch_2 + prey_b_catch_3),
            100 * prey_b_catch_3 / (prey_b_catch_1 + prey_b_catch_2 + prey_b_catch_3),
            0)), "prey_b__fleet_bc: Scaled to match suitability")
    prey_c_catch_1 <- 0.1 * 75 * 750
    prey_c_catch_2 <- 0.2 * 85 * 850
    prey_c_catch_3 <- 0.1 * 95 * 950
    ok(ut_cmp_equal(
        as.vector(r$prey_c__fleet_bc),
        c(
            0, 0, 0, 0, 0, 0,
            150 * prey_c_catch_1 / (prey_c_catch_1 + prey_c_catch_2 + prey_c_catch_3),
            150 * prey_c_catch_2 / (prey_c_catch_1 + prey_c_catch_2 + prey_c_catch_3),
            150 * prey_c_catch_3 / (prey_c_catch_1 + prey_c_catch_2 + prey_c_catch_3),
            0)), "prey_c__fleet_bc: Scaled to match suitability")
    ok(ut_cmp_equal(
        sum(as.vector(r$prey_c__fleet_bc)),
        150), "prey_c__fleet_bc: Totals 150")

    # prey_a
    ok(ut_cmp_equal(
        as.vector(r$prey_a__consratio) >= 0.95,
        c(F, F, F, F, F, F, F, F, F, F)), "prey_a__consratio: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(r$prey_a__totalpredate),
        as.vector(r$prey_a__fleet_ab) + 
        as.vector(r$prey_a__fleet_bc)), "prey_a__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_a__num),
        c(15.00000, 25.00000, 35.00000, 44.96341, 54.91057, 64.94715, 75.00000, 85.00000, 95.00000, 105.00000),
        tolerance = 1e-5), "prey_a__num: Taken out of circulation")

    # prey_b
    ok(ut_cmp_equal(
        as.vector(r$prey_b__consratio) >= 0.95,
        c(F, F, F, F, F, F, F, F, F, F)), "prey_b__consratio: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(r$prey_b__totalpredate),
        as.vector(r$prey_b__fleet_ab) + 
        as.vector(r$prey_b__fleet_bc)), "prey_b__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_b__num),
        c(15.00000, 25.00000, 35.00000, 45.00000, 55.00000, 64.94273, 74.84207, 84.86669, 94.96735, 105.00000),
        tolerance = 1e-5), "prey_b__num: Taken out of circulation")

    # prey_c
    ok(ut_cmp_equal(
        as.vector(r$prey_c__consratio) >= 0.95,
        c(F, F, F, F, F, F, F, F, F, F)), "prey_c__consratio: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(r$prey_c__totalpredate),
        as.vector(r$prey_c__fleet_ab) + 
        as.vector(r$prey_c__fleet_bc)), "prey_c__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_c__num),
        c(15.00000, 25.00000, 35.00000, 45.00000, 55.00000, 65.00000, 74.96134, 84.91237, 94.95103, 105.00000),
        tolerance = 1e-5), "prey_c__num: Taken out of circulation")

    tmb_r_compare(model_fn, model_tmb, params)
})


ok_group("Overconsumption", {
    params <- list(
        fleet_ab_a = c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0),
        fleet_ab_b = c(0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0),
        fleet_ab_c = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
        amount_ab = 1000000,
        fleet_bc_a = c(0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0),
        fleet_bc_b = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
        fleet_bc_c = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
        amount_bc = 50,
        understocking_power = 1,
        x=1.0)
    result <- model_fn(params)
    r <- attributes(result)
    # str(r, vec.len = 10000)

    ok(result > 0, "nll: Overconsumption triggered understocking")
    ok(ut_cmp_equal(
        unattr(result),
        sum(2 * r$nll_understocking__wgt),
        tolerance = 1e-7), "nll_understocking__wgt: Breakdown matches total nll")

    # prey_a
    ok(ut_cmp_equal(
        as.vector(r$prey_a__consratio) >= 0.95,
        c(F, F, F, T, T, T, F, F, F, F)), "prey_a__consratio: Overconsumed by ab")
    ok(ut_cmp_equal(
        as.vector(r$prey_a__totalpredate),
        as.vector(r$prey_a__fleet_ab) +
        as.vector(r$prey_a__fleet_bc)), "prey_a__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_a__num),
        c(15, 25, 35, 45 - (45 * 0.95), 55 - (55 * 0.95), 65 - (65 * 0.95), 75, 85, 95, 105),
        # NB: Low tolerance due to using logspace_add_vec
        tolerance = 1e-2), "prey_a__num: Still some left thanks to overconsumption being triggered")

    # prey_b
    ok(ut_cmp_equal(
        as.vector(r$prey_b__consratio) >= 0.95,
        c(F, F, F, F, F, T, T, T, F, F)), "prey_b__consratio: Overconsumed by ab")
    ok(ut_cmp_equal(
        as.vector(r$prey_b__totalpredate),
        as.vector(r$prey_b__fleet_ab) +
        as.vector(r$prey_b__fleet_bc)), "prey_b__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_b__num),
        c(15, 25, 35, 45, 55, 65 - (65 * 0.95), 75 - (75 * 0.95), 85 - (85 * 0.95), 94.96735, 105),
        # NB: Low tolerance due to using logspace_add_vec
        tolerance = 1e-2), "prey_b__num: Hit overconsumption limit")

    # prey_c
    ok(ut_cmp_equal(
        as.vector(r$prey_c__consratio) >= 0.95,
        c(F, F, F, F, F, F, F, F, F, F)), "prey_c__consratio: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(r$prey_c__totalpredate),
        as.vector(r$prey_c__fleet_ab) +
        as.vector(r$prey_c__fleet_bc)), "prey_c__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_c__num),
        c(15, 25, 35, 45, 55, 65, 74.96134, 84.91237, 94.95103, 105),
        tolerance = 1e-5), "prey_c__num: Taken out of circulation")

    tmb_r_compare(model_fn, model_tmb, params)
})
