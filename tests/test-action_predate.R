library(magrittr)
library(unittest)

library(gadget3)

tmb_r_compare <- function (model_fn, model_tmb, params) {
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        # Reformat params into a single vector in expected order
        par <- unlist(params[names(environment(model_cpp)$model_parameters)])
        model_tmb_report <- model_tmb$report(par)
        for (n in ls(environment(model_fn)$model_report)) {
            ok(ut_cmp_equal(
                as.vector(model_tmb_report[[n]]),
                as.vector(environment(model_fn)$model_report[[n]]),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}

areas <- g3_areas('a', 'b', 'c')

prey_a <- g3_stock('prey_a', seq(1, 10)) %>% g3s_livesonareas(areas[c('a')])
prey_b <- g3_stock('prey_b', seq(1, 10)) %>% g3s_livesonareas(areas[c('b')])
prey_c <- g3_stock('prey_c', seq(1, 10)) %>% g3s_livesonareas(areas[c('c')])
fleet_ab <- g3_fleet('fleet_ab') %>% g3s_livesonareas(areas[c('a', 'b')])
fleet_bc <- g3_fleet('fleet_bc') %>% g3s_livesonareas(areas[c('b', 'c')])

cur_time <- 0L
actions <- g3_collate(
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
    g3a_predate_totalfleet(
        fleet_bc,
        list(prey_a, prey_b, prey_c),
        suitabilities = list(
            # TODO: Should be testing we can use prey_l
            prey_a = ~g3_param_vector("fleet_bc_a"),
            prey_b = ~g3_param_vector("fleet_bc_b"),
            prey_c = ~g3_param_vector("fleet_bc_c")),
        amount_f = ~g3_param('amount_bc') * area),
    list(
        '999' = ~{
            g3_report(prey_a__num)
            g3_report(prey_a__wgt)
            g3_report(prey_a__totalpredate)
            g3_report(prey_a__overconsumption)

            g3_report(prey_b__num)
            g3_report(prey_b__wgt)
            g3_report(prey_b__totalpredate)
            g3_report(prey_b__overconsumption)

            g3_report(prey_c__num)
            g3_report(prey_c__wgt)
            g3_report(prey_c__totalpredate)
            g3_report(prey_c__overconsumption)

            g3_report(fleet_ab__prey_a)
            g3_report(fleet_ab__prey_b)
            g3_report(fleet_ab__prey_c)
            g3_report(fleet_ab__catch)

            g3_report(fleet_bc__prey_a)
            g3_report(fleet_bc__prey_b)
            g3_report(fleet_bc__prey_c)
            g3_report(fleet_bc__catch)

            return(g3_param('x'))
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
    x=1.0)

# Compile model
model_fn <- g3_compile_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_precompile_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
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
        x=1.0)
    result <- model_fn(params)
    # str(as.list(environment(model_fn)$model_report), vec.len = 10000)

    # Fleet_ab
    ok(ut_cmp_identical(
        as.vector(environment(model_fn)$model_report$fleet_ab__catch),
        c(100, 200)), "fleet_ab__catch: 100 from area 1, 200 from area 2")
    prey_a_catch_45 <- 0.1 * 45 * 450
    prey_a_catch_55 <- 0.2 * 55 * 550
    prey_a_catch_65 <- 0.1 * 65 * 650
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$fleet_ab__prey_a),
        c(
            0, 0, 0,
            100 * prey_a_catch_45 / (prey_a_catch_45 + prey_a_catch_55 + prey_a_catch_65),
            100 * prey_a_catch_55 / (prey_a_catch_45 + prey_a_catch_55 + prey_a_catch_65),
            100 * prey_a_catch_65 / (prey_a_catch_45 + prey_a_catch_55 + prey_a_catch_65),
            0, 0, 0, 0)), "fleet_ab__prey_a: Scaled to match suitability")
    ok(ut_cmp_equal(
        sum(as.vector(environment(model_fn)$model_report$fleet_ab__prey_a)),
        100), "fleet_ab__prey_a: Totals 100")
    prey_b_catch_65 <- 0.1 * 65 * 650
    prey_b_catch_75 <- 0.2 * 75 * 750
    prey_b_catch_85 <- 0.1 * 85 * 850
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$fleet_ab__prey_b),
        c(
            0, 0, 0, 0, 0,
            200 * prey_b_catch_65 / (prey_b_catch_65 + prey_b_catch_75 + prey_b_catch_85),
            200 * prey_b_catch_75 / (prey_b_catch_65 + prey_b_catch_75 + prey_b_catch_85),
            200 * prey_b_catch_85 / (prey_b_catch_65 + prey_b_catch_75 + prey_b_catch_85),
            0, 0)), "fleet_ab__prey_b: Scaled to match suitability")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$fleet_ab__prey_c),
        c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), "fleet_ab__prey_c: None, in wrong area")

    # Fleet_bc
    ok(ut_cmp_identical(
        as.vector(environment(model_fn)$model_report$fleet_bc__catch),
        c(2 * 50, 3 * 50)), "fleet_bc__catch: 50 * (area) in total")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$fleet_bc__prey_a),
        c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), "fleet_bc__prey_a: None, in wrong area")
    prey_b_catch_1 <- 0.1 * 75 * 750
    prey_b_catch_2 <- 0.2 * 85 * 850
    prey_b_catch_3 <- 0.1 * 95 * 950
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$fleet_bc__prey_b),
        c(
            0, 0, 0, 0, 0, 0,
            100 * prey_b_catch_1 / (prey_b_catch_1 + prey_b_catch_2 + prey_b_catch_3),
            100 * prey_b_catch_2 / (prey_b_catch_1 + prey_b_catch_2 + prey_b_catch_3),
            100 * prey_b_catch_3 / (prey_b_catch_1 + prey_b_catch_2 + prey_b_catch_3),
            0)), "fleet_bc__prey_b: Scaled to match suitability")
    prey_c_catch_1 <- 0.1 * 75 * 750
    prey_c_catch_2 <- 0.2 * 85 * 850
    prey_c_catch_3 <- 0.1 * 95 * 950
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$fleet_bc__prey_c),
        c(
            0, 0, 0, 0, 0, 0,
            150 * prey_c_catch_1 / (prey_c_catch_1 + prey_c_catch_2 + prey_c_catch_3),
            150 * prey_c_catch_2 / (prey_c_catch_1 + prey_c_catch_2 + prey_c_catch_3),
            150 * prey_c_catch_3 / (prey_c_catch_1 + prey_c_catch_2 + prey_c_catch_3),
            0)), "fleet_bc__prey_c: Scaled to match suitability")
    ok(ut_cmp_equal(
        sum(as.vector(environment(model_fn)$model_report$fleet_bc__prey_c)),
        150), "fleet_bc__prey_c: Totals 150")

    # prey_a
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$prey_a__overconsumption),
        c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), "prey_a__overconsumption: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$prey_a__totalpredate),
        as.vector(environment(model_fn)$model_report$fleet_ab__prey_a) + 
        as.vector(environment(model_fn)$model_report$fleet_bc__prey_a)), "prey_a__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$prey_a__num),
        c(15.00000, 25.00000, 35.00000, 44.96341, 54.91057, 64.94715, 75.00000, 85.00000, 95.00000, 105.00000),
        tolerance = 1e-5), "prey_a__num: Taken out of circulation")

    # prey_b
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$prey_b__overconsumption),
        c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), "prey_b__overconsumption: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$prey_b__totalpredate),
        as.vector(environment(model_fn)$model_report$fleet_ab__prey_b) + 
        as.vector(environment(model_fn)$model_report$fleet_bc__prey_b)), "prey_b__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$prey_b__num),
        c(15.00000, 25.00000, 35.00000, 45.00000, 55.00000, 64.88546, 74.68414, 84.73338, 94.93471, 105.00000),
        tolerance = 1e-5), "prey_b__num: Taken out of circulation")

    # prey_c
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$prey_c__overconsumption),
        c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), "prey_c__overconsumption: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$prey_c__totalpredate),
        as.vector(environment(model_fn)$model_report$fleet_ab__prey_c) + 
        as.vector(environment(model_fn)$model_report$fleet_bc__prey_c)), "prey_c__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$prey_c__num),
        c(15.00000, 25.00000, 35.00000, 45.00000, 55.00000, 65.00000, 74.96134, 84.91237, 94.95103, 105.00000),
        tolerance = 1e-5), "prey_c__num: Taken out of circulation")

    tmb_r_compare(model_fn, model_tmb, params)
})
