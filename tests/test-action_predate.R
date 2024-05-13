library(magrittr)
library(unittest)

library(gadget3)

cmp_code <- function (a, b) ut_cmp_identical(
    deparse(gadget3:::f_optimize(a)),
    deparse(gadget3:::f_optimize(b)))

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

ok_group("g3a_predate_catchability_totalfleet", {
    ok(cmp_code(
        g3a_predate_catchability_totalfleet(1234),
        ~stock_ss(predprey__suit) * (1234/total_predsuit)
        ), "g3a_predate_catchability_totalfleet: Inserts E into formula")
})

ok_group("g3a_predate_catchability_numberfleet", {
    ok(cmp_code(
        g3a_predate_catchability_numberfleet(1234),
        ~(stock_ss(predprey__suit)/avoid_zero_vec(stock_ss(stock__wgt))) * (1234/total_predsuitnum) * stock_ss(stock__wgt)
        ), "g3a_predate_catchability_numberfleet: Uses __catchnum instead of __catch")
})

ok_group("g3a_predate_catchability_effortfleet", {
    ok(cmp_code(
        g3a_predate_catchability_effortfleet(
            list(ling_imm = 123, ling_mat = 456),
            1234),
        ~stock_switch(stock, ling_imm = 123, ling_mat = 456) * 1234 * cur_step_size * stock_ss(predprey__suit)), "Converts list into stock_switch")
})

ok_group("g3a_predate_catchability_quotafleet", {
    ok(cmp_code(
        g3a_predate_catchability_quotafleet(data.frame(
            biomass = c(1000, 2000, Inf),
            quota = c(10, 20, 30)), 1234),
        ~(if (sum(stock__num * stock__wgt) < 1000) 10 else
          if (sum(stock__num * stock__wgt) < 2000) 20 else 30) *
              1234 * cur_step_size * stock_ss(predprey__suit)), "quota_table: Quota converted into if condition")

    ok(cmp_code(
        g3a_predate_catchability_quotafleet(
            data.frame(biomass = c(100, Inf), quota = c(0, 800)),
            2134,
            sum_stocks = list(prey_a, prey_b)),
        ~(if ((stock_with(prey_b, sum(prey_b__num * prey_b__wgt)) +
             (stock_with(prey_a, sum(prey_a__num * prey_a__wgt)) + 0)) < 100) 0 else 800) *
                2134 * cur_step_size * stock_ss(predprey__suit)), "sum_stocks: Summing all named stocks")
    # TODO: Make sure resulting formula can be predate_fleet()ed

    out_f <- g3a_predate_catchability_quotafleet(
            data.frame(biomass = c(100, Inf), quota = c(0, 800)),
            2134,
            recalc_f = ~cur_step == 1)
    quota_var_name <- ls(environment(out_f))[startsWith(ls(environment(out_f)), "stock__quotafleet_")]
    ok(ut_cmp_identical(length(quota_var_name), 1L), "recalc_f: Quota var added to env")
    ok(ut_cmp_identical(
        unattr(environment(out_f)[[quota_var_name]]),
        0.0), "recalc_f: Quota var initalized to 0")
    ok(cmp_code(
        out_f,
        gadget3:::f_substitute(
            ~(quota_var <- if (cur_step == 1) if (sum(stock__num * stock__wgt) <
                100) 0 else 800 else quota_var) * 2134 * cur_step_size * stock_ss(predprey__suit),
            list(quota_var = as.symbol(quota_var_name)))), "recalc_f: Assign to quota_var as part of code")
})

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
    g3a_time(2000, ~2000 + g3_param('years') - 1, project_years = 0),
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
        catchability_f = g3a_predate_catchability_totalfleet(~g3_param('amount_bc') * area),
        # NB: Only run on even years
        run_f = ~cur_year %% 2L == 0L),
    g3l_understocking(
        list(prey_a, prey_b, prey_c),
        power_f = ~g3_param('understocking_power'),
        weight = 2),
    list(
        '999' = ~{
            REPORT(prey_a__num)
            REPORT(prey_a__wgt)
            REPORT(prey_a__totalpredate)
            REPORT(prey_a__consratio)

            REPORT(prey_b__num)
            REPORT(prey_b__wgt)
            REPORT(prey_b__totalpredate)
            REPORT(prey_b__consratio)

            REPORT(prey_c__num)
            REPORT(prey_c__wgt)
            REPORT(prey_c__totalpredate)
            REPORT(prey_c__consratio)

            REPORT(prey_a_fleet_ab__cons)
            REPORT(prey_b_fleet_ab__cons)
            REPORT(prey_c_fleet_ab__cons)

            REPORT(prey_a_fleet_bc__cons)
            REPORT(prey_b_fleet_bc__cons)
            REPORT(prey_c_fleet_bc__cons)

            REPORT(nll)  # NB: This report triggers tmb_r_compare to compare nll
        }))
actions <- c(actions, list(g3a_report_history(actions, ".*__cons$")))

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    params <- attr(model_fn, 'parameter_template')
    params$fleet_ab_a <- c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0)
    params$fleet_ab_b <- c(0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0)
    params$fleet_ab_c <- c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0)
    params$amount_ab <- 100
    params$fleet_bc_a <- c(0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0)
    params$fleet_bc_b <- c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0)
    params$fleet_bc_c <- c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0)
    params$amount_bc <- 100
    params$understocking_power <- 2
    params$years <- 1
    params$x<-1.0

    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

ok_group("No overconsumption", {
    params <- attr(model_fn, 'parameter_template')
    params$fleet_ab_a <- c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0)
    params$fleet_ab_b <- c(0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0)
    params$fleet_ab_c <- c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0)
    params$amount_ab <- 100
    params$fleet_bc_a <- c(0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0)
    params$fleet_bc_b <- c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0)
    params$fleet_bc_c <- c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0)
    params$amount_bc <- 50
    params$understocking_power <- 2
    params$years <- 1
    params$x<-1.0

    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_equal(unattr(result), 0), "nll: No overconsumption")
    ok(ut_cmp_equal(sum(r$nll_understocking__wgt), 0), "nll_understocking__wgt: Breakdown also 0")

    # Fleet_ab
    prey_a_catch_45 <- 0.1 * 45 * 450  # NB: 0.1 = selectivity, 45 = __num, 450 = __wgt
    prey_a_catch_55 <- 0.2 * 55 * 550
    prey_a_catch_65 <- 0.1 * 65 * 650
    ok(ut_cmp_equal(
        as.vector(r$prey_a_fleet_ab__cons[,,area='a']),
        c(
            0, 0, 0,
            100 * prey_a_catch_45 / (prey_a_catch_45 + prey_a_catch_55 + prey_a_catch_65),
            100 * prey_a_catch_55 / (prey_a_catch_45 + prey_a_catch_55 + prey_a_catch_65),
            100 * prey_a_catch_65 / (prey_a_catch_45 + prey_a_catch_55 + prey_a_catch_65),
            0, 0, 0, 0)), "prey_a_fleet_ab__cons: Scaled to match suitability")
    prey_b_catch_65 <- 0.1 * 65 * 650
    prey_b_catch_75 <- 0.2 * 75 * 750
    prey_b_catch_85 <- 0.1 * 85 * 850
    ok(ut_cmp_equal(
        as.vector(r$prey_b_fleet_ab__cons[,,area='b']),
        c(
            0, 0, 0, 0, 0,
            200 * prey_b_catch_65 / (prey_b_catch_65 + prey_b_catch_75 + prey_b_catch_85),
            200 * prey_b_catch_75 / (prey_b_catch_65 + prey_b_catch_75 + prey_b_catch_85),
            200 * prey_b_catch_85 / (prey_b_catch_65 + prey_b_catch_75 + prey_b_catch_85),
            0, 0)), "prey_b_fleet_ab__cons: Scaled to match suitability")
    ok(ut_cmp_equal(
        as.vector(r$prey_c_fleet_ab__cons[,,]),
        c(
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        )), "prey_c_fleet_ab__cons: None, in wrong area")

    # Fleet_bc
    ok(ut_cmp_equal(
        as.vector(r$prey_a_fleet_bc__cons[,,]),
        c(
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), "prey_a_fleet_bc__cons: None, in wrong area")
    prey_b_catch_1 <- 0.1 * 75 * 750
    prey_b_catch_2 <- 0.2 * 85 * 850
    prey_b_catch_3 <- 0.1 * 95 * 950
    ok(ut_cmp_equal(
        as.vector(r$prey_b_fleet_bc__cons[,,area='b']),
        c(
            0, 0, 0, 0, 0, 0,
            100 * prey_b_catch_1 / (prey_b_catch_1 + prey_b_catch_2 + prey_b_catch_3),
            100 * prey_b_catch_2 / (prey_b_catch_1 + prey_b_catch_2 + prey_b_catch_3),
            100 * prey_b_catch_3 / (prey_b_catch_1 + prey_b_catch_2 + prey_b_catch_3),
            0)), "prey_b_fleet_bc__cons: Scaled to match suitability")
    prey_c_catch_1 <- 0.1 * 75 * 750
    prey_c_catch_2 <- 0.2 * 85 * 850
    prey_c_catch_3 <- 0.1 * 95 * 950
    ok(ut_cmp_equal(
        as.vector(r$prey_c_fleet_bc__cons[,,area='c']),
        c(
            0, 0, 0, 0, 0, 0,
            150 * prey_c_catch_1 / (prey_c_catch_1 + prey_c_catch_2 + prey_c_catch_3),
            150 * prey_c_catch_2 / (prey_c_catch_1 + prey_c_catch_2 + prey_c_catch_3),
            150 * prey_c_catch_3 / (prey_c_catch_1 + prey_c_catch_2 + prey_c_catch_3),
            0)), "prey_c_fleet_bc__cons: Scaled to match suitability")
    ok(ut_cmp_equal(
        sum(as.vector(r$prey_c_fleet_bc__cons)),
        150), "prey_c_fleet_bc__cons: Totals 150")

    # prey_a
    ok(ut_cmp_equal(
        as.vector(r$prey_a__consratio) > 0.9499,
        c(F, F, F, F, F, F, F, F, F, F)), "prey_a__consratio: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(r$prey_a__totalpredate),
        as.vector(rowSums(r$prey_a_fleet_ab__cons, dims = 2)) +
        as.vector(rowSums(r$prey_a_fleet_bc__cons, dims = 2))), "prey_a__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_a__num),
        c(15.00000, 25.00000, 35.00000, 44.96341, 54.91057, 64.94715, 75.00000, 85.00000, 95.00000, 105.00000),
        tolerance = 1e-5), "prey_a__num: Taken out of circulation")

    # prey_b
    ok(ut_cmp_equal(
        as.vector(r$prey_b__consratio) > 0.9499,
        c(F, F, F, F, F, F, F, F, F, F)), "prey_b__consratio: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(r$prey_b__totalpredate),
        as.vector(rowSums(r$prey_b_fleet_ab__cons, dims = 2)) +
        as.vector(rowSums(r$prey_b_fleet_bc__cons, dims = 2))), "prey_b__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_b__num),
        c(15.00000, 25.00000, 35.00000, 45.00000, 55.00000, 64.94273, 74.84207, 84.86669, 94.96735, 105.00000),
        tolerance = 1e-5), "prey_b__num: Taken out of circulation")

    # prey_c
    ok(ut_cmp_equal(
        as.vector(r$prey_c__consratio) > 0.9499,
        c(F, F, F, F, F, F, F, F, F, F)), "prey_c__consratio: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(r$prey_c__totalpredate),
        as.vector(rowSums(r$prey_c_fleet_ab__cons, dims = 2)) +
        as.vector(rowSums(r$prey_c_fleet_bc__cons, dims = 2))), "prey_c__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_c__num),
        c(15.00000, 25.00000, 35.00000, 45.00000, 55.00000, 65.00000, 74.96134, 84.91237, 94.95103, 105.00000),
        tolerance = 1e-5), "prey_c__num: Taken out of circulation")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})


ok_group("Overconsumption", {
    params <- attr(model_fn, 'parameter_template')
    params$fleet_ab_a <- c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0)
    params$fleet_ab_b <- c(0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0)
    params$fleet_ab_c <- c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0)
    params$amount_ab <- 1000000
    params$fleet_bc_a <- c(0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0)
    params$fleet_bc_b <- c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0)
    params$fleet_bc_c <- c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0)
    params$amount_bc <- 50
    params$understocking_power <- 1
    params$years <- 1
    params$x<-1.0

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
        as.vector(r$prey_a__consratio) > 0.9499,
        c(F, F, F, T, T, T, F, F, F, F)), "prey_a__consratio: Overconsumed by ab")
    ok(ut_cmp_equal(
        as.vector(r$prey_a__totalpredate),
        as.vector(rowSums(r$prey_a_fleet_ab__cons, dims = 2)) +
        as.vector(rowSums(r$prey_a_fleet_bc__cons, dims = 2))), "prey_a__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_a__num),
        c(15, 25, 35, 45 - (45 * 0.95), 55 - (55 * 0.95), 65 - (65 * 0.95), 75, 85, 95, 105),
        # NB: Low tolerance due to using logspace_add_vec
        tolerance = 1e-2), "prey_a__num: Still some left thanks to overconsumption being triggered")

    # prey_b
    ok(ut_cmp_equal(
        as.vector(r$prey_b__consratio) > 0.9499,
        c(F, F, F, F, F, T, T, T, F, F)), "prey_b__consratio: Overconsumed by ab")
    ok(ut_cmp_equal(
        as.vector(r$prey_b__totalpredate),
        as.vector(rowSums(r$prey_b_fleet_ab__cons, dims = 2)) +
        as.vector(rowSums(r$prey_b_fleet_bc__cons, dims = 2))), "prey_b__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_b__num),
        c(15, 25, 35, 45, 55, 65 - (65 * 0.95), 75 - (75 * 0.95), 85 - (85 * 0.95), 94.96735, 105),
        # NB: Low tolerance due to using logspace_add_vec
        tolerance = 1e-2), "prey_b__num: Hit overconsumption limit")

    # prey_c
    ok(ut_cmp_equal(
        as.vector(r$prey_c__consratio) > 0.9499,
        c(F, F, F, F, F, F, F, F, F, F)), "prey_c__consratio: No overconsumption")
    ok(ut_cmp_equal(
        as.vector(r$prey_c__totalpredate),
        as.vector(rowSums(r$prey_c_fleet_ab__cons, dims = 2)) +
        as.vector(rowSums(r$prey_c_fleet_bc__cons, dims = 2))), "prey_c__totalpredate: fleet_ab + fleet_ac")
    ok(ut_cmp_equal(
        as.vector(r$prey_c__num),
        c(15, 25, 35, 45, 55, 65, 74.96134, 84.91237, 94.95103, 105),
        tolerance = 1e-5), "prey_c__num: Taken out of circulation")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})

ok_group("run_f disabling", {
    params <- attr(model_fn, 'parameter_template')
    params$fleet_ab_a <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    params$fleet_ab_b <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    params$fleet_ab_c <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    params$amount_ab <- 10
    params$fleet_bc_a <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    params$fleet_bc_b <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    params$fleet_bc_c <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    params$amount_bc <- 10
    params$understocking_power <- 1
    params$years <- 4
    params$x<-1.0

    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_equal(
        colSums(r$hist_prey_a_fleet_ab__cons[,,pred_area = 'a',]),
        c("2000-01" = 10, "2001-01" = 10, "2002-01" = 10, "2003-01" = 10) ), "hist_prey_a_fleet_ab__cons: Continually fished")
    ok(ut_cmp_equal(
        colSums(r$hist_prey_b_fleet_ab__cons[,,pred_area = 'b',]),
        c("2000-01" = 20, "2001-01" = 20, "2002-01" = 20, "2003-01" = 20) ), "hist_prey_b_fleet_ab__cons: Continually fished")
    ok(ut_cmp_equal(
        colSums(r$hist_prey_c_fleet_ab__cons[,,pred_area = 'a',]),
        c("2000-01" = 0, "2001-01" = 0, "2002-01" = 0, "2003-01" = 0) ), "hist_prey_c_fleet_ab__cons: Not fished")

    ok(ut_cmp_equal(
        colSums(r$hist_prey_a_fleet_bc__cons[,,pred_area = 'b',]),
        c("2000-01" = 0, "2001-01" = 0, "2002-01" = 0, "2003-01" = 0) ), "hist_prey_a_fleet_bc__cons: Not fished")
    ok(ut_cmp_equal(
        colSums(r$hist_prey_b_fleet_bc__cons[,,pred_area = 'b',]),
        c("2000-01" = 20, "2001-01" = 0, "2002-01" = 20, "2003-01" = 0) ), "hist_prey_b_fleet_bc__cons: Only fished on even years")
    ok(ut_cmp_equal(
        colSums(r$hist_prey_c_fleet_bc__cons[,,pred_area = 'c',]),
        c("2000-01" = 30, "2001-01" = 0, "2002-01" = 30, "2003-01" = 0) ), "hist_prey_c_fleet_bc__cons: Only fished on even years")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        model_tmb <- g3_tmb_adfun(model_cpp, param_template, compile_flags = c("-O0", "-g"))
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})
