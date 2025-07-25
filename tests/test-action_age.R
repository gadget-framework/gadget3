library(magrittr)
library(unittest)

library(gadget3)

ok_group("g3a_age:no_age")
ok(ut_cmp_error({
    g3a_age(g3_stock('prey_a', seq(20, 40, 4)))
}, "age.*prey_a"), "Cannot use g3a_age() without an age dimension")
#### g3a_age:no_age

ok_group("g3a_age:single_age", {
    # prey_a, with single age, outputs into b & c
    prey_a <- g3_stock('prey_a', seq(20, 40, 4)) %>% g3s_age(11, 11)
    prey_b <- g3_stock('prey_b', seq(20, 40, 4)) %>% g3s_age(11, 15)
    prey_c <- g3_stock('prey_c', seq(20, 40, 4)) %>% g3s_age(11, 15)

    actions <- c(
        list("99999" = g3_formula({
            REPORT(prey_a__num)
            REPORT(prey_b__num)
            REPORT(prey_c__num)
        })),
        gadget3:::g3a_initialconditions_manual(prey_a, ~10 * (age-10) + prey_a__midlen * 0, ~100 * (age-10) + prey_a__midlen * 0),
        gadget3:::g3a_initialconditions_manual(prey_b, ~10 * (age-10) + prey_b__midlen * 0, ~100 * (age-10) + prey_b__midlen * 0),
        gadget3:::g3a_initialconditions_manual(prey_c, ~prey_c__midlen * 0, ~prey_c__midlen * 0),
        g3a_age(prey_a, output_stocks = list(prey_b, prey_c), output_ratios = c(0.75, 0.25)),
        # NB: Only required for testing
        gadget3:::g3l_test_dummy_likelihood(),
        g3a_time(2000, 2002) )
    model_fn <- g3_to_r(actions)
    model_cpp <- g3_to_tmb(actions, trace = FALSE)

    params <- attr(model_fn, 'parameter_template')
    r <- model_fn(params)
    for (len_idx in 1:6) {
        ok(ut_cmp_equal(
            attr(r, 'prey_a__num')[length=len_idx,],
            c(0)), paste0("prey_a__num length=", len_idx))
        # prey_b got 75% of prey_a
        ok(ut_cmp_equal(
            attr(r, 'prey_b__num')[length=len_idx,],
            c(age11=10, age12=20 + 7.5, age13=30, age14=40, age15=50)), paste0("prey_b__num length=", len_idx))
        # prey_c got 25% of prey_a
        ok(ut_cmp_equal(
            attr(r, 'prey_c__num')[length=len_idx,],
            c(age11=0, age12=2.5, age13=0, age14=0, age15=0)), paste0("prey_c__num length=", len_idx))
    }

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params, g3_test_tmb = 2)
})

# NB: We start at 11 to make sure we age into the right bracket
prey_a <- g3_stock('prey_a', c(1)) %>% g3s_age(11, 15)
prey_b <- g3_stock('prey_b', c(1)) %>% g3s_age(11, 13)
prey_c <- g3_stock('prey_c', c(1)) %>% g3s_age(11, 17)

# Store stock state in temporary variables labelled stock 0..n
report_action <- list()
for (step in 0:3) for (s in list(prey_a, prey_b, prey_c)) {
    assign(paste0("step", step, '_', s$name, '__num'), g3_stock_instance(s))
    assign(paste0("step", step, '_', s$name, '__wgt'), g3_stock_instance(s))
    report_action[[paste0("999:step", step, ':', s$name)]] <- gadget3:::f_substitute(~if (cur_time == step) {
        stepnum[] <- curnum
        stepwgt[] <- curwgt
        REPORT(stepnum)
        REPORT(stepwgt)
    }, list(
        stepnum = as.symbol(paste0("step", step, '_', s$name, '__num')),
        stepwgt = as.symbol(paste0("step", step, '_', s$name, '__wgt')),
        curnum = as.symbol(paste0(s$name, '__num')),
        curwgt = as.symbol(paste0(s$name, '__wgt')),
        step = step))
}

actions <- list(
    g3a_time(2000, 2002, step_lengths = c(6, 6), project_years = 0),
    gadget3:::g3a_initialconditions_manual(prey_a, ~10 * (age-10) + prey_a__midlen * 0, ~100 * (age-10) + prey_a__midlen * 0),
    gadget3:::g3a_initialconditions_manual(prey_b, ~10 * (age-10) + prey_b__midlen * 0, ~100 * (age-10) + prey_b__midlen * 0),
    gadget3:::g3a_initialconditions_manual(prey_c, ~prey_b__midlen * 0, ~prey_b__midlen * 0),
    g3a_age(prey_a),
    g3a_age(prey_b, output_stocks = list(prey_c)),
    g3a_age(prey_c),
    report_action,
    list('999' = ~{ nll <- nll + g3_param('x', value = 0, optimise = TRUE) }))

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
model_cpp <- g3_to_tmb(actions, trace = FALSE)

ok_group("age", {
    params <- attr(model_fn, 'parameter_template')
    params$x <- 1.0

    result <- model_fn(params)
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)

    # Step 0
    ok(ut_cmp_identical(as.vector(r$step0_prey_a__num), c(10,20,30,40,50)), "step0_prey_a__num: Populated by initialconditions, no ageing yet")
    ok(ut_cmp_identical(as.vector(r$step0_prey_a__wgt), c(100,200,300,400,500)), "step0_prey_a__wgt: Populated by initialconditions, no ageing yet")
    ok(ut_cmp_identical(as.vector(r$step0_prey_b__num), c(10,20,30)), "step0_prey_b__num: Populated by initialconditions, no ageing yet")
    ok(ut_cmp_identical(as.vector(r$step0_prey_b__wgt), c(100,200,300)), "step0_prey_b__wgt: Populated by initialconditions, no ageing yet")
    ok(ut_cmp_identical(as.vector(r$step0_prey_c__num), c(0, 0, 0, 0, 0, 0, 0)), "step0_prey_c__num: Starting off empty")
    ok(ut_cmp_identical(as.vector(r$step0_prey_c__wgt), c(0, 0, 0, 0, 0, 0, 0)), "step0_prey_c__wgt: Starting off empty")

    # Step 1
    ok(ut_cmp_identical(as.vector(r$step1_prey_a__num), c(0, 10,20,30,40 + 50)), "step1_prey_a__num: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_equal(as.vector(r$step1_prey_a__wgt), c(100, 100,200,300, (400 * 40 + 500 * 50) / (40 + 50))), "step1_prey_a__wgt: Numbers rotated by 1, final group averaged plus-group, first group kept __wgt value")
    ok(ut_cmp_identical(as.vector(r$step1_prey_b__num), c(0, 10,20)), "step1_prey_b__num: Numbers rotated by 1, final group transitioned")
    ok(ut_cmp_equal(as.vector(r$step1_prey_b__wgt), c(100, 100, 200)), "step1_prey_b__wgt: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_identical(as.vector(r$step1_prey_c__num), c(0, 0, 0, 30, 0, 0, 0)), "step1_prey_c__num: Final age of prey_b transferred")
    ok(ut_cmp_equal(as.vector(r$step1_prey_c__wgt), c(0, 0, 0, 300, 0, 0, 0)), "step1_prey_c__wgt: Final weight of prey_b transferred")

    # Step 2
    ok(ut_cmp_identical(as.vector(r$step2_prey_a__num), c(0, 10,20,30,40 + 50)), "step2_prey_a__num: Not final step, nothing changed")
    ok(ut_cmp_identical(as.vector(r$step2_prey_a__wgt), c(100, 100,200,300, (400 * 40 + 500 * 50) / (40 + 50))), "step2_prey_a__wgt: Not final step, nothing changed")
    ok(ut_cmp_identical(as.vector(r$step2_prey_b__num), c(0, 10, 20)), "step2_prey_b__num: Not final step, nothing changed")
    ok(ut_cmp_equal(as.vector(r$step2_prey_b__wgt), c(100, 100, 200)), "step2_prey_b__wgt: Not final step, nothing changed")
    ok(ut_cmp_identical(as.vector(r$step2_prey_c__num), c(0, 0, 0, 30, 0, 0, 0)), "step2_prey_c__num: Not final step, nothing changed")
    ok(ut_cmp_equal(as.vector(r$step2_prey_c__wgt), c(0, 0, 0, 300, 0, 0, 0)), "step2_prey_c__wgt: Not final step, nothing changed")

    # Step 3
    ok(ut_cmp_identical(as.vector(r$step3_prey_a__num), c(0, 0, 10,20,30 + 40 + 50)), "step3_prey_a__num: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_equal(as.vector(r$step3_prey_a__wgt), c(100, 100, 100,200, (300 * 30 + 400 * 40 + 500 * 50) / (30 + 40 + 50))), "step3_prey_a__wgt: Numbers rotated by 1, final group a averaged plus group")
    ok(ut_cmp_identical(as.vector(r$step3_prey_b__num), c(0, 0, 10)), "step3_prey_b__num: Numbers rotated by 1, final group transitioned")
    ok(ut_cmp_equal(as.vector(r$step3_prey_b__wgt), c(100, 100, 100)), "step3_prey_b__wgt: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_identical(as.vector(r$step3_prey_c__num), c(0, 0, 0, 20, 30, 0, 0)), "step3_prey_c__num: Final age of prey_b transferred, numbers rotated")
    ok(ut_cmp_equal(as.vector(r$step3_prey_c__wgt), c(0, 0, 0, 200, 300, 0, 0)), "step3_prey_c__wgt: Final age of prey_b transferred")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})
