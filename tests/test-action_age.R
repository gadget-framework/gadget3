library(magrittr)
library(unittest)

library(gadget3)

# NB: We start at 11 to make sure we age into the right bracket
prey_a <- g3_stock('prey_a', c(1)) %>% g3s_age(11, 15)
prey_b <- g3_stock('prey_b', c(1)) %>% g3s_age(11, 13)
prey_c <- g3_stock('prey_c', c(1)) %>% g3s_age(11, 17)

# Store stock state in temporary variables labelled stock 0..n
report_action <- list()
for (step in 0:3) for (s in list(prey_a, prey_b, prey_c)) {
    assign(paste0("step", step, '_', s$name, '__num'), gadget3:::stock_instance(s))
    assign(paste0("step", step, '_', s$name, '__wgt'), gadget3:::stock_instance(s))
    report_action[[paste0("999:step", step, ':', s$name)]] <- gadget3:::f_substitute(~if (cur_time == step) {
        stepnum[] <- curnum
        stepwgt[] <- curwgt
        g3_report(stepnum)
        g3_report(stepwgt)
    }, list(
        stepnum = as.symbol(paste0("step", step, '_', s$name, '__num')),
        stepwgt = as.symbol(paste0("step", step, '_', s$name, '__wgt')),
        curnum = as.symbol(paste0(s$name, '__num')),
        curwgt = as.symbol(paste0(s$name, '__wgt')),
        step = step))
}

actions <- list(
    g3a_time(2000, 2002, steps = c(6, 6), project_years = 0),
    g3a_initialconditions(prey_a, ~10 * (age-10) + prey_a__midlen * 0, ~100 * (age-10) + prey_a__midlen * 0),
    g3a_initialconditions(prey_b, ~10 * (age-10) + prey_b__midlen * 0, ~100 * (age-10) + prey_b__midlen * 0),
    g3a_initialconditions(prey_c, ~prey_b__midlen * 0, ~prey_b__midlen * 0),
    g3a_age(prey_a),
    g3a_age(prey_b, output_stocks = list(prey_c)),
    g3a_age(prey_c),
    report_action,
    list('999' = ~{ nll <- nll + g3_param('x') }))
params <- list(
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

ok_group("age", {
    params <- list(
        x=1.0)
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
    ok(ut_cmp_equal(as.vector(r$step1_prey_a__wgt), c(0, 100,200,300, (400 * 40 + 500 * 50) / (40 + 50))), "step1_prey_a__wgt: Numbers rotated by 1, final group averaged plus-group")
    ok(ut_cmp_identical(as.vector(r$step1_prey_b__num), c(0, 10,20)), "step1_prey_b__num: Numbers rotated by 1, final group transitioned")
    ok(ut_cmp_equal(as.vector(r$step1_prey_b__wgt), c(0, 100, 200)), "step1_prey_b__wgt: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_identical(as.vector(r$step1_prey_c__num), c(0, 0, 0, 30, 0, 0, 0)), "step1_prey_c__num: Final age of prey_b transferred")
    ok(ut_cmp_equal(as.vector(r$step1_prey_c__wgt), c(0, 0, 0, 300, 0, 0, 0)), "step1_prey_c__wgt: Final weight of prey_b transferred")

    # Step 2
    ok(ut_cmp_identical(as.vector(r$step2_prey_a__num), c(0, 10,20,30,40 + 50)), "step2_prey_a__num: Not final step, nothing changed")
    ok(ut_cmp_identical(as.vector(r$step2_prey_a__wgt), c(0, 100,200,300, (400 * 40 + 500 * 50) / (40 + 50))), "step2_prey_a__wgt: Not final step, nothing changed")
    ok(ut_cmp_identical(as.vector(r$step2_prey_b__num), c(0, 10, 20)), "step2_prey_b__num: Not final step, nothing changed")
    ok(ut_cmp_equal(as.vector(r$step2_prey_b__wgt), c(0, 100, 200)), "step2_prey_b__wgt: Not final step, nothing changed")
    ok(ut_cmp_identical(as.vector(r$step2_prey_c__num), c(0, 0, 0, 30, 0, 0, 0)), "step2_prey_c__num: Not final step, nothing changed")
    ok(ut_cmp_equal(as.vector(r$step2_prey_c__wgt), c(0, 0, 0, 300, 0, 0, 0)), "step2_prey_c__wgt: Not final step, nothing changed")

    # Step 3
    ok(ut_cmp_identical(as.vector(r$step3_prey_a__num), c(0, 0, 10,20,30 + 40 + 50)), "step3_prey_a__num: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_equal(as.vector(r$step3_prey_a__wgt), c(0, 0, 100,200, (300 * 30 + 400 * 40 + 500 * 50) / (30 + 40 + 50))), "step3_prey_a__wgt: Numbers rotated by 1, final group a averaged plus group")
    ok(ut_cmp_identical(as.vector(r$step3_prey_b__num), c(0, 0, 10)), "step3_prey_b__num: Numbers rotated by 1, final group transitioned")
    ok(ut_cmp_equal(as.vector(r$step3_prey_b__wgt), c(0, 0, 100)), "step3_prey_b__wgt: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_identical(as.vector(r$step3_prey_c__num), c(0, 0, 0, 20, 30, 0, 0)), "step3_prey_c__num: Final age of prey_b transferred, numbers rotated")
    ok(ut_cmp_equal(as.vector(r$step3_prey_c__wgt), c(0, 0, 0, 200, 300, 0, 0)), "step3_prey_c__wgt: Final age of prey_b transferred")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})
