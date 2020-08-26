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

prey_a <- g3_stock('prey_a', c(1)) %>% g3s_age(1, 5)
prey_b <- g3_stock('prey_b', c(1)) %>% g3s_age(1, 3)

step0_prey_a__num <- gadget3:::stock_definition(prey_a, 'stock__num')
step0_prey_a__wgt <- gadget3:::stock_definition(prey_a, 'stock__wgt')
step0_prey_b__num <- gadget3:::stock_definition(prey_b, 'stock__num')
step0_prey_b__wgt <- gadget3:::stock_definition(prey_b, 'stock__wgt')
step1_prey_a__num <- gadget3:::stock_definition(prey_a, 'stock__num')
step1_prey_a__wgt <- gadget3:::stock_definition(prey_a, 'stock__wgt')
step1_prey_b__num <- gadget3:::stock_definition(prey_b, 'stock__num')
step1_prey_b__wgt <- gadget3:::stock_definition(prey_b, 'stock__wgt')
step2_prey_a__num <- gadget3:::stock_definition(prey_a, 'stock__num')
step2_prey_a__wgt <- gadget3:::stock_definition(prey_a, 'stock__wgt')
step2_prey_b__num <- gadget3:::stock_definition(prey_b, 'stock__num')
step2_prey_b__wgt <- gadget3:::stock_definition(prey_b, 'stock__wgt')
step3_prey_a__num <- gadget3:::stock_definition(prey_a, 'stock__num')
step3_prey_a__wgt <- gadget3:::stock_definition(prey_a, 'stock__wgt')
step3_prey_b__num <- gadget3:::stock_definition(prey_b, 'stock__num')
step3_prey_b__wgt <- gadget3:::stock_definition(prey_b, 'stock__wgt')

actions <- g3_collate(
    g3a_time(2000, 2002, steps = c(6, 6)),
    g3a_initialconditions(prey_a, ~10 * age + prey_a__midlen * 0, ~100 * age + prey_a__midlen * 0),
    g3a_initialconditions(prey_b, ~10 * age + prey_b__midlen * 0, ~100 * age + prey_b__midlen * 0),
    g3a_age(prey_a),
    g3a_age(prey_b),
    list(
        '999' = ~{
            if (cur_time == 0) {
                step0_prey_a__num[] <- prey_a__num
                step0_prey_a__wgt[] <- prey_a__wgt
                step0_prey_b__num[] <- prey_b__num
                step0_prey_b__wgt[] <- prey_b__wgt
                g3_report(step0_prey_a__num)
                g3_report(step0_prey_a__wgt)
                g3_report(step0_prey_b__num)
                g3_report(step0_prey_b__wgt)
            } else if (cur_time == 1) {
                step1_prey_a__num[] <- prey_a__num
                step1_prey_a__wgt[] <- prey_a__wgt
                step1_prey_b__num[] <- prey_b__num
                step1_prey_b__wgt[] <- prey_b__wgt
                g3_report(step1_prey_a__num)
                g3_report(step1_prey_a__wgt)
                g3_report(step1_prey_b__num)
                g3_report(step1_prey_b__wgt)
            } else if (cur_time == 2) {
                step2_prey_a__num[] <- prey_a__num
                step2_prey_a__wgt[] <- prey_a__wgt
                step2_prey_b__num[] <- prey_b__num
                step2_prey_b__wgt[] <- prey_b__wgt
                g3_report(step2_prey_a__num)
                g3_report(step2_prey_a__wgt)
                g3_report(step2_prey_b__num)
                g3_report(step2_prey_b__wgt)
            } else if (cur_time == 3) {
                step3_prey_a__num[] <- prey_a__num
                step3_prey_a__wgt[] <- prey_a__wgt
                step3_prey_b__num[] <- prey_b__num
                step3_prey_b__wgt[] <- prey_b__wgt
                g3_report(step3_prey_a__num)
                g3_report(step3_prey_a__wgt)
                g3_report(step3_prey_b__num)
                g3_report(step3_prey_b__wgt)
            }
            g3_report(step3_prey_a__num)
            g3_report(step3_prey_a__wgt)
            g3_report(step3_prey_b__num)
            g3_report(step3_prey_b__wgt)

            nll <- g3_param('x')
        }))
params <- list(
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

ok_group("age", {
    params <- list(
        x=1.0)
    result <- model_fn(params)
    r <- environment(model_fn)$model_report
    # str(result)
    # str(as.list(r), vec.len = 10000)

    # Step 0
    ok(ut_cmp_identical(r$step0_prey_a__num, array(c(10,20,30,40,50), dim=c(1,5))), "step0_prey_a__num: Populated by initialconditions, no ageing yet")
    ok(ut_cmp_identical(r$step0_prey_a__wgt, array(c(100,200,300,400,500), dim=c(1,5))), "step0_prey_a__wgt: Populated by initialconditions, no ageing yet")
    ok(ut_cmp_identical(r$step0_prey_b__num, array(c(10,20,30), dim=c(1,3))), "step0_prey_b__num: Populated by initialconditions, no ageing yet")
    ok(ut_cmp_identical(r$step0_prey_b__wgt, array(c(100,200,300), dim=c(1,3))), "step0_prey_b__wgt: Populated by initialconditions, no ageing yet")

    # Step 1
    ok(ut_cmp_identical(r$step1_prey_a__num, array(c(0, 10,20,30,40 + 50), dim=c(1,5))), "step1_prey_a__num: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_equal(r$step1_prey_a__wgt, array(c(1e-05, 100,200,300,400 + 500), dim=c(1,5)), tolerance = 1e-5), "step1_prey_a__wgt: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_identical(r$step1_prey_b__num, array(c(0, 10,20 + 30), dim=c(1,3))), "step1_prey_b__num: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_equal(r$step1_prey_b__wgt, array(c(1e-05, 100,200 + 300), dim=c(1,3)), tolerance = 1e-5), "step1_prey_b__wgt: Numbers rotated by 1, final group a plus group")

    # Step 2
    ok(ut_cmp_identical(r$step2_prey_a__num, array(c(0, 10,20,30,40 + 50), dim=c(1,5))), "step2_prey_a__num: Not final step, nothing changed")
    ok(ut_cmp_equal(r$step2_prey_a__wgt, array(c(1e-05, 100,200,300,400 + 500), dim=c(1,5)), tolerance = 1e-5), "step1_prey_a__wgt: Not final step, nothing changed")
    ok(ut_cmp_identical(r$step2_prey_b__num, array(c(0, 10,20 + 30), dim=c(1,3))), "step2_prey_b__num: Not final step, nothing changed")
    ok(ut_cmp_equal(r$step2_prey_b__wgt, array(c(1e-05, 100,200 + 300), dim=c(1,3)), tolerance = 1e-5), "step2_prey_b__wgt: Not final step, nothing changed")

    # Step 3
    # NB: The tiny amounts are summing. This isn't ideal, but these should be erased by renewal in practice
    ok(ut_cmp_identical(r$step3_prey_a__num, array(c(0, 0, 10,20,30 + 40 + 50), dim=c(1,5))), "step3_prey_a__num: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_equal(r$step3_prey_a__wgt, array(c(1e-05, 2e-05, 100,200,300 + 400 + 500), dim=c(1,5)), tolerance = 1e-5), "step3_prey_a__wgt: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_identical(r$step3_prey_b__num, array(c(0, 0, 10 + 20 + 30), dim=c(1,3))), "step3_prey_b__num: Numbers rotated by 1, final group a plus group")
    ok(ut_cmp_equal(r$step3_prey_b__wgt, array(c(1e-05, 2e-05, 100 + 200 + 300), dim=c(1,3)), tolerance = 1e-5), "step3_prey_b__wgt: Numbers rotated by 1, final group a plus group")

    tmb_r_compare(model_fn, model_tmb, params)
})
