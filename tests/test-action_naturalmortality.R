library(magrittr)
library(unittest)

library(gadget3)

prey_a <- g3_stock('prey_a', c(1)) %>% g3s_age(3, 5)
# NB: 1 per age, starting at 3
naturalmortality_prey_a <- c(0.6, 0.7, 0.1)

step0_prey_a__num <- gadget3:::stock_instance(prey_a)
step0_prey_a__wgt <- gadget3:::stock_instance(prey_a)
step1_prey_a__num <- gadget3:::stock_instance(prey_a)
step1_prey_a__wgt <- gadget3:::stock_instance(prey_a)
step2_prey_a__num <- gadget3:::stock_instance(prey_a)
step2_prey_a__wgt <- gadget3:::stock_instance(prey_a)
step3_prey_a__num <- gadget3:::stock_instance(prey_a)
step3_prey_a__wgt <- gadget3:::stock_instance(prey_a)

actions <- list(
    g3a_time(2000, 2000, step_lengths = c(3, 3, 5, 1), project_years = 0),
    g3a_initialconditions(
        prey_a,
        ~10 * age + prey_a__midlen * 0,
        ~100 * age + prey_a__midlen * 0),
    g3a_naturalmortality(
        prey_a,
        g3a_naturalmortality_exp(~naturalmortality_prey_a[[age - 3 + 1]]),
        run_f = ~cur_time > 0),  # NB: No mortality on the first step
    list(
        '999' = ~{
            if (cur_time == 0) {
                step0_prey_a__num[] <- prey_a__num
                step0_prey_a__wgt[] <- prey_a__wgt
                REPORT(step0_prey_a__num)
                REPORT(step0_prey_a__wgt)
            } else if (cur_time == 1) {
                step1_prey_a__num[] <- prey_a__num
                step1_prey_a__wgt[] <- prey_a__wgt
                REPORT(step1_prey_a__num)
                REPORT(step1_prey_a__wgt)
            } else if (cur_time == 2) {
                step2_prey_a__num[] <- prey_a__num
                step2_prey_a__wgt[] <- prey_a__wgt
                REPORT(step2_prey_a__num)
                REPORT(step2_prey_a__wgt)
            } else if (cur_time == 3) {
                step3_prey_a__num[] <- prey_a__num
                step3_prey_a__wgt[] <- prey_a__wgt
                REPORT(step3_prey_a__num)
                REPORT(step3_prey_a__wgt)
            }
            REPORT(step3_prey_a__num)
            REPORT(step3_prey_a__wgt)

            nll <- nll + g3_param('x', value = 1.0)
        }))

# Compile model
model_fn <- g3_to_r(actions)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

ok_group("natural mortality", {
    params <- attr(model_fn, 'parameter_template')

    result <- model_fn(params)
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)

    # Step 0
    ok(ut_cmp_identical(as.vector(r$step0_prey_a__num), c(30, 40, 50)), "step0_prey_a__num: Natural mortality disabled by run_f")
    ok(ut_cmp_identical(as.vector(r$step0_prey_a__wgt), c(300, 400, 500)), "step0_prey_a__wgt: Weight unchanged")

    # Step 1
    ok(ut_cmp_identical(as.vector(r$step1_prey_a__num), c(
        30 * exp(-0.6 * 3 * (1/12)),
        40 * exp(-0.7 * 3 * (1/12)),
        50 * exp(-0.1 * 3 * (1/12)))), "step1_prey_a__num: Natural mortality reduced using exp(vec)")
    ok(ut_cmp_identical(as.vector(r$step1_prey_a__wgt), c(300, 400, 500)), "step1_prey_a__wgt: Weight unchanged")

    # Step 2
    ok(ut_cmp_identical(as.vector(r$step2_prey_a__num), c(
        30 * exp(-0.6 * 3 * (1/12)) * exp(-0.6 * 5 * (1/12)),
        40 * exp(-0.7 * 3 * (1/12)) * exp(-0.7 * 5 * (1/12)),
        50 * exp(-0.1 * 3 * (1/12)) * exp(-0.1 * 5 * (1/12)))), "step2_prey_a__num: Reduced again, used different step size")
    ok(ut_cmp_identical(as.vector(r$step2_prey_a__wgt), c(300, 400, 500)), "step2_prey_a__wgt: Weight unchanged")

    # Step 3
    ok(ut_cmp_identical(as.vector(r$step3_prey_a__num), c(
        30 * exp(-0.6 * 3 * (1/12)) * exp(-0.6 * 5 * (1/12)) * exp(-0.6 * 1 * (1/12)),
        40 * exp(-0.7 * 3 * (1/12)) * exp(-0.7 * 5 * (1/12)) * exp(-0.7 * 1 * (1/12)),
        50 * exp(-0.1 * 3 * (1/12)) * exp(-0.1 * 5 * (1/12)) * exp(-0.1 * 1 * (1/12)))), "step3_prey_a__num: Reduced one more time, using another step size")
    ok(ut_cmp_identical(as.vector(r$step3_prey_a__wgt), c(300, 400, 500)), "step3_prey_a__wgt: Weight unchanged")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})
