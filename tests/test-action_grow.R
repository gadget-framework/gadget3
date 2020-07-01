library(unittest)

library(gadget3)

teststock <- g3_stock('teststock', 10, 40, 5)
teststock__num <- rep(NA, 6)  # TODO: Bodge around not being available yet, should use proper initialconditions
teststock__wgt <- rep(NA, 6)  # TODO: Bodge around not being available yet, should use proper initialconditions

ok_group("g3a_grow_impl_bbinom", {
    actions <- g3_collate(  # dmu, lengthgrouplen, binn, beta
        g3a_grow(teststock,
            growth_f = list(len = ~g3_param_vector('dmu'), wgt = ~0),
            impl_f = g3a_grow_impl_bbinom(~g3_param('beta'), ~g3_param('maxlengthgroupgrowth'))),
        list(
            "0" = ~{teststock__num <- g3_param_vector("initial")},
            "999" = ~{
                g3_report("teststock__num")
                g3_report("teststock__growth_l")
                return(0)
            }))
    params <- list(
        beta = 30,
        initial = c(10, 100, 1000, 1000, 10000, 100000),
        maxlengthgroupgrowth = 100,
        dmu = c(10, 10, 10, 10, 10, 10))
    model_fn <- g3_compile_r(actions)
    # model_fn <- edit(model_fn)
    result <- model_fn(params)
    ok(ut_cmp_equal(environment(model_fn)$model_report$teststock__growth_l, array(c(
        0.102145, 0.102145, 0.102145, 0.102145, 0.102145, 0.102145, 0.262659,
        0.262659, 0.262659, 0.262659, 0.262659, 0.262659, 0.309011, 0.309011,
        0.309011, 0.309011, 0.309011, 0.309011, 0.212250, 0.212250, 0.212250,
        0.212250, 0.212250, 0.212250, 0.089543, 0.089543, 0.089543, 0.089543,
        0.089543, 0.089543, 0.021952, 0.021952, 0.021952, 0.021952, 0.021952,
        0.021952, 0.002439, 0.002439, 0.002439, 0.002439, 0.002439, 0.002439), dim = c(6,7)), tolerance = 1e-5), "Matches baseline")

    model_cpp <- g3_precompile_tmb(actions)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
    ok(ut_cmp_equal(
        model_tmb$report()$teststock__growth_l,
        environment(model_fn)$model_report$teststock__growth_l,
        tolerance = 1e-5), "C++ and R match")
})

ok_group("g3a_grow", {
    actions <- g3_collate(
        g3a_grow(teststock,
            growth_f = list(len = ~0, wgt = ~g3_param_vector('growth_w')),
            impl_f = ~g3_param_array('growth_matrix')),
        list(
            "0" = ~{
                teststock__num <- g3_param_vector("initial_num")
                teststock__wgt <- g3_param_vector("initial_wgt")
            },
            "999" = ~{
                g3_report("teststock__num")
                g3_report("teststock__wgt")
                g3_report("teststock__growth_l")
                return(0)
            }))

    gm <- array(c(
     # 10    15 20   25   30 35
        0, 0.25, 1, 0.5, 0.5, 1,  # 0
        0,    0, 0,   0,   0, 0,  # +1
        1, 0.75, 0,   0,   0, 0,  # +2
        0,    0, 0,   0,   0, 0,  # +3
        0,    0, 0,   0,   0, 0,  # +4
        0,    0, 0,   0,   0, 0,  # +5
        0,    0, 0,   0,   0, 0), dim = c(6,7))
    params <- list(
        initial_num = c(10, 100, 1000, 1000, 10000, 100000),
        initial_wgt = c(100, 200, 300, 400, 500, 600),
        growth_w = c(1,2,3,4,5,6),
        growth_matrix = gm)

    model_fn <- g3_compile_r(actions)
    # model_fn <- edit(model_fn)
    result <- model_fn(params)
    ok(ut_cmp_identical(
        environment(model_fn)$model_report$teststock__num,
        c(0, 25, 1010, 575, 5000, 100000)), "Stock individuals have been scaled by matrix")
    ok(ut_cmp_equal(environment(model_fn)$model_report$teststock__wgt, c(
        ((100 * 10) + 1) / 0.00001,
        ((200 * 100) + 2) / 25,
        ((300 * 1000) + 3) / 1010,
        ((400 * 1000) + 4) / 575,
        ((500 * 10000) + 5) / 5000,
        ((600 * 100000) + 6) / 100000)), "Weight scaled, didn't let weight go to infinity when dividing by zero")

    model_cpp <- g3_precompile_tmb(actions)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
    ok(ut_cmp_identical(
        environment(model_fn)$model_report$teststock__num,
        model_tmb$report()$teststock__num), "Stock individuals are the same in C++ and R")
    ok(ut_cmp_identical(
        environment(model_fn)$model_report$teststock__growth_l,
        model_tmb$report()$teststock__growth_l), "Stock growth matrix is the same in C++ and R")
})
