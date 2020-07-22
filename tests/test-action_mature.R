library(magrittr)
library(unittest)

library(gadget3)

ok_group('g3a_mature_constant', {
    cmp_code <- function(a, b) ut_cmp_identical(rlang::f_rhs(a), rlang::f_rhs(b))

    ok(cmp_code(
        g3a_mature_constant(),
        ~1/(1 + exp(0)) ), "No arguments")

    # Invalid combinations
    ok(ut_cmp_error(g3a_mature_constant(alpha = 2), "l50"), "Missing l50")
    ok(ut_cmp_error(g3a_mature_constant(beta = 2), "a50"), "Missing a50")
    ok(ut_cmp_error(g3a_mature_constant(gamma = 2), "k50"), "Missing k50")

    # Single alpha/beta/gamma
    ok(cmp_code(
        g3a_mature_constant(alpha = 28, l50 = 24),
        ~1/(1 + exp(0 - 28 * (stock__meanlen - 24))) ), "alpha = 28, l50 = 24")
    ok(cmp_code(
        g3a_mature_constant(beta = 18, a50 = 83),
        ~1/(1 + exp(0 - 18 * (age - 83))) ), "beta = 18, a50 = 83")
    ok(cmp_code(
        g3a_mature_constant(gamma = 82, k50 = 27),
        ~1/(1 + exp(0 - 82 * (stock__wgt[stock__iter] - 27))) ), "gamma = 82, k50 = 27")

    # Can combine parameters
    ok(cmp_code(
        g3a_mature_constant(alpha = 73, l50 = 16, beta = 54, a50 = 32),
        ~1/(1 + exp(0 - 73 * (stock__meanlen - 16)
                      - 54 * (age - 32)
                      ))), "alpha = 73, l50 = 16, beta = 54, a50 = 32")
    ok(cmp_code(
        g3a_mature_constant(beta = 73, a50 = 67, gamma = 39, k50 = 73),
        ~1/(1 + exp(0 - 73 * (age - 67)
                      - 39 * (stock__wgt[stock__iter] - 73)
                      ))), "beta = 73, a50 = 67, gamma = 39, k50 = 73")

})

ok_group('g3a_mature', {
    stock_imm <- g3_stock('stock_imm', 10, 50, 10)
    stock_mat1 <- g3_stock('stock_mat1', 10, 50, 10)
    stock_mat2 <- g3_stock('stock_mat2', 10, 50, 10)

    ok(ut_cmp_error(
        g3a_mature(stock_imm, list(stock_mat1, stock_mat2), ~g3_param_vector("maturity"), output_ratios = c(9,9,9)),
        "output_ratios"), "Length of output_ratios must match")
    ok(ut_cmp_error(
        g3a_mature(stock_imm, list(stock_mat1, stock_mat2), ~g3_param_vector("maturity"), output_ratios = c(9,9)),
        "output_ratios"), "output_ratios must sum to 1")

    cur_time <- 0L  # Initialconditions needs to know what the time is
    actions <- g3_collate(
        g3a_initialconditions(stock_imm, ~g3_param_vector("imm_init_num"), ~g3_param_vector("imm_init_wgt")),
        g3a_initialconditions(stock_mat1, ~g3_param_vector("mat1_init_num"), ~g3_param_vector("mat1_init_wgt")),
        g3a_initialconditions(stock_mat2, ~g3_param_vector("mat2_init_num"), ~g3_param_vector("mat2_init_wgt")),
        g3a_mature(stock_imm,
            list(stock_mat1, stock_mat2),
            ~g3_param_vector("maturity"),
            output_ratios = list(~g3_param("ratio_mat1"), ~g3_param("ratio_mat2")),
            run_f = ~g3_param("run_f") == 1),
        list(
            '999' = ~{
                g3_report(stock_imm__num)
                g3_report(stock_imm__wgt)
                g3_report(stock_mat1__num)
                g3_report(stock_mat1__wgt)
                g3_report(stock_mat2__num)
                g3_report(stock_mat2__wgt)
                return(g3_param('x'))
            }))
    params <- list(
        imm_init_num = c(101, 102, 103, 104),
        imm_init_wgt = c(1, 2, 3, 4),
        maturity = c(0, 0, 1, 0),
        mat1_init_num = c(10, 20, 30, 40),
        mat1_init_wgt = c(9, 8, 7, 6),
        mat2_init_num = c(0, 0, 0, 0),
        mat2_init_wgt = c(0, 0, 0, 0),
        ratio_mat1 = 0.5,
        ratio_mat2 = 0.5,
        run_f = 1,  # TODO: if(param) is naughty
        x=1.0)

    # Compile model
    model_fn <- g3_compile_r(actions)
    # model_fn <- edit(model_fn)
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_cpp <- g3_precompile_tmb(actions)
        # model_cpp <- edit(model_cpp)
        model_tmb <- g3_tmb_adfun(model_cpp, params)
    } else {
        writeLines("# skip: not compiling TMB model")
    }

    ok_group("Move all of length 30 into mat1/mat2", {
        params <- list(
            imm_init_num = c(101, 102, 103, 104),
            imm_init_wgt = c(1, 2, 3, 4),
            maturity = c(0, 0, 1, 0),
            mat1_init_num = c(10, 20, 30, 40),
            mat1_init_wgt = c(9, 8, 7, 6),
            mat2_init_num = c(0, 0, 0, 0),
            mat2_init_wgt = c(0, 0, 0, 0),
            ratio_mat1 = 0.5,
            ratio_mat2 = 0.5,
            run_f = 1,
            x=1.0)
        result <- model_fn(params)
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_imm__num),
            c(101, 102, 0, 104)), "stock_imm__num")
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_mat1__num),
            c(10, 20, 30 + (103 / 2), 40)), "stock_mat1__num (NB: Added to existing numbers)")
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_mat2__num),
            c(0, 0, 103 / 2, 0)), "stock_mat2__num")

        if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
            # Reformat params into a single vector in expected order
            par <- unlist(params[names(environment(model_cpp)$model_parameters)])
            model_tmb_report <- model_tmb$report(par)
            for (n in ls(environment(model_fn)$model_report)) {
                ok(ut_cmp_equal(
                    model_tmb_report[[n]],
                    as.vector(environment(model_fn)$model_report[[n]]),
                    tolerance = 1e-5), paste("TMB and R match", n))
            }
        } else {
            writeLines("# skip: not running TMB tests")
        }
    })

    ok_group("Move all of length 30 into 90% mat1, 10% mat2", {
        params <- list(
            imm_init_num = c(101, 102, 103, 104),
            imm_init_wgt = c(1, 2, 3, 4),
            maturity = c(0, 0, 1, 0),
            mat1_init_num = c(10, 20, 30, 40),
            mat1_init_wgt = c(9, 8, 7, 6),
            mat2_init_num = c(0, 0, 0, 0),
            mat2_init_wgt = c(0, 0, 0, 0),
            ratio_mat1 = 0.9,
            ratio_mat2 = 0.1,
            run_f = 1,
            x=1.0)
        result <- model_fn(params)
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_imm__num),
            c(101, 102, 0, 104)), "stock_imm__num")
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_mat1__num),
            c(10, 20, 30 + (103 * 0.9), 40)), "stock_mat1__num (NB: Added to existing numbers)")
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_mat2__num),
            c(0, 0, 103 * 0.1, 0)), "stock_mat2__num")

        if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
            # Reformat params into a single vector in expected order
            par <- unlist(params[names(environment(model_cpp)$model_parameters)])
            model_tmb_report <- model_tmb$report(par)
            for (n in ls(environment(model_fn)$model_report)) {
                ok(ut_cmp_equal(
                    model_tmb_report[[n]],
                    as.vector(environment(model_fn)$model_report[[n]]),
                    tolerance = 1e-5), paste("TMB and R match", n))
            }
        } else {
            writeLines("# skip: not running TMB tests")
        }
    })

    ok_group("Move 50% of length 30, 75% of length 40", {
        params <- list(
            imm_init_num = c(101, 102, 103, 104),
            imm_init_wgt = c(1, 2, 3, 4),
            maturity = c(0, 0, 0.5, 0.75),
            mat1_init_num = c(10, 20, 30, 40),
            mat1_init_wgt = c(9, 8, 7, 6),
            mat2_init_num = c(0, 0, 0, 0),
            mat2_init_wgt = c(0, 0, 0, 0),
            ratio_mat1 = 0.5,
            ratio_mat2 = 0.5,
            run_f = 1,
            x=1.0)
        result <- model_fn(params)
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_imm__num),
            c(101, 102, 103 * 0.5, 104 * 0.25)), "stock_imm__num")
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_mat1__num),
            c(10, 20, 30 + (103 * 0.5 / 2), 40 + (104 * 0.75 / 2))), "stock_mat1__num (NB: Added to existing numbers)")
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_mat2__num),
            c(0, 0, 103 * 0.5 / 2, 104 * 0.75 / 2)), "stock_mat2__num")

        if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
            # Reformat params into a single vector in expected order
            par <- unlist(params[names(environment(model_cpp)$model_parameters)])
            model_tmb_report <- model_tmb$report(par)
            for (n in ls(environment(model_fn)$model_report)) {
                ok(ut_cmp_equal(
                    model_tmb_report[[n]],
                    as.vector(environment(model_fn)$model_report[[n]]),
                    tolerance = 1e-5), paste("TMB and R match", n))
            }
        } else {
            writeLines("# skip: not running TMB tests")
        }
    })

    ok_group("Disable with run_f = 0", {
        params <- list(
            imm_init_num = c(101, 102, 103, 104),
            imm_init_wgt = c(1, 2, 3, 4),
            maturity = c(0, 0, 0.5, 0.75),
            mat1_init_num = c(10, 20, 30, 40),
            mat1_init_wgt = c(9, 8, 7, 6),
            mat2_init_num = c(0, 0, 0, 0),
            mat2_init_wgt = c(0, 0, 0, 0),
            ratio_mat1 = 0.5,
            ratio_mat2 = 0.5,
            run_f = 0,  # NB: Turned off
            x=1.0)
        result <- model_fn(params)
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_imm__num),
            c(101, 102, 103, 104)), "stock_imm__num same as start")
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_mat1__num),
            c(10, 20, 30, 40)), "stock_mat1__num same as start")
        ok(ut_cmp_equal(
            as.vector(environment(model_fn)$model_report$stock_mat2__num),
            c(0, 0, 0, 0)), "stock_mat2__num same as start")

        if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
            # Reformat params into a single vector in expected order
            par <- unlist(params[names(environment(model_cpp)$model_parameters)])
            model_tmb_report <- model_tmb$report(par)
            for (n in ls(environment(model_fn)$model_report)) {
                ok(ut_cmp_equal(
                    model_tmb_report[[n]],
                    as.vector(environment(model_fn)$model_report[[n]]),
                    tolerance = 1e-5), paste("TMB and R match", n))
            }
        } else {
            writeLines("# skip: not running TMB tests")
        }
    })
})
