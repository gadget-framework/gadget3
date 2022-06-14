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
        ~1/(1 + exp((0 - (28) * (stock__midlen - (24))))) ), "alpha = 28, l50 = 24")
    ok(cmp_code(
        g3a_mature_constant(beta = 18, a50 = 83),
        ~1/(1 + exp((0 - (18) * (age - (83))))) ), "beta = 18, a50 = 83")
    ok(cmp_code(
        g3a_mature_constant(gamma = 82, k50 = 27),
        ~1/(1 + exp((0 - (82) * (stock_ss(stock__wgt) - (27))))) ), "gamma = 82, k50 = 27")

    # Can combine parameters
    ok(cmp_code(
        g3a_mature_constant(alpha = 73, l50 = 16, beta = 54, a50 = 32),
        ~1/(1 + exp(((0 - (73) * (stock__midlen - (16)))
                      - (54) * (age - (32)))
                      ))), "alpha = 73, l50 = 16, beta = 54, a50 = 32")
    ok(cmp_code(
        g3a_mature_constant(beta = 73, a50 = 67, gamma = 39, k50 = 73),
        ~1/(1 + exp(((0 - (73) * (age - (67)))
                      - (39) * (stock_ss(stock__wgt) - (73))
                      )))), "beta = 73, a50 = 67, gamma = 39, k50 = 73")

    # alpha/beta/gamma can be formula
    ok(cmp_code(
        g3a_mature_constant(alpha = ~g3_param("ling.mat1"), l50 = ~g3_param("ling.mat2")),
        ~1/(1 + exp((0 - (g3_param("ling.mat1")) * (stock__midlen - (g3_param("ling.mat2"))))))), "alpha = formula")

    ok(cmp_code(
        g3a_mature_constant(beta = ~g3_param("ling.mat1"), a50 = ~g3_param("ling.mat2")),
        ~1/(1 + exp((0 - (g3_param("ling.mat1")) * (age - (g3_param("ling.mat2"))))))), "beta = formula")

    ok(cmp_code(
        g3a_mature_constant(gamma = ~g3_param("ling.mat1"), k50 = ~g3_param("ling.mat2")),
        ~1/(1 + exp((0 - (g3_param("ling.mat1")) * (stock_ss(stock__wgt) - (g3_param("ling.mat2"))))))), "gamma = formula")
})

ok_group('g3a_mature', {
    stock_imm <- g3_stock('stock_imm', seq(10, 40, 10)) %>% g3s_age(3, 7)
    stock_mat1 <- g3_stock('stock_mat1', seq(10, 40, 10)) %>% g3s_age(4, 7)
    stock_mat2 <- g3_stock('stock_mat2', seq(10, 40, 10)) %>% g3s_age(5, 7)

    ok(ut_cmp_error(
        g3a_mature(stock_imm, ~g3_param_vector("maturity"), list(stock_mat1, stock_mat2), output_ratios = c(9,9,9)),
        "output_ratios"), "Length of output_ratios must match")
    ok(ut_cmp_error(
        g3a_mature(stock_imm, ~g3_param_vector("maturity"), list(stock_mat1, stock_mat2), output_ratios = c(9,9)),
        "output_ratios"), "output_ratios must sum to 1")

    actions <- list(
        g3a_time(2000, 2000, project_years = 0),
        g3a_initialconditions(stock_imm, ~max(6L - age, 1L) * g3_param_vector("imm_init_num"), ~g3_param_vector("imm_init_wgt")),
        g3a_initialconditions(stock_mat1, ~max(6L - age, 1L) * g3_param_vector("mat1_init_num"), ~g3_param_vector("mat1_init_wgt")),
        g3a_initialconditions(stock_mat2, ~max(6L - age, 1L) * g3_param_vector("mat2_init_num"), ~g3_param_vector("mat2_init_wgt")),
        g3a_mature(stock_imm,
            ~g3_param_vector("maturity"),
            list(stock_mat1, stock_mat2),
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
                nll <- nll + g3_param('x')
                return(nll)
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
    model_fn <- g3_to_r(actions, trace = FALSE)
    # model_fn <- edit(model_fn)
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_cpp <- g3_to_tmb(actions, trace = FALSE)
        # model_cpp <- edit(model_cpp)
        model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
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
        r <- attributes(result)
        ok(ut_cmp_equal(
            as.vector(r$stock_imm__num[,'age5']),
            c(101, 102, 0, 104)), "stock_imm__num")
        ok(ut_cmp_equal(
            as.vector(r$stock_imm__num[length = '30:40',]),
            c(103 * 3, (103 * 2) / 2, 0, 0, 0)), "stock_imm__num: age3 and half of age4 left behind, since they don't fit in mature stocks")
        ok(ut_cmp_equal(
            as.vector(r$stock_mat1__num[,'age5']),
            c(10, 20, 30 + (103 / 2), 40)), "stock_mat1__num (NB: Added to existing numbers)")
        ok(ut_cmp_equal(
            as.vector(r$stock_mat1__num[length = '30:40',]),
            c(60 + (103 * 2) / 2, 81.5, 81.5, 81.5)), "stock_mat1__num: Got half of age4")
        ok(ut_cmp_equal(
            as.vector(r$stock_mat2__num[,'age5']),
            c(0, 0, 103 / 2, 0)), "stock_mat2__num")
        ok(ut_cmp_equal(
            as.vector(r$stock_mat2__num[length = '30:40',]),
            c(51.5, 51.5, 51.5)), "stock_mat2__num: Just age 5/6/7")

        if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
            param_template <- attr(model_cpp, "parameter_template")
            param_template$value <- params[param_template$switch]
            gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
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
        r <- attributes(result)
        ok(ut_cmp_equal(
            as.vector(r$stock_imm__num[,'age5']),
            c(101, 102, 0, 104)), "stock_imm__num")
        ok(ut_cmp_equal(
            as.vector(r$stock_mat1__num[,'age5']),
            c(10, 20, 30 + (103 * 0.9), 40)), "stock_mat1__num (NB: Added to existing numbers)")
        ok(ut_cmp_equal(
            as.vector(r$stock_mat2__num[,'age5']),
            c(0, 0, 103 * 0.1, 0)), "stock_mat2__num")

        if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
            param_template <- attr(model_cpp, "parameter_template")
            param_template$value <- params[param_template$switch]
            gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
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
        r <- attributes(result)
        ok(ut_cmp_equal(
            as.vector(r$stock_imm__num[,'age5']),
            c(101, 102, 103 * 0.5, 104 * 0.25)), "stock_imm__num")
        ok(ut_cmp_equal(
            as.vector(r$stock_mat1__num[,'age5']),
            c(10, 20, 30 + (103 * 0.5 / 2), 40 + (104 * 0.75 / 2))), "stock_mat1__num (NB: Added to existing numbers)")
        ok(ut_cmp_equal(
            as.vector(r$stock_mat2__num[,'age5']),
            c(0, 0, 103 * 0.5 / 2, 104 * 0.75 / 2)), "stock_mat2__num")

        if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
            param_template <- attr(model_cpp, "parameter_template")
            param_template$value <- params[param_template$switch]
            gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
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
        r <- attributes(result)
        ok(ut_cmp_equal(
            as.vector(r$stock_imm__num[,'age5']),
            c(101, 102, 103, 104)), "stock_imm__num same as start")
        ok(ut_cmp_equal(
            as.vector(r$stock_mat1__num[,'age5']),
            c(10, 20, 30, 40)), "stock_mat1__num same as start")
        ok(ut_cmp_equal(
            as.vector(r$stock_mat2__num[,'age5']),
            c(0, 0, 0, 0)), "stock_mat2__num same as start")

        if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
            param_template <- attr(model_cpp, "parameter_template")
            param_template$value <- params[param_template$switch]
            gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
        }
    })
})
