if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(magrittr)
library(unittest)

library(gadget3)

stock_imm1 <- g3_stock('stock_imm1', seq(10, 40, 10)) %>% g3s_age(3, 7)
stock_imm2 <- g3_stock('stock_imm2', seq(10, 40, 10)) %>% g3s_age(4, 7)
stock_mat <- g3_stock('stock_mat', seq(30, 40, 10)) %>% g3s_age(5, 7)

ok_group('g3a_spawn_recruitment_fecundity', {
    ok(ut_cmp_identical(
        rlang::f_rhs( g3a_spawn_recruitment_fecundity(90, 91, 92, 93, 94)$s ),
        quote( sum(stock__midlen^91 * age^92 * stock_ss(stock__spawningnum)^93 * stock_ss(stock__wgt)^94) )), "g3a_spawn_recruitment_fecundity$s")
    ok(ut_cmp_identical(
        rlang::f_rhs( g3a_spawn_recruitment_fecundity(90, 91, 92, 93, 94)$r ),
        quote( 90 * s  )), "g3a_spawn_recruitment_fecundity$r")
})

ok_group('g3a_spawn_recruitment_simplessb', {
    ok(ut_cmp_identical(
        rlang::f_rhs( g3a_spawn_recruitment_simplessb(91)$s ),
        quote( sum(stock_ss(stock__wgt) * stock_ss(stock__spawningnum)) )), "g3a_spawn_recruitment_simplessb$s")
    ok(ut_cmp_identical(
        rlang::f_rhs( g3a_spawn_recruitment_simplessb(91)$r ),
        quote( 91 * s )), "g3a_spawn_recruitment_simplessb$r")
})

ok(ut_cmp_error(
    g3a_spawn(stock_mat, output_stocks = list(stock_imm1, stock_imm2), output_ratios = c(9,9,9), recruitment_f = list(s = 1, r = 1)),
    "output_ratios"), "Length of output_ratios must match")
ok(ut_cmp_error(
    g3a_spawn(stock_mat, output_stocks = list(stock_imm1, stock_imm2), output_ratios = c(9,9), recruitment_f = list(s = 1, r = 1)),
    "output_ratios"), "output_ratios must sum to 1")

ok_group('g3a_spawn', {
    year_range <- 1982:1990

    # Add spawn step, spawning into a new stock with a name matching the recruitment function
    recruitment_test_step <- function (recruitment_f) {
        fn_name <- as.character(sys.call()[[2]][[1]])
        mat_stock <- g3_stock(c("mat", fn_name), seq(10, 40, 10)) %>% g3s_age(5, 5)
        imm_stock <- g3_stock(c("imm", fn_name), seq(10, 40, 10)) %>% g3s_age(0, 0)

        return(list(
            g3a_otherfood(  # Matures steadily grow over model run
                mat_stock,
                num_f = g3_formula(
                    cur_time * s1 + stock__midlen * 0,
                    s1 = g3_parameterized("abund_scalar", value = runif(1, min=90, max=110)) ),
                wgt_f = g3_formula(
                    cur_time * s2 + stock__midlen * 0,
                    s2 = g3_parameterized("mwgt_scalar", value = runif(1, min=9, max=11)) )),
            g3a_otherfood(  # Empty immature after each step
                imm_stock,
                num_f = quote( stock__midlen * 0 ),
                wgt_f = 1 ),
            g3a_spawn(
                mat_stock,
                recruitment_f,
                output_stocks = list(imm_stock) )))
    }
    actions <- list(
        g3a_time(min(year_range), max(year_range), c(3,3,3,3), project_years = 0),
        recruitment_test_step(g3a_spawn_recruitment_fecundity(
            g3_parameterized("fecundity_p0", value = runif(1, min=0.1, max=0.9)),
            g3_parameterized("fecundity_p1", value = runif(1, min=0.1, max=0.9)),
            g3_parameterized("fecundity_p2", value = runif(1, min=0.1, max=0.9)),
            g3_parameterized("fecundity_p3", value = runif(1, min=0.1, max=0.9)),
            g3_parameterized("fecundity_p4", value = runif(1, min=0.1, max=0.9)) )),
        recruitment_test_step(g3a_spawn_recruitment_simplessb(
            g3_parameterized("simplessb_mu", value = runif(1, min=0.1, max=0.9)) )),
        recruitment_test_step(g3a_spawn_recruitment_ricker(
            g3_parameterized("ricker_mu", value = runif(1, min=0.9, max=1.2)),
            g3_parameterized("ricker_lambda", value = runif(1, min=1e-6, max=1e-5)) )),
        recruitment_test_step(g3a_spawn_recruitment_bevertonholt(
            g3_parameterized("bevertonholt_mu", value = runif(1, min=0.4, max=0.6)),
            g3_parameterized("bevertonholt_lambda", value = runif(1, min=0.4, max=0.6)) )),
        recruitment_test_step(g3a_spawn_recruitment_bevertonholt_ss3()),
        recruitment_test_step(g3a_spawn_recruitment_hockeystick(
            g3_parameterized("hockeystick_r0", value = runif(1, min=0.4, max=0.6)),
            g3_parameterized("hockeystick_blim", value = runif(1, min=0.4, max=0.6)) )),
        list())

    all_actions <- c(actions, list(
        g3a_report_history(actions, '__num$|__wgt$|__offspringnum$'),
        g3a_report_history(actions, '__midlen$', out_prefix = NULL),
        NULL ))

    # Compile model
    model_fn <- g3_to_r(all_actions, trace = FALSE)
    model_cpp <- g3_to_tmb(all_actions, trace = FALSE)

    params <- attr(model_fn, 'parameter_template')

    # g3a_spawn_recruitment_bevertonholt_ss3
    params <- g3_init_val(params, 'mat_*.spawn.h', runif(1, 1.4, 1.5))
    params <- g3_init_val(params, 'mat_*.spawn.R0', runif(1, 1e4 - 10, 1e4 + 10))
    bholt_ss3_R <- runif(length(year_range), 1e2 - 10, 1e2 + 10)
    params <- g3_init_val(params, 'mat_*.spawn.R.#', bholt_ss3_R)
    params <- g3_init_val(params, 'mat_*.spawn.B0', runif(1, 1e3 - 10, 1e3 + 10))

    r <- lapply(attributes(model_fn(params)), drop)  # NB: Drop all redundant dimensions,
    age <- 5
    # Repeat midlen history for each time bin
    hist_midlen <- array(r$mat_g3a_spawn_recruitment_fecundity__midlen, dim = c(4, 36))
    hist_biomass <- colSums(r$hist_mat_g3a_spawn_recruitment_simplessb__num * r$hist_mat_g3a_spawn_recruitment_simplessb__wgt)
    ok(ut_cmp_equal(
        as.vector(colSums(r$hist_mat_g3a_spawn_recruitment_simplessb__num)),
        4 * 0:35 * params$abund_scalar,
        end = NULL ), "hist_mat_g3a_spawn_recruitment_simplessb__num: Test rig produced increasing abundance" )
    ok(ut_cmp_equal(
        as.vector(colSums(r$hist_mat_g3a_spawn_recruitment_simplessb__wgt)),
        4 * 0:35 * params$mwgt_scalar,
        end = NULL ), "hist_mat_g3a_spawn_recruitment_simplessb__wgt: Test rig produced increasing mean weight" )

    ok(ut_cmp_equal(
        as.vector(colSums(r$hist_mat_g3a_spawn_recruitment_fecundity__offspringnum) > 0),
        c(FALSE, rep(TRUE, 36 - 1)) ), "hist_mat_g3a_spawn_recruitment_fecundity__offspringnum: Some recruitment happened")
    ok(ut_cmp_equal(
        colSums(r$hist_mat_g3a_spawn_recruitment_fecundity__offspringnum),
        colSums(
            hist_midlen ^ params$fecundity_p1 *
            age ^ params$fecundity_p2 *
            r$hist_mat_g3a_spawn_recruitment_simplessb__num^params$fecundity_p3 *
            r$hist_mat_g3a_spawn_recruitment_simplessb__wgt^params$fecundity_p4 ) * params$fecundity_p0,
        end = NULL), "hist_mat_g3a_spawn_recruitment_fecundity__offspringnum: Matches formula")

    ok(ut_cmp_equal(
        as.vector(colSums(r$hist_mat_g3a_spawn_recruitment_simplessb__offspringnum) > 0),
        c(FALSE, rep(TRUE, 36 - 1)) ), "hist_mat_g3a_spawn_recruitment_simplessb__offspringnum: Some recruitment happened")
    ok(ut_cmp_equal(
        colSums(r$hist_mat_g3a_spawn_recruitment_simplessb__offspringnum),
        hist_biomass * params$simplessb_mu,
        end = NULL ), "hist_mat_g3a_spawn_recruitment_simplessb__offspringnum: Total biomass * mu")

    ok(ut_cmp_equal(
        as.vector(colSums(r$hist_mat_g3a_spawn_recruitment_ricker__offspringnum) > 0),
        c(FALSE, rep(TRUE, 36 - 1)) ), "hist_mat_g3a_spawn_recruitment_ricker__offspringnum: Some recruitment happened")
    ok(ut_cmp_equal(
        colSums(r$hist_mat_g3a_spawn_recruitment_ricker__offspringnum),
        params$ricker_mu * hist_biomass * exp(-params$ricker_lambda * hist_biomass),
        end = NULL ), "hist_mat_g3a_spawn_recruitment_ricker__offspringnum: Matches formula")

    ok(ut_cmp_equal(
        as.vector(colSums(r$hist_mat_g3a_spawn_recruitment_bevertonholt__offspringnum) > 0),
        c(FALSE, rep(TRUE, 36 - 1)) ), "hist_mat_g3a_spawn_recruitment_bevertonholt__offspringnum: Some recruitment happened")
    ok(ut_cmp_equal(
        colSums(r$hist_mat_g3a_spawn_recruitment_bevertonholt__offspringnum),
        params$bevertonholt_mu * hist_biomass/(params$bevertonholt_lambda + hist_biomass),
        end = NULL ), "hist_mat_g3a_spawn_recruitment_bevertonholt__offspringnum: Matches formula")

    ok(ut_cmp_equal(
        as.vector(colSums(r$hist_mat_g3a_spawn_recruitment_bevertonholt__offspringnum) > 0),
        c(FALSE, rep(TRUE, 36 - 1)) ), "hist_mat_g3a_spawn_recruitment_bevertonholt__offspringnum: Some recruitment happened")
    ok(ut_cmp_equal(
        colSums(r$hist_mat_g3a_spawn_recruitment_bevertonholt__offspringnum),
        params$bevertonholt_mu * hist_biomass/(params$bevertonholt_lambda + hist_biomass),
        end = NULL ), "hist_mat_g3a_spawn_recruitment_bevertonholt__offspringnum: Matches formula")

    ok(ut_cmp_equal(
        as.vector(colSums(r$hist_mat_g3a_spawn_recruitment_bevertonholt_ss3__offspringnum) > 0),
        c(FALSE, rep(TRUE, 36 - 1)) ), "hist_mat_g3a_spawn_recruitment_bevertonholt_ss3__offspringnum: Some recruitment happened")
    ok(ut_cmp_equal(
        colSums(r$hist_mat_g3a_spawn_recruitment_bevertonholt_ss3__offspringnum),
        4 * params$mat_g3a_spawn_recruitment_bevertonholt_ss3.spawn.h *
        params$mat_g3a_spawn_recruitment_bevertonholt_ss3.spawn.R0 *
        hist_biomass *
        rep(bholt_ss3_R, each = 4) / (
            params$mat_g3a_spawn_recruitment_bevertonholt_ss3.spawn.B0 *
            (1 - params$mat_g3a_spawn_recruitment_bevertonholt_ss3.spawn.h)
          +
            hist_biomass *
            (5 * params$mat_g3a_spawn_recruitment_bevertonholt_ss3.spawn.h - 1)),
        end = NULL ), "hist_mat_g3a_spawn_recruitment_bevertonholt_ss3__offspringnum: Matches formula")

    ok(ut_cmp_equal(
        as.vector(colSums(r$hist_mat_g3a_spawn_recruitment_hockeystick__offspringnum) > 0),
        c(FALSE, rep(TRUE, 36 - 1)) ), "hist_mat_g3a_spawn_recruitment_hockeystick__offspringnum: Some recruitment happened")
    ok(ut_cmp_equal(
        colSums(r$hist_mat_g3a_spawn_recruitment_hockeystick__offspringnum),
        params$hockeystick_r0 * pmin(hist_biomass/params$hockeystick_blim, 1),
        end = NULL ), "hist_mat_g3a_spawn_recruitment_hockeystick__offspringnum: Matches formula")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})
