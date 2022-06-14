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

    ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) %>%
        g3s_livesonareas(c(1)) %>%
        g3s_age(3, 10)

    ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) %>%
        g3s_livesonareas(c(1)) %>%
        g3s_age(5, 15)

    igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c(1))

    imm_report <- g3s_clone(ling_imm, 'imm_report') %>% g3s_time(year = local(year_range), step = 1:4)
    mat_report <- g3s_clone(ling_mat, 'mat_report') %>% g3s_time(year = local(year_range), step = 1:4)

    ling_imm_actions <- list(
        g3a_initialconditions_normalparam(ling_imm,
            factor_f = ~age * g3_param("lingimm.init") * g3_param("lingimm.init.scalar"),
            mean_f = ~g3_param("ling.Linf"),
            stddev_f = ~10,
            alpha_f = ~g3_param("lingimm.walpha"),
            beta_f = ~g3_param("lingimm.wbeta")),
        g3a_naturalmortality(ling_imm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
        g3a_age(ling_imm),
        list())

    ling_mat_actions <- list(
        g3a_initialconditions_normalparam(ling_mat,
            factor_f = ~age * g3_param("lingmat.init") * g3_param("lingmat.init.scalar"),
            mean_f = ~g3_param("ling.Linf"),
            stddev_f = ~10,
            alpha_f = ~g3_param("lingmat.walpha"),
            beta_f = ~g3_param("lingmat.wbeta")),
        g3a_naturalmortality(ling_mat, g3a_naturalmortality_exp(~g3_param("lingmat.M"))),
        g3a_age(ling_mat),
        g3a_spawn(
            ling_mat,
            recruitment_f = g3a_spawn_recruitment_ricker(
                ~g3_param("ricker.mu"),
                ~g3_param("ricker.lambda")),
            proportion_f = g3_suitability_exponentiall50(alpha = ~-g3_param("spawn.prop.alpha"), l50 =  ~g3_param("spawn.prop.l50")),
            mortality_f = g3_suitability_straightline(alpha = ~g3_param("spawn.mort.alpha"), beta =  ~g3_param("spawn.mort.beta")),
            weightloss_f = ~g3_param("spawn.weightloss"),
            output_stocks = list(ling_imm),
            mean_f = 50,
            stddev_f = 0.9,
            alpha_f = 1,
            beta_f = 1,
            run_f = ~cur_step==1),
        list())

    report_actions <- list(
           g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__num)),
           g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__wgt)),
           g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__num)),
           g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__wgt)),
           list())

    time_actions <- list(
        g3a_time(min(year_range), max(year_range), c(3,3,3,3), project_years = 0),
        list())

    # Add steps to exercise rest of recruitment functions, and check they produce identical TMB results
    recruitment_test_step <- function (recruitment_f) {
        action_name <- gadget3:::unique_action_name()

        # Re-implement enough of spawning to test recruitment
        stock <- ling_mat
        out_var_name <- paste0('stock__rf_', sys.call()[[2]][[1]])
        assign(out_var_name, gadget3:::stock_instance(stock))
        out <- list()
        out[[gadget3:::step_id(999, action_name)]] <- gadget3:::g3_step(gadget3:::f_substitute(
            ~g3_with(s := 0 * nll, {  # TODO: Ugly mess to get type right
                stock_iterate(stock, if (run_f) {
                    s <- s + recruitment_s_f
                    stock_ss(stock__outvar) <- 1
                } else {
                    stock_ss(stock__outvar) <- 0
                })
                g3_with(r := recruitment_r_f,
                    stock_with(stock, stock__outvar <- r * stock__outvar / avoid_zero(sum(stock__outvar))))
            }),
            list(
                recruitment_r_f = recruitment_f$r,
                recruitment_s_f = recruitment_f$s,
                stock__outvar = as.symbol(out_var_name))))
        return(out)
    }
    recruitment_f_actions <- list(
        recruitment_test_step(g3a_spawn_recruitment_fecundity(
            p0 = runif(1, min=0.1, max=0.9),
            p1 = runif(1, min=0.1, max=0.9),
            p2 = runif(1, min=0.1, max=0.9),
            p3 = runif(1, min=0.1, max=0.9),
            p4 = runif(1, min=0.1, max=0.9))),
        recruitment_test_step(g3a_spawn_recruitment_simplessb(runif(1, min=0.1, max=0.9))),
        recruitment_test_step(g3a_spawn_recruitment_ricker(runif(1, min=0.1, max=0.9), runif(1, min=0.1, max=0.9))),
        recruitment_test_step(g3a_spawn_recruitment_bevertonholt(runif(1, min=0.1, max=0.9), runif(1, min=0.1, max=0.9))),
        recruitment_test_step(g3a_spawn_recruitment_hockeystick(runif(1, min=0.1, max=0.9), runif(1, min=0.1, max=0.9))),
        list())

    actions <- c(
      ling_imm_actions,
      ling_mat_actions,
      report_actions,
      recruitment_f_actions,
      time_actions)

    params <- list(
        "lingimm.init" =     0,
        "lingimm.init.scalar" = 0,
        "lingimm.rec.scalar" = 100,
        "lingimm.M" = 0,
        "lingimm.walpha" = 1e-1,
        "lingimm.wbeta" = 2,
        "lingmat.init" = 1,
        "lingmat.init.scalar" = 100,
        "lingmat.rec.scalar" = 100,
        "lingmat.M" = 0.95,
        "lingmat.walpha" = 1e-6,
        "lingmat.wbeta" = 2,
        "ling.init.F" = 0.4,
        "ling.mat.alpha" = 0.01,
        "ling.mat.l50" = 75,
        "ling.mat.beta" = 0.01,
        "ling.mat.a50" = 7,
        "ling.Linf" = 160,
        "ling.bbin" = 6,
        "ling.k" = 10,
        "ricker.mu" = 1,
        "ricker.lambda" = 1e-6,
        "spawn.prop.alpha" = 0.5,
        "spawn.prop.l50" = 120,
        "spawn.mort.alpha" = 0,
        "spawn.mort.beta" = 0,
        "spawn.weightloss" = 0.1)

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

    # Make sure the inttest model produces identical output in TMB and R
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})
