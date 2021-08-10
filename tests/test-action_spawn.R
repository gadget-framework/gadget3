library(magrittr)
library(unittest)

library(gadget3)

tmb_r_compare <- function (model_fn, model_tmb, params) {
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        # Reformat params into a single vector in expected order
        par <- unlist(params[attr(model_cpp, 'parameter_template')$switch])
        model_tmb_report <- model_tmb$report(par)
        r_result <- model_fn(params)
        for (n in names(attributes(r_result))) {
            ok(ut_cmp_equal(
                as.vector(model_tmb_report[[n]]),
                as.vector(attr(r_result, n)),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}

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
            proportion_f = g3a_spawn_length_exponential(alpha = ~g3_param("spawn.prop.alpha"), l50 =  ~g3_param("spawn.prop.l50")),
            mortality_f = g3a_spawn_length_straightline(alpha = ~g3_param("spawn.mort.alpha"), beta =  ~g3_param("spawn.mort.beta")),
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
        g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
        list())

    actions <- c(
      ling_imm_actions,
      ling_mat_actions,
      report_actions,
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
    tmb_r_compare(model_fn, model_tmb, params)
})
