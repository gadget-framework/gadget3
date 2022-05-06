library(magrittr)
library(unittest)

library(gadget3)

ok_group('g3a_likelihood_tagging_ckmr', {
    year_range <- 1982:1990

    ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) %>%
        g3s_livesonareas(c(1)) %>%
        g3s_age(3, 10)

    ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) %>%
        g3s_livesonareas(c(1)) %>%
        g3s_age(5, 15)

    fleet_ckmr <- g3_fleet('fleet_ckmr') %>% g3s_livesonareas(c(1))

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
            weightloss_f = ~g3_param("spawn.weightloss"),
            output_stocks = list(ling_imm),
            mean_f = 50,
            stddev_f = 10,
            alpha_f = ~g3_param("lingmat.walpha"),
            beta_f = ~g3_param("lingmat.wbeta"),
            run_f = ~cur_step==1),
        list())

    fleet_actions <- list(
        g3a_predate_totalfleet(
            fleet_ckmr,
            list(ling_imm, ling_mat),
            suitabilities = list(
                ling_imm = g3_suitability_exponentiall50(alpha = 1, l50 = 80),
                ling_mat = g3_suitability_exponentiall50(alpha = 1, l50 = 80)),
            amount_f = 1000),
        list())

    likelihood_actions <- list(
        g3l_tagging_ckmr(
            'tagging_ckmr',
            list(),
            fleets = list(fleet_ckmr),
            parent_stocks = list(ling_mat),
            offspring_stocks = list(ling_imm)),
        list())

    time_actions <- list(
        g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
        list())

    actions <- c(
        ling_imm_actions,
        ling_mat_actions,
        fleet_actions,
        likelihood_actions,
        time_actions)

    params <- list(
        "lingimm.init" =     1,
        "lingimm.init.scalar" = 100,
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
        "tagging_ckmr_weight" = 1.0,
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
        model_cpp <- c()
    }

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template, ignore_dimname='year')
    }
})
