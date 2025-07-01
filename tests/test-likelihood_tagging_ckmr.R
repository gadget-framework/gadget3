library(magrittr)
library(unittest)

library(gadget3)

table_string <- function (str) {
    read.table(
        textConnection(str),
        blank.lines.skip = TRUE,
        header = TRUE,
        stringsAsFactors = FALSE)
}

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
            table_string('
year parent_age offspring_age mo_pairs
1989         5              3        1
1990         10             4        2
            '),
            fleets = list(fleet_ckmr),
            parent_stocks = list(ling_mat),
            offspring_stocks = list(ling_imm)),
        # NB: Only required for testing
        gadget3:::g3l_test_dummy_likelihood(),
        list())

    time_actions <- list(
        g3a_time(min(year_range), max(year_range), c(3,3,3,3), project_years = 0),
        list())

    actions <- c(
        ling_imm_actions,
        ling_mat_actions,
        fleet_actions,
        likelihood_actions,
        time_actions)

    # Compile model
    actions <- c(actions, list(
        g3a_report_history(actions, var_re = "tagging_ckmrmodel_(spawning|spawned|total|catch)$|__cons$|__suit$", out_prefix = "hist") ))
    model_fn <- g3_to_r(actions, trace = FALSE)
    model_cpp <- g3_to_tmb(actions, trace = FALSE)

    params <- attr(model_fn, 'parameter_template')
    params$lingimm.init <- 1
    params$lingimm.init.scalar <- 100
    params$lingimm.M <- 0
    params$lingimm.walpha <- 1e-1
    params$lingimm.wbeta <- 2
    params$lingmat.init <- 1
    params$lingmat.init.scalar <- 100
    params$lingmat.M <- 0.95
    params$lingmat.walpha <- 1e-6
    params$lingmat.wbeta <- 2
    params$ling.Linf <- 160
    params$ricker.mu <- 1
    params$ricker.lambda <- 1e-6
    params$tagging_ckmr_weight <- 1.0

    # capture.output(print(attributes(model_fn(params))), file = 'gadget3/test-likelihood_tagging_ckmr.baseline')

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})
