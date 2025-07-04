#!/usr/bin/Rscript --vanilla
library(magrittr)
library(unittest)

library(gadget3)
options(gadget3.tmb.work_dir = 'work_dir')

unattr <- function (x) {
    attributes(x) <- NULL
    return(x)
}

end <- function (x) x

areas <- list(area1=1)

ling_imm <- g3_stock(c(species = 'ling', 'imm'), seq(20, 156, 4)) %>%
    g3s_age(3, 10) %>%
    g3s_livesonareas(areas[c('area1')]) %>%
    end()

ling_mat <- g3_stock(c(species = 'ling', 'mat'), seq(20, 156, 4)) %>%
    g3s_age(5, 15) %>%
    g3s_livesonareas(areas[c('area1')]) %>%
    end()

lln <- g3_fleet('lln') %>% g3s_livesonareas(areas[c('area1')])
bmt <- g3_fleet('bmt') %>% g3s_livesonareas(areas[c('area1')])
gil <- g3_fleet('gil') %>% g3s_livesonareas(areas[c('area1')])
foreign <- g3_fleet('foreign') %>% g3s_livesonareas(areas[c('area1')])

ling_imm_stddev <- c(
    8.25,
    10.5644599516659,
    12.4081745588022,
    11.5741565728647,
    11.0523508874244,
    11.3447991170274,
    11.7721342759715,
    13.6152275606449)
ling_imm_actions <- list(
    g3a_initialconditions_normalparam(ling_imm,
        # NB: area & age factor together (gadget2 just multiplied them)
        factor_f = ~g3_param("lingimm.init.scalar") * exp(-1 * (g3_param("lingimm.M") + g3_param("ling.init.F")) * age) * g3_param_vector("lingimm.init", value = rep(0, 10 - 3 + 1))[[as_integer(age) - 3 + 1]],
        mean_f = g3a_renewal_vonb(by_stock = 'species'),
        stddev_f = ~ling_imm_stddev[[as_integer(age) - 3 + 2]],
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_renewal_normalparam(ling_imm,
        factor_f = ~g3_param("ling.rec.scalar") * g3_param_table("ling.rec", data.frame(cur_year = seq(start_year, end_year))),
        mean_f = g3a_renewal_vonb(by_stock = 'species'),
        stddev_f = ~ling_imm_stddev[[as_integer(age) - 3 + 1]],
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta"),
        run_f = ~cur_step == 1 && age == 3),
    # Additional renewal for age 5
    g3a_renewal_normalparam(ling_imm,
        factor_f = ~g3_param("ling.rec.scalar") * g3_param_table("ling.rec", data.frame(cur_year = seq(start_year, end_year))),
        mean_f = g3a_renewal_vonb(by_stock = 'species'),
        stddev_f = ~ling_imm_stddev[[as_integer(age) - 3L + 1L]],
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta"),
        run_f = ~cur_step == 1 && age == 5),
    g3a_growmature(ling_imm,
        impl_f = g3a_grow_impl_bbinom(
            g3a_grow_lengthvbsimple(
                linf_f = ~g3_param("ling.Linf"),
                kappa_f = ~g3_param("ling.K") * 0.001),
            g3a_grow_weightsimple(
                alpha_f = ~g3_param("lingimm.walpha"),
                beta_f = ~g3_param("lingimm.wbeta")),
            beta_f = ~g3_param("ling.bbin") * 10,
            maxlengthgroupgrowth = 15),
        maturity_f = g3a_mature_constant(
            alpha = ~0.001 * g3_param("ling.mat1"),
            l50 = ~g3_param("ling.mat2")),
        output_stocks = list(ling_mat)),
    g3a_naturalmortality(ling_imm,
        g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_age(ling_imm,
        output_stocks = list(ling_mat)),
    list())

ling_mat_stddev <- c(
    12.4081745588022,
    11.5741565728647,
    11.0523508874244,
    11.3447991170274,
    11.7721342759715,
     13.6152275606449,
     14.8004893270652,
     16.2753802766344,
     17.9426701121357,
     19.1787817582897,
     15.9776436358384)
ling_mat_actions <- list(
    g3a_initialconditions_normalparam(ling_mat,
        # NB: area & age factor together (gadget2 just multiplied them)
        factor_f = ~g3_param("lingmat.init.scalar") * exp(-1 * (g3_param("lingmat.M") + g3_param("ling.init.F")) * age) * g3_param_vector("lingmat.init", value = rep(0, 15 - 5 + 1))[[as_integer(age) - 5 + 1]],
        mean_f = g3a_renewal_vonb(by_stock = 'species'),
        stddev_f = ~ling_mat_stddev[[as_integer(age) - 5 + 2]],
        alpha_f = ~g3_param("lingmat.walpha"),
        beta_f = ~g3_param("lingmat.wbeta")),
    g3a_growmature(ling_mat,
        impl_f = g3a_grow_impl_bbinom(
            g3a_grow_lengthvbsimple(
                linf_f = ~g3_param("ling.Linf"),
                kappa_f = ~g3_param("ling.K") * 0.001),
            g3a_grow_weightsimple(
                alpha_f = ~g3_param("lingmat.walpha"),
                beta_f = ~g3_param("lingmat.wbeta")),
            beta_f = ~g3_param("ling.bbin") * 10,
            maxlengthgroupgrowth = 15)),
    g3a_naturalmortality(ling_mat,
        g3a_naturalmortality_exp(~g3_param("lingmat.M"))),
    g3a_age(ling_mat),
    list())

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(areas[c('area1')])
igfs_totaldata <- data.frame(
    year = rep(1994:2018, each = 4),
    step = 1:4,
    area = areas[['area1']],
    total_weight = 1:4)
igfs_obs_data <- read.table('inttest/codegeneration/catchdistribution_ldist_lln.txt', header = TRUE)
igfs_obs_data$area <- 1L
igfs_actions <- list(
    g3a_predate_fleet(igfs, list(ling_imm, ling_mat),
        suitabilities = list(
            ling_imm = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50')),
            ling_mat = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50'))),
        catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata('igfs_totaldata', igfs_totaldata))),
    list())

likelihood_actions <- list(
    g3l_understocking(list(ling_imm, ling_mat), nll_breakdown = TRUE),
    g3l_catchdistribution(
        'ldist_lln',
        igfs_obs_data,
        fleets = list(igfs),
        stocks = list(ling_imm, ling_mat),
        g3l_distribution_sumofsquares(),
        nll_breakdown = TRUE),
    list())

time <- list(
    g3a_time(start_year = 1994, end_year = 2018, c(3, 3, 3, 3), project_years = 0),
    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood(),
    list())
model_fn <- g3_to_r(c(
    ling_mat_actions,
    ling_imm_actions,
    igfs_actions,
    likelihood_actions,
    time), strict = TRUE, trace = FALSE)  # NB: "trace" turns comments into debug statements
attr(model_fn, 'actions') <- NULL  # Actions output is a bit too verbose for us
writeLines(deparse(model_fn, width.cutoff = 500L), con = 'inttest/codegeneration/ling.R')


params <- attr(model_fn, 'parameter_template')
params[["ling.Linf"]] <- 160
params[["ling.K"]] <- 90
params[["lingimm.walpha"]] <- 2.27567436711055e-06
params[["lingimm.wbeta"]] <- 3.20200445996187
params[["ling.bbin"]] <- 6
params[["lingimm.M"]] <- 0.15
params[["lingimm.init.scalar"]] <- 200
params[["ling.init.F"]] <- 0.4
params[["lingimm.init"]] <- rep(1, 10 - 3 + 1)
params[["ling.recl"]] <- 12
params[["ling.mat1"]] <- 70
params[["ling.mat2"]] <- 75
params[["ling.rec.scalar"]] <- 400
params[["lingmat.M"]] <- 0.15
params[["lingmat.init.scalar"]] <- 200
params[["lingmat.init"]] <- rep(1, 15 - 5 + 1)
params[["lingmat.walpha"]] <- 2.27567436711055e-06
params[["lingmat.wbeta"]] <- 3.20200445996187
params[["ling.igfs.alpha"]] <- 0.5
params[["ling.igfs.l50"]] <- 50
# Set recrutiment for every year
params[paste('ling.rec', 1994:2018, sep = ".")] <- 1

writeLines("***** Running Model *****")
system.time(r_result <- model_fn(params))
str(r_result)
# NB: You can do: model_fn <- edit(model_fn) ; result <- model_fn(params)

model_cpp <- g3_to_tmb(c(
    ling_mat_actions,
    ling_imm_actions,
    igfs_actions,
    likelihood_actions,
    time), strict = TRUE)
writeLines(
    grep("// Model generated with", model_cpp, invert = TRUE, value = TRUE),
    con = 'inttest/codegeneration/ling.cpp')

# model_cpp <- edit(model_cpp)
tmb_param <- attr(model_cpp, 'parameter_template')
capture.output(print(tmb_param, width = 500), file = 'inttest/codegeneration/ling.tmbparam')
# Fill parameters - Map original list into data.frame format
tmb_param$value <- I(params[tmb_param$switch])
# Random parameters with: tmb_param["ling.Linf", "random"] <- TRUE
# Fixed parameters with: tmb_param["ling.Linf", "optimise"] <- FALSE

# Run the model with debugging:
# model_cpp <- edit(model_cpp) ; writeLines(TMB::gdbsource(g3_tmb_adfun(model_cpp, tmb_param, compile_flags = c("-O0", "-g"), output_script = TRUE)))

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
