library(gadget2)
library(gadget3)
library(Rgadget)
library(magrittr)
library(unittest)

year_range <- 1982:1990

ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(3, 5)

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c(1))

imm_report <- g3_stock('imm_report', seq(20, 156, 4)) %>%
  # NB: No area
  g3s_age(3, 5) %>%
  g3s_time(year = local(year_range), step = 1:4)

ling_imm_actions <- list(
    g3a_initialconditions_normalparam(ling_imm,
        factor_f = ~g3_param("lingimm.init") * g3_param("lingimm.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_renewal_normalparam(ling_imm,
        factor_f = ~g3_param("lingimm.rec.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta"),
        run_f = ~cur_step == 1 && age == 3),
    g3a_naturalmortality(ling_imm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_growmature(ling_imm,
        impl_f = g3a_grow_impl_bbinom(
            g3a_grow_lengthvbsimple(~g3_param("ling.Linf"), ~g3_param("ling.k") * 0.001),
            g3a_grow_weightsimple(~g3_param("lingimm.walpha"), ~g3_param("lingimm.wbeta")),
            beta_f = ~g3_param("ling.bbin") * 10,
            maxlengthgroupgrowth = 15)),
    g3a_age(ling_imm),
    list())

fleet_actions <- list(
    g3a_predate_totalfleet(igfs, list(ling_imm),
        suitabilities = list(
            ling_imm = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50'))),
        amount_f = g3_timeareadata('igfs_landings', Rgadget::read.gadget.file('inttest/renewal/', 'Data/fleet.igfs.data')[[1]], 'number')),
    list())

ling_likelihood_actions <- list(
    g3l_understocking(
        weight = 100,
        list(ling_imm)),
    g3l_catchdistribution(
        'ldist_igfs',
        weight = 1,
        obs_data = structure(
            Rgadget::read.gadget.file('inttest/renewal/', 'Data/catchdistribution.ldist.igfs.sumofsquares')[[1]],
            age = list(all3 = 3:5),
            length = Rgadget::read.gadget.file('inttest/renewal','Aggfiles/catchdistribution.ldist.igfs.len.agg')[[1]]),
        fleets = list(igfs),
        stocks = list(ling_imm),
        g3l_distribution_sumofsquares()),
    list())

report_actions <- list(
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__num)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__wgt)),
#       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__igfs)),
       list())

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
    list())

actions <- c(
  ling_imm_actions,
#  fleet_actions,
#  ling_likelihood_actions,
  report_actions,
  time_actions)

model_fn <- g3_to_r(actions, strict = TRUE, trace = FALSE)

param_table <- read.table('inttest/renewal/params.in', header = TRUE)
param <- as.list(param_table$value)
names(param) <- param_table$switch

# Run gadget3 model
# model_fn <- edit(model_fn) ; model_fn(param)
r_result <- model_fn(param)
g3_r <- attributes(r_result)

# If enabled run a TMB version too
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, param, compile_flags = c("-O0", "-g"))

    model_tmb_report <- model_tmb$report()
    for (n in names(attributes(r_result))) {
        ok(all.equal(
            as.vector(model_tmb_report[[n]]),
            as.vector(attr(r_result, n)),
            tolerance = 1e-5), paste("TMB and R match", n))
    }
}

# Run gadget2 model
oldwd <- getwd()
setwd('inttest/renewal')
unlink('*.out')
system2(gadget2::gadget_binary(), " -i params.in -log debug.log")
setwd(oldwd)

g2_lingimm <- Rgadget::read.gadget.file('inttest/renewal', 'lingimm.out')[[1]]
names(g2_lingimm) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingimm <- gadget3:::g3l_likelihood_data('x', g2_lingimm)

for (t in seq_len(dim(g3_r$imm_report__num)['time'])) {
    ok(all.equal(
        unname(g2_lingimm$number[,,1,t]),
        unname(g3_r$imm_report__num[,,t]),
        tolerance = 1e-5), paste0("g3_r$imm_report__num: ", t, " - ", dimnames(g3_r$imm_report__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,1,t] * g2_lingimm$weight[,,1,t]),
        unname(g3_r$imm_report__num[,,t] * g3_r$imm_report__wgt[,,t]),
        tolerance = 1e-5), paste0("g3_r$imm_report__wgt: ", t, " - ", dimnames(g3_r$imm_report__wgt)$time[[t]]))
}
