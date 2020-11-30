# Run a single stock to extinction to check overconsumption behaviour
library(gadget2)
library(gadget3)
library(Rgadget)
library(magrittr)
library(unittest)

remove_avoid_zero <- function (action) lapply(action, function (a) {
    # replace with a pmax() call
    gadget3:::call_replace(a,
        avoid_zero = function (x) call("max", x[[2]], 1e-7),
        avoid_zero_vec = function (x) call("pmax", x[[2]], 1e-7))
})

year_range <- 1982:1990

ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(3, 5)

ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) %>%
    g3s_livesonareas(c(1)) %>%
    g3s_age(5, 15)

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c(1))

imm_report <- g3s_clone(ling_imm, 'imm_report') %>% g3s_time(year = local(year_range), step = 1:4)
mat_report <- g3s_clone(ling_mat, 'mat_report') %>% g3s_time(year = local(year_range), step = 1:4)

igfs_report <- igfs %>% g3s_clone('igfs_report') %>%
  g3s_time(year = local(year_range), step = 1:4)
nll_report <- rep(0, length(year_range) * 4)
prev_nll <- 0.0

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

ling_mat_actions <- lapply(list(
    g3a_initialconditions_normalparam(ling_mat,
        factor_f = ~age * 0.2 * g3_param("lingmat.init") * g3_param("lingmat.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10,
        alpha_f = ~g3_param("lingmat.walpha"),
        beta_f = ~g3_param("lingmat.wbeta")),
    g3a_naturalmortality(ling_mat, g3a_naturalmortality_exp(~g3_param("lingmat.M"))),
    g3a_age(ling_mat),
    list()), remove_avoid_zero)

fleet_actions <- list(
    remove_avoid_zero(g3a_predate_totalfleet(igfs, list(ling_imm, ling_mat),
        suitabilities = list(
            ling_imm = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50')),
            ling_mat = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50'))),
        amount_f = g3_timeareadata('igfs_landings', Rgadget::read.gadget.file('inttest/understocking/', 'Data/fleet.igfs.data')[[1]], 'number'),
        overconsumption_f = quote(pmin(prey_stock__consratio, 0.95)))),
    list())

ling_likelihood_actions <- list(
    g3l_understocking(
        weight = 1000,
        nll_breakdown = TRUE,
        list(ling_imm)),
    remove_avoid_zero(g3l_catchdistribution(
        'ldist_igfs',
        weight = 10,
        obs_data = structure(
            Rgadget::read.gadget.file('inttest/understocking/', 'Data/catchdistribution.ldist.igfs.sumofsquares')[[1]],
            age = list(all3 = 3:5),
            length = Rgadget::read.gadget.file('inttest/understocking','Aggfiles/catchdistribution.ldist.igfs.len.agg')[[1]]),
        fleets = list(igfs),
        stocks = list(ling_imm),
        g3l_catchdistribution_sumofsquares(),
        nll_breakdown = TRUE)),
    list())

report_actions <- list(
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__num)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__wgt)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__consratio)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__totalpredate)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__igfs)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__num)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__wgt)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__consratio)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__totalpredate)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__igfs)),
       g3a_report_stock(igfs_report,igfs, ~stock_ss(igfs__catch)),
       list('999' = ~{
           nll_report[[cur_time + 1]] <- nll - prev_nll
           prev_nll <- nll
           g3_report(nll_report)
       }))

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
    list())

actions <- c(
  ling_imm_actions,
  ling_mat_actions,
  fleet_actions,
  ling_likelihood_actions,
  report_actions,
  time_actions)

model_fn <- g3_to_r(actions, strict = TRUE, trace = FALSE)

param_table <- read.table('inttest/understocking/params.in', header = TRUE)
param <- as.list(param_table$value)
names(param) <- param_table$switch

# Run gadget3 model
# model_fn <- edit(model_fn) ; model_fn(param)
g3_nll <- model_fn(param)
g3_r <- lapply(environment(model_fn)$model_report, function (x) round(x, 6))

# If enabled run a TMB version too
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, param, compile_flags = c("-O0", "-g"))

    model_tmb_report <- model_tmb$report()
    for (n in ls(environment(model_fn)$model_report)) {
        ok(all.equal(
            as.vector(model_tmb_report[[n]]),
            as.vector(environment(model_fn)$model_report[[n]]),
            tolerance = 1e-5), paste("TMB and R match", n))
    }
}

# Run gadget2 model
oldwd <- getwd()
setwd('inttest/understocking')
unlink('*.out')
system2(gadget2::gadget_binary(), " -i params.in -log debug.log")
setwd(oldwd)

g2_lingimm <- Rgadget::read.gadget.file('inttest/understocking', 'lingimm.out')[[1]]
names(g2_lingimm) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingimm <- gadget3:::g3l_likelihood_data('x', g2_lingimm)
g2_lingmat <- Rgadget::read.gadget.file('inttest/understocking', 'lingmat.out')[[1]]
names(g2_lingmat) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingmat <- gadget3:::g3l_likelihood_data('x', g2_lingmat)

g2_igfs_imm <- Rgadget::read.gadget.file('inttest/understocking', 'igfs.lingimm.predprey.out')[[1]][,1:7]
names(g2_igfs_imm) <- c("year", "step", "area", "age", "length", "number", "weight")
attr(g2_igfs_imm, 'age') <- list(all3 = 3:5)
attr(g2_igfs_imm, 'length') <- Rgadget::read.gadget.file('inttest/understocking','Aggfiles/catchdistribution.ldist.igfs.len.agg')[[1]]
g2_igfs_imm <- gadget3:::g3l_likelihood_data('x', g2_igfs_imm)

g2_igfs_mat <- Rgadget::read.gadget.file('inttest/understocking', 'igfs.lingmat.predprey.out')[[1]][,1:7]
names(g2_igfs_mat) <- c("year", "step", "area", "age", "length", "number", "weight")
attr(g2_igfs_mat, 'age') <- list(all3 = 3:5)
attr(g2_igfs_mat, 'length') <- Rgadget::read.gadget.file('inttest/understocking','Aggfiles/catchdistribution.ldist.igfs.len.agg')[[1]]
g2_igfs_mat <- gadget3:::g3l_likelihood_data('x', g2_igfs_mat)

g3_imm_biomass <- g3_r$imm_report__num[,,,] * g3_r$imm_report__wgt[,,,]
g3_mat_biomass <- g3_r$mat_report__num[,,,] * g3_r$mat_report__wgt[,,,]

for (t in seq_len(dim(g3_r$imm_report__num)['time'])) {
    # NB: Losing accuracy at timesteps 17:19 (i.e 1984, which only has age5 left)
    #     Think we're noticing gadget2's "if (< verysmall) 0"
    ok(all.equal(
        unname(g2_lingimm$number[,,1,t]),
        unname(g3_r$imm_report__num[,1,,t]),
        tolerance = if (t %in% 17:19) 1e-3 else 1e-5), paste0("g3_r$imm_report__num: ", t, " - ", dimnames(g3_r$imm_report__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,1,t] * g2_lingimm$weight[,,1,t]),
        unname(g3_r$imm_report__num[,1,,t] * g3_r$imm_report__wgt[,1,,t]),
        tolerance = if (t %in% 17:18) 1e-3 else 1e-5), paste0("g3_r$imm_report__wgt: ", t, " - ", dimnames(g3_r$imm_report__wgt)$time[[t]]))

    if (t %in% 27:31) {
        # Gadget2 catches nonexistant fish in 1988. Check g3 is consistent at least
        ok(sum(g3_r$imm_report__num[,1,,t]) < 0.0001, paste0("g3_r$imm_report__num: ", t, " - "," No fish to be caught"))
        ok(sum(g3_r$imm_report__totalpredate[,1,,t]) < 0.0001, paste0("g3_r$imm_report__totalpredate: ", t, " - "," No fish caught"))
    } else {
        ok(all.equal(
            g2_igfs_imm$weight[,,1,t],
            rowSums(g3_r$imm_report__totalpredate[,1,,t]),
            tolerance = if (t %in% 18) 1e-3 else 1e-4), paste0("g3_r$imm_report__totalpredate: ", t, " - ", dimnames(g3_r$imm_report__wgt)$time[[t]]))
    }

    if (t == 1) {
        # Initial step, no comparisons to make
    } else if ((t - 1) %% 4 == 0) {
        # Beginning of year, ages will have jumped between timesteps
        ok(all.equal(
            rowSums(g3_imm_biomass[,,t - 1]) - rowSums(g3_r$imm_report__totalpredate[,,,t]),
            rowSums(g3_imm_biomass[,,t]),
            tolerance = if (t %in% 17) 1e-3 else 1e-6), paste0("g3_r$imm_report__totalpredate: ", t, " - Consistent with fall in stock biomass, with age jump"))
    } else {
        # In-year timestep, so can compare age breakdown
        ok(all.equal(
            g3_imm_biomass[,,t - 1] - g3_r$imm_report__totalpredate[,,,t],
            g3_imm_biomass[,,t],
            tolerance = if (t %in% 16:18) 1e-3 else 1e-5), paste0("g3_r$imm_report__totalpredate: ", t, " - Consistent with fall in stock biomass"))
    }

    ####

    ok(all.equal(
        unname(g2_lingmat$number[,,1,t]),
        unname(g3_r$mat_report__num[,1,,t]),
        tolerance = if (t %in% 17:19) 1e-3 else 1e-5), paste0("g3_r$mat_report__num: ", t, " - ", dimnames(g3_r$mat_report__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingmat$number[,,1,t] * g2_lingmat$weight[,,1,t]),
        unname(g3_r$mat_report__num[,1,,t] * g3_r$mat_report__wgt[,1,,t]),
        tolerance = if (t %in% 17) 1e-3 else 1e-5), paste0("g3_r$mat_report__wgt: ", t, " - ", dimnames(g3_r$mat_report__wgt)$time[[t]]))

    if (t %in% 27:31) {
        # Gadget2 catches nonexistant fish in 1987. Check g3 is consistent at least
        ok(sum(g3_r$mat_report__num[,1,,t]) < 0.0001, paste0("g3_r$mat_report__num: ", t, " - "," No fish to be caught"))
        ok(sum(g3_r$mat_report__totalpredate[,1,,t]) < 0.0001, paste0("g3_r$mat_report__totalpredate: ", t, " - "," No fish caught"))
    } else {
        ok(all.equal(
            g2_igfs_mat$weight[,,1,t],
            rowSums(g3_r$mat_report__totalpredate[,1,,t]),
            tolerance = if (t %in% 18) 1e-3 else 1e-4), paste0("g3_r$mat_report__totalpredate: ", t, " - ", dimnames(g3_r$mat_report__wgt)$time[[t]]))
    }

    if (t == 1) {
        # Initial step, no comparisons to make
    } else if ((t - 1) %% 4 == 0) {
        # Beginning of year, ages will have jumped between timesteps
        ok(all.equal(
            rowSums(g3_mat_biomass[,,t - 1]) - rowSums(g3_r$mat_report__totalpredate[,,,t]),
            rowSums(g3_mat_biomass[,,t]),
            tolerance = if (t %in% 17) 1e-2 else 1e-6), paste0("g3_r$mat_report__totalpredate: ", t, " - Consistent with fall in stock biomass, with age jump"))
    } else {
        # In-year timestep, so can compare age breakdown
        ok(all.equal(
            g3_mat_biomass[,,t - 1] - g3_r$mat_report__totalpredate[,,,t],
            g3_mat_biomass[,,t],
            tolerance = if (t %in% 16:18) 1e-3 else 1e-5), paste0("g3_r$mat_report__totalpredate: ", t, " - Consistent with fall in stock biomass"))
    }

    ####

    if (t %in% 25:31) {
        ok(all.equal(
            sum(g3_r$imm_report__totalpredate[,1,,t]) + sum(g3_r$mat_report__totalpredate[,1,,t]),
            g3_r$igfs_report__catch[1,t],
            tolerance = 1e-7), paste0("g3_r$igfs_report__catch: ", t, " - Total catch internally consistent"))
    } else {
        ok(all.equal(
            sum(g2_igfs_imm$weight[,,1,t] + g2_igfs_mat$weight[,,1,t]),
            g3_r$igfs_report__catch[1,t],
            tolerance = if (t %in% c(17,19,20)) 1e-3 else 1e-5), paste0("g3_r$igfs_report__catch: ", t, " - Total catch matches g2"))
    }
}
ok(all.equal(
    colSums(g3_r$igfs_report__catch[]),
    colSums(colSums(g3_r$imm_report__igfs[,,,])) + colSums(colSums(g3_r$mat_report__igfs[,,,])),
    tolerance = 1e-7), "igfs_report__catch: Consistent with imm_report__igfs")

ok(all.equal(
    g3_r$imm_report__igfs,
    g3_r$imm_report__totalpredate,
    tolerance = 1e-7), "imm_report__totalpredate: Matches imm_report__igfs")

ok(all.equal(
    sum(g3_r$nll_report),
    10 * sum(g3_r$nll_cdist_ldist_igfs__num) + 1000 * sum(g3_r$nll_understocking__wgt),
    tolerance = 1e-7), "g3_r$nll_report/g3_r$nll_cdist_ldist_igfs__num/g3_r$nll_understocking__wgt consistent with each other")
ok(ut_cmp_identical(
    dim(g3_r$nll_cdist_ldist_igfs__num),
    c(time = as.integer(length(year_range) * 4))), "g3_r$nll_cdist_ldist_igfs__num: Broken up into individual timesteps")
ok(ut_cmp_identical(
    dim(g3_r$nll_understocking__wgt),
    c(time = as.integer(length(year_range) * 4))), "g3_r$nll_understocking__wgt: Broken up into individual timesteps")

# Fill in zeros in nll report
g2_nll <- Rgadget::read.gadget.file('inttest/understocking', 'likelihood.out')[[1]]
g2_nll_understocking <- merge(g2_nll[g2_nll$component == 'understocking',], expand.grid(step=1:4, year=year_range), all.y = T)
g2_nll_understocking$likelihood_value[is.na(g2_nll_understocking$likelihood_value)] <- 0
g2_nll_understocking$weight[is.na(g2_nll_understocking$weight)] <- 0
g2_nll_ldist.igfs <- merge(g2_nll[g2_nll$component == 'ldist.igfs',], expand.grid(step=1:4, year=year_range), all.y = T)
g2_nll_ldist.igfs$likelihood_value[is.na(g2_nll_ldist.igfs$likelihood_value)] <- 0
g2_nll_ldist.igfs$weight[is.na(g2_nll_ldist.igfs$weight)] <- 0

ok(all.equal(
    g2_nll_ldist.igfs$likelihood_value,
    as.vector(g3_r$nll_cdist_ldist_igfs__num),
    tolerance = 1e-4), "g3_r$nll_cdist_ldist_igfs__num - ldist.igfs matches")
ok(all.equal(
    g2_nll_ldist.igfs$likelihood_value * g2_nll_ldist.igfs$weight,
    as.vector(g3_r$nll_report - g3_r$nll_understocking__wgt * 1000),
    tolerance = 1e-3), "g3_r$nll_report - ldist.igfs in total sum")

# NB: A fully depleted stock in gadget2 isn't reported as understocked, but gadget3 does.
#     So ignore the rest of the nll entries that g3 produces.
ok(ut_cmp_identical(
    g2_nll_understocking$likelihood_value[1:23] > 0,
    as.vector(g3_r$nll_understocking__wgt[1:23]) > 0), "g3_r$nll_understocking__wgt: Understocking triggered in same places")
