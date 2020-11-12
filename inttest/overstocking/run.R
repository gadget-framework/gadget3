# Run a single stock to extinction to check overconsumption behaviour
library(gadget2)
library(gadget3)
library(Rgadget)
library(magrittr)
library(unittest)

remove_logspace_add <- function (action) lapply(action, function (a) {
    # logspace_add is just used to avoid div/0, replace it with a pmax() call
    gadget3:::call_replace(a,
        logspace_add = function (x) call("pmax", x[[2]], x[[3]] + 1e-7),
        logspace_add_vec = function (x) call("pmax", x[[2]], x[[3]] + 1e-7))
})

year_range <- 1982:1990

ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(3, 5)

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c(1))

imm_report <- g3_stock('imm_report', seq(20, 156, 4)) %>%
  # NB: No area
  g3s_age(3, 5) %>%
  g3s_time(year = local(year_range), step = 1:4)

igfs_report <- igfs %>% g3s_clone('igfs_report') %>%
  g3s_time(year = local(year_range), step = 1:4)
nll_report <- rep(0, length(year_range) * 4)
prev_nll <- 0.0

ling_imm_actions <- list(
    g3a_initialconditions_normalparam(ling_imm,
        factor_f = ~g3_param("lingimm.init") * g3_param("lingimm.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_naturalmortality(ling_imm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_age(ling_imm),
    list())

fleet_actions <- list(
    remove_logspace_add(g3a_predate_totalfleet(igfs, list(ling_imm),
        suitabilities = list(
            ling_imm = g3_suitability_exponentiall50(~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50'))),
        amount_f = g3_timeareadata('igfs_landings', Rgadget::read.gadget.file('inttest/overstocking/', 'Data/fleet.igfs.data')[[1]], 'number'),
        overconsumption_f = quote(pmin(prey_stock__consratio, 0.95)))),
    list())

ling_likelihood_actions <- list(
    g3l_understocking(
        weight = 100,
        list(ling_imm)),
    g3l_catchdistribution(
        'ldist_igfs',
        weight = 1,
        obs_data = structure(
            Rgadget::read.gadget.file('inttest/overstocking/', 'Data/catchdistribution.ldist.igfs.sumofsquares')[[1]],
            age = list(all3 = 3:5),
            length = Rgadget::read.gadget.file('inttest/overstocking','Aggfiles/catchdistribution.ldist.igfs.len.agg')[[1]]),
        fleets = list(igfs),
        stocks = list(ling_imm),
        g3l_catchdistribution_sumofsquares()),
    list())

report_actions <- list(
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__num)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__wgt)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__consratio)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__totalpredate)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__igfs)),
       g3a_report_stock(igfs_report,igfs, ~stock_ss(igfs__catch)),
       list('999' = ~{
           nll_report[[cur_time + 1]] <- nll - prev_nll
           prev_nll <- nll
           g3_report(nll_report)
       }))

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
    list())

model_fn <- g3_to_r(c(
  ling_imm_actions,
  fleet_actions,
  ling_likelihood_actions,
  report_actions,
  time_actions), strict = TRUE, trace = FALSE)

param_table <- read.table('inttest/overstocking/params.in', header = TRUE)
param <- as.list(param_table$value)
names(param) <- param_table$switch

# Run gadget3 model
# model_fn <- edit(model_fn) ; model_fn(param)
g3_nll <- model_fn(param)
g3_r <- lapply(environment(model_fn)$model_report, function (x) round(x, 6))

# Run gadget2 model
oldwd <- getwd()
setwd('inttest/overstocking')
unlink('*.out')
system2(gadget2::gadget_binary(), " -i params.in -log debug.log")
setwd(oldwd)

g2_lingimm <- Rgadget::read.gadget.file('inttest/overstocking', 'lingimm.out')[[1]]
names(g2_lingimm) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingimm <- gadget3:::g3l_likelihood_data('x', g2_lingimm)

g2_igfs <- Rgadget::read.gadget.file('inttest/overstocking', 'igfs.lingimm.predprey.out')[[1]][,1:7]
names(g2_igfs) <- c("year", "step", "area", "age", "length", "number", "weight")
attr(g2_igfs, 'age') <- list(all3 = 3:5)
attr(g2_igfs, 'length') <- Rgadget::read.gadget.file('inttest/overstocking','Aggfiles/catchdistribution.ldist.igfs.len.agg')[[1]]
g2_igfs <- gadget3:::g3l_likelihood_data('x', g2_igfs)

for (t in seq_len(dim(g3_r$imm_report__num)['time'])) {
    ok(all.equal(
        unname(g2_lingimm$number[,,1,t]),
        unname(g3_r$imm_report__num[,,t]),
        tolerance = 1e-3), paste0("g3_r$imm_report__num: ", t, " - ", dimnames(g3_r$imm_report__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,1,t] * g2_lingimm$weight[,,1,t]),
        unname(g3_r$imm_report__num[,,t] * g3_r$imm_report__wgt[,,t]),
        tolerance = 1e-4), paste0("g3_r$imm_report__wgt: ", t, " - ", dimnames(g3_r$imm_report__wgt)$time[[t]]))

    ok(all.equal(
        g2_igfs$weight[,,1,1],
        rowSums(g3_r$imm_report__totalpredate[,,1]),
        tolerance = 1e-4), paste0("g3_r$imm_report__totalpredate: ", t, " - ", dimnames(g3_r$imm_report__wgt)$time[[t]]))
}

g3_biomass <- colSums(g3_r$imm_report__num[,1,] * g3_r$imm_report__wgt[,1,])
ok(all.equal(
    unname(colSums(g3_r$imm_report__totalpredate[,1,])),
    unname(c(colSums(g3_r$imm_report__totalpredate[,1,])[[1]], head(g3_biomass, -1) - tail(g3_biomass, -1))),
    tolerance = 1e-2), "g3_r$imm_report__totalpredate: Consistent with fall in stock biomass")
ok(all.equal(
    colSums(g3_r$igfs_report__catch[]),
    colSums(colSums(g3_r$imm_report__igfs[,,])),
    tolerance = 1e-7), "igfs_report__catch: Consistent with imm_report__igfs")

ok(all.equal(
    g3_r$imm_report__igfs,
    g3_r$imm_report__totalpredate,
    tolerance = 1e-0), "imm_report__totalpredate: Vaguely matches")  # TODO: ?

ok(all.equal(
    colSums(g2_igfs$weight[,,1,]),
    g3_r$igfs_report__catch[1,],
    tolerance = 1e-1), "g3_r$igfs_report__catch: Approximately matches igfs.lingimm.predprey.out")

g2_nll <- Rgadget::read.gadget.file('inttest/overstocking', 'likelihood.out')[[1]]
# NB: add two zeros for non-understocked initial numbers. Should really be parsing year/step and filling gaps instead.
g2_nll <- c(0, 0, g2_nll[g2_nll$component == 'understocking','likelihood_value'] * g2_nll[g2_nll$component == 'understocking','weight'])
# NB: A fully depleted stock in gadget2 isn't reported as understocked, but gadget3 does.
#     So ignore the rest of the nll entries that g3 produces.
ok(all.equal(
    g2_nll,
    g3_r$nll_report[seq_along(g2_nll)],
    tolerance = 1e-6), "g3_r$nll_report: Initial part of report matches")
