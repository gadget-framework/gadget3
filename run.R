#!/usr/bin/Rscript --vanilla
library(magrittr)

library(gadget3)

end <- function (x) x

areas <- g3_areas('a', 'b', 'c')

ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) %>%
    g3s_livesonareas(areas[c('a')]) %>%
    g3s_age(3, 10) %>%
    g3s_prey(energycontent = 5) %>%
    end()

ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) %>%
    g3s_livesonareas(areas[c('a', 'b')]) %>%
    g3s_age(5, 15) %>%
    g3s_prey(energycontent = 5) %>%
    end()

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(areas[c('a')])
lln <- g3_fleet('lln') %>% g3s_livesonareas(areas[c('a')])
bmt <- g3_fleet('bmt') %>% g3s_livesonareas(areas[c('a')])
gil <- g3_fleet('gil') %>% g3s_livesonareas(areas[c('a')])
foreign <- g3_fleet('foreign') %>% g3s_livesonareas(areas[c('a')])

ling_imm_stddev <- c(
    8.25,
    10.5644599516659,
    12.4081745588022,
    11.5741565728647,
    11.0523508874244,
    11.3447991170274,
    11.7721342759715,
    13.6152275606449)
ling_imm_actions <- g3_collate(
    g3a_initialconditions_normalparam(ling_imm,
        # NB: area & age factor together (gadget2 just multiplied them)
        factor_f = ~g3_param("lingimm.init.scalar") * exp(-1 * (g3_param("lingimm.M") + g3_param("ling.init.F")) * age) * g3_param("lingimm.init.", age),
        mean_f = ~g3_param("ling.Linf") * (1 - exp(-1 * (0.001 * g3_param("ling.k")) * (age - (1 + log(1 - g3_param("ling.recl")/g3_param("ling.Linf"))/(0.001 * g3_param("ling.k")))))),
        stddev_f = ~ling_imm_stddev[[ling_imm__age_idx]],  # TODO: age_idx is already corrected, but expecting it to work for anything is a bit of a landmine
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_grow(ling_imm,
        growth_f = g3a_grow_lengthvbsimple(
            linf_f = ~g3_param("ling.Linf"),
            kappa_f = ~g3_param("ling.k") * 0.001,
            alpha_f = ~g3_param("lingimm.walpha"),
            beta_f = ~g3_param("lingimm.wbeta")),
        impl_f = g3a_grow_impl_bbinom(
            beta_f = ~g3_param("ling.bbin") * 10,
            maxlengthgroupgrowth = 15)),
    g3a_age(ling_imm),
    g3a_mature(ling_imm, ling_mat, ~1),
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
ling_mat_actions <- g3_collate(
    g3a_initialconditions_normalparam(ling_mat,
        # NB: area & age factor together (gadget2 just multiplied them)
        factor_f = ~g3_param("lingmat.init.scalar") * exp(-1 * (g3_param("lingmat.M") + g3_param("ling.init.F")) * age) * g3_param("lingmat.init.", age),
        mean_f = ~g3_param("ling.Linf") * (1 - exp(-1 * (0.001 * g3_param("ling.k")) * (age - (1 + log(1 - g3_param("ling.recl")/g3_param("ling.Linf"))/(0.001 * g3_param("ling.k")))))),
        stddev_f = ~ling_mat_stddev[[ling_mat__age_idx]],
        alpha_f = ~g3_param("lingmat.walpha"),
        beta_f = ~g3_param("lingmat.wbeta")),
    g3a_grow(ling_mat,
        growth_f = g3a_grow_lengthvbsimple(
            linf_f = ~g3_param("ling.Linf"),
            kappa_f = ~g3_param("ling.k") * 0.001,
            alpha_f = ~g3_param("lingmat.walpha"),
            beta_f = ~g3_param("lingmat.wbeta")),
        impl_f = g3a_grow_impl_bbinom(
            beta_f = ~g3_param("ling.bbin") * 10,
            maxlengthgroupgrowth = 15)),
    list())

igfs_totaldata <- data.frame(
    year = c(rep(1983, 4), rep(1984, 4), rep(1985, 4)),
    step = 1:4,
    area = areas['a'],
    value = 1:4)

consumption_actions <- g3_collate(
    g3a_predate_totalfleet(igfs, list(ling_imm, ling_mat),
        suitabilities = list(
            ling_imm = g3_suitability_exponentiall50(g3_param('ling.igfs.alpha'), g3_param('ling.igfs.l50')),
            ling_mat = g3_suitability_exponentiall50(g3_param('ling.igfs.alpha'), g3_param('ling.igfs.l50'))),
        amount_f = g3_timeareadata('igfs_totaldata', igfs_totaldata)),
    list())

time <- g3a_time(start_year = 1983, end_year = 1985, c(3, 3, 3, 3))
ling_model <- g3_compile_r(g3_collate(
    ling_mat_actions,
    ling_imm_actions,
    consumption_actions,
    time))
writeLines(deparse(ling_model, width.cutoff = 500L), con = 'baseline/ling.R')

writeLines("***** Running Model *****")
ling_param <- list(  # ./06-ling/12-new_ass/params.in
    "ling.Linf" = 160,
    "ling.k" = 90,
    "lingimm.walpha" = 2.27567436711055e-06,
    "lingimm.wbeta" = 3.20200445996187,
    "ling.bbin" = 6,
    "lingimm.M" = 0.15,
    "lingimm.init.scalar" = 200,
    "ling.init.F" = 0.4,
    "lingimm.init.3" = 1,
    "lingimm.init.4" = 1,
    "lingimm.init.5" = 1,
    "lingimm.init.6" = 1,
    "lingimm.init.7" = 1,
    "lingimm.init.8" = 1,
    "lingimm.init.9" = 1,
    "lingimm.init.10" = 1,
    "ling.recl" = 12,
    "ling.mat1" = 70,
    "ling.mat2" = 75,
    "ling.rec.scalar" = 400,
    "ling.rec.1982" = 1,
    "ling.rec.sd" = 5,
    "ling.rec.1983" = 1,
    "ling.rec.1984" = 1,
    "ling.rec.1985" = 1,
    "ling.rec.1986" = 1,
    "ling.rec.1987" = 1,
    "ling.rec.1988" = 1,
    "ling.rec.1989" = 1,
    "ling.rec.1990" = 1,
    "ling.rec.1991" = 1,
    "ling.rec.1992" = 1,
    "ling.rec.1993" = 1,
    "ling.rec.1994" = 1,
    "ling.rec.1995" = 1,
    "ling.rec.1996" = 1,
    "ling.rec.1997" = 1,
    "ling.rec.1998" = 1,
    "ling.rec.1999" = 1,
    "ling.rec.2000" = 1,
    "ling.rec.2001" = 1,
    "ling.rec.2002" = 1,
    "ling.rec.2003" = 1,
    "ling.rec.2004" = 1,
    "ling.rec.2005" = 1,
    "ling.rec.2006" = 1,
    "ling.rec.2007" = 1,
    "ling.rec.2008" = 1,
    "ling.rec.2009" = 1,
    "ling.rec.2010" = 1,
    "ling.rec.2011" = 1,
    "ling.rec.2012" = 1,
    "ling.rec.2013" = 1,
    "ling.rec.2014" = 1,
    "ling.rec.2015" = 1,
    "ling.rec.2016" = 1,
    "ling.rec.2017" = 1,
    "ling.rec.2018" = 1,
    "lingmat.M" = 0.15,
    "lingmat.init.scalar" = 200,
    "lingmat.init.5" = 1,
    "lingmat.init.6" = 1,
    "lingmat.init.7" = 1,
    "lingmat.init.8" = 1,
    "lingmat.init.9" = 1,
    "lingmat.init.10" = 1,
    "lingmat.init.11" = 1,
    "lingmat.init.12" = 1,
    "lingmat.init.13" = 1,
    "lingmat.init.14" = 1,
    "lingmat.init.15" = 1,
    "lingmat.walpha" = 2.27567436711055e-06,
    "lingmat.wbeta" = 3.20200445996187,
    "ling.igfs.alpha" = 0.5,
    "ling.igfs.l50" = 50,
    "ling.lln.alpha" = 0.5,
    "ling.lln.l50" = 50,
    "ling.bmt.alpha" = 0.5,
    "ling.bmt.l50" = 50,
    "ling.gil.alpha" = 0.5,
    "ling.gil.l50" = 50,
    end = NULL)
result <- ling_model(ling_param)
str(result)
# NB: You can do: ling_model <- edit(ling_model) ; result <- ling_model(ling_param)

tmb_ling <- g3_precompile_tmb(g3_collate(
    ling_mat_actions,
    ling_imm_actions,
    consumption_actions,
    time))
writeLines(tmb_ling, con = 'baseline/ling.cpp')
ling_model_tmb <- g3_tmb_adfun(tmb_ling, ling_param)
# NB: You can do: tmb_ling <- edit(tmb_ling) ; g3_tmb_adfun(tmb_ling)
