# Run a single stock to extinction to check overconsumption behaviour
library(gadget2)
library(gadget3)
library(gadget2to3)
library(Rgadget)
library(unittest)

year_range <- 1982:1990

nll_report <- rep(0, length(year_range) * 4)
prev_nll <- 0.0
remove_nll_attributes <- g3_native(r = function (x) x[[1]], cpp = "[](Type x) -> Type { return x; }")

report_actions <- list(
       list('999' = ~{
           nll_report[[cur_time + 1]] <- nll - prev_nll
           prev_nll <- remove_nll_attributes(nll)
       }))

actions <- local({
    eval(g2to3_mainfile('inttest/understocking'))
    c(actions, report_actions, list(g3a_report_history(actions, var_re = "__num$|__wgt$|__consratio$|__totalpredate$|__predby_igfs$|__catch$")))
})
environment(actions[[1]][[1]])$avoid_zero <- g3_native(function (a) max(a, 1e-7), cpp = "[](Type a) -> Type { return std::max(a, (Type)1e-7); }")
environment(actions[[1]][[1]])$avoid_zero_vec <- g3_native(function (a) pmax(a, 1e-7), cpp = "[](vector<Type> a) -> vector<Type> { return a.cwiseMax(1e-7); }")
model_fn <- g3_to_r(actions, strict = TRUE, trace = FALSE)
params <- local({eval(g2to3_params_r('inttest/understocking', 'params.in')) ; params.in})

# Run gadget3 model
# model_fn <- edit(model_fn) ; model_fn(params)
r_result <- model_fn(params)
g3_r <- attributes(r_result)

# If enabled run a TMB version too
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)

    param_template <- attr(model_cpp, "parameter_template")
    param_template$value <- params[param_template$switch]
    gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
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
dimnames(g2_igfs_imm$weight)$length[dim(g2_igfs_imm$weight)['length']] <- '156:Inf'

g2_igfs_mat <- Rgadget::read.gadget.file('inttest/understocking', 'igfs.lingmat.predprey.out')[[1]][,1:7]
names(g2_igfs_mat) <- c("year", "step", "area", "age", "length", "number", "weight")
attr(g2_igfs_mat, 'age') <- list(all3 = 3:5)
attr(g2_igfs_mat, 'length') <- Rgadget::read.gadget.file('inttest/understocking','Aggfiles/catchdistribution.ldist.igfs.len.agg')[[1]]
g2_igfs_mat <- gadget3:::g3l_likelihood_data('x', g2_igfs_mat)
dimnames(g2_igfs_mat$weight)$length[dim(g2_igfs_mat$weight)['length']] <- '156:Inf'

g3_imm_biomass <- g3_r$hist_lingimm__num[,,,] * g3_r$hist_lingimm__wgt[,,,]
g3_mat_biomass <- g3_r$hist_lingmat__num[,,,] * g3_r$hist_lingmat__wgt[,,,]

for (t in seq_len(dim(g3_r$hist_lingimm__num)['time'])) {
    # NB: Losing accuracy at timesteps 17:19 (i.e 1984, which only has age5 left)
    #     Think we're noticing gadget2's "if (< verysmall) 0"
    ok(all.equal(
        unname(g2_lingimm$number[,,t,1]),
        unname(g3_r$hist_lingimm__num[,1,,t]),
        tolerance = if (t %in% 17:19) 1e-3 else 1e-5), paste0("g3_r$hist_lingimm__num: ", t, " - ", dimnames(g3_r$hist_lingimm__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,t,1] * g2_lingimm$weight[,,t,1]),
        unname(g3_r$hist_lingimm__num[,1,,t] * g3_r$hist_lingimm__wgt[,1,,t]),
        tolerance = if (t %in% 17:18) 1e-3 else 1e-5), paste0("g3_r$hist_lingimm__wgt: ", t, " - ", dimnames(g3_r$hist_lingimm__wgt)$time[[t]]))

    if (t %in% 27:31) {
        # Gadget2 catches nonexistant fish in 1988. Check g3 is consistent at least
        ok(sum(g3_r$hist_lingimm__num[,1,,t]) < 0.0001, paste0("g3_r$hist_lingimm__num: ", t, " - "," No fish to be caught"))
        ok(sum(g3_r$hist_lingimm__totalpredate[,1,,t]) < 0.0001, paste0("g3_r$hist_lingimm__totalpredate: ", t, " - "," No fish caught"))
    } else {
        ok(all.equal(
            g2_igfs_imm$weight[,,t,1],
            rowSums(g3_r$hist_lingimm__totalpredate[,1,,t]),
            tolerance = if (t %in% 18) 1e-3 else 1e-4), paste0("g3_r$hist_lingimm__totalpredate: ", t, " - ", dimnames(g3_r$hist_lingimm__wgt)$time[[t]]))
    }

    if (t == 1) {
        # Initial step, no comparisons to make
    } else if ((t - 1) %% 4 == 0) {
        # Beginning of year, ages will have jumped between timesteps
        ok(all.equal(
            rowSums(g3_imm_biomass[,,t - 1]) - rowSums(g3_r$hist_lingimm__totalpredate[,,,t]),
            rowSums(g3_imm_biomass[,,t]),
            tolerance = if (t %in% 17) 1e-3 else 1e-6), paste0("g3_r$hist_lingimm__totalpredate: ", t, " - Consistent with fall in stock biomass, with age jump"))
    } else {
        # In-year timestep, so can compare age breakdown
        ok(all.equal(
            g3_imm_biomass[,,t - 1] - g3_r$hist_lingimm__totalpredate[,,,t],
            g3_imm_biomass[,,t],
            tolerance = if (t %in% 16:18) 1e-3 else 1e-5), paste0("g3_r$hist_lingimm__totalpredate: ", t, " - Consistent with fall in stock biomass"))
    }

    ####

    ok(all.equal(
        unname(g2_lingmat$number[,,t,1]),
        unname(g3_r$hist_lingmat__num[,1,,t]),
        tolerance = if (t %in% 17:19) 1e-3 else 1e-5), paste0("g3_r$hist_lingmat__num: ", t, " - ", dimnames(g3_r$hist_lingmat__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingmat$number[,,t,1] * g2_lingmat$weight[,,t,1]),
        unname(g3_r$hist_lingmat__num[,1,,t] * g3_r$hist_lingmat__wgt[,1,,t]),
        tolerance = if (t %in% 17) 1e-3 else 1e-5), paste0("g3_r$hist_lingmat__wgt: ", t, " - ", dimnames(g3_r$hist_lingmat__wgt)$time[[t]]))

    if (t %in% 27:31) {
        # Gadget2 catches nonexistant fish in 1987. Check g3 is consistent at least
        ok(sum(g3_r$hist_lingmat__num[,1,,t]) < 0.0001, paste0("g3_r$hist_lingmat__num: ", t, " - "," No fish to be caught"))
        ok(sum(g3_r$hist_lingmat__totalpredate[,1,,t]) < 0.0001, paste0("g3_r$hist_lingmat__totalpredate: ", t, " - "," No fish caught"))
    } else {
        ok(all.equal(
            g2_igfs_mat$weight[,,t,1],
            rowSums(g3_r$hist_lingmat__totalpredate[,1,,t]),
            tolerance = if (t %in% 18) 1e-3 else 1e-4), paste0("g3_r$hist_lingmat__totalpredate: ", t, " - ", dimnames(g3_r$hist_lingmat__wgt)$time[[t]]))
    }

    if (t == 1) {
        # Initial step, no comparisons to make
    } else if ((t - 1) %% 4 == 0) {
        # Beginning of year, ages will have jumped between timesteps
        ok(all.equal(
            rowSums(g3_mat_biomass[,,t - 1]) - rowSums(g3_r$hist_lingmat__totalpredate[,,,t]),
            rowSums(g3_mat_biomass[,,t]),
            tolerance = if (t %in% 17) 1e-2 else 1e-6), paste0("g3_r$hist_lingmat__totalpredate: ", t, " - Consistent with fall in stock biomass, with age jump"))
    } else {
        # In-year timestep, so can compare age breakdown
        ok(all.equal(
            g3_mat_biomass[,,t - 1] - g3_r$hist_lingmat__totalpredate[,,,t],
            g3_mat_biomass[,,t],
            tolerance = if (t %in% 16:18) 1e-3 else 1e-5), paste0("g3_r$hist_lingmat__totalpredate: ", t, " - Consistent with fall in stock biomass"))
    }

    ####

    if (t %in% 25:31) {
        ok(all.equal(
            sum(g3_r$hist_lingimm__totalpredate[,1,,t]) + sum(g3_r$hist_lingmat__totalpredate[,1,,t]),
            g3_r$hist_igfs__catch[1,t],
            tolerance = 1e-7), paste0("g3_r$hist_igfs__catch: ", t, " - Total catch internally consistent"))
    } else {
        ok(all.equal(
            sum(g2_igfs_imm$weight[,,t,1] + g2_igfs_mat$weight[,,t,1]),
            g3_r$hist_igfs__catch[1,t],
            tolerance = if (t %in% 17:20) 1e-3 else 1e-5), paste0("g3_r$hist_igfs__catch: ", t, " - Total catch matches g2"))
    }
}
ok(all.equal(
    colSums(g3_r$hist_igfs__catch[]),
    colSums(colSums(g3_r$hist_lingimm__predby_igfs[,,,])) + colSums(colSums(g3_r$hist_lingmat__predby_igfs[,,,])),
    tolerance = 1e-7), "hist_igfs__catch: Consistent with hist_lingimm__predby_igfs")

ok(all.equal(
    g3_r$hist_lingimm__predby_igfs,
    g3_r$hist_lingimm__totalpredate,
    tolerance = 1e-7), "hist_lingimm__totalpredate: Matches hist_lingimm__predby_igfs")

ok(all.equal(
    sum(g3_r$nll_report),
    sum(
        1 * sum(g3_r[['nll_adist_surveyindices_log_si.100-120__num']]),
        10 * sum(g3_r$nll_cdist_sumofsquares_ldist.igfs.ss__num),
        10 * sum(g3_r$nll_cdist_multinomial_ldist.igfs.mn__num),
        10 * sum(g3_r$nll_understocking__wgt)),
    tolerance = 1e-7), "g3_r$nll_report/g3_r$nll_cdist_sumofsquares_ldist.igfs.ss__num/g3_r$nll_understocking__wgt consistent with each other")
ok(ut_cmp_identical(
    dim(g3_r$nll_cdist_sumofsquares_ldist.igfs.ss__num),
    c(time = as.integer(length(year_range) * 4))), "g3_r$nll_cdist_sumofsquares_ldist.igfs.ss__num: Broken up into individual timesteps")
ok(ut_cmp_identical(
    dim(g3_r$nll_cdist_multinomial_ldist.igfs.mn__num),
    c(time = as.integer(length(year_range) * 4))), "g3_r$nll_cdist_multinomial_ldist.igfs.mn__num: Broken up into individual timesteps")
ok(ut_cmp_identical(
    dim(g3_r$nll_understocking__wgt),
    c(time = as.integer(length(year_range) * 4))), "g3_r$nll_understocking__wgt: Broken up into individual timesteps")

# Fill in zeros in nll report
g2_nll <- Rgadget::read.gadget.file('inttest/understocking', 'likelihood.out')[[1]]
g2_nll_understocking <- merge(g2_nll[g2_nll$component == 'understocking',], expand.grid(step=1:4, year=year_range), all.y = T)
g2_nll_understocking$likelihood_value[is.na(g2_nll_understocking$likelihood_value)] <- 0
g2_nll_understocking$weight[is.na(g2_nll_understocking$weight)] <- 0
g2_nll_ldist_ss.igfs <- merge(g2_nll[g2_nll$component == 'ldist.igfs.ss',], expand.grid(step=1:4, year=year_range), all.y = T)
g2_nll_ldist_ss.igfs$likelihood_value[is.na(g2_nll_ldist_ss.igfs$likelihood_value)] <- 0
g2_nll_ldist_ss.igfs$weight[is.na(g2_nll_ldist_ss.igfs$weight)] <- 0
g2_nll_ldist_mn.igfs <- merge(g2_nll[g2_nll$component == 'ldist.igfs.mn',], expand.grid(step=1:4, year=year_range), all.y = T)
g2_nll_ldist_mn.igfs$likelihood_value[is.na(g2_nll_ldist_mn.igfs$likelihood_value)] <- 0
g2_nll_ldist_mn.igfs$weight[is.na(g2_nll_ldist_mn.igfs$weight)] <- 0
# NB: SI not broken down here
g2_nll_si_likelihood_value <- g2_nll[g2_nll$component == 'si.100-120', 'likelihood_value']
g2_nll_si_weight <- g2_nll[g2_nll$component == 'si.100-120', 'weight']


ok(all.equal(
    g2_nll_ldist_ss.igfs$likelihood_value,
    as.vector(g3_r$nll_cdist_sumofsquares_ldist.igfs.ss__num),
    tolerance = 1e-4), "g3_r$nll_cdist_sumofsquares_ldist.igfs.ss__num - ldist.igfs matches")
ok(all.equal(
    g2_nll_ldist_mn.igfs$likelihood_value,
    as.vector(g3_r$nll_cdist_multinomial_ldist.igfs.mn__num),
    tolerance = 1e-4), "g3_r$nll_cdist_multinomial_ldist.igfs.mn__num - ldist.igfs matches")
ok(all.equal(
    as.vector(g3_r$nll_understocking__wgt) * 10 +  # NB: Ignoring g2 understocking, using our own
        g2_nll_ldist_ss.igfs$likelihood_value * g2_nll_ldist_ss.igfs$weight +
        g2_nll_ldist_mn.igfs$likelihood_value * g2_nll_ldist_mn.igfs$weight +
        g2_nll_si_likelihood_value * g2_nll_si_weight,
    as.vector(g3_r$nll_report),
    tolerance = 1e-6), "g3_r$nll_report - ldist.igfs in total sum")

# NB: A fully depleted stock in gadget2 isn't reported as understocked, but gadget3 does.
#     So ignore the rest of the nll entries that g3 produces.
ok(all.equal(
    g2_nll_understocking$likelihood_value[1:31],
    as.vector(g3_r$nll_understocking__wgt[1:31]),
    tolerance = 1e-7), "g3_r$nll_understocking__wgt: 1..31 equal, after that gadget2 stops reporting")

# NB: Once stocks fall too low accuracy falls with it, but we limit the tests
#     to only years with reasonable amounts of stock.
ok(all.equal(
    g2_nll_si_likelihood_value,
    sum(g3_r[['nll_adist_surveyindices_log_si.100-120__num']]),
    tolerance = 1e-6), "g3_r$nll_adist_surveyindices_log_si.100-120__num: Total matches reported likelihood value")
