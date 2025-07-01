library(gadget2)
library(gadget3)
library(gadget2to3)
library(Rgadget)
library(unittest)
options(gadget3.tmb.work_dir = 'work_dir')

year_range <- 1982:1990

actions <- local({
    eval(g2to3_mainfile('inttest/maturity-continuous'))
    c(actions, list(
        g3a_report_history(actions),
        g3a_report_history(actions, "__growth_[lw]$|stock__transitioning_(num|wgt)$", "growthinternals_"),
        # NB: Only required for testing
        gadget3:::g3l_test_dummy_likelihood() ))
})

# Replace avoid_zero with a more accurate scale
actions <- c(g3_formula({
    avoid_zero(nll)
}, avoid_zero = g3_native(r = function(a) {
    dif_pmax(a, 0.0, 1e7)
}, cpp = '
template<typename X>
auto __fn__(X a) {
    return dif_pmax(a, 0.0, 1e7);
}
', depends = c("dif_pmax")) ), actions)

model_fn <- g3_to_r(actions, strict = FALSE, trace = FALSE)
model_cpp <- g3_to_tmb(actions, trace = FALSE)
params <- local({eval(g2to3_params_r('inttest/maturity-continuous', 'params.in')) ; params.in})
params <- params[names(attr(model_cpp, "parameter_template")$value)]

# Run gadget3 model
# model_fn <- edit(model_fn) ; model_fn(params)
r_result <- model_fn(params)
g3_r <- attributes(r_result)

# If enabled run a TMB version too
gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

# Run gadget2 model
oldwd <- getwd()
setwd('inttest/maturity-continuous')
unlink('*.out')
system2(gadget2::gadget_binary(), " -i params.in -log debug.log")
setwd(oldwd)

g2_lingimm <- Rgadget::read.gadget.file('inttest/maturity-continuous', 'lingimm.out')[[1]]
names(g2_lingimm) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingimm <- gadget3:::g3l_likelihood_data('x', g2_lingimm)
g2_lingimm <- list(number = g2_lingimm$obs_array$num, weight = g2_lingimm$obs_array$wgt)
g2_lingmat <- Rgadget::read.gadget.file('inttest/maturity-continuous', 'lingmat.out')[[1]]
names(g2_lingmat) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingmat <- gadget3:::g3l_likelihood_data('x', g2_lingmat)
g2_lingmat <- list(number = g2_lingmat$obs_array$num, weight = g2_lingmat$obs_array$wgt)

ok_group("Total numbers preserved", {
    g3_num_total <- colSums(colSums(g3_r$hist_lingimm__num[,1,,])) + colSums(colSums(g3_r$hist_lingmat__num[,1,,]))
    ok(all.equal(
        unname(g3_num_total / c(g3_num_total[[1]] / 0.9631944, head(g3_num_total, -1))),
        rep(0.963, length(year_range) * 4),
        tolerance = 1e-3), "g3_num_total: Consistently losing 0.963")
    g2_num_total <- colSums(colSums(g2_lingimm$number[,,,1])) + colSums(colSums(g2_lingmat$number[,,,1]))
    ok(all.equal(
        unname(g2_num_total / c(g2_num_total[[1]] / 0.9631944, head(g2_num_total, -1))),
        rep(0.963, length(year_range) * 4),
        tolerance = 1e-3), "g2_num_total: Consistently losing 0.963")
})

for (t in seq_len(dim(g3_r$hist_lingimm__num)['time'])) {
    ok(all.equal(
        unname(g2_lingimm$number[,,t,1]),
        unname(g3_r$hist_lingimm__num[,1,,t]),
        tolerance = 1e-4), paste0("g3_r$hist_lingimm__num: ", t, " - ", dimnames(g3_r$hist_lingimm__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,t,1] * g2_lingimm$weight[,,t,1]),
        unname(g3_r$hist_lingimm__num[,1,,t] * g3_r$hist_lingimm__wgt[,1,,t]),
        tolerance = 1e-4), paste0("g3_r$hist_lingimm__wgt: ", t, " - ", dimnames(g3_r$hist_lingimm__wgt)$time[[t]]))

    ok(all.equal(
        unname(g2_lingmat$number[,,t,1]),
        unname(g3_r$hist_lingmat__num[,1,,t]),
        tolerance = 1e-3), paste0("g3_r$hist_lingmat__num: ", t, " - ", dimnames(g3_r$hist_lingmat__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingmat$number[,,t,1] * g2_lingmat$weight[,,t,1]),
        unname(g3_r$hist_lingmat__num[,1,,t] * g3_r$hist_lingmat__wgt[,1,,t]),
        tolerance = 1e-3), paste0("g3_r$hist_lingmat__wgt: ", t, " - ", dimnames(g3_r$hist_lingmat__wgt)$time[[t]]))
}
