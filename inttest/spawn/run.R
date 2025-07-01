library(gadget2)
library(gadget3)
library(gadget2to3)
library(Rgadget)
library(unittest)

year_range <- 1982:1990

actions <- local({
    eval(g2to3_mainfile('inttest/spawn'))
    c(actions, list(
        g3a_report_history(actions),
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

model_fn <- g3_to_r(actions, strict = TRUE, trace = FALSE)
model_cpp <- g3_to_tmb(actions, trace = FALSE)
params <- local({eval(g2to3_params_r('inttest/spawn', 'params.in')) ; params.in})
params <- params[names(attr(model_cpp, "parameter_template")$value)]

# Run gadget3 model
# model_fn <- edit(model_fn) ; model_fn(params)
r_result <- model_fn(params)
g3_r <- attributes(r_result)

# If enabled run a TMB version too
gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

# Run gadget2 model
oldwd <- getwd()
setwd('inttest/spawn')
unlink('*.out')
system2(gadget2::gadget_binary(), " -i params.in -log debug.log")
setwd(oldwd)

g2_lingimm <- Rgadget::read.gadget.file('inttest/spawn', 'lingimm.out')[[1]]
names(g2_lingimm) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingimm <- gadget3:::g3l_likelihood_data('x', g2_lingimm)
g2_lingimm <- list(number = g2_lingimm$obs_array$num, weight = g2_lingimm$obs_array$wgt)
g2_lingmat <- Rgadget::read.gadget.file('inttest/spawn', 'lingmat.out')[[1]]
names(g2_lingmat) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingmat <- gadget3:::g3l_likelihood_data('x', g2_lingmat)
g2_lingmat <- list(number = g2_lingmat$obs_array$num, weight = g2_lingmat$obs_array$wgt)
# Paper over minlen/midlen difference
dimnames(g3_r$hist_lingimm__num)$length <- dimnames(g2_lingimm$number)$length

for (t in seq_len(dim(g3_r$hist_lingimm__num)['time'])) {
    ok(all.equal(
        unname(g2_lingimm$number[,,t,1]),
        unname(g3_r$hist_lingimm__num[,1,,t]),
        tolerance = 1e-6), paste0("g3_r$hist_lingimm__num: ", t, " - ", dimnames(g3_r$hist_lingimm__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,t,1] * g2_lingimm$weight[,,t,1]),
        unname(g3_r$hist_lingimm__num[,1,,t] * g3_r$hist_lingimm__wgt[,1,,t]),
        tolerance = 5e-6), paste0("g3_r$hist_lingimm__wgt: ", t, " - ", dimnames(g3_r$hist_lingimm__wgt)$time[[t]]))

    ok(all.equal(
        unname(g2_lingmat$number[,,t,1]),
        unname(g3_r$hist_lingmat__num[,1,,t]),
        tolerance = 1e-5), paste0("g3_r$hist_lingmat__num: ", t, " - ", dimnames(g3_r$hist_lingmat__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingmat$number[,,t,1] * g2_lingmat$weight[,,t,1]),
        unname(g3_r$hist_lingmat__num[,1,,t] * g3_r$hist_lingmat__wgt[,1,,t]),
        tolerance = 1e-5), paste0("g3_r$hist_lingmat__wgt: ", t, " - ", dimnames(g3_r$hist_lingmat__wgt)$time[[t]]))
}
