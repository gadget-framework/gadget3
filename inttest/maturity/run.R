library(gadget2)
library(gadget3)
library(gadget2to3)
library(Rgadget)
library(unittest)
options(gadget3.tmb.work_dir = 'work_dir')

year_range <- 1982:1990

actions <- local({
    eval(g2to3_mainfile('inttest/maturity'))
    c(actions, list(g3a_report_history(actions)))
})
environment(actions[[1]][[1]])$avoid_zero <- g3_native(function (a) max(a, 1e-7), cpp = "[](Type a) -> Type { return std::max(a, (Type)1e-7); }")
environment(actions[[1]][[1]])$avoid_zero_vec <- g3_native(function (a) pmax(a, 1e-7), cpp = "[](vector<Type> a) -> vector<Type> { return a.cwiseMax(1e-7); }")
model_fn <- g3_to_r(actions, strict = FALSE, trace = FALSE)
params <- local({eval(g2to3_params_r('inttest/maturity', 'params.in')) ; params.in})

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
setwd('inttest/maturity')
unlink('*.out')
system2(gadget2::gadget_binary(), " -i params.in -log debug.log")
setwd(oldwd)

g2_lingimm <- Rgadget::read.gadget.file('inttest/maturity', 'lingimm.out')[[1]]
names(g2_lingimm) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingimm <- gadget3:::g3l_likelihood_data('x', g2_lingimm)
g2_lingmat <- Rgadget::read.gadget.file('inttest/maturity', 'lingmat.out')[[1]]
names(g2_lingmat) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingmat <- gadget3:::g3l_likelihood_data('x', g2_lingmat)

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
        tolerance = 1e-3), paste0("g3_r$hist_lingimm__num: ", t, " - ", dimnames(g3_r$hist_lingimm__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,t,1] * g2_lingimm$weight[,,t,1]),
        unname(g3_r$hist_lingimm__num[,1,,t] * g3_r$hist_lingimm__wgt[,1,,t]),
        tolerance = 1e-3), paste0("g3_r$hist_lingimm__wgt: ", t, " - ", dimnames(g3_r$hist_lingimm__wgt)$time[[t]]))

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
