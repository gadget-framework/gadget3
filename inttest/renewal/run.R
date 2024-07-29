library(gadget2)
library(gadget3)
library(gadget2to3)
library(Rgadget)
library(unittest)
options(gadget3.tmb.work_dir = 'work_dir')

year_range <- 1982:1990

actions <- local({
    eval(g2to3_mainfile('inttest/renewal'))
    c(actions, list(g3a_report_history(actions)))
})
environment(actions[[1]][[1]])$avoid_zero <- g3_native(function (a) max(a, 1e-7), cpp = "[](Type a) -> Type { return std::max(a, (Type)1e-7); }")
environment(actions[[1]][[1]])$avoid_zero_vec <- g3_native(function (a) pmax(a, 1e-7), cpp = "[](vector<Type> a) -> vector<Type> { return a.cwiseMax(1e-7); }")
model_fn <- g3_to_r(actions, strict = FALSE, trace = FALSE)
params <- local({eval(g2to3_params_r('inttest/renewal', 'params.in')) ; params.in})

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
setwd('inttest/renewal')
unlink('*.out')
system2(gadget2::gadget_binary(), " -i params.in -log debug.log")
setwd(oldwd)

g2_lingimm <- Rgadget::read.gadget.file('inttest/renewal', 'lingimm.out')[[1]]
names(g2_lingimm) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingimm <- gadget3:::g3l_likelihood_data('x', g2_lingimm)
g2_lingimm <- list(number = g2_lingimm$obs_array$num, weight = g2_lingimm$obs_array$wgt)

for (t in seq_len(dim(g3_r$hist_lingimm__num)['time'])) {
    ok(all.equal(
        unname(g2_lingimm$number[,,t,1]),
        unname(g3_r$hist_lingimm__num[,1,,t]),
        tolerance = 1e-5), paste0("g3_r$hist_lingimm__num: ", t, " - ", dimnames(g3_r$hist_lingimm__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,t,1] * g2_lingimm$weight[,,t,1]),
        unname(g3_r$hist_lingimm__num[,1,,t] * g3_r$hist_lingimm__wgt[,1,,t]),
        tolerance = 1e-5), paste0("g3_r$hist_lingimm__wgt: ", t, " - ", dimnames(g3_r$hist_lingimm__wgt)$time[[t]]))
}
