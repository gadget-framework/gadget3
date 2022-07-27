library(gadget2)
library(gadget3)
library(Rgadget)
library(magrittr)
library(unittest)

year_range <- 1982:1990

lingimm <- g3_stock('lingimm', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(3, 10)

lingmat <- g3_stock('lingmat', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(5, 15)

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c(1))

lingimm_actions <- list(
    g3a_initialconditions_normalparam(lingimm,
        factor_f = ~(age * g3_param("lingimm.init")) * g3_param("lingimm.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_naturalmortality(lingimm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_age(lingimm),
    list())

lingmat_actions <- list(
    g3a_initialconditions_normalparam(lingmat,
        factor_f = ~(age * g3_param("lingmat.init")) * g3_param("lingmat.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingmat.walpha"),
        beta_f = ~g3_param("lingmat.wbeta")),
    g3a_naturalmortality(lingmat, g3a_naturalmortality_exp(~g3_param("lingmat.M"))),
    g3a_age(lingmat),
    g3a_spawn(
        lingmat,
        recruitment_f = g3a_spawn_recruitment_ricker(
            ~g3_param("ricker.mu"),
            ~g3_param("ricker.lambda")),
        # NB: exponentiall50 needs to negate alpha to match gadget2
        proportion_f = g3_suitability_exponentiall50(alpha = ~-g3_param("spawn.prop.alpha"), l50 =  ~g3_param("spawn.prop.l50")),
        mortality_f = g3_suitability_straightline(alpha = ~g3_param("spawn.mort.alpha"), beta =  ~g3_param("spawn.mort.beta")),
        weightloss_f = ~g3_param("spawn.weightloss"),
        output_stocks = list(lingimm),
        mean_f = 50,
        stddev_f = 0.9,
        alpha_f = 1,
        beta_f = 1,
        run_f = ~cur_step==1),
    list())

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
    list())

actions <- c(
  lingimm_actions,
  lingmat_actions,
  time_actions)
actions <- c(actions, list(g3a_report_history(actions)))
# Patch in our own avoid_zero which doesn't use logspace_add
environment(actions[[1]][[1]])$avoid_zero <- gadget3:::g3_native(function (a) max(a, 1e-7), cpp = "[](Type a) -> Type { return std::max(a, (Type)1e-7); }")
environment(actions[[1]][[1]])$avoid_zero_vec <- gadget3:::g3_native(function (a) pmax(a, 1e-7), cpp = "[](vector<Type> a) -> vector<Type> { return a.cwiseMax(1e-7); }")

model_fn <- g3_to_r(actions, strict = TRUE, trace = FALSE)

param_table <- read.table('inttest/spawn/params.in', header = TRUE, comment.char = ";")
params <- as.list(param_table$value)
names(params) <- param_table$switch
params <- c(params, attr(model_fn, 'parameter_template'))
params <- params[!duplicated(names(params))]

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
setwd('inttest/spawn')
unlink('*.out')
system2(gadget2::gadget_binary(), " -i params.in -log debug.log")
setwd(oldwd)

g2_lingimm <- Rgadget::read.gadget.file('inttest/spawn', 'lingimm.out')[[1]]
names(g2_lingimm) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingimm <- gadget3:::g3l_likelihood_data('x', g2_lingimm)
g2_lingmat <- Rgadget::read.gadget.file('inttest/spawn', 'lingmat.out')[[1]]
names(g2_lingmat) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingmat <- gadget3:::g3l_likelihood_data('x', g2_lingmat)
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
