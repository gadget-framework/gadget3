library(gadget2)
library(gadget3)
library(Rgadget)
library(magrittr)
library(unittest)

year_range <- 1982:1990

ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(3, 10)

ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(5, 15)

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c(1))

imm_report <- g3_stock('imm_report', seq(20, 160, 4), open_ended = FALSE) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(3, 10) %>% g3s_time(year = local(year_range), step = 1:4)
mat_report <- g3_stock('mat_report', seq(20, 160, 4), open_ended = FALSE) %>%
    g3s_livesonareas(c(1)) %>%
    g3s_age(5, 15) %>% g3s_time(year = local(year_range), step = 1:4)

ling_imm_actions <- list(
    g3a_initialconditions_normalparam(ling_imm,
        factor_f = ~age * g3_param("lingimm.init") * g3_param("lingimm.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_naturalmortality(ling_imm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_growmature(ling_imm,
        impl_f = g3a_grow_impl_bbinom(
            g3a_grow_lengthvbsimple(~g3_param("ling.Linf"), ~g3_param("ling.k") * 0.001),
            g3a_grow_weightsimple(~g3_param("lingimm.walpha"), ~g3_param("lingimm.wbeta")),
            beta_f = ~g3_param("ling.bbin") * 10,
            maxlengthgroupgrowth = 15),
        transition_f = TRUE,
        maturity_f = g3a_mature_continuous(
            alpha = ~g3_param("ling.mat.alpha"),
            l50 = ~g3_param("ling.mat.l50"),
            beta = ~g3_param("ling.mat.beta"),
            a50 = ~g3_param("ling.mat.a50")),
        output_stocks = list(ling_mat)),
    g3a_age(ling_imm),
    list())

ling_mat_actions <- list(
    g3a_initialconditions_normalparam(ling_mat,
        factor_f = ~0 * g3_param("lingmat.init") * g3_param("lingmat.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingmat.walpha"),
        beta_f = ~g3_param("lingmat.wbeta")),
    g3a_naturalmortality(ling_mat, g3a_naturalmortality_exp(~g3_param("lingmat.M"))),
    g3a_age(ling_mat),
    list())

report_actions <- list(
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__num)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__wgt)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__num)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__wgt)),
       list())

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3), project_years = 0),
    list())

actions <- c(
  ling_imm_actions,
  ling_mat_actions,
  report_actions,
  time_actions)
# Patch in our own avoid_zero which doesn't use logspace_add
environment(actions[[1]][[1]])$avoid_zero <- gadget3:::g3_native(function (a) max(a, 1e-7), cpp = "[](Type a) -> Type { return std::max(a, (Type)1e-7); }")
environment(actions[[1]][[1]])$avoid_zero_vec <- gadget3:::g3_native(function (a) pmax(a, 1e-7), cpp = "[](vector<Type> a) -> vector<Type> { return a.cwiseMax(1e-7); }")

# NB: Strict = FALSE so we don't try to compare __prevtotal
model_fn <- g3_to_r(actions, strict = FALSE, trace = FALSE)

param_table <- read.table('inttest/maturity-continuous/params.in', header = TRUE)
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
setwd('inttest/maturity-continuous')
unlink('*.out')
system2(gadget2::gadget_binary(), " -i params.in -log debug.log")
setwd(oldwd)

g2_lingimm <- Rgadget::read.gadget.file('inttest/maturity-continuous', 'lingimm.out')[[1]]
names(g2_lingimm) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingimm <- gadget3:::g3l_likelihood_data('x', g2_lingimm)
g2_lingmat <- Rgadget::read.gadget.file('inttest/maturity-continuous', 'lingmat.out')[[1]]
names(g2_lingmat) <- c("year", "step", "area", "age", "length", "number", "weight")
g2_lingmat <- gadget3:::g3l_likelihood_data('x', g2_lingmat)

ok_group("Total numbers preserved", {
    g3_num_total <- colSums(colSums(g3_r$imm_report__num[,1,,])) + colSums(colSums(g3_r$mat_report__num[,1,,]))
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

for (t in seq_len(dim(g3_r$imm_report__num)['time'])) {
    ok(all.equal(
        unname(g2_lingimm$number[,,t,1]),
        unname(g3_r$imm_report__num[,1,,t]),
        tolerance = 1e-4), paste0("g3_r$imm_report__num: ", t, " - ", dimnames(g3_r$imm_report__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,t,1] * g2_lingimm$weight[,,t,1]),
        unname(g3_r$imm_report__num[,1,,t] * g3_r$imm_report__wgt[,1,,t]),
        tolerance = 1e-4), paste0("g3_r$imm_report__wgt: ", t, " - ", dimnames(g3_r$imm_report__wgt)$time[[t]]))

    ok(all.equal(
        unname(g2_lingmat$number[,,t,1]),
        unname(g3_r$mat_report__num[,1,,t]),
        tolerance = 1e-3), paste0("g3_r$mat_report__num: ", t, " - ", dimnames(g3_r$mat_report__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingmat$number[,,t,1] * g2_lingmat$weight[,,t,1]),
        unname(g3_r$mat_report__num[,1,,t] * g3_r$mat_report__wgt[,1,,t]),
        tolerance = 1e-3), paste0("g3_r$mat_report__wgt: ", t, " - ", dimnames(g3_r$mat_report__wgt)$time[[t]]))
}
