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

lingimm <- g3_stock('lingimm', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(3, 10)

lingmat <- g3_stock('lingmat', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(5, 15)

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c(1))

imm_report <- g3_stock('imm_report', seq(20, 160, 4), open_ended = FALSE) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(3, 10) %>% g3s_time(year = local(year_range), step = 1:4)
mat_report <- g3_stock('mat_report', seq(20, 160, 4), open_ended = FALSE) %>%
    g3s_livesonareas(c(1)) %>%
    g3s_age(5, 15) %>% g3s_time(year = local(year_range), step = 1:4)

lingimm_actions <- lapply(list(
    g3a_initialconditions_normalparam(lingimm,
        factor_f = ~(age * g3_param("lingimm.init")) * g3_param("lingimm.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_naturalmortality(lingimm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_growmature(lingimm,
        impl_f = g3a_grow_impl_bbinom(
            g3a_grow_lengthvbsimple(~g3_param("ling.Linf"), ~0.001 * g3_param("ling.k")),
            g3a_grow_weightsimple(~g3_param("lingimm.walpha"), ~g3_param("lingimm.wbeta")),
            beta_f = ~10 * g3_param("ling.bbin"),
            maxlengthgroupgrowth = 15),
        maturity_f = g3a_mature_constant(
            alpha = ~g3_param("ling.mat.alpha"),
            l50 = ~g3_param("ling.mat.l50")),
        transition_f = quote( cur_step == 4L ),
        output_stocks = list(lingmat)),
    g3a_age(lingimm),
    list()), remove_avoid_zero)

lingmat_actions <- lapply(list(
    g3a_initialconditions_normalparam(lingmat,
        factor_f = ~(age * g3_param("lingmat.init")) * g3_param("lingmat.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingmat.walpha"),
        beta_f = ~g3_param("lingmat.wbeta")),
    g3a_naturalmortality(lingmat, g3a_naturalmortality_exp(~g3_param("lingmat.M"))),
    g3a_age(lingmat),
    list()), remove_avoid_zero)

report_actions <- list(
       g3a_report_stock(imm_report,lingimm, ~stock_ss(lingimm__num)),
       g3a_report_stock(imm_report,lingimm, ~stock_ss(lingimm__wgt)),
       g3a_report_stock(mat_report,lingmat, ~stock_ss(lingmat__num)),
       g3a_report_stock(mat_report,lingmat, ~stock_ss(lingmat__wgt)),
       list())

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
    list())

actions <- c(
  lingimm_actions,
  lingmat_actions,
  report_actions,
  time_actions)

# NB: with strict we'll report prevtotal, which isn't comparable with TMB
model_fn <- g3_to_r(actions, strict = FALSE, trace = FALSE)

param_table <- read.table('inttest/maturity/params.in', header = TRUE)
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
        tolerance = 1e-3), paste0("g3_r$imm_report__num: ", t, " - ", dimnames(g3_r$imm_report__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,t,1] * g2_lingimm$weight[,,t,1]),
        unname(g3_r$imm_report__num[,1,,t] * g3_r$imm_report__wgt[,1,,t]),
        tolerance = 1e-3), paste0("g3_r$imm_report__wgt: ", t, " - ", dimnames(g3_r$imm_report__wgt)$time[[t]]))

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
