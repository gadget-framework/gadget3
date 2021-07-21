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

ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(3, 10)

ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c(1)) %>%
    g3s_age(5, 15)

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c(1))

imm_report <- g3s_clone(ling_imm, 'imm_report') %>% g3s_time(year = local(year_range), step = 1:4)
mat_report <- g3s_clone(ling_mat, 'mat_report') %>% g3s_time(year = local(year_range), step = 1:4)

ling_imm_actions <- lapply(list(
    g3a_initialconditions_normalparam(ling_imm,
        factor_f = ~age * g3_param("lingimm.init") * g3_param("lingimm.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_naturalmortality(ling_imm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_age(ling_imm),
    list()), remove_avoid_zero)

ling_mat_actions <- lapply(list(
    g3a_initialconditions_normalparam(ling_mat,
        factor_f = ~age * g3_param("lingmat.init") * g3_param("lingmat.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingmat.walpha"),
        beta_f = ~g3_param("lingmat.wbeta")),
    g3a_naturalmortality(ling_mat, g3a_naturalmortality_exp(~g3_param("lingmat.M"))),
    g3a_age(ling_mat),
    g3a_spawn(
        ling_mat,
        recruitment_f = g3a_spawn_recriutment_ricker(
            ~g3_param("ricker.mu"),
            ~g3_param("ricker.lambda")),
        proportion_f = g3a_spawn_length_exponential(alpha = ~g3_param("spawn.prop.alpha"), l50 =  ~g3_param("spawn.prop.l50")),
        mortality_f = g3a_spawn_length_straightline(alpha = ~g3_param("spawn.mort.alpha"), beta =  ~g3_param("spawn.mort.beta")),
        weightloss_f = ~g3_param("spawn.weightloss"),
        output_stocks = list(ling_imm),
        mean_f = 50,
        stddev_f = 0.9,
        alpha_f = 1,
        beta_f = 1,
        run_f = ~cur_step==1),
    list()), remove_avoid_zero)

report_actions <- list(
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__num)),
       g3a_report_stock(imm_report,ling_imm, ~stock_ss(ling_imm__wgt)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__num)),
       g3a_report_stock(mat_report,ling_mat, ~stock_ss(ling_mat__wgt)),
       list())

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
    list())

actions <- c(
  ling_imm_actions,
  ling_mat_actions,
  report_actions,
  time_actions)

model_fn <- g3_to_r(actions, strict = TRUE, trace = FALSE)
param_table <- read.table('inttest/spawn/params.in', header = TRUE, comment.char = ";")
param <- as.list(param_table$value)
names(param) <- param_table$switch

# Run gadget3 model
# model_fn <- edit(model_fn) ; model_fn(param)
r_result <- model_fn(param)
g3_r <- attributes(r_result)

# If enabled run a TMB version too
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, param)

    model_tmb_report <- model_tmb$report()
    for (n in ls(attributes(r_result))) {
        ok(all.equal(
            as.vector(model_tmb_report[[n]]),
            as.vector(attr(r_result, n)),
            tolerance = 1e-5), paste("TMB and R match", n))
    }
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
dimnames(g3_r$imm_report__num)$length <- dimnames(g2_lingimm$number)$length

for (t in seq_len(dim(g3_r$imm_report__num)['time'])) {
    ok(all.equal(
        unname(g2_lingimm$number[,,1,t]),
        unname(g3_r$imm_report__num[,1,,t]),
        tolerance = 1e-6), paste0("g3_r$imm_report__num: ", t, " - ", dimnames(g3_r$imm_report__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingimm$number[,,1,t] * g2_lingimm$weight[,,1,t]),
        unname(g3_r$imm_report__num[,1,,t] * g3_r$imm_report__wgt[,1,,t]),
        tolerance = 5e-6), paste0("g3_r$imm_report__wgt: ", t, " - ", dimnames(g3_r$imm_report__wgt)$time[[t]]))

    ok(all.equal(
        unname(g2_lingmat$number[,,1,t]),
        unname(g3_r$mat_report__num[,1,,t]),
        tolerance = 1e-5), paste0("g3_r$mat_report__num: ", t, " - ", dimnames(g3_r$mat_report__num)$time[[t]]))

    ok(all.equal(
        # NB: Use total weight, since mean weight will do different things with no fish
        unname(g2_lingmat$number[,,1,t] * g2_lingmat$weight[,,1,t]),
        unname(g3_r$mat_report__num[,1,,t] * g3_r$mat_report__wgt[,1,,t]),
        tolerance = 1e-5), paste0("g3_r$mat_report__wgt: ", t, " - ", dimnames(g3_r$mat_report__wgt)$time[[t]]))
}
