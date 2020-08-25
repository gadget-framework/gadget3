library(magrittr)
library(unittest)

library(gadget3)

tmb_r_compare <- function (model_fn, model_tmb, params) {
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        # Reformat params into a single vector in expected order
        par <- unlist(params[names(environment(model_cpp)$model_parameters)])
        model_tmb_report <- model_tmb$report(par)
        for (n in ls(environment(model_fn)$model_report)) {
            ok(ut_cmp_equal(
                as.vector(model_tmb_report[[n]]),
                as.vector(environment(model_fn)$model_report[[n]]),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}
areas <- c(a = 1, b = 2, c = 3)

prey_a <- g3_stock('prey_a', seq(1, 10)) %>% g3s_livesonareas(areas[c('a')])
prey_b <- g3_stock('prey_b', seq(1, 10)) %>% g3s_livesonareas(areas[c('b')])
prey_c <- g3_stock('prey_c', seq(1, 10)) %>% g3s_livesonareas(areas[c('c')])
fleet_ab <- g3_fleet('fleet_ab') %>% g3s_livesonareas(areas[c('a', 'b')])

# Generate observation data
cd_data <- expand.grid(year = 1999:2000, step = 1, area = 'x', length = c(1,6))
cd_data$number <- c(383, 271, 166, 882)
attr(cd_data, 'area') <- list(x = c(areas[c('a', 'b', 'c')]))

# Variables to store intermediate output
step0_cdist_utcd_pred__num <- array(dim = c(2L, 1L))  # Length, area (Aggregated into one bucket)
step1_cdist_utcd_pred__num <- array(dim = c(2L, 1L))
step0_prey_a__fleet_ab <- array(dim = c(10L, 1L))
step0_prey_b__fleet_ab <- array(dim = c(10L, 1L))
step0_prey_c__fleet_ab <- array(dim = c(10L, 1L))
step1_prey_a__fleet_ab <- array(dim = c(10L, 1L))
step1_prey_b__fleet_ab <- array(dim = c(10L, 1L))
step1_prey_c__fleet_ab <- array(dim = c(10L, 1L))
step0_nll <- 0.0
step1_nll <- 0.0

actions <- g3_collate(
    g3a_time(1999,2000),
    g3a_initialconditions(prey_a, ~10 * prey_a__midlen, ~100 * prey_a__midlen),
    g3a_initialconditions(prey_b, ~10 * prey_b__midlen, ~100 * prey_b__midlen),
    g3a_initialconditions(prey_c, ~10 * prey_c__midlen, ~100 * prey_c__midlen),
    g3a_predate_totalfleet(
        fleet_ab,
        list(prey_a, prey_b, prey_c),
        suitabilities = list(
            prey_a = ~g3_param_vector("fleet_ab_a"),
            prey_b = ~g3_param_vector("fleet_ab_b"),
            prey_c = ~g3_param_vector("fleet_ab_c")),
        amount_f = ~g3_param('amount_ab') * area),
    g3l_catchdistribution(
        'utcd',
        cd_data,
        list(fleet_ab),
        list(prey_a, prey_b, prey_c),
        g3l_catchdistribution_sumofsquares()),
    list(
        '010:utcd:001:zzzz' = ~{  # Capture data just before final step erases it
            if (cur_time == 0) {
                step0_cdist_utcd_pred__num[] <- cdist_utcd_pred__num
                g3_report(step0_cdist_utcd_pred__num)
            } else if (cur_time == 1) {
                step1_cdist_utcd_pred__num[] <- cdist_utcd_pred__num
                g3_report(step1_cdist_utcd_pred__num)
            }
        },
        '999' = ~{
            if (cur_time == 0) {
                step0_prey_a__fleet_ab[] <- prey_a__fleet_ab
                step0_prey_b__fleet_ab[] <- prey_b__fleet_ab
                step0_prey_c__fleet_ab[] <- prey_c__fleet_ab
                step0_nll <- nll
                g3_report(step0_prey_a__fleet_ab)
                g3_report(step0_prey_b__fleet_ab)
                g3_report(step0_prey_c__fleet_ab)
                g3_report(step0_nll)
            } else if (cur_time == 1) {
                step1_prey_a__fleet_ab[] <- prey_a__fleet_ab
                step1_prey_b__fleet_ab[] <- prey_b__fleet_ab
                step1_prey_c__fleet_ab[] <- prey_c__fleet_ab
                step1_nll <- nll
                g3_report(step1_prey_a__fleet_ab)
                g3_report(step1_prey_b__fleet_ab)
                g3_report(step1_prey_c__fleet_ab)
                g3_report(step1_nll)
            }
            g3_report(cdist_utcd_obs__num)
            g3_report(prey_a__wgt)
            g3_report(prey_b__wgt)
            g3_report(prey_c__wgt)

            # NB: In theory we could inspect the return value, but TMB doesn't give an easy public method for that
            g3_report(nll)
        }))
params <- list(
    fleet_ab_a = c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0),
    fleet_ab_b = c(0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0),
    fleet_ab_c = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
    amount_ab = 100,
    x=1.0)

# Compile model
model_fn <- g3_compile_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_precompile_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
} else {
    writeLines("# skip: not compiling TMB model")
}

ok_group("Likelihood", {
    params <- list(
        fleet_ab_a = c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0),
        fleet_ab_b = c(0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0),
        fleet_ab_c = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
        amount_ab = 1000000,
        x=1.0)
    result <- model_fn(params)
    r <- environment(model_fn)$model_report
    # str(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_equal(
        as.vector(r$cdist_utcd_obs__num),
        cd_data$number), "cdist_utcd_obs__num: Imported from data.frame")
    ok(ut_cmp_equal(
        as.vector(r$step0_cdist_utcd_pred__num),
        c(sum(r$step0_prey_a__fleet_ab[1:5,]) / sum(r$prey_a__wgt[1:5,]) +
          sum(r$step0_prey_b__fleet_ab[1:5,]) / sum(r$prey_b__wgt[1:5,]) +
          sum(r$step0_prey_c__fleet_ab[1:5,]) / sum(r$prey_c__wgt[1:5,]),
          sum(r$step0_prey_a__fleet_ab[6:10,]) / sum(r$prey_a__wgt[6:10,]) +
          sum(r$step0_prey_b__fleet_ab[6:10,]) / sum(r$prey_b__wgt[6:10,]) +
          sum(r$step0_prey_c__fleet_ab[6:10,]) / sum(r$prey_c__wgt[6:10,]))), "step0_cdist_utcd_pred__num: Summed catch values as individuals")
    ok(ut_cmp_equal(
        as.vector(r$step1_cdist_utcd_pred__num),
        c(sum(r$step1_prey_a__fleet_ab[1:5,]) / sum(r$prey_a__wgt[1:5,]) +
          sum(r$step1_prey_b__fleet_ab[1:5,]) / sum(r$prey_b__wgt[1:5,]) +
          sum(r$step1_prey_c__fleet_ab[1:5,]) / sum(r$prey_c__wgt[1:5,]),
          sum(r$step1_prey_a__fleet_ab[6:10,]) / sum(r$prey_a__wgt[6:10,]) +
          sum(r$step1_prey_b__fleet_ab[6:10,]) / sum(r$prey_b__wgt[6:10,]) +
          sum(r$step1_prey_c__fleet_ab[6:10,]) / sum(r$prey_c__wgt[6:10,]))), "step1_cdist_utcd_pred__num: Summed catch values as individuals")

    ok(ut_cmp_equal(
        r$step0_nll,
        (r$step0_cdist_utcd_pred__num[[1]] / sum(r$step0_cdist_utcd_pred__num) -
            r$cdist_utcd_obs__num[,,1][[1]] / sum(r$cdist_utcd_obs__num[,,1])) ** 2 +
        (r$step0_cdist_utcd_pred__num[[2]] / sum(r$step0_cdist_utcd_pred__num) -
            r$cdist_utcd_obs__num[,,1][[2]] / sum(r$cdist_utcd_obs__num[,,1])) ** 2), "step0_nll: Sum of squares")
    ok(ut_cmp_equal(
        r$step1_nll - r$step0_nll,
        (r$step1_cdist_utcd_pred__num[[1]] / sum(r$step1_cdist_utcd_pred__num) -
            r$cdist_utcd_obs__num[,,2][[1]] / sum(r$cdist_utcd_obs__num[,,2])) ** 2 +
        (r$step1_cdist_utcd_pred__num[[2]] / sum(r$step1_cdist_utcd_pred__num) -
            r$cdist_utcd_obs__num[,,2][[2]] / sum(r$cdist_utcd_obs__num[,,2])) ** 2), "step1_nll: Sum of squares, including step0_nll")
    ok(ut_cmp_equal(r$step1_nll, result), "result: Reported final nll")

    tmb_r_compare(model_fn, model_tmb, params)
})
