if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st <- g3_stock(c("stst"), c(10, 20, 30))
fl <- g3_fleet(c("fl", "surv"))

actions <- list(
    g3a_time(1990, 1994, c(6,6)),
    gadget3:::g3a_initialconditions_manual(st,
        quote( 100 + stock__minlen ),
        quote( 1e4 + 0 * stock__minlen ) ),
    g3a_naturalmortality(st, g3a_naturalmortality_exp(g3_param_project(
        "Mrw",
        g3_param_project_rwalk(),
        random = FALSE ))),
    g3a_naturalmortality(st, g3a_naturalmortality_exp(g3_param_project(
        "Mdln",
        g3_param_project_dlnorm(),
        by_stock = st,
        scale = "scale",
        offset = "offset",
        random = FALSE ))),

    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood() )
full_actions <- c(actions, list(
    g3a_report_detail(actions),
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

ok_group("project_years=0") ###################################################

attr(model_fn, 'parameter_template') |>
    g3_init_val("Mrw.proj.rwalk.mean", runif(1, 0.001, 0.1)) |>
    g3_init_val("Mrw.proj.rwalk.stddev", 0.001) |>
    g3_init_val("Mrw.#.#", rnorm(5 * 2, 0.2, 0.001)) |>
    g3_init_val("stst.Mdln.proj.dlnorm.lmean", runif(1, 5, 10)) |>
    g3_init_val("stst.Mdln.proj.dlnorm.lstddev", 0.2) |>
    g3_init_val("stst.Mdln.#.#", rnorm(5 * 2, 18, 0.001)) |>
    g3_init_val("project_years", 0) |>
    identity() -> params
nll <- model_fn(params) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_equal(signif(mean(r$proj_rwalk_Mrw__var), 1), 0.2), "mean(r$proj_dnorm_Mrw__var): Same as param input")
ok(ut_cmp_equal(signif(mean(r$proj_dlnorm_stst_Mdln__var), 2), 18), "mean(r$proj_dlnorm_stst_Mdln__var): Same as param input")

ok(ut_cmp_equal(
    as.vector(r$proj_rwalk_Mrw__nll),
    as.vector(-dnorm(c("1990-01" = 0, diff(r$proj_rwalk_Mrw__var)), mean = params$Mrw.proj.rwalk.mean, sd = params$Mrw.proj.rwalk.stddev)),
    tolerance = 1e-7 ), "r$proj_rwalk_Mrw__nll: dnorm of __var")
ok(ut_cmp_equal(
    as.vector(r$proj_dlnorm_stst_Mdln__nll),
    as.vector(-dnorm(log(r$proj_dlnorm_stst_Mdln__var), params$stst.Mdln.proj.dlnorm.lmean - exp(2 * params$stst.Mdln.proj.dlnorm.lstddev)/2, exp(params$stst.Mdln.proj.dlnorm.lstddev), log = TRUE)),
    tolerance = 1e-7 ), "r$proj_dlnorm_stst_Mdln__nll: dnorm of __var")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

ok_group("project_years=0, high sd") ##########################################

attr(model_fn, 'parameter_template') |>
    g3_init_val("stst.Mdln.#.#", rnorm(5 * 2, 50, 10)) |>
    g3_init_val("stst.Mdln.proj.dlnorm.lmean", runif(1, 5, 10)) |>
    g3_init_val("stst.Mdln.proj.dlnorm.lstddev", 0.2) |>
    # Increase variance of input data to give dnorm()s something to chew on
    g3_init_val("Mrw.#.#", rnorm(5 * 2, 50, 10)) |>
    g3_init_val("Mrw.proj.rwalk.mean", runif(1, 0.001, 0.1)) |>
    g3_init_val("Mrw.proj.rwalk.stddev", 0.001) |>
    g3_init_val("project_years", 0) |>
    identity() -> params
nll <- model_fn(params) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_equal(
    as.vector(r$proj_rwalk_Mrw__nll),
    as.vector(-dnorm(c("1990-01" = 0, diff(r$proj_rwalk_Mrw__var)), mean = params$Mrw.proj.rwalk.mean, sd = params$Mrw.proj.rwalk.stddev)),
    tolerance = 1e-7 ), "r$proj_rwalk_Mrw__nll: dnorm of __var")
ok(ut_cmp_equal(
    as.vector(r$proj_dlnorm_stst_Mdln__nll),
    as.vector(-dnorm(log(r$proj_dlnorm_stst_Mdln__var), params$stst.Mdln.proj.dlnorm.lmean - exp(2 * params$stst.Mdln.proj.dlnorm.lstddev)/2, exp(params$stst.Mdln.proj.dlnorm.lstddev), log = TRUE)),
    tolerance = 1e-7 ), "r$proj_dlnorm_stst_Mdln__nll: dnorm of __var (also, by_stock has worked)")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

ok_group("project_years=20") ##################################################

attr(model_fn, 'parameter_template') |>
    g3_init_val("stst.Mdln.#.#", rnorm(5 * 2, 0.5, 0.001)) |>
    g3_init_val("stst.Mdln.proj.dlnorm.lmean", runif(1, 0.001, 0.1)) |>
    g3_init_val("stst.Mdln.proj.dlnorm.lstddev", 0.2) |>
    g3_init_val("Mrw.#.#", rnorm(5 * 2, 0.2, 0.001)) |>
    g3_init_val("Mrw.proj.rwalk.mean", runif(1, 0.001, 0.1)) |>
    g3_init_val("Mrw.proj.rwalk.stddev", 0.001) |>
    g3_init_val("project_years", 20) |>
    identity() -> params

# NB: Projections mean values between runs shouldn't match, so build list of values & compare all
rs <- list(
    attributes(model_fn(params)),
    attributes(model_fn(params)),
    attributes(model_fn(params)) )
if (nzchar(Sys.getenv("G3_TEST_TMB"))) {
    param_template <- attr(model_cpp, "parameter_template")
    param_template$value[names(params)] <- params
    model_tmb <- g3_tmb_adfun(model_cpp, param_template, compile_flags = c("-O0", "-g"))
    rs <- c(rs, list(
        model_tmb$report(),
        model_tmb$report(),
        model_tmb$report() ))
}

for (r in rs) {
    ok(ut_cmp_equal(
        length(r$proj_rwalk_Mrw__var),
        5 * 2 + params$project_years * 2 ), "length(r$proj_rwalk_Mrw__var): 5 normal years, 20 projection years, 2 steps for each")

    ok(ut_cmp_equal(signif(mean(head(r$proj_rwalk_Mrw__var, 5*2)), 1), 0.2), "mean(r$proj_rwalk_Mrw__var): Same as param input, for first 10")
    ok(ut_cmp_equal(signif(mean(head(r$proj_dlnorm_stst_Mdln__var, 5*2)), 1), 0.5), "mean(r$proj_dlnorm_stst_Mdln__var): Same as param input, for first 10")

    ok(ut_cmp_equal(
        mean(tail(r$proj_dlnorm_stst_Mdln__var, -5*2)),
        params$stst.Mdln.proj.dlnorm.lmean,
        tolerance = 1e4), "mean(r$proj_dlnorm_stst_Mdln__var): Projected values have a mean ~matching stst.Mdln.proj.dlnorm.lmean")
    ok(sd(tail(r$proj_dlnorm_stst_Mdln__var, -5*2)) > 0, "sd(r$proj_dlnorm_stst_Mdln__var): sd greater than 0 (values not equal)")
    ok(ut_cmp_equal(
        mean(diff(c(0, tail(r$proj_rwalk_Mrw__var, -5*2)))),
        params$Mrw.proj.rwalk.mean,
        tolerance = 1e4), "mean(r$proj_rwalk_Mrw__var): Projected values have a mean delta ~matching Mrw.proj.rwalk.mean")
    ok(sd(tail(r$proj_rwalk_Mrw__var, -5*2)) > 0, "sd(r$proj_rwalk_Mrw__var): sd greater than 0 (values not equal)")
}

ok_group("project_years=40, scale / offset") ###################################

attr(model_fn, 'parameter_template') |>
    g3_init_val("stst.Mdln.#.#", rnorm(5 * 2, 50, 10)) |>
    g3_init_val("stst.Mdln.proj.dlnorm.lmean", runif(1, 5, 10)) |>
    g3_init_val("stst.Mdln.proj.dlnorm.lstddev", 0.2) |>
    g3_init_val("stst.Mdln.scale", runif(1, 10, 100)) |>
    g3_init_val("stst.Mdln.offset", runif(1, 10, 100)) |>
    g3_init_val("project_years", 40) |>
    identity() -> params
nll <- model_fn(params) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_equal(
    as.vector(r$proj_dlnorm_stst_Mdln__var)[1:10],
    as.vector(unlist(params[sort(grep("stst.Mdln.[0-9]+.[0-9]+", names(params), value = TRUE))]) * params$stst.Mdln.scale + params$stst.Mdln.offset),
    tolerance = 1e-7 ), "proj_dlnorm_stst_Mdln__var: Values match input parameters with scale/offset applied")
ok(ut_cmp_equal(
    as.vector(r$proj_rwalk_Mrw__nll),
    as.vector(-dnorm(c("1990-01" = 0, diff(r$proj_rwalk_Mrw__var)), mean = params$Mrw.proj.rwalk.mean, sd = params$Mrw.proj.rwalk.stddev)),
    tolerance = 1e-7 ), "r$proj_rwalk_Mrw__nll: dnorm of __var")
ok(ut_cmp_equal(
    as.vector(r$proj_dlnorm_stst_Mdln__nll),
    as.vector(-dnorm(log(r$proj_dlnorm_stst_Mdln__var), params$stst.Mdln.proj.dlnorm.lmean - exp(2 * params$stst.Mdln.proj.dlnorm.lstddev)/2, exp(params$stst.Mdln.proj.dlnorm.lstddev), log = TRUE)),
    tolerance = 1e-7 ), "r$proj_dlnorm_stst_Mdln__nll: dnorm of __var (also, by_stock has worked)")

ok(ut_cmp_equal(
    mean(tail(r$proj_dlnorm_stst_Mdln__var, -5*2)),
    params$stst.Mdln.proj.dlnorm.lmean * params$stst.Mdln.scale + params$stst.Mdln.offset,
    tolerance = 1e4), "mean(r$proj_dlnorm_stst_Mdln__var): Projected values have a mean ~matching stst.Mdln.proj.dlnorm.lmean with scale/offset applied")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
