if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st <- g3_stock(c("stst"), c(10, 20, 30))
fl <- g3_fleet(c("fl", "surv"))

actions <- list(
    g3a_time(1990, 1994, c(6,6)),
    g3a_initialconditions(st,
        quote( 100 + stock__minlen ),
        quote( 1e4 + 0 * stock__minlen ) ),
    g3a_naturalmortality(st, g3a_naturalmortality_exp(g3_param_project(
        "Mrw",
        g3_param_project_rwalk(
            mean_f = g3_parameterized("Mrw_mean", value = 0.001),
            stddev_f = g3_parameterized("Mrw_sd", value = 0.001) ),
        random = FALSE ))),
    g3a_naturalmortality(st, g3a_naturalmortality_exp(g3_param_project(
        "Mdn",
        g3_param_project_dlnorm(
            lmean_f = g3_parameterized("Mdn_mean", value = 0.1),
            lstddev_f = g3_parameterized("Mdn_sd", value = 0.2) ),
        random = FALSE ))),
    # NB: Dummy parameter so model will compile in TMB
    quote( nll <- nll + g3_param("x", value = 0) ) )
full_actions <- c(actions, list(
    g3a_report_detail(actions),
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

ok_group("project_years=0") ###################################################

attr(model_fn, 'parameter_template') |>
    g3_init_val("Mrw_mean", runif(1, 0.001, 0.1)) |>
    g3_init_val("Mdn_mean", runif(1, 5, 10)) |>
    g3_init_val("Mrw.#.#", rnorm(5 * 2, 0.2, 0.001)) |>
    g3_init_val("Mdn.#.#", rnorm(5 * 2, 18, 0.001)) |>
    g3_init_val("project_years", 0) |>
    identity() -> params
nll <- model_fn(params) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_equal(signif(mean(r$proj_rwalk_Mrw__var), 1), 0.2), "mean(r$proj_dnorm_Mrw__var): Same as param input")
ok(ut_cmp_equal(signif(mean(r$proj_dlnorm_Mdn__var), 2), 18), "mean(r$proj_dlnorm_Mdn__var): Same as param input")

ok(ut_cmp_equal(
    as.vector(r$proj_rwalk_Mrw__nll),
    as.vector(dnorm(c("1990-01" = 0, diff(r$proj_rwalk_Mrw__var)), mean = params$Mrw_mean, sd = params$Mrw_sd)),
    tolerance = 1e-7 ), "r$proj_rwalk_Mrw__nll: dnorm of __var")
ok(ut_cmp_equal(
    as.vector(r$proj_dlnorm_Mdn__nll),
    as.vector(-dnorm(log(r$proj_dlnorm_Mdn__var), params$Mdn_mean - exp(2 * params$Mdn_sd)/2, exp(params$Mdn_sd), log = TRUE)),
    tolerance = 1e-7 ), "r$proj_dlnorm_Mdn__nll: dnorm of __var")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

ok_group("project_years=0, high sd") ##########################################

attr(model_fn, 'parameter_template') |>
    g3_init_val("Mrw_mean", runif(1, 0.001, 0.1)) |>
    g3_init_val("Mdn_mean", runif(1, 5, 10)) |>
    # Increase variance of input data to give dnorm()s something to chew on
    g3_init_val("Mrw.#.#", rnorm(5 * 2, 50, 10)) |>
    g3_init_val("Mdn.#.#", rnorm(5 * 2, 50, 10)) |>
    g3_init_val("project_years", 0) |>
    identity() -> params
nll <- model_fn(params) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_equal(
    as.vector(r$proj_rwalk_Mrw__nll),
    as.vector(dnorm(c("1990-01" = 0, diff(r$proj_rwalk_Mrw__var)), mean = params$Mrw_mean, sd = params$Mrw_sd)),
    tolerance = 1e-7 ), "r$proj_rwalk_Mrw__nll: dnorm of __var")
ok(ut_cmp_equal(
    as.vector(r$proj_dlnorm_Mdn__nll),
    as.vector(-dnorm(log(r$proj_dlnorm_Mdn__var), params$Mdn_mean - exp(2 * params$Mdn_sd)/2, exp(params$Mdn_sd), log = TRUE)),
    tolerance = 1e-7 ), "r$proj_dlnorm_Mdn__nll: dnorm of __var")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

ok_group("project_years=20") ##################################################

attr(model_fn, 'parameter_template') |>
    g3_init_val("Mrw_mean", runif(1, 0.001, 0.1)) |>
    g3_init_val("Mdn_mean", runif(1, 0.001, 0.1)) |>
    g3_init_val("Mrw.#.#", rnorm(5 * 2, 0.2, 0.001)) |>
    g3_init_val("Mdn.#.#", rnorm(5 * 2, 0.5, 0.001)) |>
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
    ok(ut_cmp_equal(signif(mean(head(r$proj_dlnorm_Mdn__var, 5*2)), 1), 0.5), "mean(r$proj_dlnorm_Mdn__var): Same as param input, for first 10")

    ok(ut_cmp_equal(
        mean(tail(r$proj_dlnorm_Mdn__var, -5*2)),
        params$Mdn_mean,
        tolerance = 1e4), "mean(r$proj_dlnorm_Mdn__var): Projected values have a mean ~matching Mdn_mean")
    ok(sd(tail(r$proj_dlnorm_Mdn__var, -5*2)) > 0, "sd(r$proj_dlnorm_Mdn__var): sd greater than 0 (values not equal)")
    ok(ut_cmp_equal(
        mean(diff(c(0, tail(r$proj_rwalk_Mrw__var, -5*2)))),
        params$Mrw_mean,
        tolerance = 1e4), "mean(r$proj_rwalk_Mrw__var): Projected values have a mean delta ~matching Mrw_mean")
    ok(sd(tail(r$proj_rwalk_Mrw__var, -5*2)) > 0, "sd(r$proj_rwalk_Mrw__var): sd greater than 0 (values not equal)")
}
