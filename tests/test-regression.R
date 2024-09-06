if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

actions <- list(
    g3a_time(1990, 1990),
    "5:regression_linear" = g3_formula({
        regression_linear_out <- regression_linear(linear_N, linear_I, g3_param("linear_intercept", value = 1), g3_param("linear_slope", value = 1))
        REPORT(regression_linear_out)
    }, linear_N = runif(10), linear_I = runif(10), regression_linear_out = c(nll = 0.0, intercept = 0.0, slope = 0.0), regression_linear = gadget3:::regression_linear),
    # NB: Dummy parameter so model will compile in TMB
    ~{nll <- nll + g3_param("x", value = 0)} )
full_actions <- c(actions, list(
    g3a_report_history(actions, var_re = "__num$|__wgt$"),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

ok_group("Fixed intercept/slope") ####################
attr(model_fn, 'parameter_template') |>
    g3_init_val("linear_intercept", 1) |>
    g3_init_val("linear_slope", 1) |>
    identity() -> params
linear_N <- runif(10, 1e3, 1e4)
linear_I <- runif(10, 1e3, 1e4)
attr(model_cpp, 'model_data')$linear_N <- environment(model_fn)$linear_N <- linear_N
attr(model_cpp, 'model_data')$linear_I <- environment(model_fn)$linear_I <- linear_I
r <- attributes(model_fn(params))

ok(ut_cmp_equal(r$regression_linear_out[["intercept"]], 1), "regression_linear_out$intercept: Fixed")
ok(ut_cmp_equal(r$regression_linear_out[["slope"]], 1), "regression_linear_out$slope: Fixed")
ok(ut_cmp_equal(r$regression_linear_out[["nll"]], sum((
    r$regression_linear_out[["intercept"]] +
    r$regression_linear_out[["slope"]] * linear_N - linear_I)**2),
    end = NULL), "regression_linear_out$nll: Consistent with intercept & slope")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

ok_group("Fixed intercept estimated slope") ####################
attr(model_fn, 'parameter_template') |>
    g3_init_val("linear_intercept", 1) |>
    g3_init_val("linear_slope", NaN) |>
    identity() -> params
linear_N <- runif(10, 1e3, 1e4)
linear_I <- runif(10, 1e3, 1e4)
linear_N[4:5] <- NaN  # Create a hole in N, will ignore values in both N & I
attr(model_cpp, 'model_data')$linear_N <- environment(model_fn)$linear_N <- linear_N
attr(model_cpp, 'model_data')$linear_I <- environment(model_fn)$linear_I <- linear_I
r <- attributes(model_fn(params))

# Omit NaNs for our caculations
linear_N <- linear_N[-(4:5)]
linear_I <- linear_I[-(4:5)]
ok(ut_cmp_equal(r$regression_linear_out[["intercept"]], 1), "regression_linear_out$intercept: Fixed")
ok(ut_cmp_equal(r$regression_linear_out[["slope"]],
    sum((linear_I - mean(linear_I)) * (linear_N - mean(linear_N)))
        / sum((linear_N - mean(linear_N))**2),
    end = NULL), "regression_linear_out$slope: Calculated")
ok(ut_cmp_equal(r$regression_linear_out[["nll"]], sum((
    r$regression_linear_out[["intercept"]] +
    r$regression_linear_out[["slope"]] * linear_N - linear_I)**2),
    end = NULL), "regression_linear_out$nll: Consistent with intercept & slope")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

ok_group("Estimated intercept fixed slope") ####################
attr(model_fn, 'parameter_template') |>
    g3_init_val("linear_intercept", NaN) |>
    g3_init_val("linear_slope", 2) |>
    identity() -> params
linear_N <- runif(10, 1e3, 1e4)
linear_I <- runif(10, 1e3, 1e4)
linear_I[6:8] <- NaN  # Create a hole in N, will ignore values in both N & I
attr(model_cpp, 'model_data')$linear_N <- environment(model_fn)$linear_N <- linear_N
attr(model_cpp, 'model_data')$linear_I <- environment(model_fn)$linear_I <- linear_I
r <- attributes(model_fn(params))

# Omit NaNs for our caculations
linear_N <- linear_N[-(6:8)]
linear_I <- linear_I[-(6:8)]
ok(ut_cmp_equal(
    r$regression_linear_out[["intercept"]],
    mean(linear_I) - r$regression_linear_out[["slope"]] * mean(linear_N),
    end = NULL), "regression_linear_out$intercept: Calculated")
ok(ut_cmp_equal(r$regression_linear_out[["slope"]], 2), "regression_linear_out$slope: Fixed")
ok(ut_cmp_equal(r$regression_linear_out[["nll"]], sum((
    r$regression_linear_out[["intercept"]] +
    r$regression_linear_out[["slope"]] * linear_N - linear_I)**2),
    end = NULL), "regression_linear_out$nll: Consistent with intercept & slope")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
