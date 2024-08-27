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
    g3a_naturalmortality(st, g3a_naturalmortality_exp(gadget3:::g3_param_project(
        "Mrw",
        gadget3:::g3_param_project_rwalk(mean_f = 0.001, stddev_f = 0.01) ))),
    g3a_naturalmortality(st, g3a_naturalmortality_exp(gadget3:::g3_param_project(
        "Mdn",
        gadget3:::g3_param_project_dnorm(mean_f = 0.01, stddev_f = 0.01) ))),
    # NB: Dummy parameter so model will compile in TMB
    quote( nll <- nll + g3_param("x", value = 0) ) )
full_actions <- c(actions, list(
    g3a_report_detail(actions),
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_fn, 'parameter_template') |>
    g3_init_val("Mrw.#.#", rnorm(5 * 2, 0.2, 0.005)) |>
    g3_init_val("Mdn.#.#", rnorm(5 * 2, 0.2, 0.005)) |>
    g3_init_val("project_years", 50) |>
    identity() -> params

nll <- model_fn(params) ; r <- attributes(nll) ; nll <- as.vector(nll)

# TODO: Can't work since RNG will produce different results for multiple runs
#gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
