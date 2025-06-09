library(unittest)

library(gadget3)

prey_a <- g3_stock('prey_a', seq(1, 10)) |> g3s_age(1,3)
pred_a <- g3_stock('pred_a', seq(50, 80, by = 10)) |> g3s_age(0, 10)
fl <- g3_fleet('fl')

actions <- list(
    g3a_time(2000, 2005, step_lengths = c(6,6), project_years = 0),
    g3a_age(prey_a),
    g3a_age(pred_a),
    g3a_initialconditions(prey_a, ~1e5 * prey_a__midlen, ~100),
    g3a_initialconditions(pred_a, ~1e-2 * pred_a__midlen, ~1000),

    g3a_growmature(prey_a, impl_f = gadget3::g3a_grow_impl_bbinom(
        maxlengthgroupgrowth = 2L) ),
    g3a_growmature(pred_a, impl_f = gadget3::g3a_grow_impl_bbinom(
        delta_wgt_f = g3a_grow_weight_weightjones(),
        maxlengthgroupgrowth = 2L) ),

    g3a_predate(
        pred_a,
        list(prey_a),
        suitabilities = 1,
        catchability_f = g3a_predate_catchability_predator() ),

    g3a_predate(
        fl,
        list(pred_a),
        suitabilities = 1,
        catchability_f = g3a_predate_catchability_totalfleet(g3_parameterized("fl_landings", value = 0, optimise = FALSE)) ),

    g3l_understocking(list(prey_a, pred_a)) )
full_actions <- c(actions, list(
    g3a_report_detail(actions),
    g3a_report_history(actions, "__num$|__wgt$", out_prefix="dend_"),  # NB: Late reporting
    g3a_report_history(actions, "quota_", out_prefix = NULL),
    # g3a_trace_var(actions, check_positive = TRUE, var_re = c("__wgt$"), on_error = "stop"),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_cpp, "parameter_template") |>
  g3_init_val("*.Linf", 10, spread = 0.2) |>
  g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
  # NB: Fish pred_a to extinction
  g3_init_val("fl_landings", 1e5) |>

  g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
  g3_init_val("*.wbeta", 3, optimise = FALSE) |>
  g3_init_val("*.weightjones.q0", 0.7991596) |>
  g3_init_val("*.weightjones.q1", 0.05000146) |>
  g3_init_val("*.weightjones.q2", 2.845905) |>
  g3_init_val("*.weightjones.q3", 0.75) |>
  g3_init_val("*.weightjones.q4", 0) |>
  g3_init_val("*.weightjones.q5", 0) |> 
  identity() -> params.in

nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_equal(g3_array_agg(r$dstart_pred_a__num, c("time")), c(
    `2000-01` = 30.8, `2000-02` = 1.54, `2001-01` = 0.077, `2001-02` = 0.00385,
    `2002-01` = 0.0001925, `2002-02` = 9.625e-06, `2003-01` = 4.813e-07,
    `2003-02` = 2.41e-08, `2004-01` = 1.2e-09, `2004-02` = 1e-10,
    `2005-01` = 0, `2005-02` = 0,
    NULL )), "dstart_pred_a__num: Fished to extinction, remains finite")
ok(ut_cmp_equal(g3_array_agg(r$dstart_pred_a__wgt, c("time")), c(
    `2000-01` = 44000, `2000-02` = 43999.9999999995, `2001-01` = 39982.1098368574,
    `2001-02` = 4704.465940942, `2002-01` = 22.6095830583, `2002-02` = 0.0085358984,
    `2003-01` = 0.0001959993, `2003-02` = 1.5617e-05, `2004-01` = 4.261e-07,
    `2004-02` = 3.91e-08, `2005-01` = 9e-10, `2005-02` = 1e-09,
    NULL )), "dstart_pred_a__wgt: Fished to extinction, remains finite")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in, g3_test_tmb = 2)
