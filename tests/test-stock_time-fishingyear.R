if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

# We can't parameterize year_length / start_step, so generate new model for each test
fishingyear_test <- function (
        start_year = 1998,
        end_year = 2003,
        steps = 4,
        year_length = 1,
        start_step = 1,
        project_years = 0 ) {

    st <- gadget3:::g3s_modeltime_fishingyear(gadget3:::g3_storage("quota"), year_length, start_step)
    actions <- list(
        g3a_time(
            start_year,
            end_year,
            step_lengths = rep(12/steps, steps),
            project_years = g3_parameterized("project_years", value = project_years, optimise = FALSE) ),
        list("005" = gadget3:::g3_step(g3_formula(
            stock_iterate(st, {
                calendar[[1]] <- st__fishingyear_idx - g3_idx(1) + 1L  # NB: Cancel out g3_idx
                calendar[[2]] <- st__fishingyear_step
                calendar[[3]] <- st__fishingyear_revstep
                stock_ss(st__calend) <- cur_year * 1000 + cur_step
            }),
            st__calend = g3_stock_instance(st, -99),
            calendar = as.array(c(idx = NA_integer_, fishingyear_step = NA_integer_, fishingyear_revstep = NA_integer_)),
            st = st ))),
        # NB: Only required for testing
        gadget3:::g3l_test_dummy_likelihood() )
    full_actions <- c(actions, list(
        g3a_report_history(actions, "^calendar"),
        g3a_report_history(actions, "^quota__cal", out_prefix = NULL),
        NULL ))
    return(full_actions)
}

ok_group("year_length = 1, start_step = 1")
full_actions <- fishingyear_test(steps = 2, year_length = 1, start_step = 1)
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)
nll <- model_fn() ; r <- attributes(nll) ; nll <- as.vector(nll)
ok(gadget3:::ut_cmp_df(as.data.frame(r$hist_calendar), '
                    1998-01 1998-02 1999-01 1999-02 2000-01 2000-02 2001-01 2001-02 2002-01 2002-02 2003-01 2003-02
idx                       1       1       2       2       3       3       4       4       5       5       6       6
fishingyear_step          1       2       1       2       1       2       1       2       1       2       1       2
fishingyear_revstep      -2      -1      -2      -1      -2      -1      -2      -1      -2      -1      -2      -1
'), "hist_calendar: 1 fishing year per calendar year")
ok(gadget3:::ut_cmp_df(as.data.frame(r$quota__calend), '
          r$quota__calend
1998:1999         1998002
1999:2000         1999002
2000:2001         2000002
2001:2002         2001002
2002:2003         2002002
2003:2004         2003002
2004:2005             -99
'), "quota_calend: 1 fishing year per calendar year")

###############################################################################

ok_group("end_year = 2001, steps = 4, year_length = 2, start_step = 3")
full_actions <- fishingyear_test(end_year = 2001, steps = 4, year_length = 2, start_step = 3)
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)
nll <- model_fn() ; r <- attributes(nll) ; nll <- as.vector(nll)
ok(gadget3:::ut_cmp_df(as.data.frame(r$hist_calendar), '
                    1998-01 1998-02 1998-03 1998-04 1999-01 1999-02 1999-03 1999-04 2000-01 2000-02 2000-03 2000-04 2001-01 2001-02 2001-03 2001-04
idx                       1       1       2       2       2       2       2       2       2       2       3       3       3       3       3       3
fishingyear_step          1       2       1       2       3       4       5       6       7       8       1       2       3       4       5       6
fishingyear_revstep      -2      -1      -8      -7      -6      -5      -4      -3      -2      -1      -8      -7      -6      -5      -4      -3

'), "hist_calendar: short initial time, then 4 steps per year")
ok(gadget3:::ut_cmp_df(as.data.frame(r$quota__calend), '
          r$quota__calend
1998:1998         1998002
1998:2000         2000002
2000:2002         2001004
2002:2004             -99
'), "quota_calend: Report final step as 2000:2002, even though model stops beforehand")

#model_cpp <- edit(model_cpp)
gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, attr(model_fn, "parameter_template"))
