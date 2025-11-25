library(magrittr)
library(unittest)

library(gadget3)

actions <- list(
    g3a_time(2000, 2005, step_lengths = c(6,6), project_years = 0),
    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood(),
    list(
        "001:ut" = gadget3:::g3_step(g3_formula(
            quote( tvout[[1]] <- x ),
            tvout = array(0L),
            x = g3_timevariable('gelda', list(
                init = g3_formula( 2L + 2L ),
                "2002" = g3_formula( y * 4L, y = 10L ),
                "2003-02" = g3_formula( z * 2L, z = 9L ),
                "2999" = -99L)))),
        "999:ut" = g3_formula( nll <- nll + g3_param('tmb_bodge', value = 1.0) )))
actions <- c(actions, list(g3a_report_history(actions, var_re = "^tvout$")))

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
model_cpp <- g3_to_tmb(actions, trace = FALSE)

ok_group("gadget3:::g3_timevariable", {
    params <- attr(model_fn, 'parameter_template')

    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_identical(r$hist_tvout[1,], c(
        "2000-01" = 4L, "2000-02" = 4L, "2001-01" = 4L, "2001-02" = 4L,
        "2002-01" = 40L, "2002-02" = 40L, "2003-01" = 40L,
        "2003-02" = 18L, "2004-01" = 18L, "2004-02" = 18L, "2005-01" = 18L, "2005-02" = 18L,
        NULL)), "hist_tvout: Started with init, altered at relevant steps")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})
