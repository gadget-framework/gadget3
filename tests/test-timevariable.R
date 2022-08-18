library(magrittr)
library(unittest)

library(gadget3)

actions <- list(
    g3a_time(2000, 2005, step_lengths = c(6,6), project_years = 0),
    list(
        "001" = gadget3:::g3_step(g3_formula(
            quote( tvout <- x ),
            tvout = array(0L),
            x = g3_timevariable('gelda', list(
                init = g3_formula( 2L + 2L ),
                "2002" = g3_formula( y * 4L, y = 10L ),
                "2003-02" = g3_formula( z * 2L, z = 9L ),
                "2999" = NaN)))),
        "999" = g3_formula( nll <- nll + g3_param('tmb_bodge', value = 1.0) )))
actions <- c(actions, list(g3a_report_history(actions, var_re = "^tvout$")))

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    # writeLines(TMB::gdbsource(g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"), output_script = TRUE)))
    model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

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

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})
