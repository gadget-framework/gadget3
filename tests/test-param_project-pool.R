if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st_imm <- g3_stock(c("stst", maturity = "imm"), c(10, 20, 30)) |> g3s_age(0, 5)
st_mat <- g3_stock(c("stst", maturity = "mat"), c(10, 20, 30)) |> g3s_age(3, 15)
stocks_st <- list(st_imm, st_mat)

rec_model <- function (project_fs) {
    actions <- list(
        g3a_time(1990, 1994, c(6,6)),
        gadget3:::g3a_initialconditions_manual(st_imm,
            quote( 100 + stock__minlen ),
            quote( 1e4 + 0 * stock__minlen ) ),
        gadget3:::g3a_initialconditions_manual(st_mat,
            quote( 100 + stock__minlen ),
            quote( 1e4 + 0 * stock__minlen ) ),
        g3a_age(st_imm),
        g3a_age(st_mat),

        g3a_spawn(
            st_mat,
            g3a_spawn_recruitment_hockeystick(
                r0 = g3_param_project(
                    "rec",
                    project_fs,
                    random = FALSE,
                    scale = "rec.scalar",
                    by_stock = stocks_st,
                    by_step = FALSE )),
                output_stocks = list(st_imm),
                run_step = 1 ),

        # NB: Only required for testing
        gadget3:::g3l_test_dummy_likelihood() )
    full_actions <- c(actions, list(
        g3a_report_detail(actions),
        g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
        NULL))
    list(fn = g3_to_r(full_actions), cpp = g3_to_tmb(full_actions))
}

rec_params <- function (model_cpp, project_years = 10, rec = rnorm(5, 1e5, 5000)) {
    attr(model_cpp, 'parameter_template') |>
        g3_init_val("stst.rec.#", rec) |>
        g3_init_val("stst_mat.spawn.blim", 1e2) |>  # blim too low to trigger

        g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
        g3_init_val("*.Linf", max(g3_stock_def(st_imm, "midlen")), spread = 0.2) |>
        g3_init_val("*.t0", g3_stock_def(st_imm, "minage") - 0.8, spread = 2) |>
        g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
        g3_init_val("*.wbeta", 3, optimise = FALSE) |>

        g3_init_val("project_years", project_years) |>
        identity()
}

ok_group("dlnorm: No from_year_f / to_year_f, lmean_f parameter still used") ###

m <- rec_model(g3_param_project_dlnorm())
ok("stst.rec.proj.dlnorm.mean" %in% attr(m$cpp, "parameter_template")$switch, "parameter_template: Has stst.rec.proj.dlnorm.mean")

ok_group("dlnorm: Constant mean of a pool of years") ###########################

m <- rec_model(g3_param_project_dlnorm(from_year_f = 1991L, to_year_f = 1993L))
ok(!("stst.rec.proj.dlnorm.mean" %in% attr(m$cpp, "parameter_template")$switch), "parameter_template: No stst.rec.proj.dlnorm.mean, lmean_f not used")
params.in <- rec_params(m$cpp) |> g3_init_val("stst.rec.proj.dlnorm.stddev", 0)
rec <- params.in[paste0("stst.rec.", 1990:1994), "value"] |> unlist() |> unname()
r <- attributes(suppressWarnings(m$fn(params.in)))
lvar <- as.vector(r$proj_dlnorm_stst_rec__lvar)

ok(ut_cmp_equal(head(lvar, 5), log(rec)), "proj_dlnorm_stst_rec__lvar: Non-projected values from parameters")
ok(length(lvar) == 15, "proj_dlnorm_stst_rec__lvar: Projected 10 years")
ok(ut_cmp_equal(exp(tail(lvar, -5)), rep(mean(rec[2:4]), 10)), "proj_dlnorm_stst_rec__lvar: Projected values are the arithmetic mean of 1991..1993")

ok_group("dlnorm: Only from_year_f, to_year_f defaults to end_year") ###########

m <- rec_model(g3_param_project_dlnorm(from_year_f = 1992L))
params.in <- rec_params(m$cpp) |> g3_init_val("stst.rec.proj.dlnorm.stddev", 0)
rec <- params.in[paste0("stst.rec.", 1990:1994), "value"] |> unlist() |> unname()
lvar <- as.vector(attr(suppressWarnings(m$fn(params.in)), "proj_dlnorm_stst_rec__lvar"))
ok(ut_cmp_equal(exp(tail(lvar, -5)), rep(mean(rec[3:5]), 10)), "proj_dlnorm_stst_rec__lvar: Projected values are the arithmetic mean of 1992..1994")

ok_group("dlnorm: Retro years aren't used") ####################################

m <- rec_model(g3_param_project_dlnorm(from_year_f = 1990L, to_year_f = 1994L))
params.in <- rec_params(m$cpp) |> g3_init_val("stst.rec.proj.dlnorm.stddev", 0) |> g3_init_val("retro_years", 2)
rec <- params.in[paste0("stst.rec.", 1990:1994), "value"] |> unlist() |> unname()
lvar <- as.vector(attr(suppressWarnings(m$fn(params.in)), "proj_dlnorm_stst_rec__lvar"))
ok(length(lvar) == 13, "proj_dlnorm_stst_rec__lvar: 3 non-retro years, 10 projected years")
ok(ut_cmp_equal(exp(tail(lvar, -3)), rep(mean(rec[1:3]), 10)), "proj_dlnorm_stst_rec__lvar: Projected values are the arithmetic mean of non-retro years 1990..1992")

ok_group("dlnorm: Noise around pool mean, nll") ################################

m <- rec_model(g3_param_project_dlnorm(from_year_f = 1990L, to_year_f = 1994L))
params.in <- rec_params(m$cpp, project_years = 2000) |> g3_init_val("stst.rec.proj.dlnorm.stddev", 0.2)  # NB: LOG parameter, so the stddev in logspace
rec <- params.in[paste0("stst.rec.", 1990:1994), "value"] |> unlist() |> unname()
r <- attributes(m$fn(params.in))
lvar <- as.vector(r$proj_dlnorm_stst_rec__lvar)
ok(ut_cmp_equal(mean(exp(tail(lvar, -5))), mean(rec), tolerance = 0.05), "proj_dlnorm_stst_rec__lvar: Projected values have the arithmetic mean of 1990..1994")
ok(ut_cmp_equal(sd(tail(lvar, -5)), 0.2, tolerance = 0.1), "proj_dlnorm_stst_rec__lvar: Projected values have a stddev of 0.2 in logspace")
# NB: Projected years 1990..1994 are all existing values, so the nll mean is the same as the projection mean
ok(ut_cmp_equal(
    as.vector(r$proj_dlnorm_stst_rec__nll),
    -dnorm(lvar, log(mean(rec)) - 0.2^2 / 2, 0.2, log = TRUE),
    tolerance = 1e-7), "proj_dlnorm_stst_rec__nll: dnorm of __lvar around log of pool mean")
gadget3:::ut_tmb_r_compare2(m$fn, m$cpp, params.in)

ok_group("dnorm: No from_year_f / to_year_f, mean_f parameter still used") #####

m <- rec_model(g3_param_project_dnorm())
ok("stst.rec.proj.dnorm.mean" %in% attr(m$cpp, "parameter_template")$switch, "parameter_template: Has stst.rec.proj.dnorm.mean")

ok_group("dnorm: Constant mean of a pool of years") ############################

m <- rec_model(g3_param_project_dnorm(from_year_f = 1991L, to_year_f = 1993L))
ok(!("stst.rec.proj.dnorm.mean" %in% attr(m$cpp, "parameter_template")$switch), "parameter_template: No stst.rec.proj.dnorm.mean, mean_f not used")
params.in <- rec_params(m$cpp) |> g3_init_val("stst.rec.proj.dnorm.stddev", 0)
rec <- params.in[paste0("stst.rec.", 1990:1994), "value"] |> unlist() |> unname()
var <- as.vector(attr(suppressWarnings(m$fn(params.in)), "proj_dnorm_stst_rec__var"))
ok(ut_cmp_equal(head(var, 5), rec), "proj_dnorm_stst_rec__var: Non-projected values from parameters")
ok(ut_cmp_equal(tail(var, -5), rep(mean(rec[2:4]), 10)), "proj_dnorm_stst_rec__var: Projected values are the mean of 1991..1993")

ok_group("dnorm: Noise around pool mean, nll") #################################

m <- rec_model(g3_param_project_dnorm(from_year_f = 1990L, to_year_f = 1994L))
params.in <- rec_params(m$cpp) |> g3_init_val("stst.rec.proj.dnorm.stddev", 5000)
rec <- params.in[paste0("stst.rec.", 1990:1994), "value"] |> unlist() |> unname()
r <- attributes(m$fn(params.in))
var <- as.vector(r$proj_dnorm_stst_rec__var)
ok(ut_cmp_equal(
    as.vector(r$proj_dnorm_stst_rec__nll),
    -dnorm(var, mean(rec), 5000, log = TRUE),
    tolerance = 1e-7), "proj_dnorm_stst_rec__nll: dnorm of __var around pool mean")
gadget3:::ut_tmb_r_compare2(m$fn, m$cpp, params.in)

ok_group("by_step = TRUE: Mean over all steps of pool years") ##################

actions <- list(
    g3a_time(1990, 1994, c(3, 3, 6)),
    g3_param_project("M", g3_param_project_dlnorm(from_year_f = 1993L, to_year_f = 1994L), random = FALSE) )
model_fn <- g3_to_r(c(actions, list(
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    NULL )))
params.in <- attr(model_fn, 'parameter_template') |>
    g3_init_val("M.#.#", runif(15, 10, 20)) |>
    g3_init_val("M.proj.dlnorm.stddev", 0) |>
    g3_init_val("project_years", 3)
# NB: Parameters are ordered by step, then year, so select 1993..1994 by name
M <- unlist(params.in[paste0("M.", rep(1993:1994, each = 3), ".", 1:3)]) |> unname()
lvar <- as.vector(attr(suppressWarnings(model_fn(params.in)), "proj_dlnorm_M__lvar"))
ok(length(lvar) == 24, "proj_dlnorm_M__lvar: 8 years of 3 steps")
ok(ut_cmp_equal(exp(tail(lvar, -15)), rep(mean(M), 9)), "proj_dlnorm_M__lvar: Projected values are the arithmetic mean of all steps in 1993..1994")
