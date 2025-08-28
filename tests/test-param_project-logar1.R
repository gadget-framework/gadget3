if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st_imm <- g3_stock(c("stst", maturity = "imm"), c(10, 20, 30)) |> g3s_age(0, 5)
st_mat <- g3_stock(c("stst", maturity = "mat"), c(10, 20, 30)) |> g3s_age(3, 15)
stocks_st <- list(st_imm, st_mat)
fl <- g3_fleet(c("fl", "surv"))

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
                g3_param_project_logar1(),
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
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

ok_group("No noise, fixed loglevel") ##########################################

attr(model_fn, 'parameter_template') |>
    g3_init_val("stst.rec.#", rnorm(5, 1e5, 500)) |>
    g3_init_val("stst.rec.proj.logar1.lstddev", -1e6) |>  # i.e. no noise
    g3_init_val("stst.rec.proj.logar1.logphi", 0.8) |>
    g3_init_val("stst.rec.proj.logar1.loglevel", log(round(runif(1, 10, 20)))) |>
    g3_init_val("stst_mat.spawn.blim", 1e2) |>  # blim too low to trigger
    g3_init_val("proj_logar1_stst_weight.proj_logar1_rec_weight", 0) |>  # i.e. disable nll output (it'll be Inf)

    g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
    g3_init_val("*.Linf", max(g3_stock_def(st_imm, "midlen")), spread = 0.2) |>
    g3_init_val("*.t0", g3_stock_def(st_imm, "minage") - 0.8, spread = 2) |>
    g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
    g3_init_val("*.wbeta", 3, optimise = FALSE) |>

    g3_init_val("project_years", 100) |>
    identity() -> params.in
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_equal(
    as.vector(tail(r$proj_logar1_stst_rec__var, 10)),
    rep(exp(params.in$stst.rec.proj.logar1.loglevel), 10),
    end = NULL ), "proj_logar1_stst_rec__var: Settles to loglevel in projection, regardless of initial value")
ok(ut_cmp_equal(
    as.vector( g3_array_agg(r$detail_stst_imm__spawnednum, c("year"), step = 1, age = 0) ),
    as.vector( r$proj_logar1_stst_rec__var ),
    end = NULL), "r$detail_stst_imm__spawnednum: projection variable used for recruitment")

ok(ut_cmp_equal(nll, 0), "nll: 0, as we disabled it with weight")
ok(all(!is.finite(r$proj_logar1_stst_rec__nll)), "r$proj_logar1_stst_rec__nll: lstddev 0, so nll infinite")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)

ok_group("With noise, no projection") #########################################

old_seed <- .Random.seed
set.seed(1234)  # Fix seed so we always choose the same stst.rec.#
attr(model_fn, 'parameter_template') |>
    g3_init_val("stst.rec.#", rnorm(5, 1e5, 500)) |>
    g3_init_val("stst.rec.proj.logar1.lstddev", -8) |>
    g3_init_val("stst.rec.proj.logar1.logphi", 0.8) |>
    g3_init_val("stst_mat.spawn.blim", 1e2) |>  # blim too low to trigger

    g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
    g3_init_val("*.Linf", max(g3_stock_def(st_imm, "midlen")), spread = 0.2) |>
    g3_init_val("*.t0", g3_stock_def(st_imm, "minage") - 0.8, spread = 2) |>
    g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
    g3_init_val("*.wbeta", 3, optimise = FALSE) |>

    g3_init_val("project_years", 0) |>
    identity() -> params.in
.Random.seed <- old_seed
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_equal(nll, sum(r$proj_logar1_stst_rec__nll)), "nll: Consistent with r$proj_logar1_stst_rec__nll (sums to same values)")
ok(ut_cmp_equal(
    as.vector(r$proj_logar1_stst_rec__nll),
    as.vector(c(0, -dnorm(
        tail(log(r$proj_logar1_stst_rec__var), -1) -
        0.8 * head(log(r$proj_logar1_stst_rec__var), -1) -
        0.2 * 0,  # level not accounted for as it's negative
        0 - exp(2*-exp(params.in[["stst.rec.proj.logar1.lstddev"]])) / 2,
        exp(params.in[["stst.rec.proj.logar1.lstddev"]]),
        1 ))),
    tolerance = 1e7), "r$proj_logar1_stst_rec__nll: Consistent with proj_logar1_stst_rec__var, level not accounted for")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)

ok_group("With noise") ########################################################

attr(model_fn, 'parameter_template') |>
    g3_init_val("stst.rec.#", rnorm(5, 1e5, 500)) |>
    g3_init_val("stst.rec.proj.logar1.lstddev", -8) |>
    g3_init_val("stst.rec.proj.logar1.logphi", 0.8) |>
    g3_init_val("stst.rec.proj.logar1.loglevel", log(1e5)) |>
    g3_init_val("stst_mat.spawn.blim", 1e2) |>  # blim too low to trigger

    g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
    g3_init_val("*.Linf", max(g3_stock_def(st_imm, "midlen")), spread = 0.2) |>
    g3_init_val("*.t0", g3_stock_def(st_imm, "minage") - 0.8, spread = 2) |>
    g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
    g3_init_val("*.wbeta", 3, optimise = FALSE) |>

    g3_init_val("project_years", 100) |>
    identity() -> params.in
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_equal(
    as.vector( g3_array_agg(r$detail_stst_imm__spawnednum, c("year"), step = 1, age = 0) ),
    as.vector( r$proj_logar1_stst_rec__var ),
    end = NULL), "r$detail_stst_imm__spawnednum: projection variable used for recruitment")

ok(all(r$proj_logar1_stst_rec__var > 1e4), "r$proj_logar1_stst_rec__var: Noise not high enough for value to drop below 1e4")

ok(ut_cmp_equal(nll, sum(r$proj_logar1_stst_rec__nll)), "nll: Consistent with r$proj_logar1_stst_rec__nll (sums to same values)")
ok(ut_cmp_equal(
    as.vector(r$proj_logar1_stst_rec__nll),
    as.vector(c(0, -dnorm(
        tail(log(r$proj_logar1_stst_rec__var), -1) -
        0.8 * head(log(r$proj_logar1_stst_rec__var), -1) -
        0.2 * params.in[["stst.rec.proj.logar1.loglevel"]],
        0 - exp(2*-exp(params.in[["stst.rec.proj.logar1.lstddev"]])) / 2,
        exp(params.in[["stst.rec.proj.logar1.lstddev"]]),
        1 ))),
    tolerance = 1e7), "r$proj_logar1_stst_rec__nll: Consistent with proj_logar1_stst_rec__var")

# plot(r$proj_logar1_stst_rec__var)

ok_group("lastx mode", local({ ################################################

    st_imm <- g3_stock(c("stst", maturity = "imm"), c(10, 20, 30)) |> g3s_age(0, 5)
    st_mat <- g3_stock(c("stst", maturity = "mat"), c(10, 20, 30)) |> g3s_age(3, 15)
    stocks_st <- list(st_imm, st_mat)
    fl <- g3_fleet(c("fl", "surv"))

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
                    g3_param_project_logar1(lastx_f = g3_parameterized("lastx", value = 2)),
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
    model_fn <- g3_to_r(full_actions)
    model_cpp <- g3_to_tmb(full_actions)

    attr(model_fn, 'parameter_template') |>
        g3_init_val("stst.rec.#", rnorm(5, 1e5, 500)) |>
        g3_init_val("stst.rec.proj.logar1.lstddev", -1e6) |>  # i.e. no noise
        g3_init_val("stst.rec.proj.logar1.logphi", 0) |>
        g3_init_val("stst_mat.spawn.blim", 1e2) |>  # blim too low to trigger
        g3_init_val("lastx", floor(runif(1, 1, 4))) |>
        g3_init_val("proj_logar1_stst_weight.proj_logar1_rec_weight", 0) |>  # i.e. disable nll output (it'll be Inf)

        g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
        g3_init_val("*.Linf", max(g3_stock_def(st_imm, "midlen")), spread = 0.2) |>
        g3_init_val("*.t0", g3_stock_def(st_imm, "minage") - 0.8, spread = 2) |>
        g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
        g3_init_val("*.wbeta", 3, optimise = FALSE) |>

        g3_init_val("project_years", 100) |>
        identity() -> params.in
    nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

    ok(!("stst.rec.proj.logar1.loglevel" %in% names(params.in)), "loglevel parameter disabled")

    ok(ut_cmp_equal(nll, 0), "nll: 0, as we disabled it with weight")
    ok(ut_cmp_equal(
        as.vector( r$proj_logar1_stst_rec__var[length(r$proj_logar1_stst_rec__var)] ),
        as.vector( exp(mean(log(r$proj_logar1_stst_rec__var[as.character(seq(1994 - params.in$lastx + 1, 1994))]))) ),
        tolerance = 1e-7 ), paste0("proj_logar1_stst_rec__var: Settled to mean of lastx (", params.in$lastx, ")"))

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)
}))
