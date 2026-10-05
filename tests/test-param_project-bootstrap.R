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

rec_params <- function (model_cpp, project_years = 100, rec = rnorm(5, 1e5, 5000)) {
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

ok_group("Default: Sample individual years from whole model period") ###########

m <- rec_model(g3_param_project_bootstrap())
params.in <- rec_params(m$cpp)
nll <- m$fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)
lvar <- as.vector(r$proj_bootstrap_stst_rec__lvar)

ok(ut_cmp_equal(
    head(lvar, 5),
    log(params.in[paste0("stst.rec.", 1990:1994), "value"] |> unlist() |> unname()) ), "proj_bootstrap_stst_rec__lvar: Non-projected values from parameters")
ok(length(lvar) == 105, "proj_bootstrap_stst_rec__lvar: Projected 100 years")
ok(all(tail(lvar, -5) %in% head(lvar, 5)), "proj_bootstrap_stst_rec__lvar: Projected values all sampled from non-projected values")
ok(all(head(lvar, 5) %in% tail(lvar, -5)), "proj_bootstrap_stst_rec__lvar: All non-projected values used at some point")
ok(ut_cmp_equal(
    as.vector( g3_array_agg(r$detail_stst_imm__spawnednum, c("year"), step = 1, age = 0) ),
    exp(lvar),
    end = NULL), "r$detail_stst_imm__spawnednum: projection variable used for recruitment")
ok(ut_cmp_equal(nll, 0), "nll: 0, no likelihood by default")

ok_group("Single year pool") ##################################################

m <- rec_model(g3_param_project_bootstrap(
    from_year_f = g3_parameterized("rec.bs.from", value = 1992),
    to_year_f = g3_parameterized("rec.bs.to", value = 1992) ))
params.in <- rec_params(m$cpp)
nll <- m$fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)
lvar <- as.vector(r$proj_bootstrap_stst_rec__lvar)

ok(ut_cmp_equal(
    tail(lvar, -5),
    rep(lvar[[3]], 100) ), "proj_bootstrap_stst_rec__lvar: All projected values are 1992")

# NB: Output is deterministic, so we can compare R & TMB
gadget3:::ut_tmb_r_compare2(m$fn, m$cpp, params.in)

ok_group("Block bootstrap") ###################################################

m <- rec_model(g3_param_project_bootstrap(block_size_f = 3L))
params.in <- rec_params(m$cpp)
nll <- m$fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)
lvar <- as.vector(r$proj_bootstrap_stst_rec__lvar)

# Position in pool of each projected value
pool_i <- match(tail(lvar, -5), head(lvar, 5)) - 1
ok(all(!is.na(pool_i)), "proj_bootstrap_stst_rec__lvar: Projected values all sampled from non-projected values")
ok(all(
    # Within a block (i.e. not at the start of a block), we move to the next value
    (head(pool_i, -1) + 1 == tail(pool_i, -1))[seq_len(99) %% 3 != 0]
), "proj_bootstrap_stst_rec__lvar: Blocks of 3 consecutive years")
ok(all(pool_i[seq(1, 100, by = 3)] %in% 0:2), "proj_bootstrap_stst_rec__lvar: Blocks start early enough to fit in pool, no wrapping")

ok_group("Block bigger than pool") ############################################

m <- rec_model(g3_param_project_bootstrap(block_size_f = 10L))
params.in <- rec_params(m$cpp, project_years = 20)
nll <- m$fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)
lvar <- as.vector(r$proj_bootstrap_stst_rec__lvar)

ok(ut_cmp_equal(
    tail(lvar, -5),
    rep(head(lvar, 5), 4) ), "proj_bootstrap_stst_rec__lvar: Block truncated to pool size, so whole pool repeated")

# NB: Output is deterministic, so we can compare R & TMB
gadget3:::ut_tmb_r_compare2(m$fn, m$cpp, params.in)

ok_group("Retro years aren't sampled") ########################################

m <- rec_model(g3_param_project_bootstrap())
params.in <- rec_params(m$cpp) |> g3_init_val("retro_years", 2)
nll <- m$fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)
lvar <- as.vector(r$proj_bootstrap_stst_rec__lvar)

ok(length(lvar) == 103, "proj_bootstrap_stst_rec__lvar: 3 non-projected, 100 projected years")
ok(all(tail(lvar, -3) %in% head(lvar, 3)), "proj_bootstrap_stst_rec__lvar: Projected values only sampled from non-retro years")

ok_group("by_step = TRUE: Whole years copied") ################################

actions <- list(
    g3a_time(1990, 1994, c(3, 3, 6)),
    g3_param_project("M", g3_param_project_bootstrap(), random = FALSE) )
model_fn <- g3_to_r(c(actions, list(
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    NULL )))
params.in <- attr(model_fn, 'parameter_template') |>
    g3_init_val("M.#.#", runif(15, 10, 20)) |>
    g3_init_val("project_years", 20)
r <- attributes(model_fn(params.in))
lvar <- matrix(as.vector(r$proj_bootstrap_M__lvar), nrow = 3)  # i.e. one column per year

ok(ncol(lvar) == 25, "proj_bootstrap_M__lvar: 25 years of 3 steps")
ok(all(apply(lvar[, 6:25], 2, function (proj_y) any(apply(lvar[, 1:5], 2, function (y) identical(y, proj_y))))),
    "proj_bootstrap_M__lvar: Each projected year is a copy of a non-projected year")

ok_group("R & TMB use the same random numbers") ###############################

m <- rec_model(g3_param_project_bootstrap(block_size_f = 2L))
params.in <- rec_params(m$cpp)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    tmb_fn <- g3_tmb_fn(m$cpp)

    set.seed(4321) ; r_lvar <- as.vector(attr(m$fn(params.in), "proj_bootstrap_stst_rec__lvar"))
    set.seed(4321) ; tmb_lvar <- as.vector(tmb_fn(params.in)$proj_bootstrap_stst_rec__lvar)
    ok(ut_cmp_equal(r_lvar, tmb_lvar), "proj_bootstrap_stst_rec__lvar: R & TMB match with the same seed")
} else {
    writeLines("# skip: not running TMB tests")
}
