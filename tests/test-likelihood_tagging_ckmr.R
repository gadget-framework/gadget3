if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

ok_group("g3l_tagging_ckmr", {
    # Length groups chosen so VonB (Linf=100, K=0.15, t0=0) places fish within range:
    #   age 5 -> ~42 cm, age 10 -> ~62 cm, offspring ages 0-4 -> 0-35 cm
    parent_st <- g3_stock("parent", seq(20, 100, 10)) |> g3s_age(5, 10)
    offspring_st <- g3_stock("offspring", seq(5, 40, 5)) |> g3s_age(0, 4)

    obs_data <- data.frame(
        year          = c(2003L, 2003L, 2003L, 2003L),
        parent_age    = c(   8L,    9L,    5L,    8L),
        offspring_age = c(   3L,    2L,    6L,   10L),
        mo_pairs      = c(   2L,    0L,    1L,    0L),
        n_comparisons = c(1000L,  500L,  100L,  100L))
    # row 1: birth_parent_age=5, birth_year=2000 → valid, mo_pairs=2 → contributes
    # row 2: mo_pairs=0 → skipped by guard
    # row 3: birth_parent_age = 5-6 = -1 → birth_parent_age_idx < g3_idx(1) → skipped
    # row 4: birth_year = 2003-10 = 1993 < start_year=2000 → modelhist__offspring_idx < g3_idx(1) → skipped

    actions <- list(
        g3a_time(2000, 2004, c(6, 6), project_years = 0),
        g3a_initialconditions_normalcv(parent_st),
        g3a_initialconditions_normalcv(offspring_st),
        g3a_age(parent_st),
        g3a_age(offspring_st),
        g3a_spawn(
            parent_st,
            recruitment_f = g3a_spawn_recruitment_fecundity(
                p0 = 1, p1 = 0, p2 = 0, p3 = 1, p4 = 0),
            output_stocks = list(offspring_st),
            run_step = 1),
        g3l_tagging_ckmr(
            "ckmr",
            obs_data,
            parent_stocks = list(parent_st),
            offspring_stocks = list(offspring_st)),
        gadget3:::g3l_test_dummy_likelihood())

    full_actions <- c(actions, list(
        g3a_report_history(actions, var_re = "^ckmrmodel__(num|spawned)$")))

    model_fn <- g3_to_r(full_actions)
    model_cpp <- g3_to_tmb(full_actions)

    params <- attr(model_fn, 'parameter_template') |>
        g3_init_val("parent.Linf", 100) |>
        g3_init_val("parent.K", 0.15) |>
        g3_init_val("parent.t0", 0) |>
        g3_init_val("parent.lencv", 0.1) |>
        g3_init_val("parent.walpha", 1e-3) |>
        g3_init_val("parent.wbeta", 3) |>
        g3_init_val("parent.init.scalar", 1000) |>
        g3_init_val("parent.init.#", 1) |>
        g3_init_val("parent.M.#", 0) |>
        g3_init_val("offspring.Linf", 40) |>
        g3_init_val("offspring.K", 0.3) |>
        g3_init_val("offspring.t0", 0) |>
        g3_init_val("offspring.lencv", 0.1) |>
        g3_init_val("offspring.walpha", 1e-3) |>
        g3_init_val("offspring.wbeta", 3) |>
        g3_init_val("offspring.init.scalar", 0) |>
        g3_init_val("offspring.init.#", 1) |>
        g3_init_val("offspring.M.#", 0) |>
        g3_init_val("init.F", 0) |>
        g3_init_val("recage", 5) |>
        identity()

    nll <- model_fn(params)
    r <- attributes(nll)
    nll <- as.vector(nll)

    # Derive expected NLL from model history.
    # hist_ckmrmodel__num / hist_ckmrmodel__spawned are [age, year, time] arrays;
    # the final time step "2004-02" has all years populated.
    num_ay     <- r$hist_ckmrmodel__num[,, "2004-02"]     # [age, year]
    spawned_ay <- r$hist_ckmrmodel__spawned[,, "2004-02"] # [age, year]

    # Row 1: birth_parent_age=5, birth_year=2000
    # modelhist minage = min(parent minage=5, offspring minage=0) = 0
    # age index (1-based R): birth_parent_age - minage + 1 = 5 - 0 + 1 = 6
    # year index (1-based R): birth_year - start_year + 1 = 2000 - 2000 + 1 = 1
    fecundity_row1  <- spawned_ay["age5", "2000"] / num_ay["age5", "2000"]
    TRO_row1        <- sum(spawned_ay[, "2000"])
    pr_pop_bya_row1 <- fecundity_row1 / TRO_row1

    ok(ut_cmp_equal(
        nll,
        -dpois(2L, 1000L * pr_pop_bya_row1, log = TRUE)),
        "nll: matches Poisson likelihood derived from model history (only row 1 is valid and has mo_pairs > 0)")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})
