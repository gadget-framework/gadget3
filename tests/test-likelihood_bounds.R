library(magrittr)
library(unittest)

library(gadget3)

############# Tests for parameter_template mode

actions <- list()
area_names <- g3_areas(c("IXa"))

actions_time <- list(
  g3a_time(
    1979L, 2023L,
    step_lengths = c(3L, 3L, 3L, 3L)),
  NULL)
actions <- c(actions, actions_time)

anch <- g3_stock("anch", seq(3L, 22L, 0.5)) |>
  g3s_livesonareas(area_names["IXa"]) |>
  g3s_age(0L, 3L)
actions_anch <- list(
  g3a_growmature(anch, g3a_grow_impl_bbinom(
    maxlengthgroupgrowth = 38L)),
  g3a_naturalmortality(anch),
  g3a_initialconditions_normalparam(anch),
  g3a_renewal_normalparam(anch,
    run_step = NULL),
  g3a_age(anch),
  NULL)
actions <- c(actions, actions_anch)


model_code <- g3_to_tmb(actions)
params.in <- attr(model_code, "parameter_template")

params.in[, 'optimise'] <- FALSE
params.in[, 'lower'] <- NA
params.in[, 'upper'] <- NA

ok(ut_cmp_identical(
    g3l_bounds_penalty(params.in),
    list()), "No penalty functions if nothing to optimise")

params.in[, 'optimise'] <- TRUE
ok(ut_cmp_identical(
    g3l_bounds_penalty(params.in),
    list()), "No penalty functions if no lower/upper bounds")

params.in['anch.rec.1990', 'lower'] <- 4
params.in['anch.rec.1991', 'lower'] <- 40
ok(ut_cmp_identical(
    g3l_bounds_penalty(params.in),
    list()), "No penalty functions if no upper bounds")

params.in['anch.rec.1990', 'upper'] <- 9
params.in['anch.rec.1991', 'upper'] <- 99
ok(ut_cmp_identical(lapply(g3l_bounds_penalty(params.in), deparse), list(
    "010:g3l_bounds_penalty  :anch.rec.1990       " = c(
        "~{",
        "    debug_label(\"g3l_bounds_penalty for anch.rec.1990\")",
        "    if (cur_time == 0) {", "        nll <- nll + 1 * ((logspace_add(1e+06 * (g3_param(\"anch.rec.1990\") - ",
        "            9)/(9 - 4), 0) + logspace_add(1e+06 * (4 - g3_param(\"anch.rec.1990\"))/(9 - ",
        "            4), 0))^2)",
        "    }",
        "}"),
    "010:g3l_bounds_penalty  :anch.rec.1991       " = c("~{",
        "    debug_label(\"g3l_bounds_penalty for anch.rec.1991\")",
        "    if (cur_time == 0) {", "        nll <- nll + 1 * ((logspace_add(1e+06 * (g3_param(\"anch.rec.1991\") - ",
        "            99)/(99 - 40), 0) + logspace_add(1e+06 * (40 - g3_param(\"anch.rec.1991\"))/(99 - ",
        "            40), 0))^2)", 
        "    }",
    "}"))), "Generated code for 2 parameters")

############# Tests for actions mode

actions <- list(g3a_time(2000, 2000), list("555" = g3_formula({
    # TMB will fail mysteriously if at least one parameter isn't optimisable
    nll <- nll + g3_param('p0', value = 0, optimise = TRUE)
    nll <- nll + g3_param('pa')
    nll <- nll + g3_param('pb')
    nll <- nll + g3_param('pc')
})))
model_code <- g3_to_tmb(c(actions, list( g3l_bounds_penalty(actions) )))
fn <- g3_to_r(c(actions, list( g3l_bounds_penalty(actions) )))

ok(ut_cmp_equal(
    as.numeric(fn(c(list(pa=1, pb=2, pc=3), attr(fn, 'parameter_template')))),
    1 + 2 + 3), "nll: R version ignores g3l_bounds_penalty()")

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    attr(model_code, 'parameter_template') |>
        g3_init_val('pa', 100.45) |>
        g3_init_val('pb', 200) |>
        g3_init_val('pc', 300.5342) |>
        identity() -> params.in
    obj.fn <- g3_tmb_adfun(model_code, params.in)
    ok(ut_cmp_equal(
        obj.fn$fn(),
        sum(unlist(params.in$value)) ), "nll: TMB version with no bounds enabled")

    suppressWarnings(attr(model_code, 'parameter_template') |>
        g3_init_val('pa', 100, lower = 50, upper = 150) |>
        g3_init_val('pb', 200) |>
        g3_init_val('pc', 300) |>
        identity() -> params.in)
    obj.fn <- g3_tmb_adfun(model_code, params.in)
    ok(ut_cmp_equal(
        obj.fn$fn(),
        sum(unlist(params.in$value)) ), "nll: TMB version, within bounds")

    suppressWarnings(attr(model_code, 'parameter_template') |>
        g3_init_val('pa', 100, lower = 10, upper = 50) |>
        g3_init_val('pb', 200) |>
        g3_init_val('pc', 300) |>
        identity() -> params.in)
    obj.fn <- g3_tmb_adfun(model_code, params.in)
    ok(ut_cmp_equal(
        obj.fn$fn(),
        1.6e12,
        tolerance=1e1), "nll: TMB version, above bounds")

    suppressWarnings(attr(model_code, 'parameter_template') |>
        g3_init_val('pa', 10, lower = 20, upper = 50) |>
        g3_init_val('pb', 200) |>
        g3_init_val('pc', 300) |>
        identity() -> params.in)
    obj.fn <- g3_tmb_adfun(model_code, params.in)
    ok(ut_cmp_equal(
        obj.fn$fn(),
        1e+11,
        tolerance=1e1), "nll: TMB version, outside bounds")

    suppressWarnings(attr(model_code, 'parameter_template') |>
        g3_init_val('pa', 10, lower = 20, upper = 50) |>
        g3_init_val('pb', 90, lower = 20, upper = 50) |>
        g3_init_val('pc', 300) |>
        identity() -> params.in)
    obj.fn <- g3_tmb_adfun(model_code, params.in)
    ok(ut_cmp_equal(
        obj.fn$fn(),
        2e12,
        tolerance=1e1), "nll: TMB version, 2 parameters outside bounds")
} else {
    writeLines("# skip: not compiling TMB model")
}
