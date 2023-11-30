library(magrittr)
library(unittest)

library(gadget3)

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
