#!/usr/bin/Rscript --vanilla
library(magrittr)
library(unittest)

library(gadget3)

# Create time/area definitions ################################################

area_names <- c("all" = 1)

actions_time <- list(
  g3a_time(1990L, 2000L),
  NULL)

actions <- actions_time

# Create stock definition for fish ############################################
fish <- g3_stock("fish", seq(50, 100, 10)) |>
  g3s_livesonareas(area_names["all"]) |>
  g3s_age(1L, 10L)

actions_fish <- list(
  g3a_growmature(fish, g3a_grow_impl_bbinom(
    maxlengthgroupgrowth = 5L)),
  g3a_naturalmortality(fish),
  g3a_initialconditions_normalcv(fish),
  g3a_renewal_normalcv(fish),
  g3a_age(fish),
  NULL)

actions_likelihood_fish <- list(
  g3l_understocking(list(fish), nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_fish, actions_likelihood_fish)

# Create fleet definition for comm ###########################################
comm_landings <- expand.grid(
    year = 1990:2000,
    area = 'all')
comm_landings$weight <- runif(nrow(comm_landings), 10000, 90000)
comm_ldist <- expand.grid(
    year = 1990:2000,
    area = 'all',
    length = cut(seq(50, 90, 10), c(seq(50, 90, 10), Inf), right = FALSE))
comm_ldist$weight <- runif(nrow(comm_ldist), 10000, 90000)

comm <- g3_fleet("comm") |> g3s_livesonareas(area_names["all"])
actions_comm <- list(
  g3a_predate_fleet(
    comm,
    list(fish),
    suitabilities = g3_suitability_exponentiall50(
      g3_parameterized('comm.alpha', by_stock = TRUE),
      g3_parameterized('comm.l50', by_stock = TRUE)),
    catchability_f = g3a_predate_catchability_numberfleet(
      g3_timeareadata("comm_landings", comm_landings, "weight", areas = area_names))),
  NULL)
actions_likelihood_comm <- list(
  g3l_catchdistribution(
    "comm_ldist",
    comm_ldist,
    fleets = list(comm),
    stocks = list(fish),
    function_f = g3l_distribution_sumofsquares(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_comm, actions_likelihood_comm)

# Create survey definition for acoustic ###################################
acoustic_dist <- expand.grid(
    year = 1990:2000,
    area = 'all')
acoustic_dist$weight <- runif(nrow(acoustic_dist), 10000, 90000)

actions_acoustic <- list()
actions_likelihood_acoustic <- list(
  g3l_abundancedistribution(
    "acoustic_dist",
    acoustic_dist,
    stocks = list(fish),
    function_f = g3l_distribution_surveyindices_log(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_acoustic, actions_likelihood_acoustic)

# Reporting & debugging actions for entire model ##############################

actions_reporting <- list(
  g3a_report_detail(actions),
  # g3a_report_history(actions),
  # g3experiments:::g3a_trace_nan(actions,  var_re = c("fish__num$", "fish__wgt$")),
  NULL)

actions <- c(actions, actions_reporting)

###############################################################################

# Build and write out R-based model
model_fn <- g3_to_r(actions)
# edit(model_fn)
attr(model_fn, 'actions') <- NULL  # Actions output is a bit too verbose for us
writeLines(deparse(model_fn, width.cutoff = 500L), con = 'inttest/codegeneration-defaults/model.R')

# Build and write out TMB-based model
model_cpp <- g3_to_tmb(actions)
writeLines(model_cpp, con = 'inttest/codegeneration-defaults/model.cpp')

# Write out default paramter_template
params <- attr(model_cpp, 'parameter_template')
capture.output(print(params), file = 'inttest/codegeneration-defaults/model.tmbparam')

r <- attributes(model_fn(params$value))
ok(all(is.finite(r$detail_fish__num)), "detail_fish__num: finite")
ok(all(is.finite(r$detail_fish__renewalnum)), "detail_fish__renewalnum: finite")
ok(all(is.finite(r$detail_fish__wgt)), "detail_fish__wgt: finite")
# TODO: detail_fish__predby_comm still NA
ok(all(is.finite(r$detail_fish__suit_comm)), "detail_fish__suit_comm: finite")


# TODO: detail_fish__predby_comm needs fixing before this will pass
#if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
#    print(system.time({model_tmb <- g3_tmb_adfun(model_cpp, params)}))
#    print(system.time(tmb_result <- model_tmb$fn()))
#
#    gadget3:::ut_tmb_r_compare(model_fn, model_tmb, params)
#}
