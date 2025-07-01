library(magrittr)
library(unittest)

library(gadget3)

tags <- c('H1-00', 'H1-01')
tags <- structure(seq_along(tags), names = tags)

prey_a <- g3_stock('prey_a', seq(1, 10)) %>% g3s_tag(tags)
fleet_a <- g3_fleet('fleet_a')

actions <- list(
    g3a_time(2000, ~2000 + g3_param('years', value = 1) - 1, step_lengths = c(6,6), project_years = 0),
    g3a_initialconditions(prey_a, ~10 * prey_a__midlen, ~100 * prey_a__midlen),
    g3a_predate_tagrelease(
        fleet_a,
        list(prey_a),
        suitabilities = list(
            prey_a = 1),
        catchability_f = g3a_predate_catchability_numberfleet(~100),
        # NB: 1000 enough to disable mortality
        mortality_f = g3_suitability_straightline(~g3_param('mort_alpha', value = 1000), ~g3_param('mort_beta', value = 0)),
        output_tag_f = g3_timeareadata('fleet_a_tags', data.frame(
            year = c(2000, 2001),
            tag = tags[c('H1-00', 'H1-01')],
            stringsAsFactors = FALSE), value_field = "tag"),
        run_f = ~cur_step == 2 && cur_year < g3_param('harvest_end', value = 2999)),
    g3a_tag_shedding(
        list(prey_a),
        tagshed_f = log(8),  # i.e. 0.125 will loose their tag
        run_f = ~cur_year >= g3_param('tagshed_start', value = 2999)),
    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood() )
actions <- c(actions, list(
    g3a_report_history(actions, "__cons"),
    g3a_report_history(actions)))

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
model_cpp <- g3_to_tmb(actions, trace = FALSE)

ok_group("tagging without mortality", {
    params <- attr(model_fn, 'parameter_template')
    params$years <- 5
    params$mort_alpha <- 1000  # NB: Enough to disable mortality
    params$mort_beta <- 0
    params$harvest_end <- 2999
    params$tagshed_start <- 2999
    result <- model_fn(params)
    # str(as.list(r), vec.len = 10000)

    for (l in seq_len(dim(attr(result, "hist_prey_a__num"))[['length']])) {
        ok(ut_cmp_equal(
            as.numeric(colSums(attr(result, "hist_prey_a__num")[l,,])),
            rep(l * 10 + 5, dim(attr(result, "hist_prey_a__num"))[['length']]),
            tolerance = 1e-7), paste0("hist_prey_a__num[", l, ",,]: No fish disappear"))
    }

    for (t in seq_len(dim(attr(result, "hist_prey_a__num"))[['time']])) {
        if (t == 1) {
            # Start, not interesting
        } else if (t %% 2 == 0) {
            # Predation step
            nums_caught <- rowSums(attr(result, "hist_prey_a_fleet_a__cons")[,,t] / attr(result, "hist_prey_a__wgt")[,,t])
            if (t == 2) {
                ok(ut_cmp_equal(
                    as.numeric(attr(result, "hist_prey_a__num")[,2,t]),
                    as.numeric(nums_caught)), paste0("hist_prey_a__num[,2,", t, "]: Fish assigned to first tag"))
                ok(ut_cmp_equal(
                    attr(result, "hist_prey_a__wgt")[,1,t],
                    attr(result, "hist_prey_a__wgt")[,2,t]), paste0("hist_prey_a__wgt[,2,", t, "]: Untagged weight copied"))
            } else if (t == 4) {
                ok(ut_cmp_equal(
                    as.numeric(attr(result, "hist_prey_a__num")[,3,t]),
                    as.numeric(nums_caught)), paste0("hist_prey_a__num[,3,", t, "]: Fish assigned to second tag"))
                ok(ut_cmp_equal(
                    attr(result, "hist_prey_a__wgt")[,1,t],
                    attr(result, "hist_prey_a__wgt")[,3,t]), paste0("hist_prey_a__wgt[,3,", t, "]: Untagged weight copied"))
            }
        } else {
            ok(ut_cmp_equal(
                attr(result, "hist_prey_a__num")[,,t],
                attr(result, "hist_prey_a__num")[,,t - 1]), paste0("hist_prey_a__num[,,", t, "]: Nothing happens outside tagrelease cycles"))
        }
    }

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})

ok_group("tagging with mortality", {
    params <- attr(model_fn, 'parameter_template')
    params$years <- 5
    params$mort_alpha <- log(4)  # NB: i.e. 0.25 tagged will die
    params$mort_beta <- 0
    params$harvest_end <- 2999
    params$tagshed_start <- 2999
    result <- model_fn(params)
    # str(as.list(r), vec.len = 10000)

    for (t in seq_len(dim(attr(result, "hist_prey_a__num"))[['time']])) {
        if (t == 1) {
            # Start, not interesting
        } else if (t %% 2 == 0) {
            # Predation step
            nums_caught <- rowSums(attr(result, "hist_prey_a_fleet_a__cons")[,,t] / attr(result, "hist_prey_a__wgt")[,,t])
            if (t == 2) {
                ok(ut_cmp_equal(
                    as.numeric(attr(result, "hist_prey_a__num")[,2,t]),
                    as.numeric(nums_caught * 0.75),
                    tolerance = 1e-8), paste0("hist_prey_a__num[,2,", t, "]: Fish assigned to first tag, after mortality"))
                ok(ut_cmp_equal(
                    attr(result, "hist_prey_a__wgt")[,1,t],
                    attr(result, "hist_prey_a__wgt")[,2,t],
                    tolerance = 1e-6), paste0("hist_prey_a__wgt[,2,", t, "]: Untagged weight copied"))
            } else if (t == 4) {
                ok(ut_cmp_equal(
                    as.numeric(attr(result, "hist_prey_a__num")[,3,t]),
                    as.numeric(nums_caught * 0.75),
                    tolerance = 1e-8), paste0("hist_prey_a__num[,3,", t, "]: Fish assigned to second tag, after mortality"))
                ok(ut_cmp_equal(
                    attr(result, "hist_prey_a__wgt")[,1,t],
                    attr(result, "hist_prey_a__wgt")[,3,t],
                    tolerance = 1e-6), paste0("hist_prey_a__wgt[,3,", t, "]: Untagged weight copied"))
            }
        } else {
            ok(ut_cmp_equal(
                attr(result, "hist_prey_a__num")[,,t],
                attr(result, "hist_prey_a__num")[,,t - 1]), paste0("hist_prey_a__num[,,", t, "]: Nothing happens outside tagrelease cycles"))
        }
    }

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})

ok_group("tag shedding", {
    params <- attr(model_fn, 'parameter_template')
    params$years <- 5
    params$mort_alpha <- 1000  # NB: Enough to disable mortality
    params$mort_beta <- 0
    params$harvest_end <- 2001
    params$tagshed_start <- 2003
    result <- model_fn(params)
    # str(as.list(r), vec.len = 10000)

    for (l in seq_len(dim(attr(result, "hist_prey_a__num"))[['length']])) {
        ok(ut_cmp_equal(
            as.numeric(colSums(attr(result, "hist_prey_a__num")[l,,])),
            rep(l * 10 + 5, dim(attr(result, "hist_prey_a__num"))[['length']]),
            tolerance = 1e-7), paste0("hist_prey_a__num[", l, ",,]: No fish disappear"))
    }

    for (t in seq_len(dim(attr(result, "hist_prey_a__num"))[['time']])) {
        if (t == 1) {
            # Start, not interesting
        } else if (t >= 7) {
            # Tag shedding enabled
            ok(ut_cmp_equal(
                as.numeric(attr(result, "hist_prey_a__num")[,'untagged',t]),
                as.numeric(attr(result, "hist_prey_a__num")[,'untagged',t-1]
                    + 0.125 * attr(result, "hist_prey_a__num")[,'H1-00', t-1]
                    + 0.125 * attr(result, "hist_prey_a__num")[,'H1-01', t-1]),
                tolerance = 1e-7), paste0("hist_prey_a__num[,'untagged',", t, "]: Fish lose tag and return to untagged"))
        } else if (t %% 2 == 0) {
            # Predation step
            nums_caught <- rowSums(attr(result, "hist_prey_a_fleet_a__cons")[,,t] / attr(result, "hist_prey_a__wgt")[,,t])
            if (t == 2) {
                ok(ut_cmp_equal(
                    as.numeric(attr(result, "hist_prey_a__num")[,2,t]),
                    as.numeric(nums_caught)), paste0("hist_prey_a__num[,2,", t, "]: Fish assigned to first tag"))
                ok(ut_cmp_equal(
                    attr(result, "hist_prey_a__wgt")[,1,t],
                    attr(result, "hist_prey_a__wgt")[,2,t]), paste0("hist_prey_a__wgt[,2,", t, "]: Untagged weight copied"))
            }
        } else {
            ok(ut_cmp_equal(
                attr(result, "hist_prey_a__num")[,,t],
                attr(result, "hist_prey_a__num")[,,t - 1]), paste0("hist_prey_a__num[,,", t, "]: Nothing happens outside tagrelease cycles"))
        }
    }

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})
