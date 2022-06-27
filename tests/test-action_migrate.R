library(unittest)

library(gadget3)

areas <- list(a=1, b=2, c=3, d=4)
# NB: stock doesn't live in b, shouldn't try to migrate there
stock_acd <- (g3_stock('stock_acd', seq(10, 40, 10))
    |> g3s_age(3, 7)
    |> g3s_livesonareas(areas[c('a', 'c', 'd')]))

ok_group("g3a_migrate_normalize", {
    mn <- function (migratematrix, area_idx, row_total = 1) {
        gadget3:::f_eval(g3a_migrate_normalize(row_total), list(
            stock__migratematrix = migratematrix,
            stock__area_idx = area_idx))
    }

    ok(ut_cmp_equal(
        mn(array(c(sqrt(0.2), 0), dim = c(3,3)), 1),
        c(0.8, 0, 0.2)), "Correct row selected, output balanced by adjusting stationary individuals")
    ok(ut_cmp_equal(
        mn(array(c(sqrt(0.2), 0), dim = c(3,3)), 2),
        c(0, 1, 0)), "Attempts to set proportion of stationary individuals ignored")
    ok(ut_cmp_equal(
        mn(array(c(sqrt(0.2), 0, 0), dim = c(3,3)), 3),
        c(0.2, 0, 0.8)), "Output balanced by adjusting stationary individuals")

    ok(ut_cmp_equal(
        mn(array(c(sqrt(0.2), 0), dim = c(3,3)), 1, row_total = 10),
        c(0.98, 0, 0.02)), "Correct row selected, output balanced by adjusting stationary individuals")
})

ok_group("g3a_migrate", {
    actions <- list(
        g3a_time(start_year = 2000, end_year = 2004, step_lengths = c(3,3,3,3), project_years = 0),
        g3a_initialconditions(stock_acd,
            ~area * 100 + stock_acd__minlen,
            ~area * 10 + stock_acd__minlen),
        g3a_migrate(
            stock_acd,
            # TODO: Can we name our areas here?
            # 0.6324555 == sqrt(0.4)
            ~if (area == 4 && dest_area == 1) g3_param("migrate_spring", value = 0.6324555320336759) else 0,
            run_f = ~cur_step == 2),
        g3a_migrate(
            stock_acd,
            ~if (area == 1 && dest_area == 3) g3_param("migrate_winter", value = 0.7745966692414834)
              else if (area == 3 && dest_area == 4) g3_param("migrate_winter", value = 0.7745966692414834)
              else 0,
            run_f = ~cur_step == 4),
        g3a_report_stock(g3s_clone(stock_acd, 'report_acd') |> gadget3:::g3s_modeltime(), stock_acd, ~stock_ss(input_stock__num)),
        g3a_report_stock(g3s_clone(stock_acd, 'report_acd') |> gadget3:::g3s_modeltime(), stock_acd, ~stock_ss(input_stock__wgt)),
        list())

    # Compile model
    model_fn <- g3_to_r(actions)
    # model_fn <- edit(model_fn)
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_cpp <- g3_to_tmb(actions, trace = FALSE)
        # model_cpp <- edit(model_cpp)
        model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
    } else {
        writeLines("# skip: not compiling TMB model")
    }

    params <- attr(model_fn, 'parameter_template')
    result <- model_fn(params)

    # Make sure the model produces identical output in TMB and R
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }

    for (a in paste0('age', 4:7)) {
        ok(ut_cmp_equal(
            attr(result, 'report_acd__num')[, age = 'age3',,],
            attr(result, 'report_acd__num')[, age = a,,]), "Age ranges match, we're not selecting by age")
    }

    # Check one age/length pair to simplify matters
    model_nums <- attr(result, 'report_acd__num')[length = '10:20', age = 'age3',,]
    model_wgts <- attr(result, 'report_acd__wgt')[length = '10:20', age = 'age3',,]
    expected_nums <- model_nums[,1]
    expected_wgts <- model_wgts[,1]
    ratio_add_vec <- function (orig_vec, orig_amount, new_vec, new_amount) (orig_vec * orig_amount + new_vec * new_amount)/(orig_amount + new_amount)
    for (t in 1:attr(result, 'cur_time')) {
        if ((t-1) %% 4 == 1) {
            # Spring migration, apply it ourselves
            expected_wgts['a'] <- ratio_add_vec(
                expected_wgts['a'], expected_nums['a'],
                expected_wgts['d'], expected_nums['d'] * 0.4)
            expected_nums['a'] <- expected_nums['a'] + expected_nums['d'] * 0.4
            expected_nums['d'] <- expected_nums['d'] - expected_nums['d'] * 0.4
        }
        if ((t-1) %% 4 == 3) {
            # Autumn migration, apply it ourselves
            # NB: We shouldn't be migrating anything direct a -> d
            # c -> d
            expected_wgts['d'] <- ratio_add_vec(
                expected_wgts['d'], expected_nums['d'],
                expected_wgts['c'], expected_nums['c'] * 0.6)
            expected_nums['d'] <- expected_nums['d'] + expected_nums['c'] * 0.6
            expected_nums['c'] <- expected_nums['c'] - expected_nums['c'] * 0.6
            # a -> c
            expected_wgts['c'] <- ratio_add_vec(
                expected_wgts['c'], expected_nums['c'],
                expected_wgts['a'], expected_nums['a'] * 0.6)
            expected_nums['c'] <- expected_nums['c'] + expected_nums['a'] * 0.6
            expected_nums['a'] <- expected_nums['a'] - expected_nums['a'] * 0.6
        }
        ok(ut_cmp_equal(model_nums[,t], expected_nums), paste0("Model numbers matched expected, t = ", t))
        ok(ut_cmp_equal(model_wgts[,t], expected_wgts), paste0("Model weights matched expected, t = ", t))
    }
})
