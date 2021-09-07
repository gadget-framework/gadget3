library(unittest)

library(gadget3)

areas <- list(a=1, b=2, c=3, d=4)
# NB: stock doesn't live in b, shouldn't try to migrate there
stock_acd <- (g3_stock('stock_acd', seq(10, 40, 10))
    |> g3s_age(3, 7)
    |> g3s_livesonareas(areas[c('a', 'c', 'd')]))

ok_group("g3a_migrate", {
    actions <- list(
        g3a_time(start_year = 2000, end_year = 2004, steps = c(3,3,3,3)),
        g3a_initialconditions(stock_acd, ~area * 100 + stock_acd__minlen, ~0),
        g3a_migrate(
            stock_acd,
            # TODO: Can we name our areas here?
            ~if (area == 1 && dest_area == 4) g3_param("migrate_spring") else 0,
            run_f = ~cur_step == 2),
        g3a_migrate(
            stock_acd,
            ~if (dest_area == 1) g3_param("migrate_winter") else 0,
            run_f = ~cur_step == 4),
        g3a_report_stock(g3s_clone(stock_acd, 'report_acd') |> gadget3:::g3s_modeltime(), stock_acd, ~stock_ss(input_stock__num)),
        list())
    params <- list(
        migrate_spring = sqrt(0.4),  # NB: Defeat x^2 normalisation
        migrate_winter = sqrt(0.6),
        x=1.0)
    model_fn <- g3_to_r(actions)
    # model_fn <- edit(model_fn)
    result <- model_fn(params)

    # Compile model
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_cpp <- g3_to_tmb(actions, trace = FALSE)
        # model_cpp <- edit(model_cpp)
        model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
    } else {
        writeLines("# skip: not compiling TMB model")
    }

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
    model_nums <- attr(result, 'report_acd__num')[length = 'len10', age = 'age3',,]
    expected_nums <- c(a = 110, c = 310, d = 410)
    for (t in 1:attr(result, 'cur_time')) {
        if ((t-1) %% 4 == 1) {
            # Spring migration, apply it ourselves
            expected_nums['d'] <- expected_nums['d'] + expected_nums['a'] * 0.4
            expected_nums['a'] <- expected_nums['a'] - expected_nums['a'] * 0.4
        }
        if ((t-1) %% 4 == 3) {
            # Autumn migration, apply it ourselves
            expected_nums['a'] <- expected_nums['a'] + expected_nums['c'] * 0.6
            expected_nums['a'] <- expected_nums['a'] + expected_nums['d'] * 0.6
            expected_nums['c'] <- expected_nums['c'] - expected_nums['c'] * 0.6
            expected_nums['d'] <- expected_nums['d'] - expected_nums['d'] * 0.6
        }
        ok(ut_cmp_equal(model_nums[,t], expected_nums), paste0("Model numbers matched expected, t = ", t))
    }
})
