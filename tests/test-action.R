library(unittest)

library(gadget3)

ok_group("step_id", {
    step_id <- gadget3:::step_id
    stock_a <- g3_stock('stock_aaa', seq(10, 35, 5))
    stock_b <- g3_stock('stock_bbb', seq(10, 35, 5))

    ok(ut_cmp_identical(step_id("camel"), "camel"), "String: camel")
    ok(ut_cmp_error({
        step_id(list(4))
    }, "list\\(4\\)"), "General lists not allowed")

    ok(ut_cmp_identical(step_id(0), "000"), "Padding numbers: 0")
    ok(ut_cmp_identical(step_id(99), "099"), "Padding numbers: 99")
    ok(ut_cmp_identical(step_id(100), "100"), "Padding numbers: 100")

    ok(ut_cmp_identical(step_id(stock_a), "stock_aaa"), "Stock name: stock_a")
    ok(ut_cmp_identical(step_id(stock_b), "stock_bbb"), "Stock name: stock_b")

    ok(ut_cmp_identical(step_id(0, 10, 200), "000:010:200"), "Multiple numbers: 0, 10, 200")
    ok(ut_cmp_identical(step_id(0, stock_a, "camel"), "000:stock_aaa:camel"), "All the things")
})

ok_group("stock_step:stock_reshape", {
    source <- g3_stock('source', seq(10, 40, 10))
    source__num <- gadget3:::stock_instance(source)
    source__wgt <- gadget3:::stock_instance(source)
    dest_even <- g3_stock('dest_even', seq(10, 40, 10))  # Input == output
    dest_even__num <- gadget3:::stock_instance(dest_even)
    dest_even__wgt <- gadget3:::stock_instance(dest_even)
    dest_combine <- g3_stock('dest_combine', seq(10, 40, 40))  # All input combined
    dest_combine__num <- gadget3:::stock_instance(dest_combine)
    dest_combine__wgt <- gadget3:::stock_instance(dest_combine)
    dest_2group <- g3_stock('dest_2group', seq(10, 40, 20))  # 2 groups
    dest_2group__num <- gadget3:::stock_instance(dest_2group)
    dest_2group__wgt <- gadget3:::stock_instance(dest_2group)
    dest_wider <- g3_stock('dest_wider', seq(0, 90, 10))  # Wider top and bottom
    dest_wider__num <- gadget3:::stock_instance(dest_wider)
    dest_wider__wgt <- gadget3:::stock_instance(dest_wider)

    cur_time <- 0L  # Initialconditions needs to know what the time is
    actions <- list(
        g3a_initialconditions(source, ~g3_param_vector("source_num"), ~g3_param_vector("source_wgt")),

        list('900:dest_even' = gadget3:::stock_step(~stock_iterate(dest_even, stock_intersect(source, {
            dest_even__num[dest_even__iter] <- stock_reshape(dest_even, source__num[source__iter])
            g3_report(dest_even__num)
        })))),

        list('900:dest_combine' = gadget3:::stock_step(~stock_iterate(dest_combine, stock_intersect(source, {
            dest_combine__num[dest_combine__iter] <- stock_reshape(dest_combine, source__num[source__iter])
            g3_report(dest_combine__num)
        })))),

        list('900:dest_2group' = gadget3:::stock_step(~stock_iterate(dest_2group, stock_intersect(source, {
            dest_2group__num[dest_2group__iter] <- stock_reshape(dest_2group, source__num[source__iter])
            g3_report(dest_2group__num)
        })))),

        list('900:dest_wider' = gadget3:::stock_step(~stock_iterate(dest_wider, stock_intersect(source, {
            dest_wider__num[dest_wider__iter] <- stock_reshape(dest_wider, source__num[source__iter])
            g3_report(dest_wider__num)
        })))),

        list('999' = ~{
            return(g3_param('x'))
        }))

    # Compile model
    params <- list(
        source_num = c(11, 22, 33, 44),
        source_wgt = c(11, 22, 33, 44),
        x = 1.0)
    model_fn <- g3_to_r(actions)
    # model_fn <- edit(model_fn)
    model_fn(params)

    ok(ut_cmp_identical(
        grep("lgmatrix", ls(environment(model_fn)$model_data), value = TRUE, fixed = TRUE),
        c("source_dest_2group_lgmatrix",
            "source_dest_combine_lgmatrix",
            "source_dest_wider_lgmatrix")), "Generated matrices for mismatched stocks, not dest_even")

    # str(as.list(environment(model_fn)$model_report))
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$dest_even__num),
        c(11, 22, 33, 44)), "dest_even__num")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$dest_combine__num),
        c(11 + 22 + 33 + 44)), "dest_combine__num")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$dest_2group__num),
        c(11 + 22, 33 + 44)), "dest_2group__num")
    ok(ut_cmp_equal(
        as.vector(environment(model_fn)$model_report$dest_wider__num),
        c(0, 11, 22, 33, 44, 0, 0, 0, 0, 0)), "dest_wider__num")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_cpp <- g3_to_tmb(actions)
        # model_cpp <- edit(model_cpp)
        model_tmb <- g3_tmb_adfun(model_cpp, params)
        model_tmb_report <- model_tmb$report()
        for (n in ls(environment(model_fn)$model_report)) {
            ok(ut_cmp_equal(
                model_tmb_report[[n]],
                as.vector(environment(model_fn)$model_report[[n]]),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
})
