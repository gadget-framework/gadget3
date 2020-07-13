library(magrittr)
library(unittest)

library(gadget3)

ok_group("g3s_livesonareas:iterate", for (allow_break in 1) {
    areas <- g3_areas('a', 'b', 'c', 'd')
    stock_a <- g3_stock('stock_a', 10, 15, 5) %>% g3s_livesonareas(areas[c('a')])
    stock_ac <- g3_stock('stock_ac', 10, 15, 5) %>% g3s_livesonareas(areas[c('a', 'c')])
    stock_bcd <- g3_stock('stock_bcd', 10, 15, 5) %>% g3s_livesonareas(areas[c('b', 'c', 'd')])
    
    cur_time <- 0L  # Initialconditions needs to know what the time is
    stock_sum_a_ac <- 0.0
    stock_sum_ac_a <- 0.0
    stock_sum_ac_bcd <- 0.0
    actions <- g3_collate(
        g3a_initialconditions(stock_a, ~area * 100 + stock_a__minlen, ~0),
        g3a_initialconditions(stock_ac, ~area * 1000 + stock_ac__minlen, ~0),
        g3a_initialconditions(stock_bcd, ~area * 10000 + stock_bcd__minlen, ~0),
        list(
            '5' = gadget3:::stock_step(~{
                stock_iterate(stock_a, stock_intersect(stock_ac, {
                    stock_sum_a_ac <- sum(stock_a__num[stock_a__iter]) + sum(stock_ac__num[stock_ac__iter])
                }))
                g3_report(stock_sum_a_ac)
                stock_iterate(stock_ac, stock_intersect(stock_a, {
                    stock_sum_ac_a <- sum(stock_ac__num[stock_ac__iter]) + sum(stock_a__num[stock_a__iter])
                }))
                g3_report(stock_sum_ac_a)
                stock_iterate(stock_ac, stock_intersect(stock_bcd, {
                    stock_sum_ac_bcd <- sum(stock_ac__num[stock_ac__iter]) + sum(stock_bcd__num[stock_bcd__iter])
                }))
                g3_report(stock_sum_ac_bcd)
            }),
            '999' = ~{
                g3_report("stock_a__num")
                g3_report("stock_ac__num")
                g3_report("stock_bcd__num")
                return(g3_param('x'))
            }))
    params <- list(x=1.0)
    model_fn <- g3_compile_r(actions)
    # model_fn <- edit(model_fn)
    result <- model_fn(params)

    # We iterated over the stock and populated using the area variable
    ok(ut_cmp_identical(
        environment(model_fn)$model_report$stock_a__num,
        array(c(110), dim = c(1,1))), "stock_a__num populated")
    ok(ut_cmp_identical(
        environment(model_fn)$model_report$stock_ac__num,
        array(c(1010, 3010), dim = c(1,2))), "stock_ac__num populated")
    ok(ut_cmp_identical(
        environment(model_fn)$model_report$stock_bcd__num,
        array(c(20010, 30010, 40010), dim = c(1,3))), "stock_bcd__num populated")

    ok(ut_cmp_identical(
        environment(model_fn)$model_report$stock_sum_a_ac,
        110 + 1010), "stock_sum_a_ac: Only includes area a")
    ok(ut_cmp_identical(
        environment(model_fn)$model_report$stock_sum_a_ac,
        110 + 1010), "stock_sum_ac_a: Only includes area a")
    ok(ut_cmp_identical(
        environment(model_fn)$model_report$stock_sum_ac_bcd,
        3010 + 30010), "stock_sum_ac_bcd: Intersection is area c")

    if (!nzchar(Sys.getenv('G3_TEST_TMB'))) { writeLines("# skip: not running TMB tests") ; break }
    model_cpp <- g3_precompile_tmb(actions)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
    model_tmb_report <- model_tmb$report()
    for (n in ls(environment(model_fn)$model_report)) {
        ok(ut_cmp_equal(
            model_tmb$report()[[n]],
            environment(model_fn)$model_report[[n]],
            tolerance = 1e-5), paste("TMB and R match", n))
    }
})