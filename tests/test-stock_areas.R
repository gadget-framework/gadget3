library(magrittr)
library(unittest)

library(gadget3)

areas <- list(a=1, b=2, c=3, d=4)
stock_a <- g3_stock('stock_a', c(10)) %>% g3s_livesonareas(areas[c('a')])
stock_ac <- g3_stock('stock_ac', c(10)) %>% g3s_livesonareas(unname(areas[c('a', 'c')]))  # NB: Remove names so we generate defaults
stock_bcd <- g3_stock('stock_bcd', c(10)) %>% g3s_livesonareas(areas[c('b', 'c', 'd')])
stock_aggregated <- g3_stock('stock_aggregated', c(10)) %>% g3s_areagroup(list(areas[c('b', 'c')], areas[c('d')]))
    
cur_time <- 0L  # Initialconditions needs to know what the time is
stock_sum_a_ac <- 0.0
stock_sum_ac_a <- 0.0
stock_sum_ac_bcd <- 0.0
actions <- list(
    g3a_initialconditions(stock_a, ~area * 100 + stock_a__minlen, ~0),
    g3a_initialconditions(stock_ac, ~area * 1000 + stock_ac__minlen, ~0),
    g3a_initialconditions(stock_bcd, ~area * 10000 + stock_bcd__minlen, ~0),
    g3a_initialconditions(stock_aggregated, ~area * 1 + stock_bcd__minlen, ~0),
    list(
        '5' = gadget3:::stock_step(~{
            comment("stock_sum_a_ac")
            stock_iterate(stock_a, stock_intersect(stock_ac, {
                stock_sum_a_ac <- stock_sum_a_ac + sum(stock_ss(stock_a__num)) + sum(stock_ss(stock_ac__num))
            }))
            g3_report(stock_sum_a_ac)

            comment("stock_sum_ac_a")
            stock_iterate(stock_ac, stock_intersect(stock_a, {
                stock_sum_ac_a <- stock_sum_ac_a + sum(stock_ss(stock_ac__num)) + sum(stock_ss(stock_a__num))
            }))
            g3_report(stock_sum_ac_a)

            comment("stock_sum_ac_bcd")
            stock_iterate(stock_ac, stock_intersect(stock_bcd, {
                stock_sum_ac_bcd <- stock_sum_ac_bcd + sum(stock_ss(stock_ac__num)) + sum(stock_ss(stock_bcd__num))
            }))
            g3_report(stock_sum_ac_bcd)

            comment("stock_aggregated stock_a")
            stock_iterate(stock_a, stock_intersect(stock_aggregated, {
                stock_ss(stock_aggregated__num) <-
                    stock_ss(stock_aggregated__num) +
                    stock_ss(stock_a__num)
            }))

            comment("stock_aggregated stock_ac")
            stock_iterate(stock_ac, stock_intersect(stock_aggregated, {
                stock_ss(stock_aggregated__num) <-
                    stock_ss(stock_aggregated__num) +
                    stock_ss(stock_ac__num)
            }))

            comment("stock_aggregated stock_bcd")
            stock_iterate(stock_bcd, stock_intersect(stock_aggregated, {
                stock_ss(stock_aggregated__num) <-
                    stock_ss(stock_aggregated__num) +
                    stock_ss(stock_bcd__num)
            }))
        }),
        '999' = ~{
            g3_report("stock_a__num")
            g3_report("stock_ac__num")
            g3_report("stock_bcd__num")
            g3_report("stock_aggregated__num")
            return(g3_param('x'))
        }))
params <- list(x=1.0)
model_fn <- g3_to_r(actions)
# model_fn <- edit(model_fn)
result <- model_fn(params)

# We iterated over the stock and populated using the area variable
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__num,
    array(
        c(110),
        dim = c(length = 1, area = 1),
        dimnames = list(length = "len10", area = "a"))), "stock_a__num populated, used names from areas lookup")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_ac__num,
    array(
        c(1010, 3010),
        dim = c(length = 1, area = 2),
        dimnames = list(length = "len10", area = c("area1","area3")))), "stock_ac__num populated, generated default names")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_bcd__num,
    array(
        c(20010, 30010, 40010),
        dim = c(length = 1, area = 3),
        dimnames = list(length = "len10", area = c("b", "c", "d")))), "stock_bcd__num populated, used names from areas lookup")

ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_aggregated__num,
    array(c(
        # NB: Areas b & c --> init + stock_ac + stock_bcd
        (12) + (3010) + (20010 + 30010),
        # NB: Area d --> init + stock_bcd
        14 + 40010), dim = c(length = 1, area = 2), dimnames = list(length = "len10", area = c("area2", "area4")))), "stock_aggregated__num combination of all stocks")

# Intersection works with any combination of single-area stock and multi-area stock
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_sum_a_ac,
    110 + 1010), "stock_sum_a_ac: Only includes area a")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_sum_ac_a,
    110 + 1010), "stock_sum_ac_a: Only includes area a")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_sum_ac_bcd,
    3010 + 30010), "stock_sum_ac_bcd: Intersection is area c")

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
    model_tmb_report <- model_tmb$report()
    for (n in ls(environment(model_fn)$model_report)) {
        ok(ut_cmp_equal(
            as.vector(model_tmb_report[[n]]),
            as.vector(environment(model_fn)$model_report[[n]]),
            tolerance = 1e-5), paste("TMB and R match", n))
    }
} else {
    writeLines("# skip: not running TMB tests")
}
