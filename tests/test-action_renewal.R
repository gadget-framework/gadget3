library(magrittr)
library(unittest)

library(gadget3)

areas <- g3_areas('a', 'b', 'c', 'd')
stock_a <- g3_stock('stock_a', seq(10, 10, 5)) %>% g3s_livesonareas(areas[c('a')])
stock_ac <- g3_stock('stock_ac', seq(10, 10, 5)) %>% g3s_livesonareas(areas[c('a', 'c')])
    
cur_time <- 0L  # Initialconditions needs to know what the time is
actions <- g3_collate(
    g3a_initialconditions(stock_a, ~area * 100 + stock_a__minlen, ~stock_a__minlen + 100),
    g3a_initialconditions(stock_ac, ~area * 1000 + stock_ac__minlen, ~stock_a__minlen + 200),
    list(
        '999' = ~{
            g3_report(stock_a__num)
            g3_report(stock_ac__num)
            g3_report(stock_a__wgt)
            g3_report(stock_ac__wgt)
            return(g3_param('x'))
        }))
params <- list(x=1.0)
model_fn <- g3_compile_r(actions)
# model_fn <- edit(model_fn)
result <- model_fn(params)

# Populated numbers
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__num,
    array(c(110), dim = c(1,1))), "stock_a__num populated")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_ac__num,
    array(c(1010, 3010), dim = c(1,2))), "stock_ac__num populated")

# Populated mean weights
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__wgt,
    array(c(110), dim = c(1,1))), "stock_a__wgt populated")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_ac__wgt,
    array(c(210, 210), dim = c(1,2))), "stock_ac__wgt populated")

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_precompile_tmb(actions)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
    model_tmb_report <- model_tmb$report()
    for (n in ls(environment(model_fn)$model_report)) {
        ok(ut_cmp_equal(
            model_tmb_report[[n]],
            environment(model_fn)$model_report[[n]],
            tolerance = 1e-5), paste("TMB and R match", n))
    }
} else {
    writeLines("# skip: not running TMB tests")
}
