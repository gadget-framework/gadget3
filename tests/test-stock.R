library(magrittr)
library(unittest)

library(gadget3)

stock_a <- g3_stock('stock_a', 10, 15, 5)
stock_b <- g3_stock('stock_b', 50, 55, 1)

actions <- g3_collate(
    list(
        '999' = gadget3:::stock_step(~{
            # NB: stock_rename also includes the stock environment, which is why we need it
            stock_rename(stock_a, {
                g3_report(stock_a__minlen)
                g3_report(stock_a__meanlen)
                g3_report(stock_a__countlen)
            })

            stock_rename(stock_b, {
                g3_report(stock_b__minlen)
                g3_report(stock_b__meanlen)
                g3_report(stock_b__countlen)
            })

            return(g3_param('x'))
        })))
params <- list(x=1.0)
model_fn <- g3_compile_r(actions)
# model_fn <- edit(model_fn)
result <- model_fn(params)

# We populated min/mean/countlen
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__minlen,
    array(c(10), dim = c(1))), "stock_a__minlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__meanlen,
    array(c(12.5), dim = c(1))), "stock_a__meanlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__countlen,
    as.integer(1)), "stock_a__countlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_b__minlen,
    array(c(50, 51, 52, 53, 54), dim = c(5))), "stock_b__minlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_b__meanlen,
    array(c(50.5, 51.5, 52.5, 53.5, 54.5), dim = c(5))), "stock_b__meanlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_b__countlen,
    as.integer(5)), "stock_b__countlen")

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_precompile_tmb(actions)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
    model_tmb_report <- model_tmb$report()
    for (n in ls(environment(model_fn)$model_report)) {
        ok(ut_cmp_equal(
            model_tmb$report()[[n]],
            # NB: TMB drops the dimensions, so we need to also
            as.vector(environment(model_fn)$model_report[[n]]),
            tolerance = 1e-5), paste("TMB and R match", n))
    }
} else {
    writeLines("# skip: not running TMB tests")
}
