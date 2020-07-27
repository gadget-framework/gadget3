library(magrittr)
library(unittest)

library(gadget3)

ok(ut_cmp_error(
    g3_stock('stock_a', c()),
    "at least 1"), "Can't create stock with 0 length groups")

stock_a <- g3_stock('stock_a', seq(10, 10, 5))
stock_b <- g3_stock('stock_b', seq(50, 54, 1))
stock_wonky <- g3_stock('stock_wonky', c(0, 10, 100, 1000))

actions <- g3_collate(
    list(
        '999' = gadget3:::stock_step(~{
            # NB: stock_rename also includes the stock environment, which is why we need it
            stock_rename(stock_a, {
                g3_report(stock_a__minlen)
                g3_report(stock_a__meanlen)
                g3_report(stock_a__dl)
            })

            stock_rename(stock_b, {
                g3_report(stock_b__minlen)
                g3_report(stock_b__meanlen)
                g3_report(stock_b__dl)
            })

            stock_rename(stock_wonky, {
                g3_report(stock_wonky__minlen)
                g3_report(stock_wonky__meanlen)
                g3_report(stock_wonky__dl)
            })

            return(g3_param('x'))
        })))
params <- list(x=1.0)
model_fn <- g3_compile_r(actions)
# model_fn <- edit(model_fn)
result <- model_fn(params)

# We populated min/mean/dl
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__minlen,
    array(c(10), dim = c(1))), "stock_a__minlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__meanlen,
    array(c(10.5), dim = c(1))), "stock_a__meanlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__dl,
    c(1)), "stock_a__dl")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_b__minlen,
    array(c(50, 51, 52, 53, 54), dim = c(5))), "stock_b__minlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_b__meanlen,
    array(c(50.5, 51.5, 52.5, 53.5, 54.5), dim = c(5))), "stock_b__meanlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_b__dl,
    c(1,1,1,1,1)), "stock_b__dl")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_wonky__minlen,
    array(c(0, 10, 100, 1000), dim = c(4))), "stock_wonky__minlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_wonky__meanlen,
    array(c(5.000, 55.000, 550.000, 1166.667), dim = c(4))), "stock_wonky__meanlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_wonky__dl,
    c(10.0000, 90.0000, 900.0000, 333.3333)), "stock_wonky__dl")

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
