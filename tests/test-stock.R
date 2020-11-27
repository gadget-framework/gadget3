library(magrittr)
library(unittest)

library(gadget3)

ok(ut_cmp_error(
    g3_stock('stock_a', c()),
    "lengthgroups"), "Can't create stock with 0 length groups")

stock_a <- g3_stock('stock_a', seq(10, 10, 5))
stock_b <- g3_stock('stock_b', seq(50, 54, 1))
stock_wonky <- g3_stock('stock_wonky', c(0, 10, 100, 200, 1000))

actions <- list(
    list(
        '999' = gadget3:::g3_step(~{
            # NB: stock_with also includes the stock environment, which is why we need it
            stock_with(stock_a, {
                g3_report(stock_a__minlen)
                g3_report(stock_a__midlen)
                g3_report(stock_a__dl)
                g3_report(stock_a__plusdl)
            })

            stock_with(stock_b, {
                g3_report(stock_b__minlen)
                g3_report(stock_b__midlen)
                g3_report(stock_b__dl)
                g3_report(stock_b__plusdl)
            })

            stock_with(stock_wonky, {
                g3_report(stock_wonky__minlen)
                g3_report(stock_wonky__midlen)
                g3_report(stock_wonky__dl)
                g3_report(stock_wonky__plusdl)
            })

            return(g3_param('x'))
        })))
params <- list(x=1.0)
model_fn <- g3_to_r(actions)
# model_fn <- edit(model_fn)
result <- model_fn(params)

# We populated min/mean/dl
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__minlen,
    array(
        c(10),
        dimnames = list("len10"),
        dim = c(1))), "stock_a__minlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__midlen,
    array(
        c(10.5),
        dimnames = list("len10"),
        dim = c(1))), "stock_a__midlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__dl,
    c(1)), "stock_a__dl")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_a__plusdl,
    1), "stock_a__plusdl")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_b__minlen,
    array(
        c(50, 51, 52, 53, 54),
        dimnames = list(c("len50", "len51", "len52", "len53", "len54")),
        dim = c(5))), "stock_b__minlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_b__midlen,
    array(
        c(50.5, 51.5, 52.5, 53.5, 54.5),
        dimnames = list(c("len50", "len51", "len52", "len53", "len54")),
        dim = c(5))), "stock_b__midlen")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_b__dl,
    c(1,1,1,1,1)), "stock_b__dl")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_b__plusdl,
    1), "stock_b__plusdl")
ok(ut_cmp_identical(
    environment(model_fn)$model_report$stock_wonky__minlen,
    array(
        c(0, 10, 100, 200, 1000),
        dimnames = list(c("len0", "len10", "len100", "len200", "len1000")),
        dim = c(5))), "stock_wonky__minlen")
ok(ut_cmp_equal(
    environment(model_fn)$model_report$stock_wonky__midlen,
    array(
        c(5, 55, 150, 600, 1005),
        dimnames = list(c("len0", "len10", "len100", "len200", "len1000")),
        dim = c(5))), "stock_wonky__midlen")
ok(ut_cmp_equal(
    environment(model_fn)$model_report$stock_wonky__dl,
    c(10, 90, 100, 800, 10)), "stock_wonky__dl")
ok(ut_cmp_equal(
    environment(model_fn)$model_report$stock_wonky__plusdl,
    10), "stock_wonky__plusdl")

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
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
