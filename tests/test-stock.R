library(magrittr)
library(unittest)

library(gadget3)

ok(ut_cmp_error(
    g3_stock('stock_a', c()),
    "lengthgroups"), "Can't create stock with 0 length groups")

multipart <- g3_stock(c(species = "ling", "imm"), 1:5)
ok(ut_cmp_identical(
    multipart$name,
    "ling_imm"), "multipart$name: Stock name got concatenated")
ok(ut_cmp_identical(
    multipart$name_parts[['species']],
    "ling"), "multipart$name_parts: Can dig out just species name from multipart name")

stock_a <- g3_stock('stock_a', seq(10, 10, 5))
stock_b <- g3_stock('stock_b', seq(50, 54, 1))
stock_wonky <- g3_stock('stock_wonky', c(0, 10, 100, 200, 1000))

actions <- list(
    list(
        '999' = gadget3:::g3_step(~{
            # NB: stock_with also includes the stock environment, which is why we need it
            stock_with(stock_a, {
                REPORT(stock_a__minlen)
                REPORT(stock_a__midlen)
                REPORT(stock_a__dl)
                REPORT(stock_a__plusdl)
            })

            stock_with(stock_b, {
                REPORT(stock_b__minlen)
                REPORT(stock_b__midlen)
                REPORT(stock_b__dl)
                REPORT(stock_b__plusdl)
            })

            stock_with(stock_wonky, {
                REPORT(stock_wonky__minlen)
                REPORT(stock_wonky__midlen)
                REPORT(stock_wonky__dl)
                REPORT(stock_wonky__plusdl)
            })

            nll <- nll + g3_param('x', value = 1.0)
            return(nll)
        })))

model_fn <- g3_to_r(actions)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
    model_cpp <- c()
}

params <- attr(model_fn, 'parameter_template')
result <- model_fn(params)
r <- attributes(result)

# We populated min/mean/dl
ok(ut_cmp_identical(
    r$stock_a__minlen,
    array(
        c(10),
        dimnames = list("10:Inf"),
        dim = c(1))), "stock_a__minlen")
ok(ut_cmp_identical(
    r$stock_a__midlen,
    array(
        c(10.5),
        dimnames = list("10:Inf"),
        dim = c(1))), "stock_a__midlen")
ok(ut_cmp_identical(
    r$stock_a__dl,
    c(1)), "stock_a__dl")
ok(ut_cmp_identical(
    r$stock_a__plusdl,
    1), "stock_a__plusdl")
ok(ut_cmp_identical(
    r$stock_b__minlen,
    array(
        c(50, 51, 52, 53, 54),
        dimnames = list(c("50:51", "51:52", "52:53", "53:54", "54:Inf")),
        dim = c(5))), "stock_b__minlen")
ok(ut_cmp_identical(
    r$stock_b__midlen,
    array(
        c(50.5, 51.5, 52.5, 53.5, 54.5),
        dimnames = list(c("50:51", "51:52", "52:53", "53:54", "54:Inf")),
        dim = c(5))), "stock_b__midlen")
ok(ut_cmp_identical(
    r$stock_b__dl,
    c(1,1,1,1,1)), "stock_b__dl")
ok(ut_cmp_identical(
    r$stock_b__plusdl,
    1), "stock_b__plusdl")
ok(ut_cmp_identical(
    r$stock_wonky__minlen,
    array(
        c(0, 10, 100, 200, 1000),
        dimnames = list(c("0:10", "10:100", "100:200", "200:1000", "1000:Inf")),
        dim = c(5))), "stock_wonky__minlen")
ok(ut_cmp_equal(
    r$stock_wonky__midlen,
    array(
        c(5, 55, 150, 600, 1005),
        dimnames = list(c("0:10", "10:100", "100:200", "200:1000", "1000:Inf")),
        dim = c(5))), "stock_wonky__midlen")
ok(ut_cmp_equal(
    r$stock_wonky__dl,
    c(10, 90, 100, 800, 10)), "stock_wonky__dl")
ok(ut_cmp_equal(
    r$stock_wonky__plusdl,
    10), "stock_wonky__plusdl")

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    param_template <- attr(model_cpp, "parameter_template")
    param_template$value <- params[param_template$switch]
    gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
} else {
    writeLines("# skip: not running TMB tests")
}
