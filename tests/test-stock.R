library(magrittr)
library(unittest)

library(gadget3)

ok_group("g3_stock_def", {
    stock_a <- g3_stock('stock_a', seq(10, 10, 5))
    stock_a <- g3s_age(stock_a, 10, 20)

    # Make sure both minage & stock__minage forms work (the latter should be deprecated though)
    ok(ut_cmp_identical(g3_stock_def(stock_a, 'minage'), 10L), "Fetched minage")
    ok(ut_cmp_identical(g3_stock_def(stock_a, 'maxage'), 20L), "Fetched maxage")
    ok(ut_cmp_identical(g3_stock_def(stock_a, 'stock__minage'), 10L), "Fetched minage")
    ok(ut_cmp_identical(g3_stock_def(stock_a, 'stock__maxage'), 20L), "Fetched maxage")

    # Make sure old internal method call works
    ok(ut_cmp_identical(suppressWarnings(gadget3:::stock_definition(stock_a, 'minage')), 10L), "Fetched minage with old gadget3:::stock_definition")
    ok(ut_cmp_identical(suppressWarnings(gadget3:::stock_definition(stock_a, 'maxage')), 20L), "Fetched maxage with old gadget3:::stock_definition")
})

ok_group("g3_stock_def:list", {
    stocks <- list(
        imm = g3_stock(c('st', 'imm'), 1:10) |> g3s_age(1, 5),
        mat = g3_stock(c('st', 'mat'), 1:10) |> g3s_age(3, 7) )
    ok(ut_cmp_identical(g3_stock_def(stocks, 'minage'), list(imm = 1L, mat = 3L)), "minage: Pulled out values from both stocks")
})

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

fleet <- g3_fleet('fleet_a')
stock_a <- g3_stock('stock_a', seq(10, 10, 5))
stock_b <- g3_stock('stock_b', seq(50, 54, 1))
stock_wonky <- g3_stock('stock_wonky', c(0, 10, 100, 200, 1000))
nll <- 0.0

actions <- list(
    list(
        g3a_time(1999, 1999),
        g3a_initialconditions(stock_a, ~100 + stock_a__minlen, ~0),
        '5:sum_fleet_stock_a' = gadget3:::g3_step(g3_formula({
            stock_iterate(fleet, stock_intersect(stock_a, {
                sum_fleet_stock_a <- sum_fleet_stock_a + stock_ss(stock_a__num, vec = single)
            }))
            REPORT(sum_fleet_stock_a)
        }, sum_fleet_stock_a = as.array(c(x = 0.0)), fleet = fleet, stock_a = stock_a)),
        '999' = gadget3:::g3_step(~{
            # NB: stock_with also includes the stock environment, which is why we need it
            stock_with(stock_a, {
                REPORT(stock_a__minlen)
                REPORT(stock_a__midlen)
                REPORT(stock_a__maxmidlen)
                REPORT(stock_a__minmidlen)
                REPORT(stock_a__dl)
                REPORT(stock_a__plusdl)
            })

            stock_with(stock_b, {
                REPORT(stock_b__minlen)
                REPORT(stock_b__midlen)
                REPORT(stock_b__maxmidlen)
                REPORT(stock_b__minmidlen)
                REPORT(stock_b__dl)
                REPORT(stock_b__plusdl)
            })

            stock_with(stock_wonky, {
                REPORT(stock_wonky__minlen)
                REPORT(stock_wonky__midlen)
                REPORT(stock_wonky__maxmidlen)
                REPORT(stock_wonky__minmidlen)
                REPORT(stock_wonky__dl)
                REPORT(stock_wonky__plusdl)
            })

            nll <- nll + g3_param('x', value = 1.0)
            return(nll)
        })))

model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions, trace = FALSE)

params <- attr(model_fn, 'parameter_template')
result <- model_fn(params)
r <- attributes(result)

# sum_fleet_stock_a
ok(ut_cmp_equal(r$sum_fleet_stock_a, as.array(c(x = 110))), "sum_fleet_stock_a: Intersected over a fleet (without length)")

# We populated min/mean/dl
ok(ut_cmp_identical(
    r$stock_a__minlen,
    c("10:Inf" = 10)), "stock_a__minlen")
ok(ut_cmp_identical(
    r$stock_a__midlen,
    structure(
        c(10.5),
        names = c("10:Inf"))), "stock_a__midlen")
ok(ut_cmp_identical(r$stock_a__minmidlen, 10.5), "stock_a__minmidlen")
ok(ut_cmp_identical(r$stock_a__maxmidlen, 10.5), "stock_a__maxmidlen")
ok(ut_cmp_identical(
    r$stock_a__dl,
    c(1)), "stock_a__dl")
ok(ut_cmp_identical(
    r$stock_a__plusdl,
    1), "stock_a__plusdl")
ok(ut_cmp_identical(
    r$stock_b__minlen,
    structure(
        c(50, 51, 52, 53, 54),
        names = c("50:51", "51:52", "52:53", "53:54", "54:Inf"))), "stock_b__minlen")
ok(ut_cmp_identical(
    r$stock_b__midlen,
    structure(
        c(50.5, 51.5, 52.5, 53.5, 54.5),
        names = c("50:51", "51:52", "52:53", "53:54", "54:Inf"))), "stock_b__midlen")
ok(ut_cmp_identical(r$stock_b__minmidlen, 50.5), "stock_b__minmidlen")
ok(ut_cmp_identical(r$stock_b__maxmidlen, 54.5), "stock_b__maxmidlen")
ok(ut_cmp_identical(
    r$stock_b__dl,
    c(1,1,1,1,1)), "stock_b__dl")
ok(ut_cmp_identical(
    r$stock_b__plusdl,
    1), "stock_b__plusdl")
ok(ut_cmp_identical(
    r$stock_wonky__minlen,
    structure(
        c(0, 10, 100, 200, 1000),
        names = c("0:10", "10:100", "100:200", "200:1000", "1000:Inf"))), "stock_wonky__minlen")
ok(ut_cmp_equal(
    r$stock_wonky__midlen,
    structure(
        c(5, 55, 150, 600, 1005),
        names = c("0:10", "10:100", "100:200", "200:1000", "1000:Inf"))), "stock_wonky__midlen")
ok(ut_cmp_identical(r$stock_wonky__minmidlen, 5), "stock_wonky__minmidlen")
ok(ut_cmp_identical(r$stock_wonky__maxmidlen, 1005), "stock_wonky__maxmidlen")
ok(ut_cmp_equal(
    r$stock_wonky__dl,
    c(10, 90, 100, 800, 10)), "stock_wonky__dl")
ok(ut_cmp_equal(
    r$stock_wonky__plusdl,
    10), "stock_wonky__plusdl")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
