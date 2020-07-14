library(unittest)

library(gadget3)

ok_group("step_id", {
    step_id <- gadget3:::step_id
    stock_a <- g3_stock('stock_aaa', 10, 40, 5)
    stock_b <- g3_stock('stock_bbb', 10, 40, 5)

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
