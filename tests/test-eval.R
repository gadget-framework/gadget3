library(unittest)

library(gadget3)

ok(ut_cmp_equal(as.numeric(g3_eval(
    g3_suitability_andersen(0,1,2,3,4),
    pred_stock = g3_stock('pred', 11:20),
    stock = g3_stock('prey', 1:10))), as.numeric(c(
        "11:12" = 1.39762234830867,
        "12:13" = 1.76710448730415,
        "13:14" = 1.92001105688051,
        "14:15" = 1.98080983811489,
        "15:16" = 1.99913177015507,
        "16:17" = 1.99765923114992,
        "17:18" = 1.98837494524607,
        "18:19" = 1.97544436079439,
        "19:20" = 1.96094041453242,
        "20:Inf" = 1.94597890503981))), "g3_eval: substituted 2 stocks")

ok(ut_cmp_equal(
    g3_eval(quote( g3_param('x', value = 99) )),
    99), "Used default value for param")
ok(ut_cmp_equal(
    g3_eval(quote( g3_param('x', value = 99) ), param.x = 88),
    88), "Overrode g3_param with environment")
ok(ut_cmp_equal(
    g3_eval(
        g3_parameterized('lln.alpha', by_stock = TRUE, value = 99),
        stock = g3_stock("fish", 1:10),
        param.fish.lln.alpha = 123),
    123), "Both evaluated stock-ified parameter and substituted")

ok_group("g3_eval error output", {
    ok(ut_cmp_error({
        g3_eval(quote( stock_prepend(stock, g3_param("x")) + stop('erk') ), stock = g3_stock("camel", 1), x = 99123)
    }, 'erk'), "Show error message")
    ok(ut_cmp_error({
        g3_eval(quote( stock_prepend(stock, g3_param("x")) + stop('erk') ), stock = g3_stock("camel", 1), x = 99123)
    }, 'g3_param\\("camel\\.x"\\)'), "Show converted formula")
    ok(ut_cmp_error({
        g3_eval(quote( stock_prepend(stock, g3_param("x")) + stop('erk') ), stock = g3_stock("camel", 1), x = 99123)
    }, '99123'), "Show content of environment")
})
