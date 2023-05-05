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

ok_group("g3_eval error output", {
    ok(ut_cmp_error({
        g3_eval(quote( stock_param(stock, "x") + stop('erk') ), stock = g3_stock("camel", 1), x = 99123)
    }, 'erk'), "Show error message")
    ok(ut_cmp_error({
        g3_eval(quote( stock_param(stock, "x") + stop('erk') ), stock = g3_stock("camel", 1), x = 99123)
    }, 'g3_param\\("camel\\.x"\\)'), "Show converted formula")
    ok(ut_cmp_error({
        g3_eval(quote( stock_param(stock, "x") + stop('erk') ), stock = g3_stock("camel", 1), x = 99123)
    }, '99123'), "Show content of environment")
})
