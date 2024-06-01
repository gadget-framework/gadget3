library(unittest)

library(gadget3)

ok(ut_cmp_equal(
    g3_eval(
        g3_suitability_andersen(0,1,2,3,4),
        predator_length = 99,
        stock = g3_stock('prey', 1:10 * 10) ),
    gadget3:::force_vector(c(
        "10:20" = 1.5385642245386,
        "20:30" = 1.90781891144376,
        "30:40" = 1.99894574784489,
        "40:50" = 1.97774955212781,
        "50:60" = 1.91681934676797,
        "60:70" = 1.83906905397844,
        "70:80" = 1.75539377253919,
        "80:90" = 1.67124659054669,
        "90:100" = 1.58937906439558,
        "100:Inf" = 1.51113553695681,
        NULL ))), "g3_eval: substituted stock & iterator value")

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
