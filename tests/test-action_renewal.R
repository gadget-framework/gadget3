library(magrittr)
library(unittest)

library(gadget3)

tmb_r_compare <- function (model_fn, model_tmb, params) {
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        # Reformat params into a single vector in expected order
        par <- unlist(params[attr(model_cpp, 'parameter_template')$switch])
        model_tmb_report <- model_tmb$report(par)
        r_result <- model_fn(params)
        for (n in names(attributes(r_result))) {
            ok(ut_cmp_equal(
                as.vector(model_tmb_report[[n]]),
                as.vector(attr(r_result, n)),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}

areas <- list(a=1, b=2, c=3, d=4)
stock_a <- g3_stock('stock_a', seq(10, 10, 5)) %>% g3s_livesonareas(areas[c('a')])
stock_ac <- g3_stock('stock_ac', seq(10, 10, 5)) %>% g3s_livesonareas(areas[c('a', 'c')])
stock_b <- g3_stock('stock_b', seq(10, 100, 10)) %>% g3s_age(5, 10)
stock_b_report <- g3s_clone(stock_b, 'report_b') %>% g3s_time(year = 2000:2003)
    
actions <- list(
    g3a_time(2000, 2003),
    g3a_initialconditions(stock_a, ~area * 100 + stock_a__minlen, ~stock_a__minlen + 100),
    g3a_initialconditions(stock_ac, ~area * 1000 + stock_ac__minlen, ~stock_a__minlen + 200),
    g3a_initialconditions_normalparam(stock_b,
        factor_f = ~g3_param("init.factor"),
        mean_f = ~g3_param("init.mean"),
        stddev_f = ~g3_param("init.stddev"),
        alpha_f = ~g3_param("init.walpha"),
        beta_f = ~g3_param("init.wbeta")),
    g3a_renewal_normalparam(stock_b,
        factor_f = ~g3_param("renewal.factor"),
        mean_f = ~g3_param("renewal.mean"),
        stddev_f = ~g3_param("renewal.stddev"),
        alpha_f = ~g3_param("renewal.walpha"),
        beta_f = ~g3_param("renewal.wbeta"),
        run_f = ~age == 5),
    g3a_renewal_normalparam(stock_b,
        factor_f = ~g3_param("renewal.factor") * 0.000001,
        mean_f = ~g3_param("renewal.mean"),
        stddev_f = ~g3_param("renewal.stddev"),
        alpha_f = ~g3_param("renewal.walpha") * 0.000001,
        beta_f = ~g3_param("renewal.wbeta") * 0.0001,
        run_f = ~age == 7),
    g3a_report_stock(stock_b_report, stock_b, ~stock_ss(stock_b__num)),
    g3a_report_stock(stock_b_report, stock_b, ~stock_ss(stock_b__wgt)),
    list(
        '999' = ~{
            g3_report(stock_a__num)
            g3_report(stock_ac__num)
            g3_report(stock_a__wgt)
            g3_report(stock_ac__wgt)

            nll <- nll + g3_param('x')
        }))
params <- list(
    init.factor = 10,
    init.mean = 50,
    init.stddev = 10,
    init.walpha = 3e-6,
    init.wbeta = 3,
    renewal.factor = 10,
    renewal.mean = 50,
    renewal.stddev = 10,
    renewal.walpha = 3e-3,
    renewal.wbeta = 3,
    x=1.0)
# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

result <- model_fn(params)
r <- attributes(result)
#str(as.list(r), vec.len = 10000)

# Populated numbers
ok(ut_cmp_identical(
    as.vector(r$stock_a__num),
    c(110)), "stock_a__num populated")
ok(ut_cmp_identical(
    as.vector(r$stock_ac__num),
    c(1010, 3010)), "stock_ac__num populated")

# Populated mean weights
ok(ut_cmp_identical(
    as.vector(r$stock_a__wgt),
    c(110)), "stock_a__wgt populated")
ok(ut_cmp_identical(
    as.vector(r$stock_ac__wgt),
    c(210, 210)), "stock_ac__wgt populated")

ok(ut_cmp_equal(
    as.data.frame(round(r$report_b__num[,,'2000'], 5)),
    read.table(header=TRUE,row.names=1,check.names=FALSE,text = '
length          age5        age6        age7        age8        age9       age10
  len10    174.53935    87.26967    87.26976    87.26967    87.26967    87.26967
  len20   3505.71653  1752.85827  1752.86002  1752.85827  1752.85827  1752.85827
  len30  25903.93612 12951.96806 12951.98101 12951.96806 12951.96806 12951.96806
  len40  70414.19883 35207.09942 35207.13462 35207.09942 35207.09942 35207.09942
  len50  70414.19883 35207.09942 35207.13462 35207.09942 35207.09942 35207.09942
  len60  25903.93612 12951.96806 12951.98101 12951.96806 12951.96806 12951.96806
  len70   3505.71653  1752.85827  1752.86002  1752.85827  1752.85827  1752.85827
  len80    174.53935    87.26967    87.26976    87.26967    87.26967    87.26967
  len90      3.19680     1.59840     1.59840     1.59840     1.59840     1.59840
  len100     0.02154     0.01077     0.01077     0.01077     0.01077     0.01077
')), "report_b__num: Initial values populaed by g3a_initialconditions_normalparam")

ok(ut_cmp_equal(
    as.data.frame(round(r$report_b__wgt[,,'2000'], 5)),
    read.table(header=TRUE,row.names=1,check.names=FALSE,text = '
length         age5    age6    age7    age8    age9   age10
  len10     5.06756 0.01012 0.01012 0.01012 0.01012 0.01012
  len20    23.46094 0.04688 0.04687 0.04688 0.04688 0.04688
  len30    64.37681 0.12863 0.12862 0.12863 0.12863 0.12863
  len40   136.82419 0.27338 0.27337 0.27338 0.27338 0.27338
  len50   249.81206 0.49912 0.49912 0.49912 0.49912 0.49912
  len60   412.34944 0.82388 0.82387 0.82388 0.82388 0.82388
  len70   633.44531 1.26562 1.26562 1.26562 1.26562 1.26562
  len80   922.10869 1.84238 1.84237 1.84238 1.84238 1.84238
  len90  1287.34856 2.57213 2.57212 2.57213 2.57213 2.57213
  len100 1738.17394 3.47288 3.47286 3.47288 3.47288 3.47288
')), "report_b__wgt: Initial values populaed by g3a_initialconditions_normalparam")

ok(ut_cmp_equal(
    as.data.frame(round(r$report_b__num[,'age5',], 5)),
    read.table(header=TRUE,row.names=1,check.names=FALSE,text = '
length          2000         2001         2002         2003
  len10    174.53935    261.80902    349.07870    436.34837
  len20   3505.71653   5258.57480   7011.43306   8764.29133
  len30  25903.93612  38855.90418  51807.87223  64759.84029
  len40  70414.19883 105621.29825 140828.39767 176035.49708
  len50  70414.19883 105621.29825 140828.39767 176035.49708
  len60  25903.93612  38855.90418  51807.87223  64759.84029
  len70   3505.71653   5258.57480   7011.43306   8764.29133
  len80    174.53935    261.80902    349.07870    436.34837
  len90      3.19680      4.79520      6.39360      7.99200
  len100     0.02154      0.03231      0.04308      0.05385
'), tolerance = 1e-7), "report_b__num: age5 has increased with time")
ok(ut_cmp_equal(
    as.data.frame(round(r$report_b__wgt[,'age5',], 5)),
    read.table(header=TRUE,row.names=1,check.names=FALSE,text = '
length         2000       2001       2002       2003
  len10     5.06756    6.75337    7.59628    8.10202
  len20    23.46094   31.26562   35.16797   37.50937
  len30    64.37681   85.79287   96.50091  102.92572
  len40   136.82419  182.34113  205.09959  218.75468
  len50   249.81206  332.91637  374.46853  399.39983
  len60   412.34944  549.52463  618.11222  659.26477
  len70   633.44531  844.17187  949.53516 1012.75312
  len80   922.10869 1228.86413 1382.24184 1474.26847
  len90  1287.34856 1715.60737 1929.73678 2058.21442
  len100 1738.17394 2316.40762 2605.52447 2778.99457
')), "report_b__wgt: age5 has increased with time")

ok(ut_cmp_equal(
    as.data.frame(round(r$report_b__num[,'age5',], 5)),
    read.table(header=TRUE,row.names=1,check.names=FALSE,text = '
length          2000         2001         2002         2003
  len10    174.53935    261.80902    349.07870    436.34837
  len20   3505.71653   5258.57480   7011.43306   8764.29133
  len30  25903.93612  38855.90418  51807.87223  64759.84029
  len40  70414.19883 105621.29825 140828.39767 176035.49708
  len50  70414.19883 105621.29825 140828.39767 176035.49708
  len60  25903.93612  38855.90418  51807.87223  64759.84029
  len70   3505.71653   5258.57480   7011.43306   8764.29133
  len80    174.53935    261.80902    349.07870    436.34837
  len90      3.19680      4.79520      6.39360      7.99200
  len100     0.02154      0.03231      0.04308      0.05385
')), "report_b__num: age5 increasing rapidly")

ok(ut_cmp_equal(
    as.data.frame(round(r$report_b__num[,'age7',], 5)),
    read.table(header=TRUE,row.names=1,check.names=FALSE,text = '
length          2000        2001        2002        2003
  len10     87.26976    87.26985    87.26994    87.27002
  len20   1752.86002  1752.86177  1752.86352  1752.86528
  len30  12951.98101 12951.99396 12952.00691 12952.01987
  len40  35207.13462 35207.16983 35207.20504 35207.24024
  len50  35207.13462 35207.16983 35207.20504 35207.24024
  len60  12951.98101 12951.99396 12952.00691 12952.01987
  len70   1752.86002  1752.86177  1752.86352  1752.86528
  len80     87.26976    87.26985    87.26994    87.27002
  len90      1.59840     1.59840     1.59840     1.59841
  len100     0.01077     0.01077     0.01077     0.01077
')), "report_b__num: age7 increasing slowly")

for (age in c('age6', 'age8', 'age9', 'age10')) {
    for (year in c('2001', '2002', '2003')) {
        ok(ut_cmp_equal(
            r$report_b__num[,age,'2000'],
            r$report_b__num[,age,year]), paste0("report_b__num: ", age, " doesn't grow in year ", year))
        ok(ut_cmp_equal(
            r$report_b__wgt[,age,'2000'],
            r$report_b__wgt[,age,year]), paste0("report_b__wgt: ", age, " doesn't grow in year ", year))
    }
}
