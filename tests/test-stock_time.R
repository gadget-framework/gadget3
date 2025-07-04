library(magrittr)
library(unittest)

library(gadget3)

ok_group("g3s_time: Times produced in order", {
    inst <- g3_stock('terry', c(1)) %>% g3s_time(
        year = 2002:2004,
        step = 1:2)
    ok(ut_cmp_identical(
        inst$dimnames$time,
        c("2002-01", "2002-02", "2003-01", "2003-02", "2004-01", "2004-02")), "dimnames$time ordered year then step")
    ok(ut_cmp_identical(
        rlang:::f_rhs( g3_stock_def(inst, 'max_time_idx') ),
        quote( g3_idx(6L) )), "stock__max_time_idx: Length of array")
})

ok_group("g3s_time_convert: correct conversions", {
  inst <- c(g3s_time_convert(2000, NULL), g3s_time_convert(2000, 1), g3s_time_convert(2000, 12),
            g3s_time_convert(200, NULL), g3s_time_convert(200, 1), g3s_time_convert(200, 12),
            g3s_time_convert(20, NULL), g3s_time_convert(20, 1), g3s_time_convert(20, 12),
            g3s_time_convert(2, NULL), g3s_time_convert(2, 1), g3s_time_convert(2, 12))
  ok(ut_cmp_identical(inst, as.integer(c(200000,200001,200012,
                                         20000,20001,20012,
                                         2000,2001,2012,
                                         200,201,212))), "Pseudoyear and year conversions correct")
})

ok(ut_cmp_identical(
    g3s_time_convert(c("1999-01", "1999-02")),
    c(199901L, 199902L)), "Parsed year/step string")

ok(ut_cmp_identical(
    g3s_time_convert(c(1999, 1999)),
    c(199900L, 199900L)), "Step ignored if NULL")
ok(ut_cmp_identical(
    g3s_time_convert(c(1999, 1999), c('all', 'all')),
    c(199900L, 199900L)), "Treated MFDB 'all' as NULL")

stock_timeyear <- g3_stock('stock_timeyear', 1) %>% g3s_time(year = c(2002, 2004))
stock_timeyear__num <- g3_stock_instance(stock_timeyear, 0)
stock_timestep <- g3_stock('stock_timestep', 1) %>% g3s_time(times = c( g3s_time_convert(c(2000, 2003),c(1,2)) ))
stock_timestep__num <- g3_stock_instance(stock_timestep, 0)
# NB: There isn't 12 steps to use, but still changes mode
stock_timebigstep <- g3_stock('stock_timebigstep', 1) %>% g3s_time(times = c( g3s_time_convert(c(2001, 2003),c(1,12)) ))
stock_timebigstep__num <- g3_stock_instance(stock_timebigstep, 0)

stock_modeltime <- g3_stock('stock_modeltime', 1) %>% gadget3:::g3s_modeltime()
stock_modeltime__num <- g3_stock_instance(stock_modeltime, 0)
stock_modelyear <- g3_stock('stock_modelyear', 1) %>% gadget3:::g3s_modeltime(by_year = TRUE)
stock_modelyear__num <- g3_stock_instance(stock_modelyear, 0)
stock_modeltime_iterator <- 100

actions <- list(
    g3a_time(
        2000, 2004,
        step_lengths = c(6,6),
        final_year_steps = ~g3_param('final_year_steps', value = 2),
        project_years = ~g3_param('projectyears', value = 0)),
    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood(),
    list(
        "500:stock_time" = gadget3:::g3_step(~{
            stock_iterate(stock_timeyear, stock_ss(stock_timeyear__num) <- stock_ss(stock_timeyear__num) + stock_modeltime_iterator)
            stock_iterate(stock_timestep, stock_ss(stock_timestep__num) <- stock_ss(stock_timestep__num) + stock_modeltime_iterator)
            stock_iterate(stock_timebigstep, stock_ss(stock_timebigstep__num) <- stock_ss(stock_timebigstep__num) + stock_modeltime_iterator)
        }),
        "500:stock_modeltime" = gadget3:::g3_step(~{
            stock_iterate(stock_modeltime, stock_ss(stock_modeltime__num) <- stock_ss(stock_modeltime__num) + stock_modeltime_iterator)
            stock_iterate(stock_modelyear, stock_ss(stock_modelyear__num) <- stock_ss(stock_modelyear__num) + stock_modeltime_iterator)
        }),
        "999" = ~{
            stock_modeltime_iterator <- stock_modeltime_iterator + 1
            nll <- g3_param('nll', value = 1)
            REPORT(stock_timeyear__num)
            REPORT(stock_timestep__num)
            REPORT(stock_timebigstep__num)
            REPORT(stock_modeltime__num)
            REPORT(stock_modelyear__num)
            REPORT(stock_modeltime__num)
        }))

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
model_cpp <- g3_to_tmb(actions, trace = FALSE)

ok_group("g3s_modeltime", {
    params <- attr(model_fn, 'parameter_template')
    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_identical(
        r$stock_timeyear__num,
        structure(
            c(104 + 105, 108 + 109),
            .Dim = structure(1:2, .Names = c("length", "time")),
            .Dimnames = list(length = "1:Inf", time = c("2002", "2004")))), "stock_timeyear__num: 2002, 2004")
    ok(ut_cmp_identical(
        r$stock_timestep__num,
        structure(
            c(100, 107),
            .Dim = structure(1:2, .Names = c("length", "time")),
            .Dimnames = list(length = "1:Inf", time = c("2000-01", "2003-02")))), "stock_timestep__num: 2000-01, 2003-02")
    ok(ut_cmp_identical(
        r$stock_timebigstep__num,
        structure(
            c(102, 0),
            .Dim = structure(1:2, .Names = c("length", "time")),
            .Dimnames = list(length = "1:Inf", time = c("2001-01", "2003-12")))), "stock_timebigstep__num: 2001-01")
    ok(ut_cmp_identical(
        r$stock_modeltime__num,
        structure(
            c(100, 101, 102, 103, 104, 105, 106, 107, 108, 109),
            .Dim = c(length = 1L, time = 10L),
            .Dimnames = list(
                length = "1:Inf",
                time = c("2000-01", "2000-02", "2001-01", "2001-02", "2002-01", "2002-02", "2003-01",
                    "2003-02", "2004-01", "2004-02")))), "stock_modeltime__num: One of each iterator")

    ok(ut_cmp_identical(
        r$stock_modelyear__num,
        structure(
            c(201, 205, 209, 213, 217),
            .Dim = c(length = 1L, year = 5L),
            .Dimnames = list(length = "1:Inf", year = c("2000", "2001", "2002", "2003", "2004")))), "stock_modelyear__num: Aggregated by year")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})

ok_group("g3s_modeltime:project", {
    params <- attr(model_fn, 'parameter_template')
    params$projectyears <- 2
    params$nll <- 1.0

    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_identical(
        r$stock_modeltime__num,
        structure(
            c(100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113),
            .Dim = c(length = 1L, time = 14L),
            .Dimnames = list(
                length = "1:Inf",
                time = c("2000-01", "2000-02", "2001-01", "2001-02", "2002-01", "2002-02", "2003-01",
                    "2003-02", "2004-01", "2004-02", "2005-01", "2005-02", "2006-01", "2006-02")))), "stock_modeltime__num: One of each iterator")

    ok(ut_cmp_identical(
        r$stock_modelyear__num,
        structure(
            c(201, 205, 209, 213, 217, 221, 225),
            .Dim = c(length = 1L, year = 7L),
            .Dimnames = list(length = "1:Inf", year = c("2000", "2001", "2002", "2003", "2004", "2005", "2006")))), "stock_modelyear__num: Aggregated by year")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})

ok_group("g3s_modeltime:final_year_steps", {
    params <- attr(model_fn, 'parameter_template')
    params$final_year_steps <- 1

    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_identical(
        r$stock_timeyear__num,
        structure(
            c(104 + 105, 108),
            .Dim = structure(1:2, .Names = c("length", "time")),
            .Dimnames = list(length = "1:Inf", time = c("2002", "2004")))), "stock_timeyear__num: 2002, 2004-01")
    ok(ut_cmp_identical(
        r$stock_timestep__num,
        structure(
            c(100, 107),
            .Dim = structure(1:2, .Names = c("length", "time")),
            .Dimnames = list(length = "1:Inf", time = c("2000-01", "2003-02")))), "stock_timestep__num: 2000-01, 2003-02")
    ok(ut_cmp_identical(
        r$stock_timebigstep__num,
        structure(
            c(102, 0),
            .Dim = structure(1:2, .Names = c("length", "time")),
            .Dimnames = list(length = "1:Inf", time = c("2001-01", "2003-12")))), "stock_timebigstep__num: 2001-01")
    ok(ut_cmp_identical(
        r$stock_modeltime__num,
        structure(
            c(100, 101, 102, 103, 104, 105, 106, 107, 108),
            .Dim = c(length = 1L, time = 9L),
            .Dimnames = list(
                length = "1:Inf",
                time = c("2000-01", "2000-02", "2001-01", "2001-02", "2002-01", "2002-02", "2003-01",
                    "2003-02", "2004-01")))), "stock_modeltime__num: One of each iterator, 2004 a short year")

    ok(ut_cmp_identical(
        r$stock_modelyear__num,
        structure(
            c(201, 205, 209, 213, 108),
            .Dim = c(length = 1L, year = 5L),
            .Dimnames = list(length = "1:Inf", year = c("2000", "2001", "2002", "2003", "2004")))), "stock_modelyear__num: Aggregated by year (2004 short)")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})
