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
        rlang:::f_rhs( gadget3:::stock_definition(inst, 'stock__max_time_idx') ),
        quote( g3_idx(6L) )), "stock__max_time_idx: Length of array")
})

stock_modeltime <- g3_stock('stock_modeltime', 1) %>% gadget3:::g3s_modeltime()
stock_modeltime__num <- gadget3:::stock_instance(stock_modeltime, 0)
stock_modelyear <- g3_stock('stock_modelyear', 1) %>% gadget3:::g3s_modeltime(by_year = TRUE)
stock_modelyear__num <- gadget3:::stock_instance(stock_modelyear, 0)
stock_modeltime_iterator <- 100

actions <- list(
    g3a_time(2000, 2004, steps = c(6,6), project_years = ~g3_param('projectyears')),
    list(
        stock_modeltime = gadget3:::g3_step(~{
            stock_iterate(stock_modeltime, stock_ss(stock_modeltime__num) <- stock_ss(stock_modeltime__num) + stock_modeltime_iterator)
            stock_iterate(stock_modelyear, stock_ss(stock_modelyear__num) <- stock_ss(stock_modelyear__num) + stock_modeltime_iterator)
            stock_modeltime_iterator <- stock_modeltime_iterator + 1
            g3_report(stock_modeltime__num)
            g3_report(stock_modelyear__num)
        }),
        "999" = ~{
            nll <- g3_param('nll')
        }))
params <- list(
    projectyears = 0,
    nll = 1.0)

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

ok_group("g3s_modeltime", {
    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_identical(
        r$stock_modeltime__num,
        structure(
            c(100, 101, 102, 103, 104, 105, 106, 107, 108, 109),
            .Dim = c(length = 1L, time = 10L),
            .Dimnames = list(
                length = "len1",
                time = c("2000-01", "2000-02", "2001-01", "2001-02", "2002-01", "2002-02", "2003-01",
                    "2003-02", "2004-01", "2004-02")))), "stock_modeltime__num: One of each iterator")

    ok(ut_cmp_identical(
        r$stock_modelyear__num,
        structure(
            c(201, 205, 209, 213, 217),
            .Dim = c(length = 1L, year = 5L),
            .Dimnames = list(length = "len1", year = c("2000", "2001", "2002", "2003", "2004")))), "stock_modelyear__num: Aggregated by year")
})

ok_group("g3s_modeltime:project", {
    params <- list(
        projectyears = 2,
        nll = 1.0)
    result <- model_fn(params)
    r <- attributes(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_identical(
        r$stock_modeltime__num,
        structure(
            c(100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113),
            .Dim = c(length = 1L, time = 14L),
            .Dimnames = list(
                length = "len1",
                time = c("2000-01", "2000-02", "2001-01", "2001-02", "2002-01", "2002-02", "2003-01",
                    "2003-02", "2004-01", "2004-02", "2005-01", "2005-02", "2006-01", "2006-02")))), "stock_modeltime__num: One of each iterator")

    ok(ut_cmp_identical(
        r$stock_modelyear__num,
        structure(
            c(201, 205, 209, 213, 217, 221, 225),
            .Dim = c(length = 1L, year = 7L),
            .Dimnames = list(length = "len1", year = c("2000", "2001", "2002", "2003", "2004", "2005", "2006")))), "stock_modelyear__num: Aggregated by year")
})
