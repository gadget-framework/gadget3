library(magrittr)
library(unittest)

library(gadget3)


ok_group('g3l_likelihood_data:time', {
    generate_ld <- function (tbl, ...) {
        tbl$number <- as.numeric(seq_len(nrow(tbl)))
        gadget3:::g3l_likelihood_data('ut', structure(tbl, ...))
    }

    ok(ut_cmp_error({
        ld <- generate_ld(
            data.frame(
                number = 1:3,
                stringsAsFactors = FALSE),
            end = NULL)
    }, "year column"), "Noticed lack of year column")

    ld <- generate_ld(expand.grid(year = c(1998, 2002, 2001)))
    ok(ut_cmp_identical(ld$number, structure(
        c(1, 0, 0, 3, 2),
        .Dimnames = list(length = "len0", time = c("1998.", "1999.", "2000.", "2001.", "2002.")),
        .Dim = c(length = 1L, time = 5L))), "Gap in years resulted in 0 padding, perserved wonky year order")

    ld <- generate_ld(data.frame(
        year = c(1998, 1998, 1999, 2000, 2000),
        step = c(1,2,1,1,2)))
    ok(ut_cmp_identical(ld$number, structure(
        c(1, 2, 3, 0, 4, 5),
        .Dimnames = list(length = "len0", time = c("1998.1", "1998.2", "1999.1", "1999.2", "2000.1", "2000.2")),
        .Dim = c(length = 1L, time = 6L))), "Gap in years resulted in 0 padding, perserved wonky year order")
})


ok_group('g3l_likelihood_data:length', {
    generate_ld <- function (tbl, ...) gadget3:::g3l_likelihood_data('ut', structure(tbl, ...))
    stock_dims <- function(ld) ld$modelstock$dimnames

    ld <- generate_ld(
        expand.grid(
            year = 1999:2001,
            number = c(1)),
        end = NULL)
    ok(ut_cmp_identical(dimnames(ld$number), list(
        length = "len0",
        time = c("1999.", "2000.", "2001."))), "Default single length dimension")
    ok(ut_cmp_identical(stock_dims(ld), list(
        length = "len0")), "modelstock got default length dimension")

    ld <- generate_ld(
        expand.grid(
            year = 1999:2001,
            length = c(1,5,10,30),
            number = c(1)),
        end = NULL)
    ok(ut_cmp_identical(dimnames(ld$number), list(
        length = c("len1", "len5", "len10", "len30"),
        time = c("1999.", "2000.", "2001."))), "Lengths read from data")
    ok(ut_cmp_identical(stock_dims(ld), list(
        length = c("len1", "len5", "len10", "len30"))), "modelstock got lengths read from data")

    ld <- generate_ld(
        expand.grid(
            year = 1999:2001,
            length = c(1,10, 20),
            number = c(1)),
        length = list(len0 = c(1,10), len10 = c(10,20), len20 = c(20,30), len30 = c(30,40)),
        end = NULL)
    ok(ut_cmp_identical(dimnames(ld$number), list(
        length = c("len1", "len10", "len20", "len30"),
        time = c("1999.", "2000.", "2001."))), "Lengths read from attr")
    ok(ut_cmp_identical(stock_dims(ld), list(
        length = c("len1", "len10", "len20", "len30"))), "modelstock got lengths read from attr")
})


ok_group('g3l_likelihood_data:age', {
    generate_ld <- function (tbl, ...) gadget3:::g3l_likelihood_data('ut', structure(tbl, ...))
    stock_dims <- function(ld) ld$modelstock$dimnames

    ld <- generate_ld(
        expand.grid(
            year = 1999:2001,
            age = c(3,4,9),
            number = c(1)),
        end = NULL)
    ok(ut_cmp_identical(dimnames(ld$number), list(
        length = "len0",
        age = c("age3", "age4", "age9"),
        time = c("1999.", "2000.", "2001."))), "Worked out age dimensions from data")
    ok(ut_cmp_identical(stock_dims(ld), list(
        length = "len0",
        age = c("age3", "age4", "age9"))), "modelstock got same dimensions")

    ld <- generate_ld(
        expand.grid(
            year = 1999:2001,
            age = c('x', 'y'),
            number = c(1)),
        age = list(x = 1:3, y = 4:5),
        end = NULL)
    ok(ut_cmp_identical(dimnames(ld$number), list(
        length = "len0",
        age = c("age1", "age4"),
        time = c("1999.", "2000.", "2001."))), "Worked out age dimensions from attribute")
    ok(ut_cmp_identical(stock_dims(ld), list(
        length = "len0",
        age = c("age1", "age4"))), "modelstock got same dimensions")
})
