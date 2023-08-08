library(magrittr)
library(unittest)

library(gadget3)


# Helper to generate ld from table string and attributes
generate_ld <- function (tbl, all_stocks = list(), all_fleets = list(), ...) {
    if (is.character(tbl)) tbl <- read.table(text = tbl, header = TRUE, stringsAsFactors = TRUE)
    if (is.null(tbl$number)) tbl$number <- as.numeric(seq_len(nrow(tbl)))
    out <- gadget3:::g3l_likelihood_data('ut', structure(tbl, ...), all_stocks = all_stocks, all_fleets = all_fleets)

    # NB: A failed merge would result in repeated instances
    ok(ut_cmp_equal(
        sort(as.numeric(out$number[out$number != 0])),
        sort(as.numeric(tbl$number)),
        deparse_frame = -2), "number array has all of source data")
    return(out)
}

# Dig minlen out of modelstock
ld_minlen <- function (ld) {
    x <- g3_stock_def(ld$modelstock, 'minlen')
    # Bodge array back to (named) vector
    as.matrix(x)[,1]
}

# Dig definitions out of modelstock
ld_upperlen <- function (ld) g3_stock_def(ld$modelstock, 'upperlen')
ld_dl <- function (ld) g3_stock_def(ld$modelstock, 'dl')
ld_plusdl <- function (ld) g3_stock_def(ld$modelstock, 'plusdl')
ld_minages <- function (ld) g3_stock_def(ld$modelstock, 'minages')

# Compare array by turning it back into a table first
cmp_array <- function (ar, table_text) {
    tbl <- read.table(
        header = TRUE,
        stringsAsFactors = FALSE,
        colClasses = c(rep("character", length(dim(ar))), "numeric"),
        text = table_text)
    ut_cmp_identical(as.data.frame.table(ar, stringsAsFactors = FALSE), tbl, deparse_frame = -2)
}


ok_group('g3l_likelihood_data:unknown', {
    ok(ut_cmp_error({
        ld <- generate_ld(
            data.frame(
                year = 1990,
                camel = 3,
                dromedary = 1,
                number = 1:3,
                stringsAsFactors = FALSE),
            end = NULL)
    }, "camel, dromedary"), "Unrecognised camels columns")
})


ok_group('g3l_likelihood_data:time', {
    ok(ut_cmp_error({
        ld <- generate_ld(
            data.frame(
                number = 1:3,
                stringsAsFactors = FALSE),
            end = NULL)
    }, "year column"), "Noticed lack of year column")

    ld <- generate_ld("
        year number
        1998 1
        2002 2
        2001 3
    ")
    ok(cmp_array(ld$number, "
        length time Freq
          0:Inf 1998    1
          0:Inf 2001    3
          0:Inf 2002    2
        "), "Year gap, wonky year order preserved")

    ld <- generate_ld("
        year step number
        1998    1      1
        1998    2      2
        1999    1      3
        2000    1      4
        2000    2      5
    ")
    ok(cmp_array(ld$number, "
        length    time Freq
          0:Inf 1998-01    1
          0:Inf 1998-02    2
          0:Inf 1999-01    3
          0:Inf 2000-01    4
          0:Inf 2000-02    5
        "), "Year gap, wonky year order preserved")
})


ok_group('g3l_likelihood_data:length', {
    ld <- generate_ld("
        year number
        1999      1
        2000      2
        2001      3
    ")
    ok(cmp_array(ld$number, "
        length time Freq
          0:Inf 1999    1
          0:Inf 2000    2
          0:Inf 2001    3
        "), "Default single length dimension if none supplied")
    ok(ut_cmp_identical(ld$modelstock$dimnames, list(
        length = "0:Inf")), "modelstock got default length dimension if none supplied")

    ld <- generate_ld("
        year length number
        1999      1      1
        2000      1      2
        2001      1      3
        1999      5      4
        2000      5      5
        2001      5      6
        1999     10      7
        2001     10      9
        2000     30     11
        2001     30     12
    ")
    ok(cmp_array(ld$number, "
        length time Freq
           1:5 1999    1
          5:10 1999    4
         10:30 1999    7
        30:Inf 1999    0
           1:5 2000    2
          5:10 2000    5
         10:30 2000    0
        30:Inf 2000    11
           1:5 2001    3
          5:10 2001    6
         10:30 2001    9
        30:Inf 2001    12
        "), "Lengths read from data, missing 2000/10 1999/30 filled in with 0")
    ok(ut_cmp_identical(
        ld_minlen(ld),
        c("1:5" = 1, "5:10" = 5, "10:30" = 10, "30:Inf" = 30)), "minlen set via. data")
    ok(ut_cmp_identical(ld_upperlen(ld), Inf), "If we guess from data, open-ended is only sensible option")

    ok(ut_cmp_error(generate_ld("
        year length number
        1999      a      1999.1
        2000      a      2000.1
        2001      a      2001.1
        1999      b      1999.2
        2000      b      2000.2
        2001      b      2001.2
        1999      c      1999.3
        2001      c      2001.3
        ",
        length = list(
            a = structure(quote(seq(10, 20)), min = 10, max = 20),
            b = structure(quote(seq(20, 40)), min = 20, max = 40),
            c = structure(quote(seq(80, 100)), min = 80, max = 100))), "Gaps in length"), "Non-contiguous length groups cause an error")

    ld <- generate_ld("
        year length number
        1999      a      1999.1
        2000      a      2000.1
        2001      a      2001.1
        1999      b      1999.2
        2000      b      2000.2
        2001      b      2001.2
        1999      c      1999.3
        2001      c      2001.3
        ",
        length = list(
            a = structure(quote(seq(10, 20)), min = 10, max = 20),
            b = structure(quote(seq(20, 40)), min = 20, max = 40),
            c = structure(quote(seq(40, 80)), min = 40, max = 80)))
    ok(cmp_array(ld$number, "
        length time   Freq
         10:20 1999 1999.1
         20:40 1999 1999.2
         40:80 1999 1999.3
         10:20 2000 2000.1
         20:40 2000 2000.2
         40:80 2000    0.0
         10:20 2001 2001.1
         20:40 2001 2001.2
         40:80 2001 2001.3
        "), "Use lengths, removed names from attribute, gaps filled in")
    ok(ut_cmp_identical(
        ld_minlen(ld),
        c("10:20" = 10, "20:40" = 20, "40:80" = 40)), "minlen set by attribute")
    ok(ut_cmp_identical(ld_upperlen(ld), 80), "Upperlen set by attribute")
    ok(ut_cmp_identical(ld_dl(ld), c(10, 20, 40)), "dl difference up to upper bound")
    ok(ut_cmp_identical(ld_plusdl(ld), 10), "plusdl is the mode")

    ld <- generate_ld("
        year length number
        1999      a      1999.1
        2000      a      2000.1
        2001      a      2001.1
        1999      b      1999.2
        2000      b      2000.2
        2001      b      2001.2
        1999      c      1999.3
        2001      c      2001.3
        ",
        length = list(
            a = structure(quote(seq(10, 20)), min = 10, max = 20),
            b = structure(quote(seq(20, 40)), min = 20, max = 40),
            c = structure(quote(seq(40, 80)), min = 40, max = 80, max_open_ended = TRUE)))
    ok(cmp_array(ld$number, "
        length time   Freq
         10:20 1999 1999.1
         20:40 1999 1999.2
        40:Inf 1999 1999.3
         10:20 2000 2000.1
         20:40 2000 2000.2
        40:Inf 2000    0.0
         10:20 2001 2001.1
         20:40 2001 2001.2
        40:Inf 2001 2001.3
        "), "Use lengths, removed names from attribute, gaps filled in")
    ok(ut_cmp_identical(ld_upperlen(ld), Inf), "upperlen now infinite")
    ok(ut_cmp_identical(ld_dl(ld), c(10, 20, 10)), "dl assumes final group is as big as the mode")
    ok(ut_cmp_identical(ld_plusdl(ld), 10), "plusdl is the mode")
    ok(ut_cmp_identical(
        ld_minlen(ld),
        c("10:20" = 10, "20:40" = 20, "40:Inf" = 40)), "minlen doesn't include the plusgroup separately")

    ld <- generate_ld("
        year length number
        1999      a      1999.1
        2000      a      2000.1
        2001      a      2001.1
        1999      b      1999.2
        2000      b      2000.2
        2001      b      2001.2
        1999      c      1999.3
        2001      c      2001.3
        ",
        length = list(
            "a" = structure(quote(seq(10, 20)), min = 10, max = 20, min_open_ended = TRUE),
            "b" = structure(quote(seq(20, 40)), min = 20, max = 40),
            "c" = structure(quote(seq(40, 80)), min = 40, max = 80)))
    ok(ut_cmp_identical(ld_upperlen(ld), 80), "upperlen set by attribute")
    ok(ut_cmp_identical(
        ld_minlen(ld),
        c("0:20" = 0, "20:40" = 20, "40:80" = 40)), "minlen down to zero due to min_open_ended")
})


ok_group('g3l_likelihood_data:length_factor', {
    ld <- generate_ld(data.frame(
        year = 1990,
        length = cut(c(14, 28, 33, 33), seq(0, 50, by = 10), right = FALSE),
        stringsAsFactors = TRUE))
    ok(ut_cmp_identical(
        ld_minlen(ld),
        c("0:10" = 0, "10:20" = 10, "20:30" = 20, "30:40" = 30, "40:50" = 40)), "ld_minlen: Not open ended")
    ok(ut_cmp_identical(ld_upperlen(ld), 50), "ld_upperlen: Not open ended")

    ld <- generate_ld(data.frame(
        year = 1990,
        length = cut(c(14, 28, 33, 33), c(seq(0, 50, by = 10), Inf), right = FALSE),
        stringsAsFactors = TRUE))
    ok(ut_cmp_identical(
        ld_minlen(ld),
        c("0:10" = 0, "10:20" = 10, "20:30" = 20, "30:40" = 30, "40:50" = 40, "50:Inf" = 50)), "ld_minlen: Open ended")
    ok(ut_cmp_identical(ld_upperlen(ld), Inf), "ld_upperlen: Open ended")

    ok(ut_cmp_error({
        ld <- generate_ld(data.frame(
            year = 1990,
            length = as.factor(c("a", "b", "b", "c")),
            stringsAsFactors = TRUE))
    }, "length levels.*a, b, c"), "Unrecognised column format, included levels in error")

    ok(ut_cmp_error({
        ld <- generate_ld(data.frame(
            year = 1990,
            length = cut(c(14, 28, 33, 33), c(seq(0, 50, by = 10), Inf), right = TRUE),
            stringsAsFactors = TRUE))
    }, "inclusive-lower.*\\(0,10\\], \\(10,20\\], \\(20,30\\], \\(30,40\\], \\(40,50\\], \\(50,Inf\\]"), "Unrecognised column format, included levels in error")  # ))))

    ok(ut_cmp_error({
        ld <- generate_ld(data.frame(
            year = 1990,
            # ((((
            length = factor(c("[0,10)", "[20, 40)"), levels = c("[0,10)", "[20, 40)")),
            stringsAsFactors = TRUE))
    # ((
    }, "Gaps in length groups are not supported: \\[0,10\\), \\[20, 40\\)"), "Complained about gaps in length groups")
})


ok_group('g3l_likelihood_data:age', {
    ld <- generate_ld("
        age year number
          3 1999      1999.3
          4 1999      1999.4
          6 1999      1999.6
          3 2000      2000.3
          6 2000      2000.6
          4 2001      2001.4
          6 2001      2001.6
        ")
    ok(cmp_array(ld$number, "
        length  age time   Freq
          0:Inf age3 1999 1999.3
          0:Inf age4 1999 1999.4
          0:Inf age5 1999    0.0
          0:Inf age6 1999 1999.6
          0:Inf age3 2000 2000.3
          0:Inf age4 2000    0.0
          0:Inf age5 2000    0.0
          0:Inf age6 2000 2000.6
          0:Inf age3 2001    0.0
          0:Inf age4 2001 2001.4
          0:Inf age5 2001    0.0
          0:Inf age6 2001 2001.6
        "), "Worked out age dimensions from data, filled in missing values, including entirely absent ones")

    ld <- generate_ld("
        age year number
          x 1999      1999.1
          y 1999      1999.2
          x 2000      2000.1
          x 2001      2001.1
          y 2001      2001.2
        ",
        age = list(
            x = structure(quote(seq(1, 3)), min = 1, max = 3),
            y = structure(quote(seq(4, 6)), min = 4, max = 6),
            z = structure(quote(seq(7, 10)), min = 7, max = 10)))
    ok(cmp_array(ld$number, "
        length  age time   Freq
          0:Inf 1:3 1999 1999.1
          0:Inf 4:6 1999 1999.2
          0:Inf 7:10 1999   0.0
          0:Inf 1:3 2000 2000.1
          0:Inf 4:6 2000    0.0
          0:Inf 7:10 2000   0.0
          0:Inf 1:3 2001 2001.1
          0:Inf 4:6 2001 2001.2
          0:Inf 7:10 2001   0.0
        "), "Worked out age dimensions from attributes, filled in missing values")
    ok(ut_cmp_identical(
        ld_minages(ld),
        gadget3:::force_vector("1:3" = 1L, "4:6" = 4L, "7:10" = 7L)), "agegroups using minages from attribute")
})


ok_group('g3l_likelihood_data:age_factor', {
    df <- data.frame(
        year = 1990,
        age = c(3,3,4,5),
        number = 1,
        stringsAsFactors = TRUE)
    df$age <- cut(df$age, seq(3, 10, by = 1), right = FALSE)
    df <- aggregate(number ~ year + age, df, sum)
    ld <- generate_ld(df)
    ok(cmp_array(ld$number, "
        length age time Freq
         0:Inf 3:3 1990    2
         0:Inf 4:4 1990    1
         0:Inf 5:5 1990    1
         0:Inf 6:6 1990    0
         0:Inf 7:7 1990    0
         0:Inf 8:8 1990    0
         0:Inf 9:9 1990    0
     "), "ld$number: included all single ages")

    ld <- generate_ld(data.frame(
        year = 1990,
        age = as.factor(c(3,4,5,8)),
        stringsAsFactors = TRUE))
    ok(cmp_array(ld$number, "
        length age time Freq
        0:Inf 3:3 1990    1
        0:Inf 4:4 1990    2
        0:Inf 5:7 1990    3
        0:Inf 8:8 1990    4
     "), "ld$number: can also use integer strings as factors")

    df <- data.frame(
        year = 1990,
        age = c(3,3,4,5),
        number = 1,
        stringsAsFactors = TRUE)
    df$age <- cut(df$age, seq(2, 10, by = 4), right = FALSE)
    df <- aggregate(number ~ year + age, df, sum)
    ld <- generate_ld(df)
    ok(cmp_array(ld$number, "
        length age time Freq
         0:Inf 2:5 1990    4
         0:Inf 6:9 1990    0
     "), "ld$number: Everything grouped into first group, second compared to zero")
})


ok_group('g3l_likelihood_data:area', {
    # Pull the area lookup definition back out
    area_lookup <- function (ld) {
        list(
            keys = environment(g3_stock_def(ld$modelstock, 'areagroup_lookup'))$keys,
            values = environment(g3_stock_def(ld$modelstock, 'areagroup_lookup'))$values)
    }

    ok(ut_cmp_error({
        ld <- generate_ld("
            area year number
              a 1999      1999.1
              b 1999      1999.2
              c 1999      1999.3
            ")
    }, "Areas in data"), "If char areas are provided without aggregation, we can't do anything")

    ok(ut_cmp_error({
        ld <- generate_ld("
            area year number
              a 1999      1999.1
              b 1999      1999.2
              c 1999      1999.3
            ", area = list(a = 1, b = 2, c = 3))
    }, "Areas in data"), "If char areas are provided without aggregation, we can't do anything. MFDB aggregates don't count")

    ld <- generate_ld("
        area year number
          1 1999      1999.1
          2 1999      1999.2
          3 1999      1999.3
          2 2000      2000.2
          3 2000      2000.3
          1 2001      2001.1
          2 2001      2001.2
        ")
    ok(cmp_array(ld$number, "
        length time area   Freq
          0:Inf 1999    1 1999.1
          0:Inf 2000    1    0.0
          0:Inf 2001    1 2001.1
          0:Inf 1999    2 1999.2
          0:Inf 2000    2 2000.2
          0:Inf 2001    2 2001.2
          0:Inf 1999    3 1999.3
          0:Inf 2000    3 2000.3
          0:Inf 2001    3    0.0
        "), "Worked out area dimensions from data, filled in missing values")

    ld <- gadget3:::g3l_likelihood_data('ut', read.table(header = TRUE, stringsAsFactors = TRUE, text = "
        area year number
          a 1999      1999.1
          b 1999      1999.2
          c 1999      1999.3
          b 2000      2000.2
          c 2000      2000.3
          a 2001      2001.1
          b 2001      2001.2
        "), area_group = list(a = 3, b = 4, c = c(1:2)))
    ok(cmp_array(ld$number, "
        length time area   Freq
          0:Inf 1999    a 1999.1
          0:Inf 2000    a    0.0
          0:Inf 2001    a 2001.1
          0:Inf 1999    b 1999.2
          0:Inf 2000    b 2000.2
          0:Inf 2001    b 2001.2
          0:Inf 1999    c 1999.3
          0:Inf 2000    c 2000.3
          0:Inf 2001    c    0.0
        "), "Worked out area dimensions from data, filled in missing values")
    ok(ut_cmp_identical(area_lookup(ld), list(keys = c(3L,4L,1L,2L), values = c(1L,2L,3L,3L))), "Areas 1 & 2 both mapped to index 3 (i.e. c)")
})

ok_group('g3l_likelihood_data:tag', {
    ld <- gadget3:::g3l_likelihood_data('ut', read.table(header = TRUE, stringsAsFactors = TRUE, text = "
        tag year number
          a 1999      1999.1
          b 1999      1999.2
          c 1999      1999.3
          b 2000      2000.2
          c 2000      2000.3
          a 2001      2001.1
          b 2001      2001.2
        "))
    ok(cmp_array(ld$number, "
        length    tag time   Freq
        0:Inf        a 1999 1999.1
        0:Inf        b 1999 1999.2
        0:Inf        c 1999 1999.3
        0:Inf        a 2000    0.0
        0:Inf        b 2000 2000.2
        0:Inf        c 2000 2000.3
        0:Inf        a 2001 2001.1
        0:Inf        b 2001 2001.2
        0:Inf        c 2001    0.0
        "), "Worked out tag dimensions from data")
    ok(ut_cmp_identical(
        g3_stock_def(ld$modelstock, 'tag_ids'),
        gadget3:::force_vector(a = 1L, b = 2L, c = 3L)), "stock__tag_ids: Worked out from factor")
})

ok_group('g3l_likelihood_data:stock', {
    ld <- generate_ld("
        age year number
          3 1999      1999.3
          4 1999      1999.4
          6 1999      1999.6
          3 2000      2000.3
          6 2000      2000.6
          4 2001      2001.4
          6 2001      2001.6
        ")
    ok(is.null(ld$stock_map), "No stock column, so no stock map")

    ok(ut_cmp_error(generate_ld("
        age year stock stock_re number
          3 1999    a  a$       1999.3
        "), "stock.*stock_re"), "Can't have both stock & stock_re")

    ld <- generate_ld("
        age year stock number
          3 1999    a  1999.3
          4 1999    b  1999.4
          6 1999    a  1999.6
          3 2000    a  2000.3
          6 2000    b  2000.6
          4 2001    b  2001.4
          6 2001    b  2001.6
        ")
    ok(ut_cmp_identical(dimnames(ld$number)[['stock']], c("a", "b")), "Array has stocks a & b")
    ok(ut_cmp_identical(ld$stock_map, list(a = 1L, b = 2L)), "stock_map is 1:1 mapping")

    # Generate a list of stocks "stock_(imm,mat)_(f,m)"
    stocks <- lapply(paste(
        'stock',
        rep(c('imm', 'mat'), each = 2),
        c('f', 'm'),
        sep = "_"), function (x) g3_stock(x, 1))
    ld <- generate_ld("
        age year stock_re number
          3 1999    _f$  1999.3
          4 1999    ^stock_mat  1999.4
          6 1999    ^stock_imm  1999.6
          3 2000    ^stock_mat  2000.3
          6 2000    ^stock_imm  2000.6
          4 2001    ^stock_imm  2001.4
          6 2001    ^stock_mat  2001.6
        ", all_stocks = stocks)
    ok(ut_cmp_identical(
        dimnames(ld$number)[['stock_re']],
        c("_f$", "^stock_mat", "^stock_imm")), "Array names are regexes")
    ok(ut_cmp_identical(
        ld$stock_map,
        list(
            stock_imm_f = 1L,
            stock_imm_m = 3L,
            stock_mat_f = 1L,
            stock_mat_m = 2L)), "Stock map used first regexes first")

    # Generate a list of stocks "stock_(imm,mat)_f" (NB: not male)
    stocks <- lapply(paste(
        'stock',
        rep(c('imm', 'mat'), each = 2),
        c('f', 'm'),
        sep = "_"), function (x) g3_stock(x, 1))
    ld <- generate_ld("
        age year stock_re number
          3 1999    _mat_f$  1999.3
          4 1999    _mat_f$  1999.4
          6 1999    _imm_f$  1999.6
          3 2000    _mat_f$  2000.3
          6 2000    _imm_f$  2000.6
          4 2001    _imm_f$  2001.4
          6 2001    _mat_f$  2001.6
        ", all_stocks = stocks)
    ok(ut_cmp_identical(
        dimnames(ld$number)[['stock_re']],
        c("_mat_f$", "_imm_f$")), "Array names are regexes")
    ok(ut_cmp_identical(
        ld$stock_map,
        list(
            stock_imm_f = 2L,
            stock_imm_m = NULL,
            stock_mat_f = 1L,
            stock_mat_m = NULL)), "Stock map ignored unused stocks")
})

ok_group('g3l_likelihood_data:fleet', {
    ld <- generate_ld("
        age year number
          3 1999      1999.3
          4 1999      1999.4
          6 1999      1999.6
          3 2000      2000.3
          6 2000      2000.6
          4 2001      2001.4
          6 2001      2001.6
        ")
    ok(is.null(ld$fleet_map), "No fleet column, so no fleet map")

    ok(ut_cmp_error(generate_ld("
        age year fleet fleet_re number
          3 1999    a  a$       1999.3
        "), "fleet.*fleet_re"), "Can't have both fleet & fleet_re")

    ld <- generate_ld("
        age year fleet number
          3 1999    a  1999.3
          4 1999    b  1999.4
          6 1999    a  1999.6
          3 2000    a  2000.3
          6 2000    b  2000.6
          4 2001    b  2001.4
          6 2001    b  2001.6
        ")
    ok(ut_cmp_identical(dimnames(ld$number)[['fleet']], c("a", "b")), "Array has fleets a & b")
    ok(ut_cmp_identical(ld$fleet_map, list(a = 1L, b = 2L)), "fleet_map is 1:1 mapping")

    # Generate a list of fleets "fleet_(trawl|gil)_(f|m)"
    fleets <- lapply(paste(
        'fleet',
        rep(c('trawl', 'gil'), each = 2),
        c('is', 'no'),
        sep = "_"), function (x) g3_stock(x, 1))
    ld <- generate_ld("
        age year fleet_re number
          3 1999    _is$  1999.3
          4 1999    ^fleet_trawl  1999.4
          6 1999    ^fleet_gil  1999.6
          3 2000    ^fleet_trawl  2000.3
          6 2000    ^fleet_gil  2000.6
          4 2001    ^fleet_gil  2001.4
          6 2001    ^fleet_trawl  2001.6
        ", all_fleets = fleets)
    ok(ut_cmp_identical(
        dimnames(ld$number)[['fleet_re']],
        c("_is$", "^fleet_trawl", "^fleet_gil")), "Array names are regexes")
    ok(ut_cmp_identical(
        ld$fleet_map,
        list(
            fleet_trawl_is = 1L,
            fleet_trawl_no = 2L,
            fleet_gil_is = 1L,
            fleet_gil_no = 3L)), "fleet map used first regexes first")

    # Generate a list of fleets "fleet_(trawl|gil)_(f|m)"
    fleets <- lapply(paste(
        'fleet',
        rep(c('trawl', 'gil'), each = 2),
        c('is', 'no'),
        sep = "_"), function (x) g3_stock(x, 1))
    ld <- generate_ld("
        age year fleet_re number
          3 1999    _gil_is$  1999.3
          4 1999    _gil_is$  1999.4
          6 1999    _trawl_is$  1999.6
          3 2000    _gil_is$  2000.3
          6 2000    _trawl_is$  2000.6
          4 2001    _trawl_is$  2001.4
          6 2001    _gil_is$  2001.6
        ", all_fleets = fleets)
    ok(ut_cmp_identical(
        dimnames(ld$number)[['fleet_re']],
        c("_gil_is$", "_trawl_is$")), "Array names are regexes")
    ok(ut_cmp_identical(
        ld$fleet_map,
        list(
            fleet_trawl_is = 2L,
            fleet_trawl_no = NULL,
            fleet_gil_is = 1L,
            fleet_gil_no = NULL)), "fleet map ignored unused fleets")
})
