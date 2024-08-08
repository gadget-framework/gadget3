if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(magrittr)
library(unittest)

library(gadget3)


# Helper to generate ld from table string and attributes
generate_ld <- function (tbl, all_stocks = list(), all_fleets = list(), all_predators = list(), use_preview = FALSE, ...) {
    if (is.character(tbl)) tbl <- read.table(text = tbl, header = TRUE, stringsAsFactors = TRUE)
    if (is.null(tbl$number)) tbl$number <- as.numeric(seq_len(nrow(tbl)))
    all_stocks <- lapply(all_stocks, function (x) g3_stock(x, 1))
    if (use_preview) {
        # Use new public preview function
        out <- list( obs_array = list(
            num = g3_distribution_preview(structure(tbl, ...), stocks = all_stocks, fleets = all_fleets, predators = all_predators) ))
    } else {
        # Fall back to old behaviour
        out <- gadget3:::g3l_likelihood_data('ut', structure(tbl, ...), all_stocks = all_stocks, all_fleets = all_fleets, all_predators = all_predators)
    }

    # NB: A failed merge would result in repeated instances
    ok(ut_cmp_equal(
        sort(as.numeric(out$obs_array$num[out$obs_array$num != 0])),
        sort(as.numeric(tbl$number)),
        deparse_frame = -2), "number array has all of source data")
    return(out)
}

# Generate example of ld being stock_iterate()d or stock_intersect()ed
generate_code <- function(ld, repl_fn, ...) {
    model_fn <- g3_to_r(list(gadget3:::g3_step(gadget3:::call_to_formula(
        substitute(
            extractme(repl_fn_sym(st, stock_ss(st__num, vec = single))),
            list(repl_fn_sym = as.symbol(repl_fn)) ),
        list(
            st = ld$obsstock,
            st__num = ld$number,
            ... )))))
    gadget3:::f_find(body(model_fn), quote(extractme))[[1]][[2]]
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
    ld <- generate_ld(
        data.frame(
            year = 1990:1992,
            number = 1:3,
            camel = 10:12,
            stringsAsFactors = FALSE),
        end = NULL)
    ok(ut_cmp_equal(ld$obs_array, list(
        num = array(1:3, dim = c(length = 1L, time = 3L), dimnames = list(length = "0:Inf", time = c("1990", "1991", "1992"))),
        camel = array(10:12, dim = c(length = 1L, time = 3L), dimnames = list(length = "0:Inf", time = c("1990", "1991", "1992"))) )), "Will create arrays from any unknown columns")
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
    ok(cmp_array(ld$obs_array$num, "
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
    ok(cmp_array(ld$obs_array$num, "
        length    time Freq
          0:Inf 1998-01    1
          0:Inf 1998-02    2
          0:Inf 1999-01    3
          0:Inf 2000-01    4
          0:Inf 2000-02    5
        "), "Year gap, wonky year order preserved")

    ld <- generate_ld("
        time number
        1998 1
        2002 2
        2001 3
    ")
    ok(cmp_array(ld$obs_array$num, "
        length time Freq
          0:Inf 1998    1
          0:Inf 2001    3
          0:Inf 2002    2
        "), "Time column used when year not present (i.e. can parse our own output)")

    ld <- generate_ld("
        time number
        1998-01 2
        1998-02 4
        1999-01 3
        1999-02 9
    ")
    ok(cmp_array(ld$obs_array$num, "
        length time Freq
          0:Inf 1998-01    2
          0:Inf 1998-02    4
          0:Inf 1999-01    3
          0:Inf 1999-02    9
        "), "Year-step separated in time column")
})

ok_group('g3l_likelihood_data:length', {
    ld <- generate_ld("
        year number
        1999      1
        2000      2
        2001      3
    ")
    ok(cmp_array(ld$obs_array$num, "
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
    ok(cmp_array(ld$obs_array$num, "
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
    ok(cmp_array(ld$obs_array$num, "
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
            c = structure(quote(seq(40, 80)), min = 40, max = 80) ),
        use_preview = TRUE )
    ok(cmp_array(ld$obs_array$num, "
        length time   Freq
         10:20 1999 1999.1
         20:40 1999 1999.2
         40:80 1999 1999.3
         10:20 2000 2000.1
         20:40 2000 2000.2
         40:80 2000     NA
         10:20 2001 2001.1
         20:40 2001 2001.2
         40:80 2001 2001.3
        "), "Use lengths, removed names from attribute, gaps filled in (with new g3_distribution_preview)")

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
    ok(cmp_array(ld$obs_array$num, "
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


ok_group('g3l_likelihood_data:age_char', {
    ld <- generate_ld(expand.grid(
        year = 1990,
        length = as.character(cut(seq(3, 47, by=5), seq(0, 50, by = 5), right = FALSE)),
        age = 1:2,
        stringsAsFactors = FALSE))
    ok(cmp_array(ld$obs_array$num, "
        length  age time   Freq
           0:5 age1 1990    1
          5:10 age1 1990    2
         10:15 age1 1990    3
         15:20 age1 1990    4
         20:25 age1 1990    5
         25:30 age1 1990    6
         30:35 age1 1990    7
         35:40 age1 1990    8
         40:45 age1 1990    9
           0:5 age2 1990   10
          5:10 age2 1990   11
         10:15 age2 1990   12
         15:20 age2 1990   13
         20:25 age2 1990   14
         25:30 age2 1990   15
         30:35 age2 1990   16
         35:40 age2 1990   17
         40:45 age2 1990   18
        "), "Converted back to factor, preserving ordering of entries")

    ld <- generate_ld(expand.grid(
        year = 1990,
        length = as.character(cut(seq(3, 47, by=5), seq(0, 50, by = 5), right = FALSE)),
        age = c('[1,3]', '[4,9]'),
        stringsAsFactors = FALSE))
    ok(cmp_array(ld$obs_array$num, "
        length  age time   Freq
           0:5 1:3 1990    1
          5:10 1:3 1990    2
         10:15 1:3 1990    3
         15:20 1:3 1990    4
         20:25 1:3 1990    5
         25:30 1:3 1990    6
         30:35 1:3 1990    7
         35:40 1:3 1990    8
         40:45 1:3 1990    9
           0:5 4:9 1990   10
          5:10 4:9 1990   11
         10:15 4:9 1990   12
         15:20 4:9 1990   13
         20:25 4:9 1990   14
         25:30 4:9 1990   15
         30:35 4:9 1990   16
         35:40 4:9 1990   17
         40:45 4:9 1990   18
        "), "Can use intervals in strings, converted to groups")

    ld <- generate_ld(data.frame(
        year = 1990,
        length = c(
            as.character(cut(seq(23, 39, by=5), seq(0, 50, by = 5), right = FALSE)),
            as.character(cut(seq(3, 47, by=5), seq(0, 50, by = 5), right = FALSE)),
            NULL),
        age = c(
            rep(1, 4),
            rep(2, 9),
            NULL),
        stringsAsFactors = FALSE))
    ok(cmp_array(ld$obs_array$num, "
        length  age time   Freq
           0:5 age1 1990    0
          5:10 age1 1990    0
         10:15 age1 1990    0
         15:20 age1 1990    0
         20:25 age1 1990    1
         25:30 age1 1990    2
         30:35 age1 1990    3
         35:40 age1 1990    4
         40:45 age1 1990    0
           0:5 age2 1990    5
          5:10 age2 1990    6
         10:15 age2 1990    7
         15:20 age2 1990    8
         20:25 age2 1990    9
         25:30 age2 1990   10
         30:35 age2 1990   11
         35:40 age2 1990   12
         40:45 age2 1990   13
        "), "Partial ranges padded out with zeros")

    ld <- generate_ld(data.frame(
        year = 1990,
        length = cut(c(4, 14, 28, 33, 33, 44), c(seq(0, 50, by = 10), Inf), right = FALSE),
        stringsAsFactors = TRUE))
    ld_loopback <- generate_ld(as.data.frame.table(ld$obs_array$num, responseName = 'number'))
    ok(ut_cmp_equal(ld$obs_array$num, ld_loopback$obs_array$num), "Can parse our own output")
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
    ok(cmp_array(ld$obs_array$num, "
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
    ok(cmp_array(ld$obs_array$num, "
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

ok_group('g3l_likelihood_data:agegroup') ###########
ld <- generate_ld(expand.grid(
     year = 2000:2005,
     length = c(1,5,10),
     age = c("[1,2)", "[3,3)")  )) # ((
ok(gadget3:::ut_cmp_code(generate_code(ld, 'stock_intersect', age = 1, cur_year = 1999, cur_step = 1), quote({
    ut_obs__time_idx <- intlookup_getdefault(ut_obs__times, (cur_year *
        100L + cur_step * 0L), -1L)
    if (ut_obs__time_idx >= (1L)) {
        for (ut_obs__agegroup_idx in seq_along(ut_obs__minages)) {
            `_age` <- ut_obs__minages[[ut_obs__agegroup_idx]]
            for (ut_obs__length_idx in seq_along(ut_obs__minlen)) {
                length <- ut_obs__midlen[[ut_obs__length_idx]]
                ut_obs__num[ut_obs__length_idx, ut_obs__agegroup_idx,
                  ut_obs__time_idx]
            }
        }
    }
}), optimize = TRUE), "stock_intersect: Renamed all vars, including ut_obs__agegroup")

ok(ut_cmp_equal(
    g3_eval(attr(ld$obsstock$env$ut_obs__agegroup, "g3_global_init_val")),
    structure(
        list(1L, 2L, 2L),
        key_var = "ut_obs__agegroup_keys",
        value_var = "ut_obs__agegroup_values" )), "ut_obs__agegroup: Sub-parts also renamed")

########### g3l_likelihood_data:agegroup

ok_group('g3l_likelihood_data:age_factor', {
    df <- data.frame(
        year = 1990,
        age = c(3,3,4,5),
        number = 1,
        stringsAsFactors = TRUE)
    df$age <- cut(df$age, seq(3, 10, by = 1), right = FALSE)
    df <- aggregate(number ~ year + age, df, sum)
    ld <- generate_ld(df)
    ok(cmp_array(ld$obs_array$num, "
        length age time Freq
         0:Inf 3:3 1990    2
         0:Inf 4:4 1990    1
         0:Inf 5:5 1990    1
         0:Inf 6:6 1990    0
         0:Inf 7:7 1990    0
         0:Inf 8:8 1990    0
         0:Inf 9:9 1990    0
     "), "ld$obs_array$num: included all single ages")

    ld <- generate_ld(data.frame(
        year = 1990,
        age = as.factor(c(3,4,5,8)),
        stringsAsFactors = TRUE))
    ok(cmp_array(ld$obs_array$num, "
        length age time Freq
        0:Inf 3:3 1990    1
        0:Inf 4:4 1990    2
        0:Inf 5:7 1990    3
        0:Inf 8:8 1990    4
     "), "ld$obs_array$num: can also use integer strings as factors")

    df <- data.frame(
        year = 1990,
        age = c(3,3,4,5),
        number = 1,
        stringsAsFactors = TRUE)
    df$age <- cut(df$age, seq(2, 10, by = 4), right = FALSE)
    df <- aggregate(number ~ year + age, df, sum)
    ld <- generate_ld(df)
    ok(cmp_array(ld$obs_array$num, "
        length age time Freq
         0:Inf 2:5 1990    4
         0:Inf 6:9 1990    0
     "), "ld$obs_array$num: Everything grouped into first group, second compared to zero")
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
    ok(cmp_array(ld$obs_array$num, "
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
    ok(cmp_array(ld$obs_array$num, "
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
    ok(cmp_array(ld$obs_array$num, "
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
    ok(is.null(ld$maps$stock), "No stock column, so no stock map")

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
        ", all_stocks = c('a', 'b'))
    ok(ut_cmp_identical(dimnames(ld$obs_array$num)[['stock']], c("a", "b")), "Array has stocks a & b")
    ok(ut_cmp_identical(ld$maps$stock, c(a = 'a', b = 'b')), "stock_map is 1:1 mapping")

    ok(ut_cmp_error(generate_ld("
        age year stock number
          3 1999    a  1999.3
          4 1999    b  1999.4
          5 1999    kapow  1999.6
          6 1999    zot  1999.6
        ", all_stocks = c('a', 'b')), "kapow, zot"), "Unknown stock names in data an error")

    # Generate a list of stocks "stock_(imm,mat)_(f,m)"
    stock_names <- paste(
        'stock',
        rep(c('imm', 'mat'), each = 2),
        c('f', 'm'),
        sep = "_")
    ld <- generate_ld("
        age year stock_re number
          3 1999    _f$  1999.3
          4 1999    ^stock_mat  1999.4
          6 1999    ^stock_imm  1999.6
          3 2000    ^stock_mat  2000.3
          6 2000    ^stock_imm  2000.6
          4 2001    ^stock_imm  2001.4
          6 2001    ^stock_mat  2001.6
        ", all_stocks = stock_names)
    ok(ut_cmp_identical(
        dimnames(ld$obs_array$num)[['stock_re']],
        c("_f$", "^stock_mat", "^stock_imm")), "Array names are regexes")
    ok(ut_cmp_identical(
        ld$maps$stock,
        c(
            stock_imm_f = '_f$',
            stock_imm_m = '^stock_imm',
            stock_mat_f = '_f$',
            stock_mat_m = '^stock_mat')), "Stock map used first regexes first")

    ok(ut_cmp_error(generate_ld("
        age year stock_re number
          3 1999    _f$  1999.3
          3 1999    _g$  1999.3
          3 1999    _h$  1999.3
        "), "_g\\$, _h\\$"), "Regexes that don't match anything an error")

    # Generate a list of stocks "stock_(imm,mat)_f" (NB: not male)
    stock_names <- paste(
        'stock',
        rep(c('imm', 'mat'), each = 2),
        c('f', 'm'),
        sep = "_")
    ld <- generate_ld("
        age year stock_re number
          3 1999    _mat_f$  1999.3
          4 1999    _mat_f$  1999.4
          6 1999    _imm_f$  1999.6
          3 2000    _mat_f$  2000.3
          6 2000    _imm_f$  2000.6
          4 2001    _imm_f$  2001.4
          6 2001    _mat_f$  2001.6
        ", all_stocks = stock_names)
    ok(ut_cmp_identical(
        dimnames(ld$obs_array$num)[['stock_re']],
        c("_mat_f$", "_imm_f$")), "Array names are regexes")
    ok(ut_cmp_identical(
        ld$maps$stock,
        c(
            stock_imm_f = '_imm_f$',
            stock_imm_m = NA,
            stock_mat_f = '_mat_f$',
            stock_mat_m = NA )), "Stock map ignored unused stocks")
})

ok_group('g3l_likelihood_data:stock:name_parts', {
    stock_groupings <- function (stock_names, stock_cols) {
        tbl <- expand.grid(number = 0, stock = stock_cols, age = 3:6, year = 1999:2001)
        tbl$number <- seq_len(nrow(tbl))
        ld <- generate_ld(tbl, all_stocks = stock_names)
        return(ld$maps$stock)
    }
    out <- stock_groupings(
        list(c('fish', 'imm'), c('fish', 'mat'), c('fish', 'sen')),
        c('fish'))
    ok(ut_cmp_equal(out, c(
        fish_imm = 'fish',
        fish_mat = 'fish',
        fish_sen = 'fish' )), '"fish": Groups both maturity groups together')

    out <- stock_groupings(
        list(c('fish', 'imm'), c('fish', 'mat'), c('fish', 'sen')),
        c('fish_mat', 'fish'))
    ok(ut_cmp_equal(out, c(
        fish_imm = 'fish',
        fish_mat = 'fish_mat',
        fish_sen = 'fish' )), '"fish_mat": Overrides "fish" group due to longer length')

    out <- stock_groupings(
        list(c('a', 'imm'), c('a', 'mat'), c('b', 'imm'), c('b', 'mat'), c('c', 'mat')),
        c('a', 'b', 'mat'))
    ok(ut_cmp_equal(out, c(
        a_imm = 'a',
        a_mat = 'a',
        b_imm = 'b',
        b_mat = 'b',
        c_mat = 'mat' )), "'b' wins over 'mat' because it comes first")

    out <- stock_groupings(
        list(c('a', 'imm'), c('a', 'mat'), c('b', 'imm'), c('b', 'mat'), c('c', 'mat')),
        c('a', 'mat', 'b'))
    ok(ut_cmp_equal(out, c(
        a_imm = 'a',
        a_mat = 'a',
        b_imm = 'b',
        b_mat = 'mat',
        c_mat = 'mat' )), "'mat' wins over 'b' because it comes first")

    out <- stock_groupings(
        list(c('a', 'imm', 'f'), c('a', 'mat', 'f'), c('a', 'imm', 'm'), c('a', 'mat', 'm'), c('c', 'mat')),
        c('a_f', 'a_m', 'c'))
    ok(ut_cmp_equal(out, c(
        a_imm_f = 'a_f',
        a_mat_f = 'a_f',
        a_imm_m = 'a_m',
        a_mat_m = 'a_m',
        c_mat = 'c' )), "Name part groupings don't have to be sequential")
})

ok_group('g3l_likelihood_data:predator', {
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
    ok(is.null(ld$maps$predator), "No predator column, so no predator map")

    ok(ut_cmp_error(generate_ld("
        age year predator predator_re number
          3 1999    a  a$       1999.3
        "), "predator.*predator_re"), "Can't have both predator & predator_re")

    ld <- generate_ld("
        age year predator number
          3 1999    a  1999.3
          4 1999    b  1999.4
          6 1999    a  1999.6
          3 2000    a  2000.3
          6 2000    b  2000.6
          4 2001    b  2001.4
          6 2001    b  2001.6
        ", all_predators = list(g3_stock('a', c(0, 10)), g3_stock('b', c(0, 10)) ))
    ok(ut_cmp_identical(dimnames(ld$obs_array$num)[['predator']], c("a", "b")), "Array has predators a & b")
    ok(ut_cmp_identical(ld$maps$predator, c(a = 'a', b = 'b')), "predator_map is 1:1 mapping")

    # Generate a list of predators "predator_(trawl|gil)_(f|m)"
    predators <- lapply(paste(
        'predator',
        rep(c('trawl', 'gil'), each = 2),
        c('is', 'no'),
        sep = "_"), function (x) g3_stock(x, 1))
    ld <- generate_ld("
        age year predator_re number
          3 1999    _is$  1999.3
          4 1999    ^predator_trawl  1999.4
          6 1999    ^predator_gil  1999.6
          3 2000    ^predator_trawl  2000.3
          6 2000    ^predator_gil  2000.6
          4 2001    ^predator_gil  2001.4
          6 2001    ^predator_trawl  2001.6
        ", all_predators = predators)
    ok(ut_cmp_identical(
        dimnames(ld$obs_array$num)[['predator_re']],
        c("_is$", "^predator_trawl", "^predator_gil")), "Array names are regexes")
    ok(ut_cmp_identical(
        ld$maps$predator,
        c(
            predator_trawl_is = '_is$',
            predator_trawl_no = '^predator_trawl',
            predator_gil_is = '_is$',
            predator_gil_no = '^predator_gil' )), "predator map used first regexes first")

    # Generate a list of predators "predator_(trawl|gil)_(f|m)"
    predators <- lapply(paste(
        'predator',
        rep(c('trawl', 'gil'), each = 2),
        c('is', 'no'),
        sep = "_"), function (x) g3_stock(x, 1))
    ld <- generate_ld("
        age year predator_re number
          3 1999    _gil_is$  1999.3
          4 1999    _gil_is$  1999.4
          6 1999    _trawl_is$  1999.6
          3 2000    _gil_is$  2000.3
          6 2000    _trawl_is$  2000.6
          4 2001    _trawl_is$  2001.4
          6 2001    _gil_is$  2001.6
        ", all_predators = predators)
    ok(ut_cmp_identical(
        dimnames(ld$obs_array$num)[['predator_re']],
        c("_gil_is$", "_trawl_is$")), "Array names are regexes")
    ok(ut_cmp_identical(
        ld$maps$predator,
        c(
            predator_trawl_is = '_trawl_is$',
            predator_trawl_no = NA,
            predator_gil_is = '_gil_is$',
            predator_gil_no = NA )), "predator map ignored unused predators")
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
    ok(is.null(ld$maps$fleet), "No fleet column, so no fleet map")

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
        ", all_fleets = list(g3_fleet('a'), g3_fleet('b')))
    ok(ut_cmp_identical(dimnames(ld$obs_array$num)[['fleet']], c("a", "b")), "Array has fleets a & b")
    ok(ut_cmp_identical(ld$maps$fleet, c(a = 'a', b = 'b')), "fleet_map is 1:1 mapping")

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
        dimnames(ld$obs_array$num)[['fleet_re']],
        c("_is$", "^fleet_trawl", "^fleet_gil")), "Array names are regexes")
    ok(ut_cmp_identical(
        ld$maps$fleet,
        c(
            fleet_trawl_is = '_is$',
            fleet_trawl_no = '^fleet_trawl',
            fleet_gil_is = '_is$',
            fleet_gil_no = '^fleet_gil' )), "fleet map used first regexes first")

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
        dimnames(ld$obs_array$num)[['fleet_re']],
        c("_gil_is$", "_trawl_is$")), "Array names are regexes")
    ok(ut_cmp_identical(
        ld$maps$fleet,
        c(
            fleet_trawl_is = '_trawl_is$',
            fleet_trawl_no = NA,
            fleet_gil_is = '_gil_is$',
            fleet_gil_no = NA )), "fleet map ignored unused fleets")
})

ok_group('g3l_likelihood_data:predator') ##########
ld <- generate_ld(expand.grid(
    length = 5:10,
    predator_length = c(10, 50, 100),
    predator_age = c('[0,5)', '[5,10)'), # ((
    predator_tag = c('a', 'b'),
    year = 1999:2000 ))

ok(gadget3:::ut_cmp_code(generate_code(ld, 'stock_iterate', cur_year = 1999, cur_step = 1), quote({
    ut_obs__time_idx <- intlookup_getdefault(ut_obs__times, (cur_year * 100L + cur_step * 0L), -1L)
    if (ut_obs__time_idx >= (1L)) {
        for (ut_obs__predator_tag_idx in seq_along(ut_obs__predator_tag_ids)) {
            predator_tag <- ut_obs__predator_tag_ids[[ut_obs__predator_tag_idx]]
            for (ut_obs__predator_agegroup_idx in seq_along(ut_obs__predator_minages)) {
                predator_age <- ut_obs__predator_minages[[ut_obs__predator_agegroup_idx]]
                for (ut_obs__predator_length_idx in seq_along(ut_obs__predator_midlen)) {
                  predator_length <- ut_obs__predator_midlen[[ut_obs__predator_length_idx]]
                  for (ut_obs__length_idx in seq_along(ut_obs__midlen)) {
                    length <- ut_obs__midlen[[ut_obs__length_idx]]
                    ut_obs__num[ut_obs__length_idx, ut_obs__predator_length_idx,
                      ut_obs__predator_agegroup_idx, ut_obs__predator_tag_idx,
                      ut_obs__time_idx]
                  }
                }
            }
        }
    }
}), optimize = TRUE), "stock_iterate: All predator dimensions included, with prefixed variables")

ok(gadget3:::ut_cmp_code(generate_code(ld, 'stock_intersect', cur_year = 1999, cur_step = 1, predator_tag = 1, predator_length = 1, predator_age = 1), quote({
    ut_obs__time_idx <- intlookup_getdefault(ut_obs__times, (cur_year * 100L + cur_step * 0L), -1L)
    if (ut_obs__time_idx >= 1L) {
        for (ut_obs__predator_tag_idx in seq_along(ut_obs__predator_tag_ids)) {
            predator_tag <- ut_obs__predator_tag_ids[[ut_obs__predator_tag_idx]]
            for (ut_obs__predator_agegroup_idx in seq_along(ut_obs__predator_minages)) {
                `_age` <- ut_obs__predator_minages[[ut_obs__predator_agegroup_idx]]
                for (ut_obs__predator_length_idx in seq_along(ut_obs__predator_minlen)) {
                  predator_length <- ut_obs__predator_midlen[[ut_obs__predator_length_idx]]
                  for (ut_obs__length_idx in seq_along(ut_obs__minlen)) {
                    length <- ut_obs__midlen[[ut_obs__length_idx]]
                    ut_obs__num[ut_obs__length_idx, ut_obs__predator_length_idx,
                      ut_obs__predator_agegroup_idx, ut_obs__predator_tag_idx,
                      ut_obs__time_idx]
                  }
                }
            }
        }
    }
}), optimize = TRUE), "stock_intersect: All predator dimensions included, with prefixed variables")

ok(ut_cmp_equal(
    ld$obsstock$env$stock__midlen,
    gadget3:::force_vector(c("5:6" = 5.5, "6:7" = 6.5, "7:8" = 7.5, "8:9" = 8.5, "9:10" = 9.5, "10:Inf" = 10.5)) ), "stock__midlen: prey vars set")
ok(ut_cmp_equal(
    ld$obsstock$env$stock__predator_midlen,
    gadget3:::force_vector(c("10:50" = 30, "50:100" = 75, "100:Inf" = 120)) ), "stock__predator_midlen: predator vars prefixed")
ok(ut_cmp_equal(
    environment(attr(ld$obsstock$env$ut_obs__predator_agegroup, "g3_global_init_val"))$ut_obs__predator_agegroup_keys,
    gadget3:::force_vector(0:9)), "ut_obs__predator_agegroup_keys: initval keys got renamed")
ok(ut_cmp_equal(
    environment(attr(ld$obsstock$env$ut_obs__predator_agegroup, "g3_global_init_val"))$ut_obs__predator_agegroup_values,
    gadget3:::force_vector( rep(1:2, each = 5) )), "ut_obs__predator_agegroup_values: initval values got renamed")

########## g3l_likelihood_data:predator
