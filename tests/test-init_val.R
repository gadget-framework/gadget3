library(unittest)
if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

library(gadget3)

default_pt <- function (sn) data.frame(
        switch = sn,
        value = I(as.list( rep(NA, length(sn)) )),
        lower = NA,
        upper = NA,
        parscale = NA,
        optimise = FALSE,
        random = FALSE)

name_spec_matched <- function (name_spec, all_names) {
    l <- as.list(rep(0, length(all_names)))
    names(l) <- all_names
    out <- unlist(g3_init_val(l, name_spec, value = 1))
    out <- names(out[out == 1])
    if (length(out) == 0) return(NULL)  # So we can compare to c()
    out
}

# Set single value with likely-default options
iv_options <- function (sn, ...) {
    as.list(g3_init_val(default_pt(sn), sn, ...))
}

sample_names = c(
    paste('ling', 'imm', 'm', sep = "."),
    paste('ling', 'mat', 'm', sep = "."),
    paste('ling', 'imm', 'm', 0:3, sep = "."),
    paste('ling', 'mat', 'm', 0:5, sep = "."),
    paste('ling', 'imm', 'init', 0:3, sep = "."),
    paste('ling', 'mat', 'init', 0:5, sep = "."),
    paste('ling', 'imm', 'rec', 0:3, sep = "."),
    paste('ling', 'mat', 'rec', 0:5, sep = "."),
    NULL)

#### name_spec matching behaviour

ok(ut_cmp_equal(
    suppressWarnings(name_spec_matched('ling.imm', sample_names)),
    c()), "Partial matches do nothing")
ok(ut_cmp_equal(
    suppressWarnings(name_spec_matched('imm.m', sample_names)),
    c()), "Partial matches do nothing")
ok(ut_cmp_equal(
    name_spec_matched('ling.imm.m', sample_names),
    c('ling.imm.m')), "Partial matches do nothing (but do match whole)")

ok(ut_cmp_equal(
    name_spec_matched('moo(c)', c("moo(c)", "mooc")),
    c('moo(c)')), "Regex in parts escaped")

ok(ut_cmp_equal(name_spec_matched('ling.imm.*.*', sample_names), c(
    'ling.imm.m.0', 'ling.imm.m.1', 'ling.imm.m.2', 'ling.imm.m.3',
    'ling.imm.init.0', 'ling.imm.init.1', 'ling.imm.init.2', 'ling.imm.init.3',
    'ling.imm.rec.0', 'ling.imm.rec.1', 'ling.imm.rec.2', 'ling.imm.rec.3',
    NULL)), "* matches strings and numeric")
ok(ut_cmp_equal(name_spec_matched('ling.*m*.*.1', sample_names), c(
    'ling.imm.m.1', 'ling.mat.m.1',
    'ling.imm.init.1', 'ling.mat.init.1',
    'ling.imm.rec.1', 'ling.mat.rec.1',
    NULL)), "* matches variable lengths")
ok(ut_cmp_equal(
    name_spec_matched('ling.m*t.m*.1', sample_names),
    c('ling.mat.m.1')), "* can be used to partially match")
ok(ut_cmp_equal(name_spec_matched('ling.*m*.*.1', sample_names), c(
    'ling.imm.m.1', 'ling.mat.m.1',
    'ling.imm.init.1', 'ling.mat.init.1',
    'ling.imm.rec.1', 'ling.mat.rec.1',
    NULL)), "* can be used to match multiple times")

ok(ut_cmp_equal(
    name_spec_matched('ling.imm.m.#', sample_names),
    c('ling.imm.m.0', 'ling.imm.m.1', 'ling.imm.m.2', 'ling.imm.m.3')), "# matches numeric field")
ok(ut_cmp_equal(
    suppressWarnings(name_spec_matched('ling.imm.#.1', sample_names)),
    c()), "# ignores non-numeric")

ok(ut_cmp_equal(name_spec_matched('ling.*m*.i*t|rec.#', sample_names), c(
    'ling.imm.init.0', 'ling.imm.init.1', 'ling.imm.init.2', 'ling.imm.init.3',
    'ling.mat.init.0', 'ling.mat.init.1', 'ling.mat.init.2', 'ling.mat.init.3', 'ling.mat.init.4', 'ling.mat.init.5',
    'ling.imm.rec.0', 'ling.imm.rec.1', 'ling.imm.rec.2', 'ling.imm.rec.3',
    'ling.mat.rec.0', 'ling.mat.rec.1', 'ling.mat.rec.2', 'ling.mat.rec.3', 'ling.mat.rec.4', 'ling.mat.rec.5',
    NULL)), "Pipe scoped to work within section, wildcards work within")

ok(ut_cmp_equal(
    name_spec_matched('moo.[2-18]', paste0("moo.", 1:40)),
    paste0("moo.", seq(2, 18))), "Range match 2--18 (but not 20)")

out <- as.list(rep(0, 10))
names(out) <- paste0('moo.', seq_along(out) - 1)
ok(ut_cmp_equal(g3_init_val(out, 'moo.[3-7]', 13:17), list(
    "moo.0" = 0,
    "moo.1" = 0,
    "moo.2" = 0,
    "moo.3" = 13,
    "moo.4" = 14,
    "moo.5" = 15,
    "moo.6" = 16,
    "moo.7" = 17,
    "moo.8" = 0,
    "moo.9" = 0)), "Can apply values with a vector")

#### data.frame option handling

ok(ut_cmp_equal(iv_options('x', value = 4), list(
    switch = "x",
    value = I(list(4)),
    lower = NA,
    upper = NA,
    parscale = NA,
    optimise = FALSE,
    random = FALSE)), "Optimise stays off by default")

ok(ut_cmp_equal(iv_options('x', value = 4, lower = 4), list(
    switch = "x",
    value = I(list(4)),
    lower = 4,
    upper = NA,
    parscale = NA,
    optimise = FALSE,
    random = FALSE)), "Optimise stays off with lower set")

ok(ut_cmp_equal(iv_options('x', value = 4, upper = 8), list(
    switch = "x",
    value = I(list(4)),
    lower = NA,
    upper = 8,
    parscale = NA,
    optimise = FALSE,
    random = FALSE)), "Optimise stays off with upper set")

ok(ut_cmp_equal(iv_options('x', value = 4, lower = 2, upper = 8), list(
    switch = "x",
    value = I(list(4)),
    lower = 2,
    upper = 8,
    parscale = 6,
    optimise = TRUE,
    random = FALSE)), "Lower & upper turn optimise on, set parscale")

ok(ut_cmp_equal(iv_options('x', value = 4, lower = 2, upper = 8, optimise = FALSE), list(
    switch = "x",
    value = I(list(4)),
    lower = 2,
    upper = 8,
    parscale = 6,
    optimise = FALSE,
    random = FALSE)), "Lower & upper turn optimise on, can be forced off again")

ok(ut_cmp_equal(iv_options('x', value = 4, optimise = TRUE), list(
    switch = "x",
    value = I(list(4)),
    lower = NA,
    upper = NA,
    parscale = NA,
    optimise = TRUE,
    random = FALSE)), "Can turn on optimise without bounds")

ok(ut_cmp_equal(iv_options('x', value = 4, lower = 2, upper = 8, random = TRUE), list(
    switch = "x",
    value = I(list(4)),
    lower = 2,
    upper = 8,
    parscale = 6,
    optimise = FALSE,
    random = TRUE)), "random = TRUE --> optimise = FALSE (as you can't have both)")

#### auto_exp

pt <- default_pt(c('moo.1', 'moo.1_exp', 'baa.2', 'baa.2_exp', 'oink.1', 'oink.1_exp'))
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1', 4, auto_exponentiate = TRUE)$value,
    I(list(4, log(4), NA, NA, 4, log(4)))), "log() values that are in _exp columns")
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1', lower = 22, auto_exponentiate = TRUE)$lower,
    c(22, log(22), NA, NA, 22, log(22))), "Can auto_exp lower")
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1', upper = 22, auto_exponentiate = TRUE)$upper,
    c(22, log(22), NA, NA, 22, log(22))), "Can auto_exp upper")
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1', 4, auto_exponentiate = FALSE)$value,
    I(list(4, NA, NA, NA, 4, NA))), "Can disable auto_exponentiate, values aren't matched")
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1_exp', 8, auto_exponentiate = TRUE)$value,
    I(list(NA, 8, NA, NA, NA, 8))), "Manual _exp matching still works, no log()")
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1_exp', 8, auto_exponentiate = FALSE)$value,
    I(list(NA, 8, NA, NA, NA, 8))), "Manual _exp matching still works")

#### Warning

cmp_contains <- function (a, b) {
    if (grepl(a, b, fixed = TRUE)) return(TRUE)
    return(c(a, " not in ", b))
}
captureWarning <- function (x) {
    tryCatch(
        list(x, warning = ""),
        warning = function (w) list(suppressWarnings(x), warning = w$message))
}

pt <- default_pt(c('moo.1', 'moo.1_exp', 'baa.2', 'baa.2_exp', 'oink.1', 'oink.1_exp'))
out <- captureWarning(g3_init_val(pt, "neigh.#", value = 9))
ok(ut_cmp_identical(pt, out[[1]]), "Non-matching g3_init_val makes no modification")
ok(cmp_contains("neigh.#", out$warning), "name_spec in warning output")

#### test with a real parameter template

params.in <- attr(g3_to_tmb(list( g3a_time(1980L, 2000L), g3_formula(
    quote(d + e + f + g + h),
    d = g3_parameterized('par.years', value = 0, by_year = TRUE),
    e = g3_parameterized('par.yrs.exp', value = 0, by_year = TRUE, exponentiate = TRUE),
    f = g3_parameterized('pare', value = 1),
    g = g3_parameterized('par.a', value = 2),
    h = g3_parameterized('par.b', value = 3, exponentiate = TRUE),
    x = NA) )), 'parameter_template')

params.in <- g3_init_val(params.in, 'par.years.#', value = 99, optimise = FALSE)
params.in <- g3_init_val(params.in, 'par.yrs.exp.#', value = 100, optimise = FALSE)
params.in <- g3_init_val(params.in, 'par.yrs.exp.1999', value = 9, optimise = FALSE)
params.in <- g3_init_val(params.in, 'par.years.[1986-1994]', value = 11:19, lower = 1:9, upper = 101:109)
params.in <- g3_init_val(params.in, 'par.a|b', value = 100, spread = 0.1)

ok(ut_cmp_equal(params.in$value, I(list(
    retro_years = 0, project_years = 0,
    par.years.1980 = 99, par.years.1981 = 99, par.years.1982 = 99, par.years.1983 = 99, par.years.1984 = 99, par.years.1985 = 99,
    par.years.1986 = 11L, par.years.1987 = 12L, par.years.1988 = 13L,
    par.years.1989 = 14L, par.years.1990 = 15L, par.years.1991 = 16L,
    par.years.1992 = 17L, par.years.1993 = 18L, par.years.1994 = 19L,
    par.years.1995 = 99, par.years.1996 = 99, par.years.1997 = 99, par.years.1998 = 99,
    par.years.1999 = 99, par.years.2000 = 99,
    par.yrs.exp.1980_exp = log(100), par.yrs.exp.1981_exp = log(100), par.yrs.exp.1982_exp = log(100),
    par.yrs.exp.1983_exp = log(100), par.yrs.exp.1984_exp = log(100), par.yrs.exp.1985_exp = log(100),
    par.yrs.exp.1986_exp = log(100), par.yrs.exp.1987_exp = log(100), par.yrs.exp.1988_exp = log(100),
    par.yrs.exp.1989_exp = log(100), par.yrs.exp.1990_exp = log(100), par.yrs.exp.1991_exp = log(100),
    par.yrs.exp.1992_exp = log(100), par.yrs.exp.1993_exp = log(100), par.yrs.exp.1994_exp = log(100),
    par.yrs.exp.1995_exp = log(100), par.yrs.exp.1996_exp = log(100), par.yrs.exp.1997_exp = log(100),
    par.yrs.exp.1998_exp = log(100), par.yrs.exp.1999_exp = log(9), par.yrs.exp.2000_exp = log(100),
    pare = 1,
    par.a = 100,
    par.b_exp = log(100) ))), "params.in$value: Applied vector, wildcard, auto_exp")

ok(ut_cmp_equal(structure(params.in$lower, names = params.in$switch), c(
    retro_years = NA, project_years = NA,
    par.years.1980 = NA, par.years.1981 = NA, par.years.1982 = NA, par.years.1983 = NA,
    par.years.1984 = NA, par.years.1985 = NA,
    par.years.1986 = 1, par.years.1987 = 2, par.years.1988 = 3,
    par.years.1989 = 4, par.years.1990 = 5, par.years.1991 = 6,
    par.years.1992 = 7, par.years.1993 = 8, par.years.1994 = 9,
    par.years.1995 = NA, par.years.1996 = NA, par.years.1997 = NA,
    par.years.1998 = NA, par.years.1999 = NA, par.years.2000 = NA,
    par.yrs.exp.1980_exp = NA, par.yrs.exp.1981_exp = NA, par.yrs.exp.1982_exp = NA,
    par.yrs.exp.1983_exp = NA, par.yrs.exp.1984_exp = NA, par.yrs.exp.1985_exp = NA,
    par.yrs.exp.1986_exp = NA, par.yrs.exp.1987_exp = NA, par.yrs.exp.1988_exp = NA,
    par.yrs.exp.1989_exp = NA, par.yrs.exp.1990_exp = NA, par.yrs.exp.1991_exp = NA,
    par.yrs.exp.1992_exp = NA, par.yrs.exp.1993_exp = NA, par.yrs.exp.1994_exp = NA,
    par.yrs.exp.1995_exp = NA, par.yrs.exp.1996_exp = NA, par.yrs.exp.1997_exp = NA,
    par.yrs.exp.1998_exp = NA, par.yrs.exp.1999_exp = NA, par.yrs.exp.2000_exp = NA,
    pare = NA,
    par.a = 90,
    par.b_exp = log(90) )), "params.in$lower: Applied vector, auto_exp")

ok(ut_cmp_equal(structure(params.in$upper, names = params.in$switch), c(
    retro_years = NA, project_years = NA, par.years.1980 = NA,
    par.years.1981 = NA, par.years.1982 = NA, par.years.1983 = NA,
    par.years.1984 = NA, par.years.1985 = NA,
    par.years.1986 = 101, par.years.1987 = 102, par.years.1988 = 103,
    par.years.1989 = 104, par.years.1990 = 105, par.years.1991 = 106,
    par.years.1992 = 107, par.years.1993 = 108, par.years.1994 = 109,
    par.years.1995 = NA,
    par.years.1996 = NA, par.years.1997 = NA, par.years.1998 = NA,
    par.years.1999 = NA, par.years.2000 = NA,
    par.yrs.exp.1980_exp = NA, par.yrs.exp.1981_exp = NA, par.yrs.exp.1982_exp = NA,
    par.yrs.exp.1983_exp = NA, par.yrs.exp.1984_exp = NA, par.yrs.exp.1985_exp = NA,
    par.yrs.exp.1986_exp = NA, par.yrs.exp.1987_exp = NA, par.yrs.exp.1988_exp = NA,
    par.yrs.exp.1989_exp = NA, par.yrs.exp.1990_exp = NA, par.yrs.exp.1991_exp = NA,
    par.yrs.exp.1992_exp = NA, par.yrs.exp.1993_exp = NA, par.yrs.exp.1994_exp = NA,
    par.yrs.exp.1995_exp = NA, par.yrs.exp.1996_exp = NA, par.yrs.exp.1997_exp = NA,
    par.yrs.exp.1998_exp = NA, par.yrs.exp.1999_exp = NA, par.yrs.exp.2000_exp = NA,
    pare = NA,
    par.a = 110,
    par.b_exp = log(110) )), "params.in$upper: Applied vector, auto_exp")
