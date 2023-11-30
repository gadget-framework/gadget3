library(unittest)
if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

library(gadget3)

default_pt <- function (sn) data.frame(
        switch = sn,
        value = NA,
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
    value = 4,
    lower = NA,
    upper = NA,
    parscale = NA,
    optimise = FALSE,
    random = FALSE)), "Optimise stays off by default")

ok(ut_cmp_equal(iv_options('x', value = 4, lower = 4), list(
    switch = "x",
    value = 4,
    lower = 4,
    upper = NA,
    parscale = NA,
    optimise = FALSE,
    random = FALSE)), "Optimise stays off with lower set")

ok(ut_cmp_equal(iv_options('x', value = 4, upper = 8), list(
    switch = "x",
    value = 4,
    lower = NA,
    upper = 8,
    parscale = NA,
    optimise = FALSE,
    random = FALSE)), "Optimise stays off with upper set")

ok(ut_cmp_equal(iv_options('x', value = 4, lower = 2, upper = 8), list(
    switch = "x",
    value = 4,
    lower = 2,
    upper = 8,
    parscale = 6,
    optimise = TRUE,
    random = FALSE)), "Lower & upper turn optimise on, set parscale")

ok(ut_cmp_equal(iv_options('x', value = 4, lower = 2, upper = 8, optimise = FALSE), list(
    switch = "x",
    value = 4,
    lower = 2,
    upper = 8,
    parscale = 6,
    optimise = FALSE,
    random = FALSE)), "Lower & upper turn optimise on, can be forced off again")

ok(ut_cmp_equal(iv_options('x', value = 4, optimise = TRUE), list(
    switch = "x",
    value = 4,
    lower = NA,
    upper = NA,
    parscale = NA,
    optimise = TRUE,
    random = FALSE)), "Can turn on optimise without bounds")

ok(ut_cmp_equal(iv_options('x', value = 4, lower = 2, upper = 8, random = TRUE), list(
    switch = "x",
    value = 4,
    lower = 2,
    upper = 8,
    parscale = 6,
    optimise = FALSE,
    random = TRUE)), "random = TRUE --> optimise = FALSE (as you can't have both)")

#### auto_exp

pt <- default_pt(c('moo.1', 'moo.1_exp', 'baa.2', 'baa.2_exp', 'oink.1', 'oink.1_exp'))
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1', 4, auto_exponentiate = TRUE)$value,
    c(4, log(4), NA, NA, 4, log(4))), "log() values that are in _exp columns")
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1', lower = 22, auto_exponentiate = TRUE)$lower,
    c(22, log(22), NA, NA, 22, log(22))), "Can auto_exp lower")
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1', upper = 22, auto_exponentiate = TRUE)$upper,
    c(22, log(22), NA, NA, 22, log(22))), "Can auto_exp upper")
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1', 4, auto_exponentiate = FALSE)$value,
    c(4, NA, NA, NA, 4, NA)), "Can disable auto_exponentiate, values aren't matched")
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1_exp', 8, auto_exponentiate = TRUE)$value,
    c(NA, 8, NA, NA, NA, 8)), "Manual _exp matching still works, no log()")
ok(ut_cmp_equal(
    g3_init_val(pt, '*.1_exp', 8, auto_exponentiate = FALSE)$value,
    c(NA, 8, NA, NA, NA, 8)), "Manual _exp matching still works")

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
