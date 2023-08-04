library(magrittr)
library(unittest)
if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

library(gadget3)

cmp_code <- function (a, b) ut_cmp_identical(deparse(a), deparse(b))

# Generate parameter template for all given parameters (beginning with "ut.")
param_tmpl <- function (...) {
    actions <- c(
        list(g3a_time(1990, 1994)),
        lapply(list(...), function (p) {
            x <- 4
            stock <- g3_stock(c(species = 'ut', sex = 'm', maturity = 'imm'), 1)
            gadget3:::g3_step(gadget3:::f_substitute(
                ~{x <- p},
                list(p = p)))
        }))
    m <- g3_to_tmb(actions)
    pt <- attr(m, 'parameter_template')
    pt[startsWith(rownames(pt), 'ut.'),]
}

stock_mimm <- g3_stock(c(species = 'st', sex = 'm', maturity = 'imm'), seq(10, 35, 5)) %>% g3s_age(1, 5)
stock_mmat <- g3_stock(c(species = 'st', sex = 'm', maturity = 'mat'), seq(10, 35, 5)) %>% g3s_age(3, 7)
stock_fimm <- g3_stock(c(species = 'st', sex = 'f', maturity = 'imm'), seq(10, 35, 5)) %>% g3s_age(1, 5)
stock_fmat <- g3_stock(c(species = 'st', sex = 'f', maturity = 'mat'), seq(10, 35, 5)) %>% g3s_age(3, 7)
# Put x through g3_step() as a g3 action would, the "stock" being stock_mimm
pretend_stock_action <- function (x) {
    rlang::f_rhs(gadget3:::g3_step(gadget3:::call_to_formula(
        x,
        env = list2env(list(stock = stock_mimm), parent = baseenv()))))
}

ok(cmp_code(
    call("{",  # }
        g3_parameterized('parp', by_stock = FALSE),
    NULL), quote({
        g3_param("parp")
    NULL})), "Not 'by' anything, so just a regular parameter")

ok(cmp_code(
    call("{",  # }
        g3_parameterized('parp', by_stock = FALSE, exponentiate = TRUE, offset = 5),
        g3_parameterized('parp', by_stock = FALSE, scale = 0.001, offset = 2),
        g3_parameterized('parp_avz', by_stock = FALSE, scale = 0.2, offset = 4, avoid_zero = TRUE),
    NULL), quote({
        exp(g3_param("parp_exp")) + 5
        0.001 * g3_param("parp") + 2
        avoid_zero(0.2 * g3_param("parp_avz")) + 4
    NULL})), "Can wrap with exp(), scale, offset, avoid_zero")

ok(cmp_code(
    call("{",  # }
        g3_parameterized('parp', by_stock = FALSE, value = 4, lower = 2, upper = 9),
        g3_parameterized('parp', by_stock = FALSE, optimise = FALSE),
        g3_parameterized('parp', by_stock = FALSE, random = TRUE),
        g3_parameterized('parp', by_year = TRUE, ifmissing = g3_parameterized('peep')),
    NULL), quote({
        g3_param("parp", value = 4, lower = 2, upper = 9)
        g3_param("parp", optimise = FALSE)
        g3_param("parp", random = TRUE)
        g3_param_table("parp", expand.grid(cur_year = seq(start_year, end_year)), ifmissing = g3_param("peep"))
    NULL})), "Extra parameters passed through")

ok(cmp_code(
    pretend_stock_action(g3_parameterized('parp', by_stock = TRUE)),
    quote(g3_param("st_m_imm.parp"))), "by_stock, so will use stock_prepend() to rename variables")

ok(ut_cmp_error(
    g3_parameterized('parp', by_stock = FALSE, by_age = TRUE),
    'by_age'), "!by_stock and by_age is nonsensical, throws error")

ok(cmp_code(
    pretend_stock_action(call("{",  # }
        g3_parameterized('parp', by_stock = TRUE, by_year = TRUE),
        g3_parameterized('parp', by_stock = TRUE, by_year = TRUE, by_age = TRUE),
    NULL)), quote({
        g3_param_table("st_m_imm.parp", expand.grid(
            cur_year = seq(start_year, end_year)))
        g3_param_table("st_m_imm.parp", expand.grid(
            cur_year = seq(start_year, end_year),
            age = seq(st_m_imm__minage, st_m_imm__maxage)))
    NULL})), "Adding by_year or by_age turns it into a table")

ok(cmp_code(
    pretend_stock_action(call("{",  # }
        g3_parameterized('yr', by_year = TRUE),
        g3_parameterized('st', by_step = TRUE),
        g3_parameterized('yrst', by_year = TRUE, by_step = TRUE),
    NULL)), quote({
    g3_param_table("yr", expand.grid(
        cur_year = seq(start_year, end_year)))
    g3_param_table("st", expand.grid(
        cur_step = seq_along(step_lengths)))
    g3_param_table("yrst", expand.grid(
        cur_year = seq(start_year, end_year),
        cur_step = seq_along(step_lengths)))
    NULL})), "by_year & by_step can be combined")

ok(cmp_code(
    pretend_stock_action(call("{",  # }
        g3_parameterized('parp', by_stock = 'species', lower = 3),
        g3_parameterized('parp', by_stock = c('species', 'sex'), lower = 3),
        g3_parameterized('parp', by_stock = 'sex', by_year = TRUE),
    NULL)), quote({
        g3_param("st.parp", lower = 3)
        g3_param("st_m.parp", lower = 3)
        g3_param_table("m.parp", expand.grid(cur_year = seq(start_year, end_year)))
    NULL})), "Can specify which name_part should be used in the name")

ok(cmp_code(
    call("{",  # }
        g3_parameterized('parp', by_stock = list(stock_mimm, stock_mmat)),
        g3_parameterized('parp', by_stock = list(stock_mimm, stock_fmat)),  # M vs F, so only species matches
        g3_parameterized('parp', by_stock = list(stock_fimm, stock_fmat), by_age = TRUE),
    NULL), quote({
        g3_param("st.m.parp")
        g3_param("st.parp")
        g3_param_table("st.f.parp", expand.grid(
            age = seq(min(st_f_imm__minage, st_f_mat__minage), max(st_f_imm__maxage, st_f_mat__maxage))))
    NULL})), "Can give a list of stocks, in which case it works out name parts for you")

ok(cmp_code(
    call("{",  # }
        g3_parameterized('rec', by_stock = 'species', scale = 'rec.scalar'),
        g3_parameterized('rec', by_stock = 'species', scale = 'rec.scalar', offset = 'rec.offset'),
        g3_parameterized('rec', by_stock = 'species', by_age = TRUE, scale = 'rec.scalar'),
    NULL), quote({
        stock_prepend(stock, g3_param("rec.scalar"), name_part = "species") * stock_prepend(stock, g3_param("rec"), name_part = "species")
        stock_prepend(stock, g3_param("rec.scalar"), name_part = "species") * stock_prepend(stock, g3_param("rec"), name_part = "species") + stock_prepend(stock, g3_param("rec.offset"), name_part = "species")
        stock_prepend(stock, g3_param("rec.scalar"), name_part = "species") * stock_prepend(stock,
            g3_param_table("rec", expand.grid(age = seq(stock__minage, stock__maxage))), name_part = "species")
    NULL})), "scale / offset can be character, in which case they are also a param. Only by_stock is honoured though")

year_range <- 1982:1986
ok(ut_cmp_identical(param_tmpl(
    g3_parameterized('ut.param.def', by_year = TRUE),
    g3_parameterized('ut.param.var', by_year = year_range),
    g3_parameterized('ut.param.custom', by_year = 1999:2004))$switch, c(
    "ut.param.def.1990", "ut.param.def.1991", "ut.param.def.1992", "ut.param.def.1993", "ut.param.def.1994",
    "ut.param.var.1982", "ut.param.var.1983", "ut.param.var.1984", "ut.param.var.1985", "ut.param.var.1986",
    "ut.param.custom.1999", "ut.param.custom.2000", "ut.param.custom.2001",
    "ut.param.custom.2002", "ut.param.custom.2003", "ut.param.custom.2004",
    NULL)), "by_year: Can customise ranges")
