library(unittest)
if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

library(gadget3)

cmp_code <- function (a, b) ut_cmp_identical(deparse(a), deparse(b))

stock_mimm <- g3_stock(c(species = 'st', sex = 'm', maturity = 'imm'), seq(10, 35, 5)) |> g3s_age(1, 5)
stock_mmat <- g3_stock(c(species = 'st', sex = 'm', maturity = 'mat'), seq(10, 35, 5)) |> g3s_age(3, 7)
stock_fimm <- g3_stock(c(species = 'st', sex = 'f', maturity = 'imm'), seq(10, 35, 5)) |> g3s_age(1, 5)
stock_fmat <- g3_stock(c(species = 'st', sex = 'f', maturity = 'mat'), seq(10, 35, 5)) |> g3s_age(3, 7)
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
    NULL), quote({
        exp(g3_param("parp")) + 5
        0.001 * g3_param("parp") + 2
    NULL})), "Can wrap with exp(), scale, offset")

ok(cmp_code(
    call("{",  # }
        g3_parameterized('parp', by_stock = FALSE, value = 4, lower = 2, upper = 9),
        g3_parameterized('parp', by_stock = FALSE, optimise = FALSE),
        g3_parameterized('parp', by_stock = FALSE, random = TRUE),
    NULL), quote({
        g3_param("parp", value = 4, lower = 2, upper = 9)
        g3_param("parp", optimise = FALSE)
        g3_param("parp", random = TRUE)
    NULL})), "Extra parameters passed through")

ok(cmp_code(
    pretend_stock_action(g3_parameterized('parp', by_stock = TRUE)),
    quote(g3_param("st_m_imm.parp"))), "by_stock, so will use stock_param() to rename variables")

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
        stock_param(stock, "rec.scalar", name_part = "species") * stock_param(stock,
            "rec", name_part = "species")
        stock_param(stock, "rec.scalar", name_part = "species") * stock_param(stock,
            "rec", name_part = "species") + stock_param(stock, "rec.offset", name_part = "species")
        stock_param(stock, "rec.scalar", name_part = "species") * stock_param_table(stock,
            "rec", name_part = "species", expand.grid(age = seq(stock__minage, stock__maxage)))
    NULL})), "scale / offset can be character, in which case they are also a param. Only by_stock is honoured though")
