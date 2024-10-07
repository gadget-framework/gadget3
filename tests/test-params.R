library(magrittr)
library(unittest)
if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

library(gadget3)

cmp_code <- function (a, b) ut_cmp_identical(deparse(a), deparse(b))

# Generate parameter template for all given parameters (beginning with "ut.")
param_model <- function (...) {
    areas <- g3_areas(c('a', 'b', 'c'))
    stock <- g3_stock(c(species = 'had', sex = 'm', maturity = 'imm'), 1) %>% g3s_age(1, 5) %>% g3s_livesonareas(areas[c('a', 'c')])
    predstock <- g3_fleet(c(country = 'is', 'comm')) %>% g3s_age(1, 5) %>% g3s_livesonareas(areas[c('a', 'b', 'c')])
    predprey <- gadget3:::g3s_stockproduct(stock, pred = predstock)
    actions <- c(
        list(g3a_time(1990, 1994)),
        lapply(list(...), function (p) {
            if (is.null(p)) return(~{})
            stock <- stock
            predstock <- predstock
            predprey <- predprey
            predprey__num <- g3_stock_instance(predprey)
            gadget3:::g3_step(gadget3:::f_substitute(
                ~stock_iterate(stock, stock_interact(predstock, stock_with(predprey, stock_ss(predprey__num, vec = single) <- p), prefix = "pred")),
                list(p = p)))
        }),
        list(gadget3:::g3_step(~{
            stock_with(predprey, REPORT(predprey__num))
        })) )
    return(actions)
}
param_tmpl <- function (...) {
    actions <- param_model(...)
    m <- g3_to_tmb(actions)
    pt <- attr(m, 'parameter_template')
    pt[grepl('paramut', rownames(pt), fixed = TRUE),]
}

areas <- g3_areas(c('a', 'b', 'c'))
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

##### g3_parameterized_breakdown
ok(ut_cmp_identical(
    gadget3:::g3_parameterized_breakdown(g3_parameterized('par', by_year = TRUE)),
    c("cur_year"),
    filter = NULL), "g3_parameterized_breakdown: by_year")
ok(ut_cmp_identical(
    gadget3:::g3_parameterized_breakdown(g3_parameterized('par', by_year = TRUE, by_step = TRUE)),
    c("cur_year", "cur_step"),
    filter = NULL), "g3_parameterized_breakdown: by_year, by_step")
ok(ut_cmp_identical(
    gadget3:::g3_parameterized_breakdown(g3_parameterized('par', by_stock = TRUE)),
    c("stock"),
    filter = NULL), "g3_parameterized_breakdown: by_stock")
ok(ut_cmp_identical(
    gadget3:::g3_parameterized_breakdown(g3_parameterized('par', by_stock = TRUE, by_year = TRUE)),
    c("stock", "cur_year"),
    filter = NULL), "g3_parameterized_breakdown: by_stock, by_year")
ok(ut_cmp_identical(
    gadget3:::g3_parameterized_breakdown(g3_parameterized('par', by_stock = TRUE, by_year = TRUE, exponentiate = TRUE)),
    c("stock", "cur_year"),
    filter = NULL), "g3_parameterized_breakdown: by_stock, by_year, exponentiate")
ok(ut_cmp_identical(
    gadget3:::g3_parameterized_breakdown(g3_parameterized('par', by_stock = TRUE, scale = 4, avoid_zero = TRUE)),
    c("stock"),
    filter = NULL), "g3_parameterized_breakdown: by_stock, scale, avoid_zero")
ok(ut_cmp_identical(
    gadget3:::g3_parameterized_breakdown(g3_parameterized('par', by_stock = TRUE, offset = 4)),
    c("stock"),
    filter = NULL), "g3_parameterized_breakdown: by_stock, offset")

#### g3_parameterized

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
        g3_param("parp") * 0.001 + 2
        avoid_zero(g3_param("parp_avz") * 0.2) + 4
    NULL})), "Can wrap with exp(), scale, offset, avoid_zero")

ok(cmp_code(
    call("{",  # }
        g3_parameterized('byst', by_stock = TRUE, ifmissing = "def.byst"),
        g3_parameterized('nby', by_stock = FALSE, ifmissing = "def.nby"),
        g3_parameterized('parp', by_year = TRUE, ifmissing = g3_parameterized('peep')),
    NULL), quote({
        stock_prepend(stock, g3_param(
            "byst",
            ifmissing = stock_prepend(stock, g3_param("def.byst"), name_part = NULL)
        ), name_part = NULL)
        g3_param("nby", ifmissing = g3_param("def.nby"))
        g3_param_table("parp", expand.grid(cur_year = seq(start_year, end_year)), ifmissing = g3_param("peep"))
    NULL})), "ifmissing can be character (and gets assigned a parameter)")

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
            age = seq(st_m_imm__minage, st_m_imm__maxage))) + 0 * age
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
        # No common parts, so concatenate everything after sorting
        g3_parameterized('nocommon', by_stock = list(g3_stock(c("zz", "b"), 1), g3_stock(c("c", "d"), 1))),
    NULL), quote({
        stock_prepend("st.m", g3_param("parp"))
        stock_prepend("st", g3_param("parp"))
        stock_prepend("st.f", g3_param_table("parp", expand.grid(
            age = seq(min(st_f_imm__minage, st_f_mat__minage), max(st_f_imm__maxage, st_f_mat__maxage))))) + 0 * age
        stock_prepend("c_d.zz_b", g3_param("nocommon"))
    NULL})), "Can give a list of stocks, in which case it works out name parts for you")

ok(cmp_code(
    call("{",  # }
        g3_parameterized('rec', by_stock = 'species', scale = 'rec.scalar'),
        g3_parameterized('rec', by_stock = 'species', scale = 'rec.scalar', offset = 'rec.offset'),
        g3_parameterized('rec', by_stock = 'species', by_age = TRUE, scale = 'rec.scalar'),
    NULL), quote({
        stock_prepend(stock, g3_param("rec"), name_part = "species") * stock_prepend(stock, g3_param("rec.scalar"), name_part = "species")
        stock_prepend(stock, g3_param("rec"), name_part = "species") * stock_prepend(stock, g3_param("rec.scalar"), name_part = "species") + stock_prepend(stock, g3_param("rec.offset"), name_part = "species")
        stock_prepend(stock, g3_param_table("rec", expand.grid(age = seq(stock__minage, stock__maxage))), name_part = "species") * stock_prepend(stock, g3_param("rec.scalar"), name_part = "species") + 0 * age
    NULL})), "scale / offset can be character, in which case they are also a param. Only by_stock is honoured though")

year_range <- 1982:1986
ok(ut_cmp_identical(param_tmpl(
    g3_parameterized('paramut.def', by_year = TRUE),
    g3_parameterized('paramut.var', by_year = year_range),
    g3_parameterized('paramut.custom', by_year = 1999:2004),
    NULL)$switch, c(
    "paramut.def.1990", "paramut.def.1991", "paramut.def.1992", "paramut.def.1993", "paramut.def.1994",
    "paramut.var.1982", "paramut.var.1983", "paramut.var.1984", "paramut.var.1985", "paramut.var.1986",
    "paramut.custom.1999", "paramut.custom.2000", "paramut.custom.2001",
    "paramut.custom.2002", "paramut.custom.2003", "paramut.custom.2004")), "by_year: Can customise ranges")

ok(ut_cmp_identical(param_tmpl(
    g3_parameterized('paramut.fleet', by_predator = TRUE),
    g3_parameterized('paramut.stockfleet', by_predator = TRUE, by_stock = TRUE),
    g3_parameterized('paramut.stockfleetcty', by_predator = "country", by_stock = TRUE),
    NULL)$switch, c(
    "is_comm.paramut.fleet",
    "had_m_imm.is_comm.paramut.stockfleet",
    "had_m_imm.is.paramut.stockfleetcty",
    NULL)), "by_predator: Can combine with by_stock")

ok_group("by_area") ##########
ok(ut_cmp_identical(param_tmpl(
    g3_parameterized('paramut', by_stock = TRUE, by_area = TRUE, by_year = TRUE),
    NULL)$switch, c(
        "had_m_imm.paramut.1990.a", "had_m_imm.paramut.1991.a", "had_m_imm.paramut.1992.a",
        "had_m_imm.paramut.1993.a", "had_m_imm.paramut.1994.a",
        "had_m_imm.paramut.1990.c", "had_m_imm.paramut.1991.c", "had_m_imm.paramut.1992.c",
        "had_m_imm.paramut.1993.c", "had_m_imm.paramut.1994.c" )), "by_area: Generate parameters with area name")

actions <- param_model(g3_parameterized('paramut', by_stock = TRUE, by_area = TRUE, by_year = TRUE))
model_fn <- g3_to_r(actions)
params <- attr(model_fn, 'parameter_template')
params[grepl('param', names(params))] <- 100 + seq_along(grep('param', names(params)))
ok(ut_cmp_equal(attr(model_fn(params), "had_m_imm_is_comm__num")[,age='age1',,pred_age='age1',], array(
    c(params[["had_m_imm.paramut.1994.a"]], NA, NA, NA, NA, params[["had_m_imm.paramut.1994.c"]]),
    dim = c(area = 2L, pred_area = 3L),
    dimnames = list(area = c("a", "c"), pred_area = c("a", "b", "c")) )), "Model used areas by area name")

ok(ut_cmp_identical(param_tmpl(
    g3_parameterized('paramut.fleet', by_predator = TRUE, by_area = TRUE),
    NULL)$switch, c(
        "is_comm.paramut.fleet.a",
        "is_comm.paramut.fleet.b",
        "is_comm.paramut.fleet.c",
        NULL )), "by_predator/by_area: Used predator areas instead of stock")

########## by_area
