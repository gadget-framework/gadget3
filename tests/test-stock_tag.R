library(magrittr)
library(unittest)

library(gadget3)

tag_ids <- function (s) g3_stock_def(s, 'tag_ids')
untagged_idx <- function (s) g3_stock_def(s, 'untagged_idx')

s <- g3_stock('gelda', c(1)) %>% g3s_tag(c(x = 1L, y = 2L))
ok(ut_cmp_identical(
    tag_ids(s),
    gadget3:::force_vector("untagged" = 0L, "x" = 1L, "y" = 2L)), "Added 'untagged' id in")
ok(ut_cmp_identical(untagged_idx(s), quote(g3_idx(1L))), "Untagged is first item")

s <- g3_stock('gelda', c(1)) %>% g3s_tag(c(x = 1L, y = 2L), force_untagged = FALSE)
ok(ut_cmp_identical(
    tag_ids(s),
    gadget3:::force_vector("x" = 1L, "y" = 2L)), "force_untagged off, 'untagged' not added")
ok(ut_cmp_identical(untagged_idx(s), NA), "No untagged item")

s <- g3_stock('gelda', c(1)) %>% g3s_tag(c(nowt = 0L, x = 1L, y = 2L))
ok(ut_cmp_identical(
    tag_ids(s),
    gadget3:::force_vector("nowt" = 0L, "x" = 1L, "y" = 2L)), "Existing untagged not disturbed")
ok(ut_cmp_identical(untagged_idx(s), quote(g3_idx(1L))), "Untagged is first item")

s <- g3_stock('gelda', c(1)) %>% g3s_tag(c(y = 2L, nowt = 0L, x = 1L))
ok(ut_cmp_identical(
    tag_ids(s),
    gadget3:::force_vector("y" = 2L, "nowt" = 0L, "x" = 1L)), "Existing untagged is not disturbed")
ok(ut_cmp_identical(untagged_idx(s), quote(g3_idx(2L))), "Untagged is middle item")

tags <- c(a = 1L, b = 2L, c = 3L, d = 4L)
stock_notag <- g3_stock('stock_notag', c(10))
stock_a <- g3_stock('stock_a', c(10)) %>% g3s_tag(tags[c('a')])
stock_ac <- g3_stock('stock_ac', c(10)) %>% g3s_tag(tags[c('a', 'c')])
stock_bcd <- g3_stock('stock_bcd', c(10)) %>% g3s_tag(tags[c('b', 'c', 'd')])

actions <- list(
    g3a_time(1999, 1999),
    g3a_otherfood(stock_notag, ~1 + stock_ac__minlen, 0),
    # NB: Roll our own so we get something in all tags
    "0:stock_a" = gadget3:::g3_step(g3_formula({
        stock_iterate(stock_a, stock_ss(stock_a__num) <- tag * 100 + stock_a__minlen)
    }, stock_a = stock_a, stock_a__num = g3_stock_instance(stock_a, 0))),
    "0:stock_ac" = gadget3:::g3_step(g3_formula({
        stock_iterate(stock_ac, stock_ss(stock_ac__num) <- tag * 100 + stock_ac__minlen)
    }, stock_ac = stock_ac, stock_ac__num = g3_stock_instance(stock_ac, 0))),
    "0:stock_bcd" = gadget3:::g3_step(g3_formula({
        stock_iterate(stock_bcd, stock_ss(stock_bcd__num) <- tag * 100 + stock_bcd__minlen)
    }, stock_bcd = stock_bcd, stock_bcd__num = g3_stock_instance(stock_bcd, 0))),
    list(
        '5:sum_ac_notag' = gadget3:::g3_step(g3_formula({
            comment("sum_ac_notag")
            stock_iterate(stock_ac, stock_intersect(stock_notag, {
                sum_ac_notag <- sum_ac_notag +
                    sum(stock_ss(stock_ac__num)) + sum(stock_ss(stock_notag__num))
            }))
            REPORT(sum_ac_notag)
        }, sum_ac_notag = 0.0, stock_ac = stock_ac, stock_notag = stock_notag) ),
        '5:sum_notag_bcd' = gadget3:::g3_step(g3_formula({
            comment("sum_notag_bcd")
            stock_iterate(stock_notag, stock_intersect(stock_bcd, {
                sum_notag_bcd <- sum_notag_bcd +
                    sum(stock_ss(stock_notag__num)) + sum(stock_ss(stock_bcd__num))
            }))
            REPORT(sum_notag_bcd)
        }, sum_notag_bcd = 0.0, stock_bcd = stock_bcd, stock_notag = stock_notag) ),

        # NB: Dummy parameter so model will compile in TMB
        ~{nll <- nll + g3_param("x", value = 0)} ))
actions <- c(actions, list(
    g3a_report_history(actions, var_re = "__num$", out_prefix = NULL) ))
model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)

attr(model_fn, 'parameter_template') |>
    identity() -> params
r <- attributes(model_fn(params))

# Intersection between stocks without tag means we iterate over all tags
ok(ut_cmp_identical(
    r$sum_notag_bcd,
    sum(r$stock_bcd__num) + as.vector(r$stock_notag__num) * 4 ), "sum_notag_bcd: summed notag for each of the bcd tags")
ok(ut_cmp_identical(
    r$sum_ac_notag,
    as.vector(r$stock_notag__num) * 3 + sum(r$stock_ac__num) ), "sum_ac_notag: summed notag for each of the ac tags")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
