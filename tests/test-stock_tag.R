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
