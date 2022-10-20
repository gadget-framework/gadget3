library(magrittr)
library(unittest)

library(gadget3)

tag_ids <- function (s) g3_stock_def(s, 'tag_ids')
untagged_idx <- function (s) g3_stock_def(s, 'untagged_idx')

s <- g3_stock('gelda', c(1)) %>% g3s_tag(c(x = 1L, y = 2L))
ok(ut_cmp_identical(tag_ids(s), structure(
    0:2,
    .Dim = 3L,
    .Dimnames = list(c("untagged", "x", "y")))), "Added 'untagged' id in")
ok(ut_cmp_identical(untagged_idx(s), quote(g3_idx(1L))), "Untagged is first item")

s <- g3_stock('gelda', c(1)) %>% g3s_tag(c(x = 1L, y = 2L), force_untagged = FALSE)
ok(ut_cmp_identical(tag_ids(s), structure(
    1:2,
    .Dim = 2L,
    .Dimnames = list(c("x", "y")))), "force_untagged off, 'untagged' not added")
ok(ut_cmp_identical(untagged_idx(s), NA), "No untagged item")

s <- g3_stock('gelda', c(1)) %>% g3s_tag(c(nowt = 0L, x = 1L, y = 2L))
ok(ut_cmp_identical(tag_ids(s), structure(
    0:2,
    .Dim = 3L,
    .Dimnames = list(c("nowt", "x", "y")))), "Existing untagged not disturbed")
ok(ut_cmp_identical(untagged_idx(s), quote(g3_idx(1L))), "Untagged is first item")

s <- g3_stock('gelda', c(1)) %>% g3s_tag(c(y = 2L, nowt = 0L, x = 1L))
ok(ut_cmp_identical(tag_ids(s), structure(
    as.integer(c(2, 0, 1)),
    .Dim = 3L,
    .Dimnames = list(c("y", "nowt", "x")))), "Existing untagged is not disturbed")
ok(ut_cmp_identical(untagged_idx(s), quote(g3_idx(2L))), "Untagged is middle item")
