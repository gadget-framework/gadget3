library(magrittr)
library(unittest)

library(gadget3)

tag_ids <- function (s) gadget3:::stock_definition(s, 'stock__tag_ids')

s <- g3_stock('gelda', c(1)) %>% g3s_tag(c(x = 1L, y = 2L))
ok(ut_cmp_identical(tag_ids(s), structure(
    0:2,
    .Dim = 3L,
    .Dimnames = list(c("untagged", "x", "y")))), "Added 'untagged' id in")

s <- g3_stock('gelda', c(1)) %>% g3s_tag(c(nowt = 0L, x = 1L, y = 2L))
ok(ut_cmp_identical(tag_ids(s), structure(
    0:2,
    .Dim = 3L,
    .Dimnames = list(c("nowt", "x", "y")))), "Existing untagged not disturbed")
