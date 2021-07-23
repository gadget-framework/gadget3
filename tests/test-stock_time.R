library(magrittr)
library(unittest)

library(gadget3)

ok_group("Times produced in order", {
    inst <- g3_stock('terry', c(1)) %>% g3s_time(
        year = 2002:2004,
        step = 1:2)
    ok(ut_cmp_identical(
        inst$dimnames$time,
        c("2002-01", "2002-02", "2003-01", "2003-02", "2004-01", "2004-02")), "dimnames$time ordered year then step")
})
