library(magrittr)
library(unittest)

library(gadget3)

ok_group("Times produced in order", {
    inst <- g3_stock('terry', c(1)) %>% g3s_time(
        year = 2002:2004,
        step = 1:2)
    ok(ut_cmp_identical(
        inst$dimnames$time,
        c(2002001, 2002002, 2003001, 2003002, 2004001, 2004002)), "dimnames$time ordered year then step")
})