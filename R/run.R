library(magrittr)

source('g3.R')

ling_imm <- g3_stock('ling_imm', c(0, 10, 20, 30)) %>% g3s_age(c(1,2,3,4)) %>% g3s_growth(delt_l ~ 2 * cur_time)

time <- g3_time(g3_data("strtyr"), g3_data("endyr"), 4)
g3m <- g3_model(time, ling_imm)
writeLines(g3_run(g3m))
parse(text = g3_run(g3m))