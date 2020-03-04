library(magrittr)

source('g3.R')

growth_rate <- 3
ling_imm <- (g3_stock('ling_imm', c(0, 10, 20, 30))
    %>% g3s_livesonareas(areas = c(1,2,3))
    %>% g3s_growth(ages = c(1,2,3,4), delt_l =~ g3_param("growth_rate") * cur_time))

si <- (g3s_fleet('si')
    %>% g3s_livesonareas(areas = c(1))
    %>% g3s_predator_number(
          ling_imm = (~ 0.5 * preylen),
          ling_mat = (~ 0.5 * preylen),
          )

time <- g3_time(g3_data("strtyr"), g3_data("endyr"), 4)
g3m <- g3_model(time, ling_imm, si)
writeLines(g3_run(g3m))
parse(text = g3_run(g3m))
