library(magrittr)

source('g3.R')

end <- function (x) x

ling_imm <- g3_stock('ling_imm', c(0, 10, 20, 30)) %>%
    g3s_livesonareas(c(1,2,3)) %>% 
    g3s_age(c(1,2,3,4)) %>%
    g3s_prey(energycontent = 5) %>%
    end()

ling_imm_actions <- c(list(),
    # TODO: I should be able to refer to variables here, but I can't.
    g3a_grow(ling_imm, delt_l =~ g3_param("growth_rate") * cur_time),
    g3a_age(ling_imm),
    list())

ling_mat <- g3_stock('ling_mat', c(0, 10, 20, 30, 50, 90)) %>%
    g3s_livesonareas(c(1,2,3)) %>% 
    g3s_age(c(1,2,3,4)) %>%
    g3s_prey(energycontent = 5) %>%
    end()

ling_mat_actions <- c(list(),
    g3a_grow(ling_mat, delt_l =~ g3_param("mat_growth_rate") * cur_time),
    list())

#si <- g3s_fleet('si') %>% 
#    g3s_livesonareas(areas = c(1)) %>% 
#    g3s_predator_number(
#          ling_imm = (~ 0.5 * preylen),
#          ling_mat = (~ 0.5 * preylen),
#          )

time <- g3a_time(g3_data("strtyr"), g3_data("endyr"), 4)
out <- g3_compile(c(time, ling_mat_actions, ling_imm_actions))
print(out)
