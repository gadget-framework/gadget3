library(magrittr)

source('g3.R')

end <- function (x) x

ling_imm <- g3_stock('ling_imm', 20, 160, 4) %>%
    g3s_livesonareas(c("1","2","3")) %>%
    g3s_age(3, 10) %>%
    g3s_prey(energycontent = 5) %>%
    end()

ling_imm_actions <- c(list(),
    # TODO: I should be able to refer to variables here, but I can't.
    g3a_initialconditions(ling_imm,
        # NB: area & age factor together (gadget2 just multiplied them)
        factor_f = ~g3_param("lingimm.init.scalar") * exp(-1 * (g3_param("lingimm.M") + g3_param("ling.init.F")) * age) * g3_param("lingimm.init.", age),
        mean_f = ~g3_param("ling.Linf") * (1 - exp(-1 * (0.001 * g3_param("ling.k")) * (age - (1 + log(1 - g3_param("ling.recl")/g3_param("ling.Linf"))/(0.001 * g3_param("ling.k")))))),
        stddev_f = ~switch(age, 3,8.25, 4,10.5644599516659, 5,12.4081745588022, 6,11.5741565728647, 7,11.0523508874244, 8,11.3447991170274, 9,11.7721342759715, 10,13.6152275606449),
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_grow(ling_imm,
        growth_fn = g3a_grow_lengthvbsimple(1, 2, 3, 4),
        impl_fn = g3a_grow_impl_bbinom(1, 9)),
    g3a_age(ling_imm),
    list())

ling_mat <- g3_stock('ling_mat', 0, 90, 10) %>%
    g3s_livesonareas(c("1","2","3")) %>%
    g3s_age(1, 4) %>%
    g3s_prey(energycontent = 5) %>%
    end()

ling_mat_actions <- c(list(),
    g3a_grow(ling_mat,
        growth_fn = g3a_grow_lengthvbsimple(1, 2, 3, 4),
        impl_fn = g3a_grow_impl_bbinom(1, 9)),
    list())

#si <- g3s_fleet('si') %>% 
#    g3s_livesonareas(areas = c(1)) %>% 
#    g3s_predator_number(
#          ling_imm = (~ 0.5 * preylen),
#          ling_mat = (~ 0.5 * preylen),
#          )

time <- g3a_time(g3_data("strtyr"), g3_data("endyr"), c(3, 3, 3, 3))
ling_model <- g3_compile(c(
#    ling_mat_actions,
    ling_imm_actions,
    time))
print(ling_model)

writeLines("***** Running Model *****")
ling_data <- list(
    strtyr = 1990,
    endyr = 1995,
    end = NULL
)
ling_param <- list(
    lingimm.init.scalar = 200,
    lingimm.M = 0.15,
    ling.init.F = 0.4,
    "lingimm.init.3" = 1,
    "lingimm.init.4" = 1,
    "lingimm.init.5" = 1,
    "lingimm.init.6" = 1,
    "lingimm.init.7" = 1,
    "lingimm.init.8" = 1,
    "lingimm.init.9" = 1,
    "lingimm.init.10" = 1,
    ling.Linf = 160,
    ling.k = 90,
    ling.recl = 12,
    lingimm.walpha = 2.27567436711055e-06,
    lingimm.wbeta = 3.20200445996187,
    end = NULL)
result <- ling_model(ling_data, ling_param)
str(result)
# NB: You can do: ling_model <- edit(ling_model) ; result <- ling_model(ling_data, ling_param)
