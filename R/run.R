source('g3.R')

time <- g3_time(g3_data("strtyr"), g3_data("endyr"), 4)
g3m <- g3_model(time)
g3_run(g3m)
