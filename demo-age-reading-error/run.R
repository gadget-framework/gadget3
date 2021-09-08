library(gadget3)

# NB: Also added some aggregate areas for fleet data
areas <- list(a = 1, b = 2, c = 3, x = 1:2, y = 3)
prey_a <- g3_stock('prey_a', seq(1, 10)) |> g3s_age(1, 5)
fleet_abc <- g3_fleet('fleet_abc')

# Generate observation data for stock distribution
sd_data <- expand.grid(year = 1999:2000, step = c(1, 2), age = 1:5, stock = c("prey_a"), length = c(1,6))
sd_data$number <- floor(runif(length(sd_data$year), min=100, max=999))

actions <- list(
    g3a_time(1999,2000, steps = c(6, 6)),
    g3a_initialconditions(prey_a, ~10 * prey_a__midlen, ~100 * prey_a__midlen),
    g3a_predate_totalfleet(
        fleet_abc,
        list(prey_a),
        suitabilities = list(
            prey_a = ~g3_param_vector("fleet_abc_a")),
        amount_f = ~g3_param('amount_a') * age),
    g3l_catchdistribution(
        'utsd',
        sd_data,
        list(fleet_abc),
        list(prey_a),
        transform_fs = list(
            age = ~g3_param_array('reader1matrix')[age, destage]),
        area_group = areas,
        report = TRUE,
        g3l_distribution_sumofsquares()),
    g3l_controlset(
        'controlset',
        data.frame(
            age = 1:5,
            reader1 = c(2,3,4,5,1),
            stringsAsFactors = FALSE),
        input_betas = c(2, 2, 1, 1, 1),
        transform_fs = list(
            reader1 = ~g3_param_array('reader1matrix')[age, destage])),
    NULL)

params <- list(
    fleet_abc_a = c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0),
    amount_a = 100,
    reader1matrix = diag(5)[c(2:5,1),],
    controlset_weight = 1,
    cdist_sumofsquares_utsd_weight = 1)

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
    model_cpp <- c()
}
result <- model_fn(params)
# str(attributes(result), vec.len = 10000)
