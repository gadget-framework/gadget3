library(unittest)

library(gadget3)


# Stratified sumofsquares
prey_a <- g3_stock('prey_a', seq(1, 5, by = 1)) |> g3s_livesonareas(1) |> g3s_age(1,5)
prey_a__init <- g3_stock_instance(prey_a)
prey_a__init[] <- rep(10000, length(prey_a__init))

obsdata <- expand.grid(
    year = 2000:2005,
    age = 2:5)  # NB: Only report age 2,5
obsdata$number <- runif(nrow(obsdata)) * 10000

actions <- list(
    g3a_time(2000, 2005, c(6L, 6L)),
    g3a_initialconditions(
        prey_a,
        num_f = g3_formula(stock_ss(prey_a__init), prey_a__init = prey_a__init),
        wgt_f = 10),
    g3a_naturalmortality(prey_a),
    g3a_spawn(
        prey_a,
        g3a_spawn_recruitment_bevertonholt(10, 10),
        output_stocks = list(prey_a)),
    g3a_age(prey_a),
    g3l_abundancedistribution("adist",
        obsdata,
        function_f = g3l_distribution_surveyindices_linear(alpha = NULL, beta = 1.8),
        stocks = list(prey_a),
        report = TRUE))
actions <- c(actions, list(g3a_report_detail(actions)))
model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)

params <- attr(model_fn, 'parameter_template')
params['report_detail'] <- 1

r <- model_fn(params)
modeldata <- attr(r, 'prey_a__num')

exp_nll <- 0
for (age in unique(obsdata$age)) {
    ok_group(paste0("age = ", age))
    N <- colSums(attr(r, 'dstart_prey_a__num')[,,age = age,], 2)
    N <- colSums(array(N, dim=c(2,length(N) / 2)))  # Group timestep by year
    I <- obsdata[obsdata$age == age, 'number']
    alpha <- mean(I) - 1.8 * mean(N)
    exp_nll <- exp_nll + sum((alpha + 1.8 * N - I)**2)

    # NB:  __params is a bit broken from a reporting perspective. Should be giving all params, not just the final
    if (age == max(obsdata$age)) ok(ut_cmp_equal(
        attr(r, 'adist_surveyindices_linear_adist_model__params'),
        c(alpha, 1.8)), "__params: Linear regression matches expected")
}
ok(ut_cmp_equal(as.vector(r), exp_nll), "Total nll matches expected")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params, g3_test_tmb = 2)
