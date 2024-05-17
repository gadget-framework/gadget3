library(unittest)

library(gadget3)

prey_a <- g3_stock('prey_a', seq(1, 10)) |> g3s_age(1,3)
prey_b <- g3_stock('prey_b', seq(1, 10)) |> g3s_age(1,3)
pred_a <- g3_stock('pred_a', seq(50, 80, by = 10)) |> g3s_age(0, 10)

pred_a_catch_obs <- expand.grid(
    year = 2000:2005,
    number = 0 )

actions <- list(
    g3a_time(2000, 2005, step_lengths = c(6,6), project_years = 0),
    g3a_age(prey_a),
    g3a_age(prey_b),
    g3a_age(pred_a),
    g3a_initialconditions(prey_a, ~1e10 + 0 * prey_a__midlen, ~100),
    g3a_initialconditions(prey_b, ~2e10 + 0 * prey_b__midlen, ~200),
    g3a_initialconditions(pred_a, ~1e5 + 0 * pred_a__midlen, ~1000),

    g3a_predate(
        pred_a,
        list(prey_a, prey_b),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1),
        catchability_f = g3a_predate_catchability_predator(
            temperature = g3_parameterized('temp', value = 0, by_year = TRUE, optimise = FALSE)) ),

    g3l_understocking(list(prey_a, prey_b)),

    # NB: Dummy parameter so model will compile in TMB
    ~{nll <- nll + g3_param("x", value = 0)} )
actions <- c(actions, list(
    g3a_report_history(actions, var_re = "__cons|__suit$", out_prefix = "catchhist"),
    g3a_report_detail(actions) ))
model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)

ok_group("Default params") ########
params <- attr(model_fn, 'parameter_template')
result <- model_fn(params)
def_r <- attributes(result)

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## Default params

ok_group("Energy content") ########
params <- attr(model_fn, 'parameter_template')
params$prey_a.energycontent <- 2
params$prey_b.energycontent <- 1
result <- model_fn(params)
r <- attributes(result)

ok(all.equal(
    def_r$catchhistprey_a_pred_a__suit[] * 2,
    r$catchhistprey_a_pred_a__suit[],
    tolerance = 1e-6), "catchhistprey_a_pred_a__suit: Doubled thanks to energycontent")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## Energy content

ok_group("consumption.m3") ########

ok(ut_cmp_equal(
    diff(def_r$catchhistprey_a_pred_a__cons[1,1,,1,1]),
    c("60:70" = 0, "70:80" = 0, "80:Inf" = 0),
    tolerance = 1e-8 ), "def_r$catchhistprey_a_pred_a__cons: No difference in pred.length consumption")

params <- attr(model_fn, 'parameter_template')
params$pred_a.consumption.m3 <- 2
result <- model_fn(params) ; r <- attributes(result)

ok(all.equal(
    def_r$catchhistprey_a_pred_a__suit,
    r$catchhistprey_a_pred_a__suit,
    tolerance = 1e-3 ), "r$catchhistprey_a_pred_a__suit: consumption.m3 results in approximately equal __suit")

ok(ut_cmp_equal(
    r$catchhistprey_a_pred_a__cons[1,1,2,1,1],
    r$catchhistprey_a_pred_a__cons[1,1,1,1,1] / 55^2 * 65^2,
    tolerance = 1e-8 ), "r$catchhistprey_a_pred_a__cons: Jump in consumption 55 -> 65")
ok(ut_cmp_equal(
    r$catchhistprey_a_pred_a__cons[1,1,3,1,1],
    r$catchhistprey_a_pred_a__cons[1,1,2,1,1] / 65^2 * 75^2,
    tolerance = 1e-8 ), "r$catchhistprey_a_pred_a__cons: Jump in consumption 65 -> 75")
ok(ut_cmp_equal(
    r$catchhistprey_a_pred_a__cons[1,1,4,1,1],
    r$catchhistprey_a_pred_a__cons[1,1,3,1,1] / 75^2 * 85^2,
    tolerance = 1e-8 ), "r$catchhistprey_a_pred_a__cons: Jump in consumption 75 -> 85")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## consumption.m3
