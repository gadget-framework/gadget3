if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

other_wgt <- g3_stock('other_wgt', 0)
other_np <- g3_stock('other_np', seq(10, 50, by = 10)) |> g3s_age(3,3)
other_cv <- g3_stock('other_cv', seq(50, 100, by = 10)) |> g3s_age(5,10)
prey_stocks <- list(other_wgt, other_np, other_cv)
pred_a <- g3_stock('pred_a', seq(50, 80, by = 10)) |> g3s_age(0, 10)

pred_a_catch_obs <- expand.grid(
    year = 2000:2005,
    length = seq(0, 100, 10),
    stock = c('other_wgt', 'other_np', 'other_cv'),
    number = 0 )

other_abund_obs <- expand.grid(
    year = 2000:2005,
    length = seq(0, 100, 10),
    stock = c('other_wgt', 'other_np', 'other_cv'),
    number = 0 )

actions <- list(
    g3a_time(2000, 2005, step_lengths = c(6,6), project_years = 0),
    # Parameterized abundance, mean weight

    g3a_otherfood(
       other_wgt,
       g3_parameterized('of_abund', by_year = TRUE, by_stock = TRUE),
       g3_parameterized('of_meanwgt', by_stock = TRUE)),
    g3a_otherfood_normalparam(other_np),
    g3a_otherfood_normalcv(other_cv),
    g3a_age(pred_a),
    g3a_initialconditions(pred_a, ~1e5 + 0 * pred_a__midlen, ~1000),

    g3a_predate(
        pred_a,
        prey_stocks,
        suitabilities = list(
            other_wgt = g3_parameterized('suit', by_stock = TRUE, value = 1),
            other_np = g3_suitability_exponentiall50(),
            other_cv = g3_suitability_exponentiall50() ),
        catchability_f = g3a_predate_catchability_predator(
            temperature = g3_parameterized('temp', value = 0, by_year = TRUE, optimise = FALSE)) ),

    g3l_understocking(prey_stocks),
    g3l_catchdistribution(
        'pred_a_catch',
        pred_a_catch_obs,
        fleets = list(pred_a),
        stocks = prey_stocks,
        g3l_distribution_sumofsquares(),
        nll_breakdown = TRUE,
        report = TRUE ),

    g3l_abundancedistribution(
        'other_abund',
        other_abund_obs,
        fleets = list(),
        stocks = prey_stocks,
        g3l_distribution_sumofsquares(),
        nll_breakdown = TRUE,
        report = TRUE ),

    # NB: Dummy parameter so model will compile in TMB
    ~{nll <- nll + g3_param("x", value = 0)} )
actions <- c(actions, list(
    g3a_report_history(actions, var_re = "__cons|__suit$", out_prefix = "catchhist"),
    g3a_report_detail(actions) ))
model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)

ok_group("Default params") ########
attr(model_fn, 'parameter_template') |>
    g3_init_val("other_np.Linf", max(g3_stock_def(other_np, "midlen"))) |>
    g3_init_val("other_cv.Linf", max(g3_stock_def(other_cv, "midlen"))) |>
    g3_init_val("*.walpha", 0.01) |>
    g3_init_val("*.wbeta", 3) |>
    g3_init_val("other_wgt.of_abund.#", 2000:2005 * 1e3) |>
    g3_init_val("other_wgt.of_meanwgt", 100) |>

    g3_init_val("*.*.alpha", 0.07, lower = 0.01, upper = 0.2) |>
    g3_init_val("other_np.*.l50", 45) |>
    g3_init_val("other_cv.*.l50", 55) |>

    identity() -> params
result <- model_fn(params)

r <- attributes(result)

## other_wgt catch
ok(ut_cmp_equal(
    sum(r$cdist_sumofsquares_pred_a_catch_model__num[-1, stock="other_wgt",]),
    0), "cdist_sumofsquares_pred_a_catch_model__num: No other_wgt caught outside it's single lengthgroup")
ok(ut_cmp_equal(
    r$cdist_sumofsquares_pred_a_catch_model__num[1,stock="other_wgt",],
    c("2000" = 11323.67, "2001" = 11327.88, "2002" = 11332.08, "2003" = 11336.29, "2004" = 11340.49, "2005" = 11344.69),
    tolerance = 1e-6 ), "cdist_sumofsquares_pred_a_catch_model__num: other_wgt catch steadily increasing")

## other_np catch
ok(ut_cmp_equal(
    r$cdist_sumofsquares_pred_a_catch_model__num[,stock="other_np",time="2000"],
    c("0:10" = 0, "10:20" = 0.01, "20:30" = 0.21, "30:40" = 2.69, "40:50" = 11.57,
    "50:60" = 16.23, "60:70" = 0, "70:80" = 0, "80:90" = 0, "90:100" = 0, "100:Inf" = 0),
    tolerance = 1e-3 ), "cdist_sumofsquares_pred_a_catch_model__num: other_np catch has selectivity curve")
for (yr in 2000:2005) ok(ut_cmp_equal(
    r$cdist_sumofsquares_pred_a_catch_model__num[,stock="other_np",time=as.character(yr)],
    r$cdist_sumofsquares_pred_a_catch_model__num[,stock="other_np",time="2000"],
    tolerance = 1e-3), paste0("cdist_sumofsquares_pred_a_catch_model__num: other_np catch ~stable across year ", yr))

## other_cv catch
ok(ut_cmp_equal(
    r$cdist_sumofsquares_pred_a_catch_model__num[,stock="other_cv",time="2000"],
    c("0:10" = 0, "10:20" = 0, "20:30" = 0, "30:40" = 0, "40:50" = 0, "50:60" = 0,
    "60:70" = 0.09, "70:80" = 2.66, "80:90" = 28.07, "90:100" = 113.36, "100:Inf" = 179.08),
    tolerance = 1e-3 ), "cdist_sumofsquares_pred_a_catch_model__num: other_cv catch has selectivity curve")
for (yr in 2000:2005) ok(ut_cmp_equal(
    r$cdist_sumofsquares_pred_a_catch_model__num[,stock="other_cv",time=as.character(yr)],
    r$cdist_sumofsquares_pred_a_catch_model__num[,stock="other_cv",time="2000"],
    tolerance = 1e-3), paste0("cdist_sumofsquares_pred_a_catch_model__num: other_cv catch ~stable across year ", yr))

## other_wgt abundance
ok(gadget3:::ut_cmp_array(r$detail_other_wgt__num, "
   length    time Freq
1   0:Inf 2000-01 2000000
2   0:Inf 2000-02 2000000
3   0:Inf 2001-01 2001000
4   0:Inf 2001-02 2001000
5   0:Inf 2002-01 2002000
6   0:Inf 2002-02 2002000
7   0:Inf 2003-01 2003000
8   0:Inf 2003-02 2003000
9   0:Inf 2004-01 2004000
10  0:Inf 2004-02 2004000
11  0:Inf 2005-01 2005000
12  0:Inf 2005-02 2005000
"), "detail_other_wgt__num: current year * 1000, not affected by catch")
ok(gadget3:::ut_cmp_array(r$detail_other_wgt__wgt, "
   length    time Freq
1   0:Inf 2000-01  100
2   0:Inf 2000-02  100
3   0:Inf 2001-01  100
4   0:Inf 2001-02  100
5   0:Inf 2002-01  100
6   0:Inf 2002-02  100
7   0:Inf 2003-01  100
8   0:Inf 2003-02  100
9   0:Inf 2004-01  100
10  0:Inf 2004-02  100
11  0:Inf 2005-01  100
12  0:Inf 2005-02  100
"), "detail_other_wgt__wgt: All 100, not affected by catch")

## other_np abundance
ok(gadget3:::ut_cmp_array(r$detail_other_np__num[,,"2000-01", drop = FALSE], "
  length  age    time        Freq
1  10:20 age3 2000-01    8.755424
2  20:30 age3 2000-01  184.602700
3  30:40 age3 2000-01 1431.872712
4  40:50 age3 2000-01 4085.792385
5 50:Inf age3 2000-01 4288.976779
"), "detail_other_np__num: vonB applied")
for (t in dimnames(r$detail_other_np__num)$time) ok(ut_cmp_equal(
    r$detail_other_np__num[,,"2000-01"],
    r$detail_other_np__num[,,t],
    ignore = NULL ), paste0("detail_other_np__num: Stable across timestep ", t, ", not affected by catch"))
for (t in dimnames(r$detail_other_np__wgt)$time) ok(ut_cmp_equal(
    r$detail_other_np__wgt[,,"2000-01"],
    r$detail_other_np__wgt[,,t],
    ignore = NULL ), paste0("detail_other_np__wgt: Stable across timestep ", t, ", not affected by catch"))

## other_cv abundance
ok(gadget3:::ut_cmp_array(r$detail_other_cv__num[,,"2000-01", drop = FALSE], "
  length  age    time        Freq
1    50:60  age5 2000-01 8.260941e-02
2    60:70  age5 2000-01 4.816416e+00
3    70:80  age5 2000-01 1.110709e+02
4    80:90  age5 2000-01 1.013116e+03
5   90:100  age5 2000-01 3.655102e+03
6  100:Inf  age5 2000-01 5.215813e+03
7    50:60  age6 2000-01 7.133300e-02
8    60:70  age6 2000-01 4.202005e+00
9    70:80  age6 2000-01 9.918938e+01
10   80:90  age6 2000-01 9.382451e+02
11  90:100  age6 2000-01 3.556395e+03
12 100:Inf  age6 2000-01 5.401897e+03
13   50:60  age7 2000-01 6.764495e-02
14   60:70  age7 2000-01 3.999179e+00
15   70:80  age7 2000-01 9.519219e+01
16   80:90  age7 2000-01 9.122770e+02
17  90:100  age7 2000-01 3.520036e+03
18 100:Inf  age7 2000-01 5.468428e+03
19   50:60  age8 2000-01 6.634484e-02
20   60:70  age8 2000-01 3.927444e+00
21   70:80  age8 2000-01 9.376889e+01
22   80:90  age8 2000-01 9.029290e+02
23  90:100  age8 2000-01 3.506666e+03
24 100:Inf  age8 2000-01 5.492642e+03
25   50:60  age9 2000-01 6.587396e-02
26   60:70  age9 2000-01 3.901431e+00
27   70:80  age9 2000-01 9.325152e+01
28   80:90  age9 2000-01 8.995175e+02
29  90:100  age9 2000-01 3.501749e+03
30 100:Inf  age9 2000-01 5.501514e+03
31   50:60 age10 2000-01 6.570172e-02
32   60:70 age10 2000-01 3.891912e+00
33   70:80 age10 2000-01 9.306202e+01
34   80:90 age10 2000-01 8.982661e+02
35  90:100 age10 2000-01 3.499941e+03
36 100:Inf age10 2000-01 5.504774e+03
", tolerance = 1e-7), "detail_other_cv__num: vonB applied to all ages")
for (t in dimnames(r$detail_other_cv__num)$time) ok(ut_cmp_equal(
    r$detail_other_cv__num[,,"2000-01"],
    r$detail_other_cv__num[,,t],
    ignore = NULL ), paste0("detail_other_cv__num: Stable across timestep ", t, ", not affected by catch"))
for (t in dimnames(r$detail_other_cv__wgt)$time) ok(ut_cmp_equal(
    r$detail_other_cv__wgt[,,"2000-01"],
    r$detail_other_cv__wgt[,,t],
    ignore = NULL ), paste0("detail_other_cv__wgt: Stable across timestep ", t, ", not affected by catch"))

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## Default params
