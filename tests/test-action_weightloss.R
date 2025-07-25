if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st <- g3_stock('st', seq(10, 50, by = 10)) |> g3s_age(1,5)

abund_obs <- expand.grid(
    year = 2000:2005,
    step = 1:3,
    age = 1:5,
    length = seq(10, 50, by = 10),
    number = 0 )

actions <- list(
    g3a_time(2000, 2005, step_lengths = c(3, 3, 6), project_years = 0),
    gadget3:::g3a_initialconditions_manual(st,
        ~1e5 + 0 * st__midlen,
        ~1000 * age + 0 * st__midlen ),
    g3a_age(st),

    g3a_weightloss(
        st,
        rel_loss = g3_parameterized("ut_rel_loss", value = 0),
        abs_loss = g3_parameterized("ut_abs_loss", value = 0),
        min_weight = g3_parameterized("ut_min_weight", value = 1e-7),
        run_step = 2 ),
    
    g3a_weightloss(
        st,
        rel_loss = g3_parameterized("ut_rel_loss_len_mw", value = 0),
        min_weight = g3_formula(mw * st__midlen, mw = g3_parameterized("ut_min_weight_len_mw", value = 0)),
        run_step = 2,
        run_f = g3_formula(x > 0, x = g3_parameterized("ut_rel_loss_len_mw", value = 0)) ),

    g3a_weightloss(st,
        # Remove "10" from body weight, with a minimum based on length
        abs_loss = g3_parameterized("ut_abs_loss_len_mw", value = 0),
        min_weight = g3_formula(mw * st__midlen, mw = g3_parameterized("ut_min_weight_len_mw", value = 0)),
        run_f = g3_formula(x > 0, x = g3_parameterized("ut_abs_loss_len_mw", value = 0)) ),

    g3l_abundancedistribution(
        'test_results',
        abund_obs,
        fleets = list(),
        stocks = list(st),
        g3l_distribution_sumofsquares(),
        nll_breakdown = TRUE,
        report = TRUE ),

    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood() )
actions <- c(actions, list(
    g3a_report_detail(actions) ))
model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)

ok_group("Default params") #######
params <- attr(model_fn, 'parameter_template')
r <- lapply(attributes(model_fn(params)), drop)

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## Default params

ok_group("rel_loss:0.25") ########
params <- attr(model_fn, 'parameter_template')
params$ut_rel_loss <- 0.25
r <- lapply(attributes(model_fn(params)), drop)
ok(gadget3:::ut_cmp_df(r$dstart_st__wgt[1,,], '
     2000-01 2000-02 2000-03 2001-01 2001-02 2001-03 2002-01 2002-02  2002-03  2003-01  2003-02   2003-03  2004-01  2004-02  2004-03  2005-01  2005-02  2005-03
age1    1000    1000     750     750     750  562.50   562.5   562.5  421.875  421.875  421.875  316.4062 316.4062 316.4062 237.3047 237.3047 237.3047 177.9785
age2    2000    2000    1500     750     750  562.50   562.5   562.5  421.875  421.875  421.875  316.4062 316.4062 316.4062 237.3047 237.3047 237.3047 177.9785
age3    3000    3000    2250    1500    1500 1125.00   562.5   562.5  421.875  421.875  421.875  316.4062 316.4062 316.4062 237.3047 237.3047 237.3047 177.9785
age4    4000    4000    3000    2250    2250 1687.50  1125.0  1125.0  843.750  421.875  421.875  316.4062 316.4062 316.4062 237.3047 237.3047 237.3047 177.9785
age5    5000    5000    3750    3375    3375 2531.25  2250.0  2250.0 1687.500 1476.562 1476.562 1107.4219 949.2188 949.2188 711.9141 711.9141 711.9141 533.9355
', tolerance = 1e-6), "dstart_st__wgt[1,,]: Divide by 0.75 every second step (report is early)")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## rel_loss:0.25

ok_group("rel_loss:0.5") ########
params <- attr(model_fn, 'parameter_template')
params$ut_rel_loss <- 0.5
r <- lapply(attributes(model_fn(params)), drop)
ok(gadget3:::ut_cmp_df(r$dstart_st__wgt[1,,], '
     2000-01 2000-02 2000-03 2001-01 2001-02 2001-03 2002-01 2002-02 2002-03 2003-01 2003-02 2003-03 2004-01 2004-02 2004-03 2005-01 2005-02 2005-03
age1    1000    1000     500     500     500     250     250     250     125   125.0   125.0   62.50    62.5    62.5   31.25   31.25   31.25  15.625
age2    2000    2000    1000     500     500     250     250     250     125   125.0   125.0   62.50    62.5    62.5   31.25   31.25   31.25  15.625
age3    3000    3000    1500    1000    1000     500     250     250     125   125.0   125.0   62.50    62.5    62.5   31.25   31.25   31.25  15.625
age4    4000    4000    2000    1500    1500     750     500     500     250   125.0   125.0   62.50    62.5    62.5   31.25   31.25   31.25  15.625
age5    5000    5000    2500    2250    2250    1125    1000    1000     500   437.5   437.5  218.75   187.5   187.5   93.75   93.75   93.75  46.875
', tolerance = 1e-6), "dstart_st__wgt[1,,]: Divide by 0.5 every second step (report is early)")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## rel_loss:0.5

ok_group("abs_loss:650") ########
params <- attr(model_fn, 'parameter_template')
params$ut_abs_loss <- 650
r <- lapply(attributes(model_fn(params)), drop)
ok(gadget3:::ut_cmp_df(round(r$dstart_st__wgt[1,,], 5), '
       2000-01 2000-02 2000-03 2001-01 2001-02 2001-03 2002-01 2002-02 2002-03 2003-01 2003-02 2003-03 2004-01 2004-02 2004-03 2005-01 2005-02 2005-03
  age1    1000    1000     350     350     350       0       0       0       0       0       0       0       0       0       0       0       0       0
  age2    2000    2000    1350     350     350       0       0       0       0       0       0       0       0       0       0       0       0       0
  age3    3000    3000    2350    1350    1350     700       0       0       0       0       0       0       0       0       0       0       0       0
  age4    4000    4000    3350    2350    2350    1700     700     700      50       0       0       0       0       0       0       0       0       0
  age5    5000    5000    4350    3850    3850    3200    2700    2700    2050    1550    1550     900     720     720      70      70      70       0
', tolerance = 1e-6), "dstart_st__wgt[1,,]: Subtract 650, don't fall below min_weight")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## abs_loss:333

ok_group("rel_loss:0.1,abs_loss:100") ########
params <- attr(model_fn, 'parameter_template')
params$ut_rel_loss <- 0.1
params$ut_abs_loss <- 50
r <- lapply(attributes(model_fn(params)), drop)
ok(gadget3:::ut_cmp_df(round(r$dstart_st__wgt[1,,], 5), '
     2000-01 2000-02 2000-03 2001-01 2001-02 2001-03 2002-01 2002-02 2002-03 2003-01 2003-02 2003-03 2004-01 2004-02  2004-03  2005-01  2005-02   2005-03
age1    1000    1000     850     850     850     715     715     715   593.5   593.5   593.5  484.15  484.15  484.15  385.735  385.735  385.735  297.1615
age2    2000    2000    1750     850     850     715     715     715   593.5   593.5   593.5  484.15  484.15  484.15  385.735  385.735  385.735  297.1615
age3    3000    3000    2650    1750    1750    1525     715     715   593.5   593.5   593.5  484.15  484.15  484.15  385.735  385.735  385.735  297.1615
age4    4000    4000    3550    2650    2650    2335    1525    1525  1322.5   593.5   593.5  484.15  484.15  484.15  385.735  385.735  385.735  297.1615
age5    5000    5000    4450    4000    4000    3550    3145    3145  2780.5  2416.0  2416.0 2124.40 1796.35 1796.35 1566.715 1566.715 1566.715 1360.0435
', tolerance = 1e-6), "dstart_st__wgt[1,,]: Divide by 0.9, then subtract")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## rel_loss:0.1,abs_loss:100

ok_group("rel_loss:0.1,abs_loss:100,min_weight:500") ########
params <- attr(model_fn, 'parameter_template')
params$ut_rel_loss <- 0.1
params$ut_abs_loss <- 50
params$ut_min_weight <- 500
r <- lapply(attributes(model_fn(params)), drop)
ok(gadget3:::ut_cmp_df(r$dstart_st__wgt[1,,], '
     2000-01 2000-02 2000-03 2001-01 2001-02 2001-03 2002-01 2002-02 2002-03 2003-01 2003-02 2003-03 2004-01 2004-02 2004-03 2005-01 2005-02  2005-03
age1    1000    1000     900     900     900     810     810     810     729   729.0   729.0  656.10   656.1   656.1  590.49  590.49  590.49  531.441
age2    2000    2000    1800     900     900     810     810     810     729   729.0   729.0  656.10   656.1   656.1  590.49  590.49  590.49  531.441
age3    3000    3000    2700    1800    1800    1620     810     810     729   729.0   729.0  656.10   656.1   656.1  590.49  590.49  590.49  531.441
age4    4000    4000    3600    2700    2700    2430    1620    1620    1458   729.0   729.0  656.10   656.1   656.1  590.49  590.49  590.49  531.441
age5    5000    5000    4500    4050    4050    3645    3240    3240    2916  2551.5  2551.5 2296.35  1968.3  1968.3 1771.47 1771.47 1771.47 1594.323
', tolerance = 1e-6), "dstart_st__wgt[1,,]: Hit 500 limit and don't go lower, with both rel & abs")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## rel_loss:0.1,abs_loss:100,min_weight:500

ok_group("rel_loss min_weight by length") ########
params <- attr(model_fn, 'parameter_template') |>
    g3_init_val("ut_rel_loss_len_mw", 0.75) |>
    g3_init_val("ut_min_weight_len_mw", 50) |>
    identity() -> params
r <- lapply(attributes(model_fn(params)), drop)

ok(gadget3:::ut_cmp_df(r$dstart_st__wgt[,1,], '
         2000-01 2000-02 2000-03 2001-01 2001-02  2001-03  2002-01  2002-02   2002-03   2003-01   2003-02   2003-03   2004-01   2004-02   2004-03   2005-01   2005-02  2005-03
  10:20     1000    1000   812.5   812.5   812.5  765.625  765.625  765.625  753.9062  753.9062  753.9062  750.9766  750.9766  750.9766  750.2441  750.2441  750.2441  750.061
  20:30     1000    1000  1187.5  1187.5  1187.5 1234.375 1234.375 1234.375 1246.0938 1246.0938 1246.0938 1249.0234 1249.0234 1249.0234 1249.7559 1249.7559 1249.7559 1249.939
  30:40     1000    1000  1562.5  1562.5  1562.5 1703.125 1703.125 1703.125 1738.2812 1738.2812 1738.2812 1747.0703 1747.0703 1747.0703 1749.2676 1749.2676 1749.2676 1749.817
  40:50     1000    1000  1937.5  1937.5  1937.5 2171.875 2171.875 2171.875 2230.4688 2230.4688 2230.4688 2245.1172 2245.1172 2245.1172 2248.7793 2248.7793 2248.7793 2249.695
  50:Inf    1000    1000  2312.5  2312.5  2312.5 2640.625 2640.625 2640.625 2722.6562 2722.6562 2722.6562 2743.1641 2743.1641 2743.1641 2748.2910 2748.2910 2748.2910 2749.573
', tolerance = 1e-6), "dstart_st__wgt[1,,]: Limit hit depends on length")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## rel_loss min_weight by length

ok_group("abs_loss min_weight by length") ########
params <- attr(model_fn, 'parameter_template') |>
    g3_init_val("ut_abs_loss_len_mw", 18) |>
    g3_init_val("ut_min_weight_len_mw", 50) |>
    identity() -> params
r <- lapply(attributes(model_fn(params)), drop)

ok(gadget3:::ut_cmp_df(r$dstart_st__wgt[,1,], '
         2000-01 2000-02 2000-03 2001-01 2001-02 2001-03 2002-01 2002-02 2002-03 2003-01 2003-02 2003-03 2004-01 2004-02 2004-03 2005-01 2005-02 2005-03
  10:20     1000     982     964     946     928     910     892     874     856     838     820     802     784     766     750     750     750     750
  20:30     1000    1250    1250    1250    1250    1250    1250    1250    1250    1250    1250    1250    1250    1250    1250    1250    1250    1250
  30:40     1000    1750    1750    1750    1750    1750    1750    1750    1750    1750    1750    1750    1750    1750    1750    1750    1750    1750
  40:50     1000    2250    2250    2250    2250    2250    2250    2250    2250    2250    2250    2250    2250    2250    2250    2250    2250    2250
  50:Inf    1000    2750    2750    2750    2750    2750    2750    2750    2750    2750    2750    2750    2750    2750    2750    2750    2750    2750
', tolerance = 1e-6), "dstart_st__wgt[1,,]: Limit hit depends on length")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## abs_loss min_weight by length
