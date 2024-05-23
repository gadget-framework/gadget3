library(unittest)

library(gadget3)

prey_a <- g3_stock('prey_a', seq(1, 10)) |> g3s_age(1,3)
prey_b <- g3_stock('prey_b', seq(1, 10)) |> g3s_age(1,3)
pred_a <- g3_stock('pred_a', seq(50, 80, by = 10)) |> g3s_age(0, 10)

pred_a_catch_obs <- expand.grid(
    year = 2000:2005,
    length = c(1,5,10),
    predator_length = c(50,70),
    predator_age = c("[0,5)", "[6,10)"), # ((
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
    g3l_catchdistribution(
        'pred_a_catch',
        pred_a_catch_obs,
        fleets = list(pred_a),
        stocks = list(prey_a, prey_b),
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

ok(gadget3:::ut_cmp_array(r$cdist_sumofsquares_pred_a_catch_model__num, '
length predator_length predator_age time     Freq
1     1:5           50:70          0:4 2000  8700000
2    5:10           50:70          0:4 2000 10875000
3  10:Inf           50:70          0:4 2000  2175000
4     1:5          70:Inf          0:4 2000 15420000
5    5:10          70:Inf          0:4 2000 19275000
6  10:Inf          70:Inf          0:4 2000  3855000
7     1:5           50:70          6:9 2000  6960000
8    5:10           50:70          6:9 2000  8700000
9  10:Inf           50:70          6:9 2000  1740000
10    1:5          70:Inf          6:9 2000 12336000
11   5:10          70:Inf          6:9 2000 15420000
12 10:Inf          70:Inf          6:9 2000  3084000
13    1:5           50:70          0:4 2001  6960000
14   5:10           50:70          0:4 2001  8700000
15 10:Inf           50:70          0:4 2001  1740000
16    1:5          70:Inf          0:4 2001 12336000
17   5:10          70:Inf          0:4 2001 15420000
18 10:Inf          70:Inf          0:4 2001  3084000
19    1:5           50:70          6:9 2001  6960000
20   5:10           50:70          6:9 2001  8700000
21 10:Inf           50:70          6:9 2001  1740000
22    1:5          70:Inf          6:9 2001 12336000
23   5:10          70:Inf          6:9 2001 15420000
24 10:Inf          70:Inf          6:9 2001  3084000
25    1:5           50:70          0:4 2002  5220000
26   5:10           50:70          0:4 2002  6525000
27 10:Inf           50:70          0:4 2002  1305000
28    1:5          70:Inf          0:4 2002  9252000
29   5:10          70:Inf          0:4 2002 11565000
30 10:Inf          70:Inf          0:4 2002  2313000
31    1:5           50:70          6:9 2002  6960000
32   5:10           50:70          6:9 2002  8700000
33 10:Inf           50:70          6:9 2002  1740000
34    1:5          70:Inf          6:9 2002 12336000
35   5:10          70:Inf          6:9 2002 15420000
36 10:Inf          70:Inf          6:9 2002  3084000
37    1:5           50:70          0:4 2003  3480000
38   5:10           50:70          0:4 2003  4350000
39 10:Inf           50:70          0:4 2003   870000
40    1:5          70:Inf          0:4 2003  6168000
41   5:10          70:Inf          0:4 2003  7710000
42 10:Inf          70:Inf          0:4 2003  1542000
43    1:5           50:70          6:9 2003  6960000
44   5:10           50:70          6:9 2003  8700000
45 10:Inf           50:70          6:9 2003  1740000
46    1:5          70:Inf          6:9 2003 12336000
47   5:10          70:Inf          6:9 2003 15420000
48 10:Inf          70:Inf          6:9 2003  3084000
49    1:5           50:70          0:4 2004  1740000
50   5:10           50:70          0:4 2004  2175000
51 10:Inf           50:70          0:4 2004   435000
52    1:5          70:Inf          0:4 2004  3084000
53   5:10          70:Inf          0:4 2004  3855000
54 10:Inf          70:Inf          0:4 2004   771000
55    1:5           50:70          6:9 2004  6960000
56   5:10           50:70          6:9 2004  8700000
57 10:Inf           50:70          6:9 2004  1740000
58    1:5          70:Inf          6:9 2004 12336000
59   5:10          70:Inf          6:9 2004 15420000
60 10:Inf          70:Inf          6:9 2004  3084000
# NB: Ages 0..4 have all grown up by this point, none left
61    1:5           50:70          0:4 2005        0
62   5:10           50:70          0:4 2005        0
63 10:Inf           50:70          0:4 2005        0
64    1:5          70:Inf          0:4 2005        0
65   5:10          70:Inf          0:4 2005        0
66 10:Inf          70:Inf          0:4 2005        0
67    1:5           50:70          6:9 2005  6960000
68   5:10           50:70          6:9 2005  8700000
69 10:Inf           50:70          6:9 2005  1740000
70    1:5          70:Inf          6:9 2005 12336000
71   5:10          70:Inf          6:9 2005 15420000
72 10:Inf          70:Inf          6:9 2005  3084000
'), "cdist_sumofsquares_pred_a_catch_model__num: Baseline output")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## consumption.m3
