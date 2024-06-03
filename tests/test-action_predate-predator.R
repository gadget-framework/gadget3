library(unittest)

library(gadget3)

prey_a <- g3_stock('prey_a', seq(1, 10)) |> g3s_age(1,3)
prey_b <- g3_stock('prey_b', seq(1, 10)) |> g3s_age(1,3)
pred_a <- g3_stock('pred_a', seq(50, 80, by = 10)) |> g3s_age(0, 10)
otherfood <- g3_stock('otherfood', 0)

pred_a_catch_obs <- expand.grid(
    year = 2000:2005,
    length = c(0,5,10),
    stock = c('prey_a', 'prey_b', 'otherfood'),
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
    g3a_otherfood(otherfood, ~1e10 + 0 * otherfood__midlen, 100),

    g3a_predate(
        pred_a,
        list(prey_a, prey_b, otherfood),
        suitabilities = list(prey_a = 0.1, prey_b = 0.1, otherfood = 0.01),
        catchability_f = g3a_predate_catchability_predator(
            temperature = g3_parameterized('temp', value = 0, by_year = TRUE, optimise = FALSE)) ),

    g3l_understocking(list(prey_a, prey_b)),
    g3l_catchdistribution(
        'pred_a_catch',
        pred_a_catch_obs,
        fleets = list(pred_a),
        stocks = list(prey_a, prey_b, otherfood),
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
    length     stock predator_length predator_age time         Freq
1      0:5    prey_a           50:70          0:4 2000  2898067.884
2     5:10    prey_a           50:70          0:4 2000  3622584.854
3   10:Inf    prey_a           50:70          0:4 2000   724516.971
4      0:5    prey_b           50:70          0:4 2000  5796135.767
5     5:10    prey_b           50:70          0:4 2000  7245169.709
6   10:Inf    prey_b           50:70          0:4 2000  1449033.942
7      0:5 otherfood           50:70          0:4 2000    24151.455
# NB: All otherfood are length 0
8     5:10 otherfood           50:70          0:4 2000        0.000
9   10:Inf otherfood           50:70          0:4 2000        0.000
10     0:5    prey_a          70:Inf          0:4 2000  5136575.490
11    5:10    prey_a          70:Inf          0:4 2000  6420719.363
12  10:Inf    prey_a          70:Inf          0:4 2000  1284143.873
13     0:5    prey_b          70:Inf          0:4 2000 10273150.980
14    5:10    prey_b          70:Inf          0:4 2000 12841438.726
15  10:Inf    prey_b          70:Inf          0:4 2000  2568287.745
16     0:5 otherfood          70:Inf          0:4 2000    42806.372
17    5:10 otherfood          70:Inf          0:4 2000        0.000
18  10:Inf otherfood          70:Inf          0:4 2000        0.000
19     0:5    prey_a           50:70          6:9 2000  2318454.307
20    5:10    prey_a           50:70          6:9 2000  2898067.884
21  10:Inf    prey_a           50:70          6:9 2000   579613.577
22     0:5    prey_b           50:70          6:9 2000  4636908.614
23    5:10    prey_b           50:70          6:9 2000  5796135.767
24  10:Inf    prey_b           50:70          6:9 2000  1159227.153
25     0:5 otherfood           50:70          6:9 2000    19321.164
26    5:10 otherfood           50:70          6:9 2000        0.000
27  10:Inf otherfood           50:70          6:9 2000        0.000
28     0:5    prey_a          70:Inf          6:9 2000  4109260.392
29    5:10    prey_a          70:Inf          6:9 2000  5136575.490
30  10:Inf    prey_a          70:Inf          6:9 2000  1027315.098
31     0:5    prey_b          70:Inf          6:9 2000  8218520.784
32    5:10    prey_b          70:Inf          6:9 2000 10273150.980
33  10:Inf    prey_b          70:Inf          6:9 2000  2054630.196
34     0:5 otherfood          70:Inf          6:9 2000    34245.098
35    5:10 otherfood          70:Inf          6:9 2000        0.000
36  10:Inf otherfood          70:Inf          6:9 2000        0.000
37     0:5    prey_a           50:70          0:4 2001  2318454.079
38    5:10    prey_a           50:70          0:4 2001  2898067.599
39  10:Inf    prey_a           50:70          0:4 2001   579613.520
40     0:5    prey_b           50:70          0:4 2001  4636908.159
41    5:10    prey_b           50:70          0:4 2001  5796135.198
42  10:Inf    prey_b           50:70          0:4 2001  1159227.040
43     0:5 otherfood           50:70          0:4 2001    19324.009
44    5:10 otherfood           50:70          0:4 2001        0.000
45  10:Inf otherfood           50:70          0:4 2001        0.000
46     0:5    prey_a          70:Inf          0:4 2001  4109259.989
47    5:10    prey_a          70:Inf          0:4 2001  5136574.986
48  10:Inf    prey_a          70:Inf          0:4 2001  1027314.997
49     0:5    prey_b          70:Inf          0:4 2001  8218519.978
50    5:10    prey_b          70:Inf          0:4 2001 10273149.972
51  10:Inf    prey_b          70:Inf          0:4 2001  2054629.994
52     0:5 otherfood          70:Inf          0:4 2001    34250.140
53    5:10 otherfood          70:Inf          0:4 2001        0.000
54  10:Inf otherfood          70:Inf          0:4 2001        0.000
55     0:5    prey_a           50:70          6:9 2001  2318454.079
56    5:10    prey_a           50:70          6:9 2001  2898067.599
57  10:Inf    prey_a           50:70          6:9 2001   579613.520
58     0:5    prey_b           50:70          6:9 2001  4636908.159
59    5:10    prey_b           50:70          6:9 2001  5796135.198
60  10:Inf    prey_b           50:70          6:9 2001  1159227.040
61     0:5 otherfood           50:70          6:9 2001    19324.009
62    5:10 otherfood           50:70          6:9 2001        0.000
63  10:Inf otherfood           50:70          6:9 2001        0.000
64     0:5    prey_a          70:Inf          6:9 2001  4109259.989
65    5:10    prey_a          70:Inf          6:9 2001  5136574.986
66  10:Inf    prey_a          70:Inf          6:9 2001  1027314.997
67     0:5    prey_b          70:Inf          6:9 2001  8218519.978
68    5:10    prey_b          70:Inf          6:9 2001 10273149.972
69  10:Inf    prey_b          70:Inf          6:9 2001  2054629.994
70     0:5 otherfood          70:Inf          6:9 2001    34250.140
71    5:10 otherfood          70:Inf          6:9 2001        0.000
72  10:Inf otherfood          70:Inf          6:9 2001        0.000
73     0:5    prey_a           50:70          0:4 2002  1738840.389
74    5:10    prey_a           50:70          0:4 2002  2173550.486
75  10:Inf    prey_a           50:70          0:4 2002   434710.097
76     0:5    prey_b           50:70          0:4 2002  3477680.777
77    5:10    prey_b           50:70          0:4 2002  4347100.972
78  10:Inf    prey_b           50:70          0:4 2002   869420.194
79     0:5 otherfood           50:70          0:4 2002    14495.141
80    5:10 otherfood           50:70          0:4 2002        0.000
81  10:Inf otherfood           50:70          0:4 2002        0.000
82     0:5    prey_a          70:Inf          0:4 2002  3081944.689
83    5:10    prey_a          70:Inf          0:4 2002  3852430.861
84  10:Inf    prey_a          70:Inf          0:4 2002   770486.172
85     0:5    prey_b          70:Inf          0:4 2002  6163889.378
86    5:10    prey_b          70:Inf          0:4 2002  7704861.723
87  10:Inf    prey_b          70:Inf          0:4 2002  1540972.345
88     0:5 otherfood          70:Inf          0:4 2002    25691.387
89    5:10 otherfood          70:Inf          0:4 2002        0.000
90  10:Inf otherfood          70:Inf          0:4 2002        0.000
91     0:5    prey_a           50:70          6:9 2002  2318453.852
92    5:10    prey_a           50:70          6:9 2002  2898067.315
93  10:Inf    prey_a           50:70          6:9 2002   579613.463
94     0:5    prey_b           50:70          6:9 2002  4636907.703
95    5:10    prey_b           50:70          6:9 2002  5796134.629
96  10:Inf    prey_b           50:70          6:9 2002  1159226.926
97     0:5 otherfood           50:70          6:9 2002    19326.854
98    5:10 otherfood           50:70          6:9 2002        0.000
99  10:Inf otherfood           50:70          6:9 2002        0.000
100    0:5    prey_a          70:Inf          6:9 2002  4109259.585
101   5:10    prey_a          70:Inf          6:9 2002  5136574.482
102 10:Inf    prey_a          70:Inf          6:9 2002  1027314.896
103    0:5    prey_b          70:Inf          6:9 2002  8218519.171
104   5:10    prey_b          70:Inf          6:9 2002 10273148.963
105 10:Inf    prey_b          70:Inf          6:9 2002  2054629.793
106    0:5 otherfood          70:Inf          6:9 2002    34255.183
107   5:10 otherfood          70:Inf          6:9 2002        0.000
108 10:Inf otherfood          70:Inf          6:9 2002        0.000
109    0:5    prey_a           50:70          0:4 2003  1159226.812
110   5:10    prey_a           50:70          0:4 2003  1449033.515
111 10:Inf    prey_a           50:70          0:4 2003   289806.703
112    0:5    prey_b           50:70          0:4 2003  2318453.624
113   5:10    prey_b           50:70          0:4 2003  2898067.030
114 10:Inf    prey_b           50:70          0:4 2003   579613.406
115    0:5 otherfood           50:70          0:4 2003     9664.850
116   5:10 otherfood           50:70          0:4 2003        0.000
117 10:Inf otherfood           50:70          0:4 2003        0.000
118    0:5    prey_a          70:Inf          0:4 2003  2054629.591
119   5:10    prey_a          70:Inf          0:4 2003  2568286.989
120 10:Inf    prey_a          70:Inf          0:4 2003   513657.398
121    0:5    prey_b          70:Inf          0:4 2003  4109259.182
122   5:10    prey_b          70:Inf          0:4 2003  5136573.977
123 10:Inf    prey_b          70:Inf          0:4 2003  1027314.795
124    0:5 otherfood          70:Inf          0:4 2003    17130.114
125   5:10 otherfood          70:Inf          0:4 2003        0.000
126 10:Inf otherfood          70:Inf          0:4 2003        0.000
127    0:5    prey_a           50:70          6:9 2003  2318453.624
128   5:10    prey_a           50:70          6:9 2003  2898067.030
129 10:Inf    prey_a           50:70          6:9 2003   579613.406
130    0:5    prey_b           50:70          6:9 2003  4636907.248
131   5:10    prey_b           50:70          6:9 2003  5796134.060
132 10:Inf    prey_b           50:70          6:9 2003  1159226.812
133    0:5 otherfood           50:70          6:9 2003    19329.701
134   5:10 otherfood           50:70          6:9 2003        0.000
135 10:Inf otherfood           50:70          6:9 2003        0.000
136    0:5    prey_a          70:Inf          6:9 2003  4109259.182
137   5:10    prey_a          70:Inf          6:9 2003  5136573.977
138 10:Inf    prey_a          70:Inf          6:9 2003  1027314.795
139    0:5    prey_b          70:Inf          6:9 2003  8218518.364
140   5:10    prey_b          70:Inf          6:9 2003 10273147.954
141 10:Inf    prey_b          70:Inf          6:9 2003  2054629.591
142    0:5 otherfood          70:Inf          6:9 2003    34260.228
143   5:10 otherfood          70:Inf          6:9 2003        0.000
144 10:Inf otherfood          70:Inf          6:9 2003        0.000
145    0:5    prey_a           50:70          0:4 2004   579613.349
146   5:10    prey_a           50:70          0:4 2004   724516.686
147 10:Inf    prey_a           50:70          0:4 2004   144903.337
148    0:5    prey_b           50:70          0:4 2004  1159226.698
149   5:10    prey_b           50:70          0:4 2004  1449033.373
150 10:Inf    prey_b           50:70          0:4 2004   289806.675
151    0:5 otherfood           50:70          0:4 2004     4833.137
152   5:10 otherfood           50:70          0:4 2004        0.000
153 10:Inf otherfood           50:70          0:4 2004        0.000
154    0:5    prey_a          70:Inf          0:4 2004  1027314.695
155   5:10    prey_a          70:Inf          0:4 2004  1284143.368
156 10:Inf    prey_a          70:Inf          0:4 2004   256828.674
157    0:5    prey_b          70:Inf          0:4 2004  2054629.389
158   5:10    prey_b          70:Inf          0:4 2004  2568286.736
159 10:Inf    prey_b          70:Inf          0:4 2004   513657.347
160    0:5 otherfood          70:Inf          0:4 2004     8566.319
161   5:10 otherfood          70:Inf          0:4 2004        0.000
162 10:Inf otherfood          70:Inf          0:4 2004        0.000
163    0:5    prey_a           50:70          6:9 2004  2318453.396
164   5:10    prey_a           50:70          6:9 2004  2898066.745
165 10:Inf    prey_a           50:70          6:9 2004   579613.349
166    0:5    prey_b           50:70          6:9 2004  4636906.792
167   5:10    prey_b           50:70          6:9 2004  5796133.490
168 10:Inf    prey_b           50:70          6:9 2004  1159226.698
169    0:5 otherfood           50:70          6:9 2004    19332.548
170   5:10 otherfood           50:70          6:9 2004        0.000
171 10:Inf otherfood           50:70          6:9 2004        0.000
172    0:5    prey_a          70:Inf          6:9 2004  4109258.778
173   5:10    prey_a          70:Inf          6:9 2004  5136573.473
174 10:Inf    prey_a          70:Inf          6:9 2004  1027314.695
175    0:5    prey_b          70:Inf          6:9 2004  8218517.556
176   5:10    prey_b          70:Inf          6:9 2004 10273146.945
177 10:Inf    prey_b          70:Inf          6:9 2004  2054629.389
178    0:5 otherfood          70:Inf          6:9 2004    34265.274
179   5:10 otherfood          70:Inf          6:9 2004        0.000
180 10:Inf otherfood          70:Inf          6:9 2004        0.000
# NB: Ages 0..4 have all grown up by this point, none left
181    0:5    prey_a           50:70          0:4 2005        0.000
182   5:10    prey_a           50:70          0:4 2005        0.000
183 10:Inf    prey_a           50:70          0:4 2005        0.000
184    0:5    prey_b           50:70          0:4 2005        0.000
185   5:10    prey_b           50:70          0:4 2005        0.000
186 10:Inf    prey_b           50:70          0:4 2005        0.000
187    0:5 otherfood           50:70          0:4 2005        0.000
188   5:10 otherfood           50:70          0:4 2005        0.000
189 10:Inf otherfood           50:70          0:4 2005        0.000
190    0:5    prey_a          70:Inf          0:4 2005        0.000
191   5:10    prey_a          70:Inf          0:4 2005        0.000
192 10:Inf    prey_a          70:Inf          0:4 2005        0.000
193    0:5    prey_b          70:Inf          0:4 2005        0.000
194   5:10    prey_b          70:Inf          0:4 2005        0.000
195 10:Inf    prey_b          70:Inf          0:4 2005        0.000
196    0:5 otherfood          70:Inf          0:4 2005        0.000
197   5:10 otherfood          70:Inf          0:4 2005        0.000
198 10:Inf otherfood          70:Inf          0:4 2005        0.000
199    0:5    prey_a           50:70          6:9 2005  2318453.168
200   5:10    prey_a           50:70          6:9 2005  2898066.460
201 10:Inf    prey_a           50:70          6:9 2005   579613.292
202    0:5    prey_b           50:70          6:9 2005  4636906.337
203   5:10    prey_b           50:70          6:9 2005  5796132.921
204 10:Inf    prey_b           50:70          6:9 2005  1159226.584
205    0:5 otherfood           50:70          6:9 2005    19335.396
206   5:10 otherfood           50:70          6:9 2005        0.000
207 10:Inf otherfood           50:70          6:9 2005        0.000
208    0:5    prey_a          70:Inf          6:9 2005  4109258.374
209   5:10    prey_a          70:Inf          6:9 2005  5136572.968
210 10:Inf    prey_a          70:Inf          6:9 2005  1027314.594
211    0:5    prey_b          70:Inf          6:9 2005  8218516.748
212   5:10    prey_b          70:Inf          6:9 2005 10273145.936
213 10:Inf    prey_b          70:Inf          6:9 2005  2054629.187
214    0:5 otherfood          70:Inf          6:9 2005    34270.322
215   5:10 otherfood          70:Inf          6:9 2005        0.000
216 10:Inf otherfood          70:Inf          6:9 2005        0.000
'), "cdist_sumofsquares_pred_a_catch_model__num: Baseline output")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## consumption.m3
