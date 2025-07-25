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
    predator = c('pred_a'),  # NB: Not essential if there's only one predator, only for testing
    predator_length = c(50,70),
    predator_age = c("[0,5)", "[6,10)"), # ((
    number = 0 )

pred_a_preypref_obs <- expand.grid(
    year = 2000:2005,
    predator_length = c(50,70),
    stock = c('prey_a', 'prey_b', 'otherfood'),
    number = 0 )

pred_a_sizepref_obs <- expand.grid(
    year = 2000:2005,
    predator_age = c("[0,5)", "[6,10)"), # ((
    length = seq(1, 10),  # i.e. the union of prey_a / prey_b lengths
    number = 0 )

actions <- list(
    g3a_time(2000, 2005, step_lengths = c(6,6), project_years = 0),
    g3a_age(prey_a),
    g3a_age(prey_b),
    g3a_age(pred_a),
    gadget3:::g3a_initialconditions_manual(prey_a, ~1e10 + 0 * prey_a__midlen, ~100),
    gadget3:::g3a_initialconditions_manual(prey_b, ~2e10 + 0 * prey_b__midlen, ~200),
    gadget3:::g3a_initialconditions_manual(pred_a, ~1e5 + 0 * pred_a__midlen, ~1000),
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
        predators = list(pred_a),
        stocks = list(prey_a, prey_b, otherfood),
        function_f = g3l_distribution_sumofsquares(),
        nll_breakdown = TRUE,
        report = TRUE ),

    g3l_catchdistribution(
        'pred_a_preypref',
        pred_a_preypref_obs,
        predators = list(pred_a),
        stocks = list(prey_a, prey_b, otherfood),
        function_f = g3l_distribution_sumofsquares(),
        nll_breakdown = TRUE,
        report = TRUE ),

    g3l_catchdistribution(
        'pred_a_sizepref',
        pred_a_sizepref_obs,
        predators = list(pred_a),
        stocks = list(prey_a, prey_b),  # NB: otherfood missing
        function_f = g3l_distribution_sumofsquares(),
        nll_breakdown = TRUE,
        report = TRUE ),

    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood() )
actions <- c(actions, list(
    g3a_report_history(actions, var_re = "__feedinglevel$|__totalsuit$"),
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
    def_r$detail_prey_a_pred_a__suit[] * 2,
    r$detail_prey_a_pred_a__suit[],
    tolerance = 1e-6), "detail_prey_a_pred_a__suit: Doubled thanks to energycontent")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## Energy content

ok_group("consumption.m3") ########

ok(ut_cmp_equal(
    diff(def_r$detail_prey_a_pred_a__cons[1,1,,1,1]),
    c("60:70" = 0, "70:80" = 0, "80:Inf" = 0),
    tolerance = 1e-8 ), "def_r$detail_prey_a_pred_a__cons: No difference in pred.length consumption")

params <- attr(model_fn, 'parameter_template')
params$pred_a.consumption.m3 <- 2
params$pred_a.halffeeding <- 1e15  # Needs to be proportion higher than total_predsuit to see any result
result <- model_fn(params) ; r <- attributes(result)

ok(all.equal(
    def_r$detail_prey_a_pred_a__suit,
    r$detail_prey_a_pred_a__suit,
    tolerance = 1e-3 ), "r$detail_prey_a_pred_a__suit: consumption.m3 results in approximately equal __suit")

ok(ut_cmp_equal(
    r$detail_prey_a_pred_a__cons[1,1,2,1,1],
    r$detail_prey_a_pred_a__cons[1,1,1,1,1] / 55^2 * 65^2,
    tolerance = 1e-8 ), "r$detail_prey_a_pred_a__cons: Jump in consumption 55 -> 65")
ok(ut_cmp_equal(
    r$detail_prey_a_pred_a__cons[1,1,3,1,1],
    r$detail_prey_a_pred_a__cons[1,1,2,1,1] / 65^2 * 75^2,
    tolerance = 1e-8 ), "r$detail_prey_a_pred_a__cons: Jump in consumption 65 -> 75")
ok(ut_cmp_equal(
    r$detail_prey_a_pred_a__cons[1,1,4,1,1],
    r$detail_prey_a_pred_a__cons[1,1,3,1,1] / 75^2 * 85^2,
    tolerance = 1e-8 ), "r$detail_prey_a_pred_a__cons: Jump in consumption 75 -> 85")

ok(ut_cmp_identical(
    dimnames(r$cdist_sumofsquares_pred_a_catch_model__num)$predator,
    c("pred_a") ), "cdist_sumofsquares_pred_a_catch_model__num: Single predator dimension for our one predator")

ok(gadget3:::ut_cmp_array(drop(r$cdist_sumofsquares_pred_a_catch_model__num), '
    length     stock predator_length predator_age time        Freq
1      0:5    prey_a           50:70          0:4 2000  84464.2913
2     5:10    prey_a           50:70          0:4 2000 105580.3641
3   10:Inf    prey_a           50:70          0:4 2000  21116.0728
4      0:5    prey_b           50:70          0:4 2000 168928.5826
5     5:10    prey_b           50:70          0:4 2000 211160.7283
6   10:Inf    prey_b           50:70          0:4 2000  42232.1457
7      0:5 otherfood           50:70          0:4 2000    703.8698
# NB: All otherfood are length 0
8     5:10 otherfood           50:70          0:4 2000      0.0000
9   10:Inf otherfood           50:70          0:4 2000      0.0000
10     0:5    prey_a          70:Inf          0:4 2000 149705.6750
11    5:10    prey_a          70:Inf          0:4 2000 187132.0937
12  10:Inf    prey_a          70:Inf          0:4 2000  37426.4187
13     0:5    prey_b          70:Inf          0:4 2000 299411.3499
14    5:10    prey_b          70:Inf          0:4 2000 374264.1874
15  10:Inf    prey_b          70:Inf          0:4 2000  74852.8375
16     0:5 otherfood          70:Inf          0:4 2000   1247.5486
17    5:10 otherfood          70:Inf          0:4 2000      0.0000
18  10:Inf otherfood          70:Inf          0:4 2000      0.0000
19     0:5    prey_a           50:70          6:9 2000  67571.4331
20    5:10    prey_a           50:70          6:9 2000  84464.2913
21  10:Inf    prey_a           50:70          6:9 2000  16892.8583
22     0:5    prey_b           50:70          6:9 2000 135142.8661
23    5:10    prey_b           50:70          6:9 2000 168928.5826
24  10:Inf    prey_b           50:70          6:9 2000  33785.7165
25     0:5 otherfood           50:70          6:9 2000    563.0959
26    5:10 otherfood           50:70          6:9 2000      0.0000
27  10:Inf otherfood           50:70          6:9 2000      0.0000
28     0:5    prey_a          70:Inf          6:9 2000 119764.5400
29    5:10    prey_a          70:Inf          6:9 2000 149705.6750
30  10:Inf    prey_a          70:Inf          6:9 2000  29941.1350
31     0:5    prey_b          70:Inf          6:9 2000 239529.0799
32    5:10    prey_b          70:Inf          6:9 2000 299411.3499
33  10:Inf    prey_b          70:Inf          6:9 2000  59882.2700
34     0:5 otherfood          70:Inf          6:9 2000    998.0389
35    5:10 otherfood          70:Inf          6:9 2000      0.0000
36  10:Inf otherfood          70:Inf          6:9 2000      0.0000
37     0:5    prey_a           50:70          0:4 2001  67571.1514
38    5:10    prey_a           50:70          0:4 2001  84463.9393
39  10:Inf    prey_a           50:70          0:4 2001  16892.7879
40     0:5    prey_b           50:70          0:4 2001 135142.3028
41    5:10    prey_b           50:70          0:4 2001 168927.8785
42  10:Inf    prey_b           50:70          0:4 2001  33785.5757
43     0:5 otherfood           50:70          0:4 2001    563.0960
44    5:10 otherfood           50:70          0:4 2001      0.0000
45  10:Inf otherfood           50:70          0:4 2001      0.0000
46     0:5    prey_a          70:Inf          0:4 2001 119764.0408
47    5:10    prey_a          70:Inf          0:4 2001 149705.0510
48  10:Inf    prey_a          70:Inf          0:4 2001  29941.0102
49     0:5    prey_b          70:Inf          0:4 2001 239528.0816
50    5:10    prey_b          70:Inf          0:4 2001 299410.1019
51  10:Inf    prey_b          70:Inf          0:4 2001  59882.0204
52     0:5 otherfood          70:Inf          0:4 2001    998.0390
53    5:10 otherfood          70:Inf          0:4 2001      0.0000
54  10:Inf otherfood          70:Inf          0:4 2001      0.0000
55     0:5    prey_a           50:70          6:9 2001  67571.1514
56    5:10    prey_a           50:70          6:9 2001  84463.9393
57  10:Inf    prey_a           50:70          6:9 2001  16892.7879
58     0:5    prey_b           50:70          6:9 2001 135142.3028
59    5:10    prey_b           50:70          6:9 2001 168927.8785
60  10:Inf    prey_b           50:70          6:9 2001  33785.5757
61     0:5 otherfood           50:70          6:9 2001    563.0960
62    5:10 otherfood           50:70          6:9 2001      0.0000
63  10:Inf otherfood           50:70          6:9 2001      0.0000
64     0:5    prey_a          70:Inf          6:9 2001 119764.0408
65    5:10    prey_a          70:Inf          6:9 2001 149705.0510
66  10:Inf    prey_a          70:Inf          6:9 2001  29941.0102
67     0:5    prey_b          70:Inf          6:9 2001 239528.0816
68    5:10    prey_b          70:Inf          6:9 2001 299410.1019
69  10:Inf    prey_b          70:Inf          6:9 2001  59882.0204
70     0:5 otherfood          70:Inf          6:9 2001    998.0390
71    5:10 otherfood          70:Inf          6:9 2001      0.0000
72  10:Inf otherfood          70:Inf          6:9 2001      0.0000
73     0:5    prey_a           50:70          0:4 2002  50678.1523
74    5:10    prey_a           50:70          0:4 2002  63347.6904
75  10:Inf    prey_a           50:70          0:4 2002  12669.5381
76     0:5    prey_b           50:70          0:4 2002 101356.3047
77    5:10    prey_b           50:70          0:4 2002 126695.3808
78  10:Inf    prey_b           50:70          0:4 2002  25339.0762
79     0:5 otherfood           50:70          0:4 2002    422.3220
80    5:10 otherfood           50:70          0:4 2002      0.0000
81  10:Inf otherfood           50:70          0:4 2002      0.0000
82     0:5    prey_a          70:Inf          0:4 2002  89822.6562
83    5:10    prey_a          70:Inf          0:4 2002 112278.3202
84  10:Inf    prey_a          70:Inf          0:4 2002  22455.6640
85     0:5    prey_b          70:Inf          0:4 2002 179645.3124
86    5:10    prey_b          70:Inf          0:4 2002 224556.6405
87  10:Inf    prey_b          70:Inf          0:4 2002  44911.3281
88     0:5 otherfood          70:Inf          0:4 2002    748.5294
89    5:10 otherfood          70:Inf          0:4 2002      0.0000
90  10:Inf otherfood          70:Inf          0:4 2002      0.0000
91     0:5    prey_a           50:70          6:9 2002  67570.8698
92    5:10    prey_a           50:70          6:9 2002  84463.5872
93  10:Inf    prey_a           50:70          6:9 2002  16892.7174
94     0:5    prey_b           50:70          6:9 2002 135141.7395
95    5:10    prey_b           50:70          6:9 2002 168927.1744
96  10:Inf    prey_b           50:70          6:9 2002  33785.4349
97     0:5 otherfood           50:70          6:9 2002    563.0960
98    5:10 otherfood           50:70          6:9 2002      0.0000
99  10:Inf otherfood           50:70          6:9 2002      0.0000
100    0:5    prey_a          70:Inf          6:9 2002 119763.5416
101   5:10    prey_a          70:Inf          6:9 2002 149704.4270
102 10:Inf    prey_a          70:Inf          6:9 2002  29940.8854
103    0:5    prey_b          70:Inf          6:9 2002 239527.0832
104   5:10    prey_b          70:Inf          6:9 2002 299408.8540
105 10:Inf    prey_b          70:Inf          6:9 2002  59881.7708
106    0:5 otherfood          70:Inf          6:9 2002    998.0392
107   5:10 otherfood          70:Inf          6:9 2002      0.0000
108 10:Inf otherfood          70:Inf          6:9 2002      0.0000
109    0:5    prey_a           50:70          0:4 2003  33785.2941
110   5:10    prey_a           50:70          0:4 2003  42231.6176
111 10:Inf    prey_a           50:70          0:4 2003   8446.3235
112    0:5    prey_b           50:70          0:4 2003  67570.5881
113   5:10    prey_b           50:70          0:4 2003  84463.2352
114 10:Inf    prey_b           50:70          0:4 2003  16892.6470
115    0:5 otherfood           50:70          0:4 2003    281.5480
116   5:10 otherfood           50:70          0:4 2003      0.0000
117 10:Inf otherfood           50:70          0:4 2003      0.0000
118    0:5    prey_a          70:Inf          0:4 2003  59881.5212
119   5:10    prey_a          70:Inf          0:4 2003  74851.9015
120 10:Inf    prey_a          70:Inf          0:4 2003  14970.3803
121    0:5    prey_b          70:Inf          0:4 2003 119763.0424
122   5:10    prey_b          70:Inf          0:4 2003 149703.8030
123 10:Inf    prey_b          70:Inf          0:4 2003  29940.7606
124    0:5 otherfood          70:Inf          0:4 2003    499.0196
125   5:10 otherfood          70:Inf          0:4 2003      0.0000
126 10:Inf otherfood          70:Inf          0:4 2003      0.0000
127    0:5    prey_a           50:70          6:9 2003  67570.5881
128   5:10    prey_a           50:70          6:9 2003  84463.2352
129 10:Inf    prey_a           50:70          6:9 2003  16892.6470
130    0:5    prey_b           50:70          6:9 2003 135141.1763
131   5:10    prey_b           50:70          6:9 2003 168926.4703
132 10:Inf    prey_b           50:70          6:9 2003  33785.2941
133    0:5 otherfood           50:70          6:9 2003    563.0961
134   5:10 otherfood           50:70          6:9 2003      0.0000
135 10:Inf otherfood           50:70          6:9 2003      0.0000
136    0:5    prey_a          70:Inf          6:9 2003 119763.0424
137   5:10    prey_a          70:Inf          6:9 2003 149703.8030
138 10:Inf    prey_a          70:Inf          6:9 2003  29940.7606
139    0:5    prey_b          70:Inf          6:9 2003 239526.0848
140   5:10    prey_b          70:Inf          6:9 2003 299407.6060
141 10:Inf    prey_b          70:Inf          6:9 2003  59881.5212
142    0:5 otherfood          70:Inf          6:9 2003    998.0393
143   5:10 otherfood          70:Inf          6:9 2003      0.0000
144 10:Inf otherfood          70:Inf          6:9 2003      0.0000
145    0:5    prey_a           50:70          0:4 2004  16892.5766
146   5:10    prey_a           50:70          0:4 2004  21115.7208
147 10:Inf    prey_a           50:70          0:4 2004   4223.1442
148    0:5    prey_b           50:70          0:4 2004  33785.1532
149   5:10    prey_b           50:70          0:4 2004  42231.4416
150 10:Inf    prey_b           50:70          0:4 2004   8446.2883
151    0:5 otherfood           50:70          0:4 2004    140.7740
152   5:10 otherfood           50:70          0:4 2004      0.0000
153 10:Inf otherfood           50:70          0:4 2004      0.0000
154    0:5    prey_a          70:Inf          0:4 2004  29940.6358
155   5:10    prey_a          70:Inf          0:4 2004  37425.7948
156 10:Inf    prey_a          70:Inf          0:4 2004   7485.1590
157    0:5    prey_b          70:Inf          0:4 2004  59881.2716
158   5:10    prey_b          70:Inf          0:4 2004  74851.5895
159 10:Inf    prey_b          70:Inf          0:4 2004  14970.3179
160    0:5 otherfood          70:Inf          0:4 2004    249.5099
161   5:10 otherfood          70:Inf          0:4 2004      0.0000
162 10:Inf otherfood          70:Inf          0:4 2004      0.0000
163    0:5    prey_a           50:70          6:9 2004  67570.3065
164   5:10    prey_a           50:70          6:9 2004  84462.8831
165 10:Inf    prey_a           50:70          6:9 2004  16892.5766
166    0:5    prey_b           50:70          6:9 2004 135140.6130
167   5:10    prey_b           50:70          6:9 2004 168925.7662
168 10:Inf    prey_b           50:70          6:9 2004  33785.1532
169    0:5 otherfood           50:70          6:9 2004    563.0962
170   5:10 otherfood           50:70          6:9 2004      0.0000
171 10:Inf otherfood           50:70          6:9 2004      0.0000
172    0:5    prey_a          70:Inf          6:9 2004 119762.5432
173   5:10    prey_a          70:Inf          6:9 2004 149703.1790
174 10:Inf    prey_a          70:Inf          6:9 2004  29940.6358
175    0:5    prey_b          70:Inf          6:9 2004 239525.0865
176   5:10    prey_b          70:Inf          6:9 2004 299406.3581
177 10:Inf    prey_b          70:Inf          6:9 2004  59881.2716
178    0:5 otherfood          70:Inf          6:9 2004    998.0394
179   5:10 otherfood          70:Inf          6:9 2004      0.0000
180 10:Inf otherfood          70:Inf          6:9 2004      0.0000
# NB: Ages 0..4 have all grown up by this point, none left
181    0:5    prey_a           50:70          0:4 2005      0.0000
182   5:10    prey_a           50:70          0:4 2005      0.0000
183 10:Inf    prey_a           50:70          0:4 2005      0.0000
184    0:5    prey_b           50:70          0:4 2005      0.0000
185   5:10    prey_b           50:70          0:4 2005      0.0000
186 10:Inf    prey_b           50:70          0:4 2005      0.0000
187    0:5 otherfood           50:70          0:4 2005      0.0000
188   5:10 otherfood           50:70          0:4 2005      0.0000
189 10:Inf otherfood           50:70          0:4 2005      0.0000
190    0:5    prey_a          70:Inf          0:4 2005      0.0000
191   5:10    prey_a          70:Inf          0:4 2005      0.0000
192 10:Inf    prey_a          70:Inf          0:4 2005      0.0000
193    0:5    prey_b          70:Inf          0:4 2005      0.0000
194   5:10    prey_b          70:Inf          0:4 2005      0.0000
195 10:Inf    prey_b          70:Inf          0:4 2005      0.0000
196    0:5 otherfood          70:Inf          0:4 2005      0.0000
197   5:10 otherfood          70:Inf          0:4 2005      0.0000
198 10:Inf otherfood          70:Inf          0:4 2005      0.0000
199    0:5    prey_a           50:70          6:9 2005  67570.0249
200   5:10    prey_a           50:70          6:9 2005  84462.5311
201 10:Inf    prey_a           50:70          6:9 2005  16892.5062
202    0:5    prey_b           50:70          6:9 2005 135140.0497
203   5:10    prey_b           50:70          6:9 2005 168925.0621
204 10:Inf    prey_b           50:70          6:9 2005  33785.0124
205    0:5 otherfood           50:70          6:9 2005    563.0962
206   5:10 otherfood           50:70          6:9 2005      0.0000
207 10:Inf otherfood           50:70          6:9 2005      0.0000
208    0:5    prey_a          70:Inf          6:9 2005 119762.0440
209   5:10    prey_a          70:Inf          6:9 2005 149702.5551
210 10:Inf    prey_a          70:Inf          6:9 2005  29940.5110
211    0:5    prey_b          70:Inf          6:9 2005 239524.0881
212   5:10    prey_b          70:Inf          6:9 2005 299405.1101
213 10:Inf    prey_b          70:Inf          6:9 2005  59881.0220
214    0:5 otherfood          70:Inf          6:9 2005    998.0395
215   5:10 otherfood          70:Inf          6:9 2005      0.0000
216 10:Inf otherfood          70:Inf          6:9 2005      0.0000
'), "cdist_sumofsquares_pred_a_catch_model__num: Baseline output")

for (t in dimnames(r$hist_pred_a__feedinglevel)$time) {
    ok(all(r$hist_pred_a__feedinglevel[,,time = t] == r$hist_pred_a__feedinglevel[1,1,time = t]), paste0(
        "r$hist_pred_a__feedinglevel: Not dependent on length/age", t))
}
ok(ut_cmp_equal(r$hist_pred_a__feedinglevel[1,1,], c(
    "2000-01" = 0.0291450651443661,
    "2000-02" = 0.0291450044465519,
    "2001-01" = 0.0291449437488566,
    "2001-02" = 0.0291448830512803,
    "2002-01" = 0.0291448223538228,
    "2002-02" = 0.0291447616564842,
    "2003-01" = 0.0291447009592646,
    "2003-02" = 0.0291446402621638,
    "2004-01" = 0.029144579565182,
    "2004-02" = 0.029144518868319,
    "2005-01" = 0.029144458171575,
    "2005-02" = 0.0291443974749498,
    NULL ), tolerance = 1e-8), "r$hist_pred_a__feedinglevel[1,1,]: Gentrly dropping as predators age out")

ok(ut_cmp_identical(dimnames(r$cdist_sumofsquares_pred_a_sizepref_model__num), list(
    length = c("1:2", "2:3", "3:4", "4:5", "5:6", "6:7", "7:8", "8:9", "9:10", "10:Inf"),
    predator_age = c("0:4", "6:9"),
    time = c("2000", "2001", "2002", "2003", "2004", "2005") )), "r$cdist_sumofsquares_pred_a_sizepref_model__num: expected dimensions")

ok(ut_cmp_identical(dimnames(r$cdist_sumofsquares_pred_a_preypref_model__num), list(
    length = "0:Inf",
    stock = c("prey_a", "prey_b", "otherfood"),
    predator_length = c("50:70", "70:Inf"),
    time = c("2000", "2001", "2002", "2003", "2004", "2005") )), "r$cdist_sumofsquares_pred_a_preypref_model__num: expected dimensions")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## consumption.m3
