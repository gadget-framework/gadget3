if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })

library(unittest)

library(gadget3)

prey_a <- g3_stock('prey_a', seq(1, 10)) |> g3s_age(1,3) |> g3s_livesonareas(c(1,2))
prey_b <- g3_stock('prey_b', seq(1, 10)) |> g3s_age(1,3) |> g3s_livesonareas(c(2,3))
fleet_a <- g3_fleet('fleet_a') |> g3s_livesonareas(c(1))
fleet_b <- g3_fleet('fleet_b') |> g3s_livesonareas(c(1))
fleet_c <- g3_fleet('fleet_c') |> g3s_livesonareas(c(1))
pred_a <- g3_stock('pred_a', seq(50, 80, by = 10)) |> g3s_age(0, 10) |> g3s_livesonareas(c(1,2,3))

actions <- list(
    g3a_time(2000, 2002, step_lengths = c(6,6), project_years = 0),
    gadget3:::g3a_initialconditions_manual(prey_a, ~1e10 + 0 * prey_a__midlen, ~100),
    gadget3:::g3a_initialconditions_manual(prey_b, ~2e10 + 0 * prey_b__midlen, ~200),
    gadget3:::g3a_initialconditions_manual(pred_a, ~1e5 + 0 * pred_a__midlen, ~1000),
    # NB: Don't call g3a_suitability_report() directly, use g3a_predate() interface
    g3a_predate(
        fleet_a,
        list(prey_a, prey_b),
        suitabilities = list(
            prey_a = g3_suitability_exponentiall50(),
            prey_b = g3_suitability_andersenfleet() ),
        catchability_f = g3a_predate_catchability_totalfleet(0) ),
    g3a_predate(
        fleet_b,
        list(prey_a),
        suitabilities = quote( cur_year * age ),
        catchability_f = g3a_predate_catchability_totalfleet(0) ),
    g3a_predate(
        fleet_c,
        list(prey_a),
        suitabilities = quote( cur_step * stock__midlen ),
        catchability_f = g3a_predate_catchability_totalfleet(0) ),
    g3a_predate(
        pred_a,
        list(prey_a, prey_b),
        suitabilities = list(
            prey_a = g3_suitability_andersen(p0 = 0, p1 = log(2), p2 = 1, p4 = 0.1),
            prey_b = g3_suitability_andersen(p0 = quote( age ), p1 = log(2), p2 = 1, p4 = 0.1) ),
        catchability_f = g3a_predate_catchability_totalfleet(0) ),
    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood() )
actions <- c(actions, list(
    g3a_report_history(actions, "suit_.*__report"),
    g3a_report_detail(actions) ))
model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)

ok_group("Report dimensions") ########
params <- attr(model_fn, 'parameter_template')
result <- model_fn(params)
r <- attributes(result)

ok(ut_cmp_identical(
    dimnames(r$suit_prey_a_fleet_a__report),
    list(
        length = c("1:2", "2:3", "3:4", "4:5", "5:6", "6:7", "7:8", "8:9", "9:10", "10:Inf") )), "suit_prey_a_fleet_a__report")
ok(ut_cmp_identical(
    dimnames(r$suit_prey_b_fleet_a__report),
    list(
        length = c("1:2", "2:3", "3:4", "4:5", "5:6", "6:7", "7:8", "8:9", "9:10", "10:Inf") )), "suit_prey_b_fleet_a__report")
ok(ut_cmp_identical(
    dimnames(r$suit_prey_a_pred_a__report),
    list(
        length = c("1:2", "2:3", "3:4", "4:5", "5:6", "6:7", "7:8", "8:9", "9:10", "10:Inf"),
        predator_length = c("50:60", "60:70", "70:80", "80:Inf") )), "suit_prey_a_pred_a__report")
ok(ut_cmp_identical(
    dimnames(r$suit_prey_b_pred_a__report),
    list(
        length = c("1:2", "2:3", "3:4", "4:5", "5:6", "6:7", "7:8", "8:9", "9:10", "10:Inf"),
        age = c("age1", "age2", "age3"),
        predator_length = c("50:60", "60:70", "70:80", "80:Inf") )), "suit_prey_b_pred_a__report")

ok(gadget3:::ut_cmp_df(as.data.frame(r$hist_suit_prey_a_fleet_b__report), '
     2000-01 2000-02 2001-01 2001-02 2002-01 2002-02
age1    2000    2000    2001    2001    2002    2002
age2    4000    4000    4002    4002    4004    4004
age3    6000    6000    6003    6003    6006    6006
'), "hist_suit_prey_a_fleet_b__report: Updated every year")

ok(gadget3:::ut_cmp_df(as.data.frame(r$hist_suit_prey_a_fleet_c__report), '
       2000-01 2000-02 2001-01 2001-02 2002-01 2002-02
1:2        1.5       3     1.5       3     1.5       3
2:3        2.5       5     2.5       5     2.5       5
3:4        3.5       7     3.5       7     3.5       7
4:5        4.5       9     4.5       9     4.5       9
5:6        5.5      11     5.5      11     5.5      11
6:7        6.5      13     6.5      13     6.5      13
7:8        7.5      15     7.5      15     7.5      15
8:9        8.5      17     8.5      17     8.5      17
9:10       9.5      19     9.5      19     9.5      19
10:Inf    10.5      21    10.5      21    10.5      21
'), "hist_suit_prey_a_fleet_c__report: Flips each step")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## Report dimensions

ok_group("Randomise parameters, check values") ########
params[grepl('prey', names(params))] <- runif(sum(grepl("prey", names(params))))
result <- model_fn(params)
r <- attributes(result)

ok(ut_cmp_equal(
    as.vector(r$suit_prey_a_fleet_a__report),
    as.vector(g3_eval(g3_suitability_exponentiall50(
         alpha = params$prey_a.fleet_a.alpha,
         l50 = params$prey_a.fleet_a.l50 ), stock = prey_a, predstock = fleet_a ))), "suit_prey_a_fleet_a__report")
ok(ut_cmp_equal(
    as.vector(r$suit_prey_b_fleet_a__report),
    as.vector(g3_eval(g3_suitability_andersenfleet(
         p0 = params$prey_b.andersen.p0,
         p1 = params$prey_b.fleet_a.andersen.p1,
         p2 = params$prey_b.andersen.p2,
         p3 = exp(params$prey_b.fleet_a.andersen.p3_exp),
         p4 = exp(params$prey_b.fleet_a.andersen.p4_exp) ), stock = prey_b, predstock = fleet_a ))), "suit_prey_b_fleet_a__report")

for (predator_length_idx in seq_along(g3_stock_def(pred_a, "midlen"))) {
    predator_length <- g3_stock_def(pred_a, "midlen")[[predator_length_idx]]
    ok(ut_cmp_equal(
        as.vector(r$suit_prey_a_pred_a__report[,predator_length = predator_length_idx]),
        as.vector(g3_eval(g3_suitability_andersen(
            p0 = 0,
            p1 = log(2),
            p2 = 1,
            p4 = 0.1 ), stock = prey_a, predstock = pred_a, predator_length = predator_length ))), "suit_prey_a_pred_a__report")
    for (age in seq(g3_stock_def(prey_b, "minage"), g3_stock_def(prey_b, "maxage"))) {
        age_idx <- age - g3_stock_def(prey_b, "minage") + 1
        ok(ut_cmp_equal(
            as.vector(r$suit_prey_b_pred_a__report[,age = age_idx, predator_length = predator_length_idx]),
            as.vector(g3_eval(g3_suitability_andersen(
                p0 = age,
                p1 = log(2),
                p2 = 1,
                p4 = 0.1 ), stock = prey_b, predstock = pred_a, predator_length = predator_length, age = age ))), "suit_prey_b_pred_a__report")
    }
}

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
######## Randomise parameters, check values
