if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

ok_group("g3s_sparsedata:area", {
    sd <- gadget3:::g3s_sparsedata("frank", data.frame(
        year = 1994,
        area = c("a", "a", "c"),
        length = 2,
        stringsAsFactors = FALSE), area_group = c(a=1,b=2,c=3))
    model_fn <- g3_to_r(list(g3a_time(1990, 1999), g3_step(g3_formula(
        stock_iterate(sd, {
            print(c(year = cur_year, length = length, area = area, sd__i = stock_ss(sd__i, vec = single)))
        }),
        sd__i = gadget3:::g3_sparsedata_instance(sd, 50.1 * seq_along(g3_stock_def(sd, 'year'))),
        sd = sd ))))
    ok(ut_cmp_identical(capture.output(model_fn()), c(
        "  year length   area  sd__i ",
        "1994.0    2.0    1.0   50.1 ",
        "  year length   area  sd__i ",
        "1994.0    2.0    1.0  100.2 ",
        "  year length   area  sd__i ",
        "1994.0    2.0    3.0  150.3 ",
        "[1] 0")), "model_fn, iterated over numeric areas")
})

st <- g3_stock("stst", c(10, 20, 30)) |> g3s_age(3,5)
fl <- g3_fleet(c("fl", "surv"))

obs_linreg_df <- data.frame(
    # NB: No 1993, just ignored
    year = rep(c(1990, 1991, 1992, 1994), each = 2),
    step = 1:2 )
obs_linreg_df$age <- floor(runif(nrow(obs_linreg_df), min = 3, max = 5.1))
obs_linreg_df$length <- floor(runif(nrow(obs_linreg_df), min = 10, max = 50))
obs_linreg_df$mean <- runif(nrow(obs_linreg_df), min = 10, max = 1000)
# Occasionally test with integer values
if (runif(1) < 0.5) obs_linreg_df$mean <- as.integer(obs_linreg_df$mean)

obs_ss_df <- data.frame(
    # NB: No step, just year
    year = c(1990, 1992, 1993) )
obs_ss_df$mean = runif(nrow(obs_ss_df), min = 10, max = 1000)

obs_flc_df <- data.frame(
    year = rep(c(1990, 1991, 1992, 1994), each = 2) )
obs_flc_df$length <- floor(runif(nrow(obs_linreg_df), min = 10, max = 50))
obs_flc_df$mean <- runif(nrow(obs_flc_df), min = 10, max = 1000)
obs_flc_df$stddev <- runif(nrow(obs_flc_df), min = 0.01, max = 0.1)
obs_flc_df$number <- as.integer(runif(nrow(obs_flc_df), min = 10, max = 10))

actions <- list(
    g3a_time(1990, 1994, c(6,6)),
    g3a_otherfood(st,
        quote( age * 100 + stock__minlen ),
        quote( cur_year * 1e5 + cur_step * 1e4 + 0 * stock__minlen ) ),
    g3a_predate(
        fl,
        list(st),
        suitabilities = g3_suitability_exponentiall50(),
        catchability_f = g3a_predate_catchability_numberfleet(g3_parameterized("predate_num", value = 0)) ),
    g3l_sparsesample(
        "bt",
        obs_linreg_df,
        list(st),
        measurement_f = g3_formula(
            wgt + length,
            end = NULL ),
        function_f = g3l_sparsesample_linreg(fit = "linear") ),
    g3l_sparsesample(
        "cs_model",
        obs_ss_df,
        list(st),
        measurement_f = g3_formula(
            wgt * age,
            end = NULL ),
        function_f = g3l_sparsesample_sumsquares(weighting = 'model_stddev') ),
    g3l_sparsesample(
        "flc",
        obs_flc_df,
        list(st),
        predstocks = list(fl),
        measurement_f = quote(wgt + length),
        function_f = g3l_sparsesample_sumsquares(weighting = 'obs_stddev') ),
    # NB: Dummy parameter so model will compile in TMB
    quote( nll <- nll + g3_param("x", value = 0) ) )
full_actions <- c(actions, list(
    g3a_report_history(actions, var_re = "__num$|__wgt$|__cons$|^nll_sp(abund|catch)_.+__(model|obs)_.+$"),
    g3a_report_history(actions, var_re = "^nll_sp(abund|catch)_.+__nll$", out_prefix = NULL),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_fn, 'parameter_template') |>
    g3_init_val("bt_weight", runif(1, 0.5, 5)) |>
    g3_init_val("cs_model_weight", runif(1, 0.5, 5)) |>
    g3_init_val("flc_weight", 0) |>
    identity() -> params

nll <- model_fn(params) ; r <- attributes(nll) ; nll <- as.vector(nll)

# Final result can be derived from stock abundance
expected_m <- r$hist_stst__wgt + c(15, 25, 35)
length_grp <- as.character(cut(obs_linreg_df$length, c(9, 20, 30, Inf), right = FALSE, labels = c("10:20", "20:30", "30:Inf")))
for (i in seq_len(nrow(obs_linreg_df))) {
    m <- expected_m[
        length = length_grp[[i]],
        age = sprintf("age%d", obs_linreg_df[i, "age"]),
        time = sprintf("%04d-%02d", obs_linreg_df[i, "year"], obs_linreg_df[i, "step"]) ]
    count <- r$hist_stst__num[
        length = length_grp[[i]],
        age = sprintf("age%d", obs_linreg_df[i, "age"]),
        time = sprintf("%04d-%02d", obs_linreg_df[i, "year"], obs_linreg_df[i, "step"]) ]
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spabund_bt__model_sum[i, '1994-02']),
        sum(m * count) ), paste0("r$hist_nll_spabund_bt__model_sum[", i, ", '1994-02']"))
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spabund_bt__model_sqsum[i, '1994-02']),
        sum(m**2 * count) ), paste0("r$hist_nll_spabund_bt__model_sqsum[", i, ", '1994-02']"))
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spabund_bt__model_n[i, '1994-02']),
        sum(count) ), paste0("r$hist_nll_spabund_bt__model_n[", i, ", '1994-02']"))
}
ok(ut_cmp_equal(
    r$hist_nll_spabund_bt__model_sum,
    ifelse(r$hist_nll_spabund_bt__model_n > 0, r$hist_nll_spabund_bt__model_sum[,"1994-02"], 0),
    end = NULL), "hist_nll_spabund_bt__model_sum: History is the same shape as __model_n")
ok(gadget3:::ut_cmp_df(as.data.frame(r$hist_nll_spabund_bt__model_n > 0), '
1990-01 1990-02 1991-01 1991-02 1992-01 1992-02 1993-01 1993-02 1994-01 1994-02
   TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
  FALSE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
  FALSE   FALSE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
  FALSE   FALSE   FALSE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
  FALSE   FALSE   FALSE   FALSE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
  FALSE   FALSE   FALSE   FALSE   FALSE    TRUE    TRUE    TRUE    TRUE    TRUE
  FALSE   FALSE   FALSE   FALSE   FALSE   FALSE   FALSE   FALSE    TRUE    TRUE
  FALSE   FALSE   FALSE   FALSE   FALSE   FALSE   FALSE   FALSE   FALSE    TRUE
'), "hist_nll_spabund_bt__model_n: Added value once per step, apart from 1993")
ok(ut_cmp_equal(
    r$hist_nll_spabund_bt__obs_mean,
    array(
        obs_linreg_df$mean,
        dim = c(vec = length(obs_linreg_df$mean), time = 10),
        dimnames = list(vec = NULL, time = paste0(rep(1990:1994, each=2), c("-01", "-02"))) ),
    end = NULL), "hist_nll_spabund_bt__obs_mean: Repeats the input observations")
nll_mean <- r$hist_nll_spabund_bt__model_sum[, '1994-02']/r$hist_nll_spabund_bt__model_n[, '1994-02']
ok(ut_cmp_equal(
    r$nll_spabund_bt__nll,
    gadget3:::regression_linear(nll_mean, obs_linreg_df$mean, NaN, 1) ), "r$nll_spabund_bt__nll: Matches derived version")

# cs_model
ok(ut_cmp_equal(colSums(r$hist_nll_spabund_cs_model__model_n) / colSums(r$hist_stst__num, dims = 2), c(
    "1990-01" = 1,
    "1990-02" = 2,
    "1991-01" = 2,
    "1991-02" = 2,
    "1992-01" = 3,
    "1992-02" = 4,
    "1993-01" = 5,
    "1993-02" = 6,
    "1994-01" = 6,
    "1994-02" = 6,
    NULL )), "hist_nll_spabund_cs_model__model_n: Final data point has 6 step's worth of individuals")
expected_m <- r$hist_stst__wgt * rep(3:5, each = 3)
for (i in seq_len(nrow(obs_ss_df))) {
    # Pick out all relevant measurements to this row
    m <- expected_m[,,time = paste0(obs_ss_df[i, "year"], c("-01", "-02"))]
    count <- r$hist_stst__num[,,time = paste0(obs_ss_df[i, "year"], c("-01", "-02"))]
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spabund_cs_model__model_sum[i, '1994-02']),
        sum(m * count) ), paste0("r$hist_nll_spabund_cs_model__model_sum[", i, ", '1994-02']"))
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spabund_cs_model__model_sqsum[i, '1994-02']),
        sum(m**2 * count) ), paste0("r$hist_nll_spabund_cs_model__model_sqsum[", i, ", '1994-02']"))
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spabund_cs_model__model_n[i, '1994-02']),
        sum(count) ), paste0("r$hist_nll_spabund_cs__model_n[", i, ", '1994-02']"))
}
nll_var <- r$hist_nll_spabund_cs_model__model_sqsum[, '1994-02']/r$hist_nll_spabund_cs_model__model_n[, '1994-02'] -
    (r$hist_nll_spabund_cs_model__model_sum[, '1994-02']/r$hist_nll_spabund_cs_model__model_n[, '1994-02'])**2
nll_mean <- r$hist_nll_spabund_cs_model__model_sum[, '1994-02']/r$hist_nll_spabund_cs_model__model_n[, '1994-02']
ok(ut_cmp_equal(
    (r$nll_spabund_cs_model__nll),
    array((nll_mean - obs_ss_df$mean)^2 * 1/nll_var, dim = c(row = 3L), dimnames = list(row = NULL)) ), "r$nll_spabund_cs_model__nll: Matches derived version")

# Overall nll
ok(ut_cmp_equal(nll,
    params$bt_weight * r$nll_spabund_bt__nll[["nll"]] +
    params$cs_model_weight * sum(r$nll_spabund_cs_model__nll) +
    params$flc_weight * sum(r$nll_spcatch_flc__nll) +
    0 ), "nll: Overall value matches")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

ok_group("flc") ###############################################################
attr(model_fn, 'parameter_template') |>
    g3_init_val("bt_weight", 0) |>
    g3_init_val("cs_model_weight", 0) |>
    g3_init_val("flc_weight", runif(1, 0.5, 5)) |>
    g3_init_val("predate_num", 100) |>
    g3_init_val("stst.fl_surv.alpha", 10) |>
    g3_init_val("stst.fl_surv.l50", 25) |>
    identity() -> params

nll <- model_fn(params) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(gadget3:::ut_cmp_df(as.data.frame((r$hist_stst_fl_surv__cons / r$hist_stst__wgt)[,,1]), '
           age3     age4     age5
10:20   0.00000  0.00000  0.00000
20:30   8.33333 10.93750 13.54167
30:Inf 17.18750 22.39583 27.60417
', tolerance = 1e-5), "hist_stst_fl_surv__cons: Caught 100 fish, with a selectivity pattern")

expected_m <- r$hist_stst__wgt + c(15, 25, 35)
length_grp <- as.character(cut(obs_flc_df$length, c(9, 20, 30, Inf), right = FALSE, labels = c("10:20", "20:30", "30:Inf")))
for (i in seq_len(nrow(obs_ss_df))) {
    m <- expected_m[
        length = length_grp[[i]],
        age = ,
        time = sprintf("%04d-%02d", obs_flc_df[i, "year"], 1:2) ]
    count <- (r$hist_stst_fl_surv__cons / r$hist_stst__wgt)[
        length = length_grp[[i]],
        age = ,
        time = sprintf("%04d-%02d", obs_flc_df[i, "year"], 1:2) ]
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spcatch_flc__model_n[i, '1994-02']),
        sum(count) ), paste0("r$hist_nll_spcatch_flc__model_n[", i, ", '1994-02']"))
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spcatch_flc__model_sum[i, '1994-02']),
        sum(m * count) ), paste0("r$hist_nll_spcatch_flc__model_sum[", i, ", '1994-02']"))
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spcatch_flc__model_sqsum[i, '1994-02']),
        sum(m**2 * count) ), paste0("r$hist_nll_spcatch_flc__model_sqsum[", i, ", '1994-02']"))
}

nll_var <- obs_flc_df$stddev^2  # Using weighting = 'obs_stddev'
nll_mean <- r$hist_nll_spcatch_flc__model_sum[, '1994-02']/r$hist_nll_spcatch_flc__model_n[, '1994-02']
ok(ut_cmp_equal(
    r$nll_spcatch_flc__nll,
    array((nll_mean - obs_flc_df$mean)^2 * (1/nll_var) * obs_flc_df$number, dim = c(row = length(r$nll_spcatch_flc__nll)), dimnames = list(row = NULL)) ), "r$nll_spcatch_flc__nll: Matches derived version")

# Overall nll
ok(ut_cmp_equal(nll,
    params$bt_weight * r$nll_spabund_bt__nll[["nll"]] +
    params$cs_model_weight * sum(r$nll_spabund_cs_model__nll) +
    params$flc_weight * sum(r$nll_spcatch_flc__nll) +
    0 ), "nll: Overall value matches")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
