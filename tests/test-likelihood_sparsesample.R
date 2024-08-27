if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st <- g3_stock("stst", c(10, 20, 30)) |> g3s_age(3,5)

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

actions <- list(
    g3a_time(1990, 1994, c(6,6)),
    g3a_otherfood(st,
        quote( age * 100 + stock__minlen ),
        quote( cur_year * 1e5 + cur_step * 1e4 + 0 * stock__minlen ) ),
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
    # NB: Dummy parameter so model will compile in TMB
    ~{nll <- nll + g3_param("x", value = 0)} )
full_actions <- c(actions, list(
    g3a_report_history(actions, var_re = "__num$|__wgt$|^nll_spabund_.+__(model|obs)_.+$"),
    g3a_report_history(actions, var_re = "^nll_spabund_.+__nll$", out_prefix = NULL),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_fn, 'parameter_template') |>
    identity() -> params

nll <- model_fn(params) ; r <- attributes(nll) ; nll <- as.vector(nll)

# Final result can be derived from stock abundance
expected_m <- r$hist_stst__num * c(r$hist_stst__wgt + c(15, 25, 35))
length_grp <- as.character(cut(obs_linreg_df$length, c(9, 20, 30, Inf), right = FALSE, labels = c("10:20", "20:30", "30:Inf")))
for (i in seq_len(nrow(obs_linreg_df))) {
    x <- expected_m[
        length = length_grp[[i]],
        age = sprintf("age%d", obs_linreg_df[i, "age"]),
        time = sprintf("%04d-%02d", obs_linreg_df[i, "year"], obs_linreg_df[i, "step"]) ]
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spabund_bt__model_sum[i, '1994-02']),
        sum(x) ), paste0("r$hist_nll_spabund_bt__model_sum[", i, ", '1994-02']"))
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spabund_bt__model_sqsum[i, '1994-02']),
        sum(x**2) ), paste0("r$hist_nll_spabund_bt__model_sqsum[", i, ", '1994-02']"))
}
ok(ut_cmp_equal(
    r$hist_nll_spabund_bt__model_sum,
    ifelse(r$hist_nll_spabund_bt__model_n > 0, r$hist_nll_spabund_bt__model_sum[,"1994-02"], 0),
    end = NULL), "hist_nll_spabund_bt__model_sum: History is the same shape as __model_n")
ok(gadget3:::ut_cmp_df(as.data.frame(r$hist_nll_spabund_bt__model_n), '
1990-01 1990-02 1991-01 1991-02 1992-01 1992-02 1993-01 1993-02 1994-01 1994-02
      1       1       1       1       1       1       1       1       1       1
      0       1       1       1       1       1       1       1       1       1
      0       0       1       1       1       1       1       1       1       1
      0       0       0       1       1       1       1       1       1       1
      0       0       0       0       1       1       1       1       1       1
      0       0       0       0       0       1       1       1       1       1
      0       0       0       0       0       0       0       0       1       1
      0       0       0       0       0       0       0       0       0       1
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
ok(gadget3:::ut_cmp_df(as.data.frame(r$hist_nll_spabund_cs_model__model_n), '
1990-01 1990-02 1991-01 1991-02 1992-01 1992-02 1993-01 1993-02 1994-01 1994-02
      9      18      18      18      18      18      18      18      18      18
      0       0       0       0       9      18      18      18      18      18
      0       0       0       0       0       0       9      18      18      18
'), "hist_nll_spabund_cs_model__model_n: 18 data points (step,age,length) in each cell, gathered per year")
expected_m <- r$hist_stst__num * c(r$hist_stst__wgt * rep(3:5, each = 3))
for (i in seq_len(nrow(obs_ss_df))) {
    # Pick out all relevant measurements to this row
    x <- expected_m[,,time = paste0(obs_ss_df[i, "year"], c("-01", "-02"))]
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spabund_cs_model__model_sum[i, '1994-02']),
        sum(x) ), paste0("r$hist_nll_spabund_cs_model__model_sum[", i, ", '1994-02']"))
    ok(ut_cmp_equal(
        as.vector(r$hist_nll_spabund_cs_model__model_sqsum[i, '1994-02']),
        sum(x**2) ), paste0("r$hist_nll_spabund_cs_model__model_sqsum[", i, ", '1994-02']"))
}
nll_var <- r$hist_nll_spabund_cs_model__model_sqsum[, '1994-02']/r$hist_nll_spabund_cs_model__model_n[, '1994-02'] -
    (r$hist_nll_spabund_cs_model__model_sum[, '1994-02']/r$hist_nll_spabund_cs_model__model_n[, '1994-02'])**2
nll_mean <- r$hist_nll_spabund_cs_model__model_sum[, '1994-02']/r$hist_nll_spabund_cs_model__model_n[, '1994-02']
ok(ut_cmp_equal(
    r$nll_spabund_cs_model__nll,
    (nll_mean - obs_ss_df$mean)^2 * 1/nll_var), "r$nll_spabund_cs_model__nll: Matches derived version")

# Overall nll
ok(ut_cmp_equal(nll,
    r$nll_spabund_bt__nll[[1]] +
    sum(r$nll_spabund_cs_model__nll) +
    0 ), "nll: Overall value matches")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
