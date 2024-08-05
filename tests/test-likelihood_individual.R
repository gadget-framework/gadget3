if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st <- g3_stock("stst", c(10, 20, 30)) |> g3s_age(3,5)

obs_df <- data.frame(
    # NB: No 1993, just ignored
    year = rep(c(1990, 1991, 1992, 1994), each = 2),
    step = 1:2 )
obs_df$age = floor(runif(nrow(obs_df), min = 3, max = 5.1))
obs_df$length = floor(runif(nrow(obs_df), min = 10, max = 50))
obs_df$bt = runif(nrow(obs_df), min = 10, max = 1000)

actions <- list(
    g3a_time(1990, 1994, c(6,6)),
    g3a_otherfood(st,
        quote( age * 100 + stock__minlen ),
        quote( cur_year * 1e5 + cur_step * 1e4 + 0 * stock__minlen ) ),
    g3l_individual(
        "bt",
        obs_df,
        list(st),
        transform_f = g3_formula(
            stock_ss(stock__wgt, vec = single) + stock_ss(stock__num, vec = single),
            end = NULL ),
        function_f = g3l_individual_linreg(fit = "linear") ),
    # NB: Dummy parameter so model will compile in TMB
    ~{nll <- nll + g3_param("x", value = 0)} )
full_actions <- c(actions, list(
    g3a_report_history(actions, var_re = "__num$|__wgt$|^nll_ind_.+__(model|model_n|obs)$"),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_fn, 'parameter_template') |>
    identity() -> params

r <- attributes(model_fn(params))

# Final result can be derived by combining the otherfood & transform_f equations
length_grp <- as.numeric(as.character(cut(obs_df$length, c(9, 20, 30, Inf), labels = c(10, 20, 30))))
ok(ut_cmp_equal(
    r$hist_nll_ind_bt__model[,'1994-02'],
    obs_df$year * 1e5 + obs_df$step * 1e4 + obs_df$age * 1e2 + length_grp,
    tolerance = 1e-7), "hist_nll_ind_bt__model: Can derive final values")

ok(ut_cmp_equal(
    r$hist_nll_ind_bt__model,
    ifelse(r$hist_nll_ind_bt__model_n > 0, r$hist_nll_ind_bt__model[,"1994-02"], 0),
    end = NULL), "hist_nll_ind_bt__model: History is the same shape as __model_n")

ok(gadget3:::ut_cmp_df(as.data.frame(r$hist_nll_ind_bt__model_n), '
1990-01 1990-02 1991-01 1991-02 1992-01 1992-02 1993-01 1993-02 1994-01 1994-02
      1       1       1       1       1       1       1       1       1       1
      0       1       1       1       1       1       1       1       1       1
      0       0       1       1       1       1       1       1       1       1
      0       0       0       1       1       1       1       1       1       1
      0       0       0       0       1       1       1       1       1       1
      0       0       0       0       0       1       1       1       1       1
      0       0       0       0       0       0       0       0       1       1
      0       0       0       0       0       0       0       0       0       1
'), "hist_nll_ind_bt__model_n: Added value once per step, apart from 1993")

ok(ut_cmp_equal(
    r$hist_nll_ind_bt__obs,
    array(
        obs_df$bt,
        dim = c(vec = length(obs_df$bt), time = 10),
        dimnames = list(vec = NULL, time = paste0(rep(1990:1994, each=2), c("-01", "-02"))) ),
    end = NULL), "hist_nll_ind_bt__obs: Repeats the input observations")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
