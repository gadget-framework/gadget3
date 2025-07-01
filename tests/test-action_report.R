library(magrittr)
library(unittest)

library(gadget3)

# Compare array by turning it back into a table first
cmp_array <- function (ar, table_text) {
    tbl <- read.table(
        header = TRUE,
        stringsAsFactors = FALSE,
        colClasses = c(rep("character", length(dim(ar))), "numeric"),
        text = table_text)
    ut_cmp_identical(as.data.frame.table(ar, stringsAsFactors = FALSE), tbl)
}

capture_warnings <- function(x, full_object = FALSE) {
    all_warnings <- list()
    rv <- withCallingHandlers(x, warning = function (w) {
        all_warnings <<- c(all_warnings, list(w))
        invokeRestart("muffleWarning")
    })
    if (!full_object) all_warnings <- vapply(all_warnings, function (w) w$message, character(1))
    return(list(rv = rv, warnings = all_warnings))
}

ok_group("action_reports")
ok(ut_cmp_equal(
    gadget3:::action_reports(list(g3_formula(quote({
        const_var <- 4
        arr_var[2] <- 4
        doublearr_var[2][[1]] <- 4
    }))), REPORT = '.'), quote({
        REPORT(arr_var)
        REPORT(const_var)
        REPORT(doublearr_var)
    })), "action_reports: Removed subsets")

ok(ut_cmp_identical(
    g3a_report_history(g3_formula(comment(testreport_missing)), '^testreport_'),
    list() ), "Don't try and report undefined variables (let compilation finish)")

#######################################

prey_a <- g3_stock('prey_a', c(1)) %>% g3s_age(1, 5)

# Report that aggregates ages together
agg_report <- g3_stock('agg_report', c(1)) %>% 
    g3s_agegroup(list(young = 1:3, old = 4:5)) %>%
    g3s_time(year = 2000:2002)
# Generate dissaggregated report by cloning the source stock, adding time
raw_report <- g3s_clone(prey_a, 'raw_report') %>%
    g3s_time(year = 2000:2002)

actions <- list(
    g3a_time(2000, 2002, step_lengths = c(6, 6), project_years = 0),
    g3a_initialconditions(prey_a, ~10 * age + prey_a__midlen * 0, ~100 * age + prey_a__midlen * 0),
    g3a_age(prey_a),
    "5:testreport_vec" = g3_formula({
        testreport_vec <- testreport_vec + cur_time
    }, testreport_vec = 1:4),
    g3a_report_stock(agg_report, prey_a, ~stock_ss(prey_a__num), include_adreport = TRUE),
    g3a_report_stock(raw_report, prey_a, ~stock_ss(input_stock__num)),  # NB: We can let g3_step rename it for us
    # NB: Only required for testing
    gadget3:::g3l_test_dummy_likelihood() )
actions <- c(actions, list(
    g3a_report_history(actions, '^prey_a__(num|wgt|midlen)|^testreport_') ))
            
# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
model_cpp <- g3_to_tmb(actions, trace = FALSE)

ok_group("report", {
    params <- attr(model_fn, 'parameter_template')
    result <- capture_warnings(model_fn(params))
    ok(ut_cmp_identical(result$warnings, "No ADREPORT functionality available in R"), "Tried to ADREPORT, moved on")
    result <- result$rv
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)

    ok(cmp_array(r$raw_report__num, "
        length  age time Freq
         1:Inf age1 2000   20
         1:Inf age2 2000   40
         1:Inf age3 2000   60
         1:Inf age4 2000   80
         1:Inf age5 2000  100
         1:Inf age1 2001    0
         1:Inf age2 2001   20
         1:Inf age3 2001   40
         1:Inf age4 2001   60
         1:Inf age5 2001  180
         1:Inf age1 2002    0
         1:Inf age2 2002    0
         1:Inf age3 2002   20
         1:Inf age4 2002   40
         1:Inf age5 2002  240
    "), "raw_report__num: Can see growth happening")

    ok(cmp_array(r$agg_report__num, "
        length   age time Freq
         1:Inf young 2000  120
         1:Inf   old 2000  180
         1:Inf young 2001   60
         1:Inf   old 2001  240
         1:Inf young 2002   20
         1:Inf   old 2002  280
    "), "agg_report__num: Aggregated report only has young/old")

    ok(ut_cmp_identical(grep('^hist_', names(r), value = TRUE), c(
        "hist_prey_a__midlen",
        "hist_prey_a__num",
        "hist_prey_a__wgt",
        "hist_testreport_vec",
        NULL )), "Logged history of numbers / weight (ignored reports though)")
    ok(cmp_array(r$hist_prey_a__num, "
        length   age    time Freq
         1:Inf  age1 2000-01   10
         1:Inf  age2 2000-01   20
         1:Inf  age3 2000-01   30
         1:Inf  age4 2000-01   40
         1:Inf  age5 2000-01   50
         1:Inf  age1 2000-02   10
         1:Inf  age2 2000-02   20
         1:Inf  age3 2000-02   30
         1:Inf  age4 2000-02   40
         1:Inf  age5 2000-02   50
         1:Inf  age1 2001-01    0
         1:Inf  age2 2001-01   10
         1:Inf  age3 2001-01   20
         1:Inf  age4 2001-01   30
         1:Inf  age5 2001-01   90
         1:Inf  age1 2001-02    0
         1:Inf  age2 2001-02   10
         1:Inf  age3 2001-02   20
         1:Inf  age4 2001-02   30
         1:Inf  age5 2001-02   90
         1:Inf  age1 2002-01    0
         1:Inf  age2 2002-01    0
         1:Inf  age3 2002-01   10
         1:Inf  age4 2002-01   20
         1:Inf  age5 2002-01  120
         1:Inf  age1 2002-02    0
         1:Inf  age2 2002-02    0
         1:Inf  age3 2002-02   10
         1:Inf  age4 2002-02   20
         1:Inf  age5 2002-02  120
  "), "hist_prey_a__num: Full history")
  ok(gadget3:::ut_cmp_df(as.data.frame(r$hist_prey_a__midlen), '
         2000-01 2000-02 2001-01 2001-02 2002-01 2002-02
             1.5     1.5     1.5     1.5     1.5     1.5
  '), "hist_prey_a__midlen: History of single value")
  ok(gadget3:::ut_cmp_df(as.data.frame(r$hist_testreport_vec), '
         2000-01 2000-02 2001-01 2001-02 2002-01 2002-02
               1       1       2       4       7      11
               2       2       3       5       8      12
               3       3       4       6       9      13
               4       4       5       7      10      14
  '), "hist_testreport_vec: History of vector")

    capture_warnings(gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params))
})

ok_group("adreport", {
    params <- attr(model_fn, 'parameter_template')
    result <- capture_warnings(model_fn(params))
    ok(ut_cmp_identical(result$warnings, "No ADREPORT functionality available in R"), "Tried to ADREPORT, moved on")
    result <- result$rv
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
        sdrep <- TMB::sdreport(model_tmb)
        ok(ut_cmp_equal(
            summary(sdrep, 'report'),
            array(
              c(as.vector(r$agg_report__num), rep(NaN, length(r$agg_report__num))),
              dim = c(length(r$agg_report__num), 2),
              dimnames = list(
                  rep("agg_report__num", length(r$agg_report__num)),
                  c("Estimate", "Std. Error")))), "TMB included report_stock__num in adreport")
    }
})
