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

prey_a <- g3_stock('prey_a', c(1)) %>% g3s_age(1, 5)

# Report that aggregates ages together
agg_report <- g3_stock('agg_report', c(1)) %>% 
    g3s_agegroup(list(young = 1:3, old = 4:5)) %>%
    g3s_time(year = 2000:2002)
# Generate dissaggregated report by cloning the source stock, adding time
raw_report <- g3s_clone(prey_a, 'raw_report') %>%
    g3s_time(year = 2000:2002)

actions <- list(
    g3a_time(2000, 2002, steps = c(6, 6)),
    g3a_initialconditions(prey_a, ~10 * age + prey_a__midlen * 0, ~100 * age + prey_a__midlen * 0),
    g3a_age(prey_a),
    g3a_report_stock(agg_report, prey_a, ~stock_ss(prey_a__num)),
    g3a_report_stock(raw_report, prey_a, ~stock_ss(input_stock__num)),  # NB: We can let g3_step rename it for us
    list('999' = ~{ nll <- nll + g3_param('x') }))
params <- list(
    x=1.0)
            
# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
}

ok_group("report", {
    params <- list(
        x=1.0)
    result <- model_fn(params)
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)

    ok(cmp_array(r$raw_report__num, "
        length  age time Freq
          len1 age1 2000   20
          len1 age2 2000   40
          len1 age3 2000   60
          len1 age4 2000   80
          len1 age5 2000  100
          len1 age1 2001    0
          len1 age2 2001   20
          len1 age3 2001   40
          len1 age4 2001   60
          len1 age5 2001  180
          len1 age1 2002    0
          len1 age2 2002    0
          len1 age3 2002   20
          len1 age4 2002   40
          len1 age5 2002  240
    "), "raw_report__num: Can see growth happening")

    ok(cmp_array(r$agg_report__num, "
        length   age time Freq
          len1 young 2000  120
          len1   old 2000  180
          len1 young 2001   60
          len1   old 2001  240
          len1 young 2002   20
          len1   old 2002  280
    "), "agg_report__num: Aggregated report only has young/old")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})
