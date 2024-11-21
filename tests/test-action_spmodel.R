if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

stock_a <- g3_stock(c("stock", "a"), seq(10, 50, by = 10))
stocks <- list(stock_a)
fleet_a <- g3_fleet(c('fleet', "a"))

actions <- list(
    g3a_time(2000, 2010, step_lengths = c(6,6), project_years = 0),

    g3a_spmodel(
        stock_a,
        # Only run on first step
        run_f = quote( cur_step == 1 )),
    g3a_predate(
        fleet_a,
        stocks,
        suitabilities = g3_suitability_exponentiall50(),
        catchability_f = g3a_predate_catchability_linearfleet(
            g3_parameterized("effort", value = 1e-2, by_predator = TRUE) )),

    # NB: Dummy parameter so model will compile in TMB
    ~{nll <- nll + g3_param("x", value = 0, optimise = TRUE)} )
actions <- c(actions, list(
    g3a_report_history(actions, "__num$|__wgt$", out_prefix="dend_"),  # NB: Late reporting
    g3a_report_detail(actions) ))
model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)

ok_group("Default params") ########
attr(model_cpp, 'parameter_template') |>
    # Surplus production model parameters
    g3_init_val("*.spm_n0", 1e6) |>
    g3_init_val("*.spm_r", 0.1) |>
    g3_init_val("*.spm_p", 0.01) |>
    g3_init_val("*.spm_K", 1e8, lower = 0, upper = 1e20) |>

    # Predation parameters
    g3_init_val("stock_a.fleet_a.alpha", 0.2) |>
    g3_init_val("stock_a.fleet_a.l50", 30) |>

    identity() -> params.in
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)
# barplot(r$suit_stock_a_fleet_a__report)
# barplot(r$dend_stock_a__num, las = 2)

ok(gadget3:::ut_cmp_df(r$dend_stock_a__num, '
         2000-01 2000-02 2001-01 2001-02 2002-01 2002-02 2003-01 2003-02 2004-01 2004-02 2005-01 2005-02 2006-01 2006-02 2007-01 2007-02 2008-01 2008-02 2009-01 2009-02 2010-01 2010-02
  10:20   999763  999526 1013983 1013742 1028282 1028038 1042660 1042413 1057118 1056867 1071655 1071401 1086272 1086015 1100969 1100708 1115745 1115480 1130601 1130333 1145536 1145265
  20:30   998655  997312 1010649 1009290 1022696 1021321 1034797 1033406 1046952 1045544 1059160 1057736 1071422 1069981 1083737 1082280 1096105 1094632 1108528 1107037 1121003 1119495
  30:40   996345  992703 1003718 1000049 1011123 1007427 1018559 1014836 1026028 1022278 1033528 1029750 1041060 1037254 1048622 1044789 1056216 1052356 1063841 1059953 1071497 1067580
  40:50   995237  990497 1000407  995642 1005613 1000823 1010855 1006040 1016133 1011294 1021448 1016583 1026797 1021907 1032182 1027266 1037603 1032661 1043057 1038090 1048547 1043553
  50:Inf  995033  990092  999799  994833 1004602  999613 1009444 1004430 1014323 1009286 1019241 1014178 1024195 1019108 1029187 1024075 1034215 1029078 1039280 1034118 1044381 1039194
', tolerance = 1e-6), "r$dend_stock_a__num: spm & fishing effort roughly balanced")

ok(gadget3:::ut_cmp_df(r$detail_stock_a__renewalnum, '
         2000-01 2000-02 2001-01 2001-02 2002-01 2002-02 2003-01 2003-02 2004-01 2004-02 2005-01 2005-02 2006-01 2006-02 2007-01 2007-02 2008-01 2008-02 2009-01 2009-02 2010-01 2010-02
  10:20        0       0   14697       0   14783       0   14870       0   14956       0   15042       0   15129       0   15215       0   15302       0   15389       0   15475       0
  20:30        0       0   14697       0   14783       0   14870       0   14956       0   15042       0   15129       0   15215       0   15302       0   15389       0   15475       0
  30:40        0       0   14697       0   14783       0   14870       0   14956       0   15042       0   15129       0   15215       0   15302       0   15389       0   15475       0
  40:50        0       0   14697       0   14783       0   14870       0   14956       0   15042       0   15129       0   15215       0   15302       0   15389       0   15475       0
  50:Inf       0       0   14697       0   14783       0   14870       0   14956       0   15042       0   15129       0   15215       0   15302       0   15389       0   15475       0
', tolerance = 5e-5), "r$detail_stock_a__renewalnum: Only happens every other year, even over all length groups")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)
