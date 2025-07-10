if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

stocks_st <- list(
    imm = g3_stock(c(species = "st", maturity = "imm"), c(10, 20, 30)) |> g3s_age(1, 5),
    mat = g3_stock(c(species = "st", maturity = "mat"), c(10, 20, 30)) |> g3s_age(3, 15) )

stocks_fl <- list(
    biomass_st = g3_fleet(c("fl", unit = "biomass", when = "step")),
    biomass_yr = g3_fleet(c("fl", unit = "biomass", when = "year")),
    individuals_st = g3_fleet(c("fl", unit = "individuals", when = "step")),
    individuals_yr = g3_fleet(c("fl", unit = "individuals", when = "year")) )
    # TODO: harvest-rate / harvest-rate-year

set_unit <- function (f, unit_string) {
    attr(f, "catchability_unit") <- unit_string
    return(f)
}

actions <- list(
    g3a_time(1990, 1991, c(3,3,3,3)),
    g3a_otherfood_normalcv(stocks_st$imm),
    g3a_otherfood_normalcv(stocks_st$mat),
    g3a_predate(
        stocks_fl$biomass_st,
        stocks_st,
        catchability_f = g3a_predate_catchability_project(
            set_unit(g3_parameterized("quota", value = 0, by_predator = TRUE, by_species = TRUE), "biomass"),
            g3_parameterized("landings", value = 0, by_predator = TRUE, by_species = TRUE),
            unit = "biomass" ),
        g3_suitability_exponentiall50(by_stock = 'species') ),
    g3a_predate(
        stocks_fl$biomass_yr,
        stocks_st,
        catchability_f = g3a_predate_catchability_project(
            set_unit(g3_parameterized("quota", value = 0, by_predator = TRUE, by_species = TRUE), "biomass-year"),
            g3_parameterized("landings", value = 0, by_predator = TRUE, by_species = TRUE),
            unit = "biomass-year" ),
        g3_suitability_exponentiall50(by_stock = 'species') ),
    g3a_predate(
        stocks_fl$individuals_st,
        stocks_st,
        catchability_f = g3a_predate_catchability_project(
            set_unit(g3_parameterized("quota", value = 0, by_predator = TRUE, by_species = TRUE), "individuals"),
            g3_parameterized("landings", value = 0, by_predator = TRUE, by_species = TRUE),
            unit = "individuals" ),
        g3_suitability_exponentiall50(by_stock = 'species') ),
    g3a_predate(
        stocks_fl$individuals_yr,
        stocks_st,
        catchability_f = g3a_predate_catchability_project(
            set_unit(g3_parameterized("quota", value = 0, by_predator = TRUE, by_species = TRUE), "individuals-year"),
            g3_parameterized("landings", value = 0, by_predator = TRUE, by_species = TRUE),
            unit = "individuals-year" ),
        g3_suitability_exponentiall50(by_stock = 'species') ),
    NULL )
full_actions <- c(actions, list(
    g3a_report_detail(actions),
    g3a_report_history(actions, "__num$|__wgt$", out_prefix="dend_"),  # NB: Late reporting
    g3a_report_history(actions, "quota_", out_prefix = NULL),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_cpp, "parameter_template") |>
    g3_init_val("project_years", 2) |>

    g3_init_val("*.of_abund.#|proj", 1e10) |>
    g3_init_val("*.Linf", max(g3_stock_def(stocks_st$mat, "midlen"))) |>
    g3_init_val("*.walpha", 0.01) |>
    g3_init_val("*.wbeta", 3) |>

    g3_init_val("*.*.alpha", 0.07, lower = 0.01, upper = 0.2) |>
    g3_init_val("*.*.l50", 0.07, mean(g3_stock_def(stocks_st$mat, "midlen"))) |>

    g3_init_val("fl_biomass_step.quota", runif(1, 100, 199)) |>
    g3_init_val("fl_biomass_step.landings", runif(1, 100, 199)) |>
    g3_init_val("fl_biomass_year.quota", runif(1, 100, 199)) |>
    g3_init_val("fl_biomass_year.cons.step.#", c(0, 0.25, 0.65, 0.1)) |>
    g3_init_val("fl_biomass_year.landings", runif(1, 100, 199)) |>
    g3_init_val("fl_individuals_step.quota", runif(1, 100, 199)) |>
    g3_init_val("fl_individuals_step.landings", runif(1, 100, 199)) |>
    g3_init_val("fl_individuals_year.quota", runif(1, 100, 199)) |>
    g3_init_val("fl_individuals_year.cons.step.#", c(0.1, 0.65, 0.25, 0)) |>
    g3_init_val("fl_individuals_year.landings", runif(1, 100, 199)) |>
    
    identity() -> params.in

nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

ok(ut_cmp_equal(
    as.vector(g3_array_agg(g3_array_combine(list(r$detail_st_imm_fl_biomass_step__cons, r$detail_st_mat_fl_biomass_step__cons)), "time")), as.vector(c(
        "1990-01" = params.in$value$fl_biomass_step.landings,
        "1990-02" = params.in$value$fl_biomass_step.landings,
        "1990-03" = params.in$value$fl_biomass_step.landings,
        "1990-04" = params.in$value$fl_biomass_step.landings,
        "1991-01" = params.in$value$fl_biomass_step.landings,
        "1991-02" = params.in$value$fl_biomass_step.landings,
        "1991-03" = params.in$value$fl_biomass_step.landings,
        "1991-04" = params.in$value$fl_biomass_step.landings,
        "1992-01" = params.in$value$fl_biomass_step.quota,
        "1992-02" = params.in$value$fl_biomass_step.quota,
        "1992-03" = params.in$value$fl_biomass_step.quota,
        "1992-04" = params.in$value$fl_biomass_step.quota,
        "1993-01" = params.in$value$fl_biomass_step.quota,
        "1993-02" = params.in$value$fl_biomass_step.quota,
        "1993-03" = params.in$value$fl_biomass_step.quota,
        "1993-04" = params.in$value$fl_biomass_step.quota,
        NULL))), "fl_biomass_step__cons: Used quota values evenly")

ok(ut_cmp_equal(
    as.vector(g3_array_agg(g3_array_combine(list(r$detail_st_imm_fl_biomass_year__cons, r$detail_st_mat_fl_biomass_year__cons)), "year")), as.vector(c(
        "1990" = params.in$value$fl_biomass_year.landings,
        "1991" = params.in$value$fl_biomass_year.landings,
        "1992" = params.in$value$fl_biomass_year.quota,
        "1993" = params.in$value$fl_biomass_year.quota,
        NULL))), "fl_biomass_year__cons: Spread quota over entire year")
ok(ut_cmp_equal(
    as.vector(
        g3_array_agg(r$detail_st_imm_fl_biomass_year__cons, c("step")) /
        sum(g3_array_agg(r$detail_st_imm_fl_biomass_year__cons, c("step"))) ),
    c(0, 0.25, 0.65, 0.10) ), "fl_biomass_year__cons: Used cons.step proportions")

ok(ut_cmp_equal(
    as.vector(g3_array_agg(g3_array_combine(list(
        r$detail_st_imm_fl_individuals_step__cons / r$dstart_st_imm__wgt,
        r$detail_st_mat_fl_individuals_step__cons / r$dstart_st_mat__wgt)), "time")), as.vector(c(
        "1990-01" = params.in$value$fl_individuals_step.landings,
        "1990-02" = params.in$value$fl_individuals_step.landings,
        "1990-03" = params.in$value$fl_individuals_step.landings,
        "1990-04" = params.in$value$fl_individuals_step.landings,
        "1991-01" = params.in$value$fl_individuals_step.landings,
        "1991-02" = params.in$value$fl_individuals_step.landings,
        "1991-03" = params.in$value$fl_individuals_step.landings,
        "1991-04" = params.in$value$fl_individuals_step.landings,
        "1992-01" = params.in$value$fl_individuals_step.quota,
        "1992-02" = params.in$value$fl_individuals_step.quota,
        "1992-03" = params.in$value$fl_individuals_step.quota,
        "1992-04" = params.in$value$fl_individuals_step.quota,
        "1993-01" = params.in$value$fl_individuals_step.quota,
        "1993-02" = params.in$value$fl_individuals_step.quota,
        "1993-03" = params.in$value$fl_individuals_step.quota,
        "1993-04" = params.in$value$fl_individuals_step.quota,
        NULL)), tolerance = 1e-6), "fl_individuals_step__cons: Used quota values evenly")

ok(ut_cmp_equal(
    as.vector(g3_array_agg(g3_array_combine(list(
        r$detail_st_imm_fl_individuals_year__cons / r$dstart_st_imm__wgt,
        r$detail_st_mat_fl_individuals_year__cons / r$dstart_st_mat__wgt)), "year")), as.vector(c(
        "1990" = params.in$value$fl_individuals_year.landings,
        "1991" = params.in$value$fl_individuals_year.landings,
        "1992" = params.in$value$fl_individuals_year.quota,
        "1993" = params.in$value$fl_individuals_year.quota,
        NULL)), tolerance = 1e-6), "fl_individuals_year__cons: Spread quota over entire year")
ok(ut_cmp_equal(
    as.vector(
        g3_array_agg(r$detail_st_imm_fl_individuals_year__cons, c("step")) /
        sum(g3_array_agg(r$detail_st_imm_fl_individuals_year__cons, c("step"))) ),
    c(0.1, 0.65, 0.25, 0) ), "fl_individuals_year__cons: Used cons.step proportions")

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params.in)
