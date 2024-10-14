if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

st_imm <- g3_stock(c(species = "st", maturity = "imm"), c(10, 20, 30)) |> g3s_age(1, 5)
st_mat <- g3_stock(c(species = "st", maturity = "mat"), c(10, 20, 30)) |> g3s_age(1, 5)
stocks_st <- list(st_imm, st_mat)
fl1 <- g3_fleet(c(type = "surv", 1))
fl2 <- g3_fleet(c(type = "surv", 2))
stocks_fl <- list(fl1, fl2)

# Sub-function, pulled into assess_fn's closure
tac_fff <- function (cn, smb, smh, sw, cw, immtotal, mattotal) {
    (sum(immtotal) + sum(mattotal)) / 1e10
}

# Assessment function, gets pulled into model by g3_formula
assess_fn <- function (
        # The start model year, as defined by g3a_time,
        start_year,
        # The current model year, as defined by g3a_time,
        cur_year,
        # Nested list of pred -> prey -> detail_prey_pred__cons
        cons,
        # List of prey -> detail_prey__num
        abund,
        # List of prey -> detail_prey__wgt
        meanwgt ) {
    years <- seq(start_year, cur_year - 2)  # We assess as if it was 2 years ago

    ## catch in numbers at age over all fleets
    cn <- NULL
    for (pred_n in names(cons)) for (prey_n in names(cons[[pred_n]])) {
        x <- g3_array_agg(cons[[pred_n]][[prey_n]] / meanwgt[[prey_n]], c("age", "year"), year = years)
        cn <- if (is.null(cn)) x else cn + x
    }

    ## Abundance by age at step 1
    smb <- NULL
    for (prey_n in names(abund)) {
        x <- g3_array_agg(abund[[prey_n]], c("age", "year"), year = years, step = 1)
        smb <- if (is.null(smb)) x else smb + x
    }

    ## Abundance by age at step 3
    smh <- NULL
    for (prey_n in names(abund)) {
        x <- g3_array_agg(abund[[prey_n]], c("age", "year"), year = years, step = 3)
        smh <- if (is.null(smh)) x else smh + x
    }

    ## Biomass at age divided by abundance at step 1
    sw <- NULL
    for (prey_n in names(abund)) {
        x <- g3_array_agg(abund[[prey_n]] * meanwgt[[prey_n]], c("age", "year"), year = years, step = 1)
        sw <- if (is.null(sw)) x else sw + x
    }
    sw <- sw / sum(sw)

    ## catch in biomass at age divided by cn -> meanweight of catch
    cw <- NULL
    for (pred_n in names(cons)) for (prey_n in names(cons[[pred_n]])) {
        # TODO: This has to be wrong
        x <- g3_array_agg(cons[[pred_n]][[prey_n]] / (cons[[pred_n]][[prey_n]] / meanwgt[[prey_n]]), c("age", "year"), year = years)
        cw <- if (is.null(cw)) x else cw + x
    }

    ## total abundance by maturity at step 1 by age
    immtotal <- g3_array_agg(abund$st_imm, c("age", "year"), year = years, step = 1)
    mattotal <- g3_array_agg(abund$st_mat, c("age", "year"), year = years, step = 1)

    # NB: tac_fff won't be part of the model, but in assess_fn's closure so will find it that way
    tac_fff(cn, smb, smh, sw, cw, immtotal, mattotal)
}

fl_quota <- gadget3:::g3_quota(
    gadget3:::g3_quota_assess(stocks_fl, stocks_st, g3_formula(
        assess_fn(start_year, cur_year, cons, abund, meanwgt),
        assess_fn = assess_fn )),
    # NB: No lag, we apply lag when rolling up time
    run_step = 1 )

actions <- list(
    g3a_time(1990, 1994, c(3,3,3,3)),
    g3a_otherfood_normalcv(
        st_imm,
        factor_f = g3_parameterized(
            'of_abund', by_year = TRUE, by_stock = TRUE,
            # TODO: We don't really want ifmissing, we want ifproj
            ifmissing = "of_abund.proj" )),
    g3a_otherfood_normalcv(
        st_mat,
        factor_f = g3_parameterized(
            'of_abund', by_year = TRUE, by_stock = TRUE,
            # TODO: We don't really want ifmissing, we want ifproj
            ifmissing = "of_abund.proj" )),
    g3a_predate(
        fl1,
        stocks_st,
        catchability_f = gadget3:::g3a_predate_catchability_project(
            fl_quota,
            g3_parameterized("landings", value = 0, by_year = TRUE, by_predator = TRUE),
            active_at = 1),
        g3_suitability_exponentiall50(by_stock = 'species') ),
    g3a_predate(
        fl2,
        stocks_st,
        catchability_f = gadget3:::g3a_predate_catchability_project(
            fl_quota,
            g3_parameterized("landings", value = 0, by_year = TRUE, by_predator = TRUE),
            active_at = c(2,4)),
        g3_suitability_exponentiall50(by_stock = 'species') ),
    # NB: Dummy parameter so model will compile in TMB
    quote( nll <- nll + g3_param("x", value = 0) ) )
full_actions <- c(actions, list(
    g3a_report_detail(actions),
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    g3a_report_history(actions, 'quota_.*', out_prefix = NULL),
    NULL))
model_fn <- g3_to_r(full_actions)
model_cpp <- g3_to_tmb(full_actions)

attr(model_cpp, 'parameter_template') |>
    g3_init_val("project_years", 10) |>
    g3_init_val("st_*.of_abund.#", runif(5 * 2, 1e5, 1e6)) |>
    g3_init_val("st_*.of_abund.proj", runif(2, 1e5, 1e6)) |>

    g3_init_val("*.K", 0.3, lower = 0.04, upper = 1.2) |>
    g3_init_val("*.Linf", max(g3_stock_def(st_imm, "midlen")), spread = 0.2) |>
    g3_init_val("*.t0", g3_stock_def(st_imm, "minage") - 0.8, spread = 2) |>
    g3_init_val("*.lencv", 0.1, optimise = FALSE) |>
    g3_init_val("*.walpha", 0.01, optimise = FALSE) |>
    g3_init_val("*.wbeta", 3, optimise = FALSE) |>
    g3_init_val("st_*.of_abund.proj", runif(2, 1e5, 1e6)) |>

    g3_init_val("*.*.l50", g3_stock_def(st_imm, "midlen")[[length(g3_stock_def(st_imm, "midlen")) / 2]], spread = 0.25) |>
    # surv_1 works in step_1, surv_2 works in step_2
    g3_init_val("surv_1.landings.#", runif(5, 1e5, 1e6)) |>
    g3_init_val("surv_2.landings.#", runif(5, 1e5, 1e6)) |>
    g3_init_val("surv.hf.harvest_rate", runif(1, 1e5, 1e6)) |>
    g3_init_val("surv.hf.btrigger", runif(1, 1e5, 1e6)) |>

    identity() -> params.in
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)




