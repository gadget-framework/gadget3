if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

do_fishing_calendar <- function(wall_calendar, fishing_calendar) {
    fl_quota <- do.call(g3_quota, c(
        # Our quota values are year/step at the assessment time step
        list( quote( cur_year * 10 + cur_step ) ),
        fishing_calendar), quote = TRUE)
    actions <- list(
        do.call(g3a_time, wall_calendar, quote = TRUE),
        fl_quota,
        # NB: before projection, g3a_predate_catchability_project() will use landings data not the quota
        g3_step(g3_formula(
            writeLines(paste(cur_year, cur_step, if (cur_year_projection) q else "landings")),
            q = fl_quota )),
        NULL)
    model_fn <- g3_to_r(c(actions,
        g3a_report_history(actions, "quota_", out_prefix = NULL) ))
    capture.output({ r <- model_fn() })
}

ok_group("init_val", {
    out <- do_fishing_calendar(wall_calendar = list(
        start_year = 2019,
        end_year = 2024,
        project_years = 10,
        step_lengths = rep(3L, 4)
    ), fishing_calendar = list(
        year_length = 1L,
        start_step = 4L,
        run_revstep = -3L ))
    ok(ut_cmp_identical(out, c(
        "2019 1 landings", "2019 2 landings", "2019 3 landings", "2019 4 landings",
        "2020 1 landings", "2020 2 landings", "2020 3 landings", "2020 4 landings",
        "2021 1 landings", "2021 2 landings", "2021 3 landings", "2021 4 landings",
        "2022 1 landings", "2022 2 landings", "2022 3 landings", "2022 4 landings",
        "2023 1 landings", "2023 2 landings", "2023 3 landings", "2023 4 landings",
        "2024 1 landings", "2024 2 landings", "2024 3 landings", "2024 4 landings",
        "2025 1 NaN", "2025 2 NaN", "2025 3 NaN",
        "2025 4 20251", "2026 1 20251", "2026 2 20251", "2026 3 20251",
        "2026 4 20261", "2027 1 20261", "2027 2 20261", "2027 3 20261",
        "2027 4 20271", "2028 1 20271", "2028 2 20271", "2028 3 20271",
        "2028 4 20281", "2029 1 20281", "2029 2 20281", "2029 3 20281",
        "2029 4 20291", "2030 1 20291", "2030 2 20291", "2030 3 20291",
        "2030 4 20301", "2031 1 20301", "2031 2 20301", "2031 3 20301",
        "2031 4 20311", "2032 1 20311", "2032 2 20311", "2032 3 20311",
        "2032 4 20321", "2033 1 20321", "2033 2 20321", "2033 3 20321",
        "2033 4 20331", "2034 1 20331", "2034 2 20331", "2034 3 20331",
        "2034 4 20341")), "Fishing calendar with init_val showing in the interim")

    out <- do_fishing_calendar(wall_calendar = list(
        start_year = 2019,
        end_year = 2024,
        retro_years = 6L,
        project_years = 10,
        step_lengths = rep(3L, 4)
    ), fishing_calendar = list(
        init_val = 99,
        year_length = 1L,
        start_step = 4L,
        run_revstep = -3L ))
    ok(ut_cmp_identical(out, c(
        "2019 1 99", "2019 2 99", "2019 3 99",
        "2019 4 20191", "2020 1 20191", "2020 2 20191", "2020 3 20191",
        "2020 4 20201", "2021 1 20201", "2021 2 20201", "2021 3 20201",
        "2021 4 20211", "2022 1 20211", "2022 2 20211", "2022 3 20211",
        "2022 4 20221", "2023 1 20221", "2023 2 20221", "2023 3 20221",
        "2023 4 20231", "2024 1 20231", "2024 2 20231", "2024 3 20231",
        "2024 4 20241", "2025 1 20241", "2025 2 20241", "2025 3 20241",
        "2025 4 20251", "2026 1 20251", "2026 2 20251", "2026 3 20251",
        "2026 4 20261", "2027 1 20261", "2027 2 20261", "2027 3 20261",
        "2027 4 20271", "2028 1 20271", "2028 2 20271", "2028 3 20271",
        "2028 4 20281")), "init_val also applied before any assessment was run in all-projection model")
})
