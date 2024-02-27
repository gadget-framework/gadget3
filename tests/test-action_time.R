library(magrittr)
library(unittest)

library(gadget3)

# rep(), but allow (times) to extract fractional portions of (x)
frac_rep <- function (x, times = 1) {
    x <- c(
        # Repeat sequence whole-part times
        rep(x, times = floor(times)),
        # Fractional part is proportion of sequence
        head(x, round((times - floor(times)) * length(x))),
        NULL)
    return(x)
}

ok_group("MFDB compatibility", {
    # Fake mfdb default groupings
    timestep_quarterly <- structure(
        list("1" = 1:3, "2" = 4:6, "3" = 7:9, "4" = 10:12),
        table_name = "temp_rvtbi",
        class = c("mfdb_group", "mfdb_aggregate"))
    timestep_biannually <- structure(
        list("1" = 1:6, "2" = 7:12),
        table_name = "temp_wntki",
        class = c("mfdb_group", "mfdb_aggregate"))

    a <- g3a_time(1990, 1997, step_lengths = timestep_quarterly)
    ok(ut_cmp_identical(
        as.vector(environment(a[[1]])$step_lengths),
        c(3L,3L,3L,3L)), "mfdb::mfdb_timestep_quarterly converted")

    a <- g3a_time(1990, 1997, step_lengths = timestep_biannually)
    ok(ut_cmp_identical(
        as.vector(environment(a[[1]])$step_lengths),
        c(6L,6L)), "mfdb::mfdb_timestep_biannually converted")
})


all_time <- array(dim = c(1))
attr(all_time, 'dynamic_dim') <- list(quote(as_integer(total_steps + 1L)))
all_step <- array(dim = c(1))
attr(all_step, 'dynamic_dim') <- list(quote(as_integer(total_steps + 1L)))
all_step_size <- array(dim = c(1))
attr(all_step_size, 'dynamic_dim') <- list(quote(as_integer(total_steps + 1L)))
all_year <- array(dim = c(1))
attr(all_year, 'dynamic_dim') <- list(quote(as_integer(total_steps + 1L)))
all_step_final <- array(FALSE, dim = c(1))
attr(all_step_final, 'dynamic_dim') <- list(quote(as_integer(total_steps + 1L)))
all_cur_year_projection <- array(FALSE, dim = c(1))
attr(all_cur_year_projection, 'dynamic_dim') <- list(quote(as_integer(total_steps + 1L)))

actions <- list(
    g3a_time(
        start_year = ~as_integer(g3_param('p_start_year', value = 1990)),
        end_year = ~as_integer(g3_param('p_end_year', value = 1997)),
        step_lengths = ~g3_param_vector('step_lengths'),
        final_year_steps = ~as_integer(g3_param('final_year_steps', value = 3))),
    list(
        '999' = ~{
            all_time[[cur_time + 1]] <- cur_time
            all_step[[cur_time + 1]] <- cur_step
            all_step_size[[cur_time + 1]] <- cur_step_size
            all_year[[cur_time + 1]] <- cur_year
            all_step_final[[cur_time + 1]] <- cur_step_final
            all_cur_year_projection[[cur_time + 1]] <- cur_year_projection
            REPORT(all_time)
            REPORT(all_step)
            REPORT(all_step_size)
            REPORT(all_year)
            REPORT(all_step_final)
            REPORT(all_cur_year_projection)
            REPORT(total_years)

            nll <- nll + g3_param('x', value = 1.0)  # ...or TMB falls over
        }))

model_fn <- g3_to_r(actions)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    params <- attr(model_fn, 'parameter_template')
    params$step_lengths <- c(3,3,6)

    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
    model_cpp <- c()
}

params <- attr(model_fn, 'parameter_template')
params$step_lengths <- c(3,3,6)
result <- model_fn(params)

ok(ut_cmp_identical(
    as.vector(attr(result, 'all_time')),
    as.integer(1:((1997 - 1990 + 1) * 3) - 1)), "cur_time populated")
ok(ut_cmp_identical(
    as.vector(attr(result, 'all_step')),
    as.integer(rep(c(1,2,3), 8))), "cur_step populated")
ok(ut_cmp_identical(
    as.vector(attr(result, 'all_step_size')),
    as.numeric(rep(c(3/12,3/12,6/12), 8))), "cur_step_size populated")
ok(ut_cmp_identical(
    as.vector(attr(result, 'all_year')),
    as.integer(rep(1990:1997, each = 3))), "cur_year populated")
ok(ut_cmp_identical(
    as.vector(attr(result, 'all_step_final')),
    rep(c(FALSE, FALSE, TRUE), 8)), "cur_step_final populated")
ok(ut_cmp_identical(
    as.vector(attr(result, 'all_cur_year_projection')),
    rep(FALSE, (1997 - 1990 + 1) * 3)), "cur_year_projection populated")
ok(ut_cmp_identical(
    attr(result, 'total_years'),
    1997 - 1990 + 1), "total_years populated")

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    param_template <- attr(model_cpp, "parameter_template")
    param_template$value <- params[param_template$switch]
    gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
} else {
    writeLines("# skip: not running TMB tests")
}

ok_group('even steps', {
    params <- attr(model_fn, 'parameter_template')
    params$p_start_year <- 1992
    params$p_end_year <- 1999
    params$step_lengths <- c(4,4,4)
    params$final_year_steps <- 3
    params$project_years <- 0
    result <- model_fn(params)
    # str(r)

    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_time')),
        as.integer(1:((1999 - 1992 + 1) * 3) - 1)), "cur_time populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_step')),
        as.integer(rep(c(1,2,3), 8))), "cur_step populated")
    ok(ut_cmp_equal(
        as.vector(attr(result, 'all_step_size')),
        as.numeric(rep(c(4/12,4/12,4/12), 8))), "cur_step_size populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_year')),
        as.integer(rep(1992:1999, each = 3))), "cur_year populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_step_final')),
        rep(c(FALSE, FALSE, TRUE), 8)), "cur_step_final populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_cur_year_projection')),
        rep(FALSE, (1999 - 1992 + 1) * 3)), "cur_year_projection populated")
    ok(ut_cmp_identical(
        attr(result, 'total_years'),
        1999 - 1992 + 1), "total_years populated")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    } else {
        writeLines("# skip: not running TMB tests")
    }
})

ok_group("projection: Project_years = 4", {
    params <- attr(model_fn, 'parameter_template')
    params$p_start_year <- 1992
    params$p_end_year <- 1999
    params$step_lengths <- c(4,4,4)
    params$final_year_steps <- 3
    params$project_years <- 4
    result <- model_fn(params)

    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_time')),
        as.integer(1:(3 * (1999 - 1992 + 1 + 4)) - 1)), "cur_time populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_step')),
        as.integer(rep(c(1,2,3), 1999 - 1992 + 1 + 4))), "cur_step populated")
    ok(ut_cmp_equal(
        as.vector(attr(result, 'all_step_size')),
        as.numeric(rep(c(4/12,4/12,4/12), 1999 - 1992 + 1 + 4))), "cur_step_size populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_year')),
        as.integer(rep(1992:(1999 + 4), each = 3))), "cur_year populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_step_final')),
        rep(c(FALSE, FALSE, TRUE), 1999 - 1992 + 1 + 4)), "cur_step_final populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_cur_year_projection')),
        c(rep(FALSE, (1999 - 1992 + 1) * 3), rep(TRUE, 4 * 3))), "cur_year_projection populated")
    ok(ut_cmp_identical(
        attr(result, 'total_years'),
        1999 - 1992 + 1 + 4), "total_years populated")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    } else {
        writeLines("# skip: not running TMB tests")
    }
})

ok_group("projection: retro_years = 3", {
    params <- attr(model_fn, 'parameter_template')
    params$p_start_year <- 1992
    params$p_end_year <- 1999
    params$step_lengths <- c(4,4,4)
    params$final_year_steps <- 3
    params$retro_years <- 3
    result <- model_fn(params)

    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_time')),
        as.integer(1:(3 * (1999 - 1992 + 1 - 3)) - 1)), "cur_time populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_step')),
        as.integer(rep(c(1,2,3), 1999 - 1992 + 1 - 3))), "cur_step populated")
    ok(ut_cmp_equal(
        as.vector(attr(result, 'all_step_size')),
        as.numeric(rep(c(4/12,4/12,4/12), 1999 - 1992 + 1 - 3))), "cur_step_size populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_year')),
        as.integer(rep(1992:(1999 - 3), each = 3))), "cur_year populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_step_final')),
        rep(c(FALSE, FALSE, TRUE), 1999 - 1992 + 1 - 3)), "cur_step_final populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_cur_year_projection')),
        c(rep(FALSE, (1999 - 1992 + 1 - 3) * 3), rep(TRUE, 0))), "cur_year_projection populated (NB: No projection since we removed years)")
    ok(ut_cmp_identical(
        attr(result, 'total_years'),
        1999 - 1992 + 1 - 3), "total_years populated")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    } else {
        writeLines("# skip: not running TMB tests")
    }
})

ok_group("retro_years, project_years must have correct sense", {
    cmp_warn <- function(x, expected_regexp, ...) {
        last_warn <- NULL
        rv <- withCallingHandlers(x, warning = function (w) {
            last_warn <<- w
            invokeRestart("muffleWarning")
        })
        # NB: This would need moving in a generic version
        ok(ut_cmp_identical(rv, NaN), "Function returned NaN")

        if (is.null(last_warn)) return(c("No warning returned"))
        if (grepl(expected_regexp, last_warn$message, ...)) {
            return(TRUE)
        }
        return(c(last_warn$message, "Did not match:-", expected_regexp))
    }

    params <- attr(model_fn, 'parameter_template')
    params$p_start_year <- 1992
    params$p_end_year <- 1999
    params$step_lengths <- c(4,4,4)
    params$project_years <- -1
    ok(cmp_warn({
        model_fn(params)
    }, 'project_years'), "project_years: Can't be negative")
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        ok(cmp_warn({
            result <- model_fn(params)
        }, 'project_years'), "project_years: Can't be negative")
    } else {
        writeLines("# skip: not running TMB tests")
    }

    params <- attr(model_fn, 'parameter_template')
    params$p_start_year <- 1992
    params$p_end_year <- 1999
    params$step_lengths <- c(4,4,4)
    params$retro_years <- -1
    ok(cmp_warn({
        result <- model_fn(params)
    }, 'retro_years'), "retro_years: Can't be negative")
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        ok(cmp_warn({
            result <- model_fn(params)
        }, 'retro_years'), "retro_years: Can't be negative")
    } else {
        writeLines("# skip: not running TMB tests")
    }
})

ok_group("Short final year: final_year_steps = 1", {
    params <- attr(model_fn, 'parameter_template')
    params$p_start_year <- 1992
    params$p_end_year <- 1999
    params$step_lengths <- c(4,4,4)
    params$final_year_steps <- 1
    params$project_years <- 0
    result <- model_fn(params)

    # 1992..1998 whole years, and single step for 1999
    test_total_steps <- 3 * (1998 - 1992 + 1) + 1

    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_time')),
        as.integer(0:(test_total_steps - 1))), "cur_time populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_step')),
        as.integer(frac_rep(c(1,2,3), test_total_steps / 3 ))), "cur_step populated")
    ok(ut_cmp_equal(
        as.vector(attr(result, 'all_step_size')),
        as.numeric(frac_rep(c(4/12,4/12,4/12), test_total_steps / 3 ))), "cur_step_size populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_year')),
        as.integer(c(
            rep(1992:1998, each = 3),
            1999))), "cur_year populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_step_final')),
        frac_rep(c(FALSE, FALSE, TRUE), test_total_steps / 3)), "cur_step_final populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_cur_year_projection')),
        frac_rep(FALSE, test_total_steps )), "cur_year_projection populated (NB: No projection since we removed years)")
    ok(ut_cmp_identical(
        attr(result, 'total_years'),
        ceiling(test_total_steps / 3)), "total_years populated (NB: Ignores final_year_steps)")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    } else {
        writeLines("# skip: not running TMB tests")
    }
})

ok_group("1 year forecast from 2nd retro peel: retro_years = 2, project_years = 1", {
    params <- attr(model_fn, 'parameter_template')
    params$p_start_year <- 1992
    params$p_end_year <- 1999
    params$step_lengths <- c(4,4,4)
    params$retro_years <- 2
    params$project_years <- 1
    result <- model_fn(params)

    # 1992..1998 whole years (2 less for retro_years, +1 for project years)
    test_year_steps <- length(params$step_lengths)
    test_total_steps <- test_year_steps * (
        params$p_end_year - params$p_start_year + 1 +
        params$project_years - params$retro_years)

    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_time')),
        as.integer(0:(test_total_steps - 1))), "cur_time populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_step')),
        as.integer(frac_rep(c(1,2,3), test_total_steps / 3 ))), "cur_step populated")
    ok(ut_cmp_equal(
        as.vector(attr(result, 'all_step_size')),
        as.numeric(frac_rep(c(4/12,4/12,4/12), test_total_steps / 3 ))), "cur_step_size populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_year')),
        as.integer(rep(1992:1998, each = 3))), "cur_year populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_step_final')),
        frac_rep(c(FALSE, FALSE, TRUE), test_total_steps / 3)), "cur_step_final populated")
    ok(ut_cmp_identical(
        as.vector(attr(result, 'all_cur_year_projection')),
        c(
            frac_rep(FALSE, test_total_steps - params$project_years * 3 ),
            frac_rep(TRUE, params$project_years * 3 ),
            NULL)), "cur_year_projection populated")
    ok(ut_cmp_identical(
        attr(result, 'total_years'),
        ceiling(test_total_steps / 3)), "total_years populated")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    } else {
        writeLines("# skip: not running TMB tests")
    }
})
