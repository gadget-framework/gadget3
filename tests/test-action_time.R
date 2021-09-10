library(magrittr)
library(unittest)

library(gadget3)

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

    a <- g3a_time(1990, 1997, steps = timestep_quarterly)
    ok(ut_cmp_identical(
        as.vector(environment(a[[1]])$step_lengths),
        c(3L,3L,3L,3L)), "mfdb::mfdb_timestep_quarterly converted")

    a <- g3a_time(1990, 1997, steps = timestep_biannually)
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

actions <- list(
    g3a_time(1990, 1997, steps = c(3,3,6)),
    list(
        '999' = ~{
            all_time[g3_idx(cur_time + 1)] <- cur_time
            g3_report(all_time)

            all_step[g3_idx(cur_time + 1)] <- cur_step
            g3_report(all_step)

            all_step_size[g3_idx(cur_time + 1)] <- cur_step_size
            g3_report(all_step_size)

            all_year[g3_idx(cur_time + 1)] <- cur_year
            g3_report(all_year)

            all_step_final[g3_idx(cur_time + 1)] <- cur_step_final
            g3_report(all_step_final)

            nll <- nll + g3_param('x')  # ...or TMB falls over
        }))
params <- list(x=1.0)
model_fn <- g3_to_r(actions)
# model_fn <- edit(model_fn)
result <- model_fn(params)
# str(r)

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

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
    param_template <- attr(model_cpp, "parameter_template")
    param_template$value <- params[param_template$switch]
    gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
} else {
    writeLines("# skip: not running TMB tests")
}

ok_group('even steps', {
    actions <- list(
        g3a_time(1992, 1999, steps = c(4,4,4)),
        list(
            '999' = ~{
                all_time[g3_idx(cur_time + 1)] <- cur_time
                g3_report(all_time)

                all_step[g3_idx(cur_time + 1)] <- cur_step
                g3_report(all_step)

                all_step_size[g3_idx(cur_time + 1)] <- cur_step_size
                g3_report(all_step_size)

                all_year[g3_idx(cur_time + 1)] <- cur_year
                g3_report(all_year)

                all_step_final[g3_idx(cur_time + 1)] <- cur_step_final
                g3_report(all_step_final)

                nll <- nll + g3_param('x')  # ...or TMB falls over
            }))
    params <- list(x=1.0)
    model_fn <- g3_to_r(actions)
    # model_fn <- edit(model_fn)
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

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_cpp <- g3_to_tmb(actions)
        # model_cpp <- edit(model_cpp)
        model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    } else {
        writeLines("# skip: not running TMB tests")
    }
})
