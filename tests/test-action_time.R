library(magrittr)
library(unittest)

library(gadget3)

expected_steps <- 8 * 3  # i.e. 1990..1997, with 3 steps
all_time <- array(dim = c(expected_steps))
all_step <- array(dim = c(expected_steps))
all_step_len <- array(dim = c(expected_steps))
all_year <- array(dim = c(expected_steps))
all_step_final <- array(FALSE, dim = c(expected_steps))

actions <- g3_collate(
    g3a_time(1990, 1997, steps = c(3,3,6)),
    list(
        '999' = ~{
            all_time[g3_idx(cur_time + 1)] <- cur_time
            g3_report(all_time)

            all_step[g3_idx(cur_time + 1)] <- cur_step
            g3_report(all_step)

            all_step_len[g3_idx(cur_time + 1)] <- cur_step_len
            g3_report(all_step_len)

            all_year[g3_idx(cur_time + 1)] <- cur_year
            g3_report(all_year)

            all_step_final[g3_idx(cur_time + 1)] <- cur_step_final
            g3_report(all_step_final)

            nll <- g3_param('x')  # ...or TMB falls over
        }))
params <- list(x=1.0)
model_fn <- g3_to_r(actions)
# model_fn <- edit(model_fn)
result <- model_fn(params)
# str(as.list(environment(model_fn)$model_report))

ok(ut_cmp_identical(
    as.vector(environment(model_fn)$model_report$all_time),
    1:expected_steps - 1), "cur_time populated")
ok(ut_cmp_identical(
    as.vector(environment(model_fn)$model_report$all_step),
    rep(c(1,2,3), 8)), "cur_step populated")
ok(ut_cmp_identical(
    as.vector(environment(model_fn)$model_report$all_step_len),
    as.integer(rep(c(3,3,6), 8))), "cur_step_len populated")
ok(ut_cmp_identical(
    as.vector(environment(model_fn)$model_report$all_year),
    as.numeric(rep(1990:1997, each = 3))), "cur_year populated")
ok(ut_cmp_identical(
    as.vector(environment(model_fn)$model_report$all_step_final),
    rep(c(FALSE, FALSE, TRUE), 8)), "cur_step_final populated")

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
    model_tmb_report <- model_tmb$report()
    for (n in ls(environment(model_fn)$model_report)) {
        ok(ut_cmp_equal(
            model_tmb_report[[n]],
            # NB: as.numeric to avoid differences with logical
            as.numeric(as.vector(environment(model_fn)$model_report[[n]])),
            tolerance = 1e-5), paste("TMB and R match", n))
    }
} else {
    writeLines("# skip: not running TMB tests")
}
