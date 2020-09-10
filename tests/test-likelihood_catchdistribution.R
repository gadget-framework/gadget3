library(magrittr)
library(unittest)

library(gadget3)

tmb_r_compare <- function (model_fn, model_tmb, params) {
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        # Reformat params into a single vector in expected order
        par <- unlist(params[names(environment(model_cpp)$model_parameters)])
        model_tmb_report <- model_tmb$report(par)
        for (n in ls(environment(model_fn)$model_report)) {
            ok(ut_cmp_equal(
                as.vector(model_tmb_report[[n]]),
                as.vector(environment(model_fn)$model_report[[n]]),
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}

ok(grepl(
    'stock_ssinv\\(modelstock__num,\\s+"area",\\s+"age"\\)',
    paste(deparse(g3l_catchdistribution_sumofsquares(c('area', 'age'))), collapse = ""),
    perl = TRUE), "Added custom totals to sumofsquares modelstock__num")
ok(grepl(
    'stock_ssinv\\(obsstock__num,\\s+"time",\\s+"area",\\s+"age"\\)',
    paste(deparse(g3l_catchdistribution_sumofsquares(c('area', 'age'))), collapse = ""),
    perl = TRUE), "Added custom totals to sumofsquares obsstock__num")


ok_group('g3l_likelihood_data:time', {
    generate_ld <- function (tbl, ...) gadget3:::g3l_likelihood_data('ut', structure(tbl, ...))
    stock_dims <- function(ld) dimnames(gadget3:::stock_definition(ld$modelstock, 'stock__num'))

    ok(ut_cmp_error({
        ld <- generate_ld(
            data.frame(
                number = 1:3,
                stringsAsFactors = FALSE),
            end = NULL)
    }, "year column"), "Noticed lack of year column")
})


ok_group('g3l_likelihood_data:age', {
    generate_ld <- function (tbl, ...) gadget3:::g3l_likelihood_data('ut', structure(tbl, ...))
    stock_dims <- function(ld) dimnames(gadget3:::stock_definition(ld$modelstock, 'stock__num'))

    ld <- generate_ld(
        expand.grid(
            year = 1999:2001,
            age = c(3,4,9),
            number = c(1)),
        end = NULL)
    ok(ut_cmp_identical(dimnames(ld$number), list(
        "len0",
        c("age3", "age4", "age9"),
        c("1999.", "2000.", "2001."))), "Worked out age dimensions from data")
    ok(ut_cmp_identical(stock_dims(ld), list(
        "len0",
        c("age3", "age4", "age9"))), "modelstock got same dimensions")

    ld <- generate_ld(
        expand.grid(
            year = 1999:2001,
            age = c('x', 'y'),
            number = c(1)),
        age = list(x = 1:3, y = 4:5),
        end = NULL)
    ok(ut_cmp_identical(dimnames(ld$number), list(
        "len0",
        c("age1", "age4"),
        c("1999.", "2000.", "2001."))), "Worked out age dimensions from attribute")
    ok(ut_cmp_identical(stock_dims(ld), list(
        "len0",
        c("age1", "age4"))), "modelstock got same dimensions")
})

areas <- c(a = 1, b = 2, c = 3)
prey_a <- g3_stock('prey_a', seq(1, 10)) %>% g3s_livesonareas(areas[c('a')])
prey_b <- g3_stock('prey_b', seq(1, 10)) %>% g3s_livesonareas(areas[c('b')])
prey_c <- g3_stock('prey_c', seq(1, 10)) %>% g3s_livesonareas(areas[c('c')])
fleet_abc <- g3_fleet('fleet_abc') %>% g3s_livesonareas(areas[c('a', 'b', 'c')])

# Generate observation data for stock distribution
# NB: No prey_b, only compare prey_a and prey_c
sd_data <- expand.grid(year = 1999:2000, step = c(1, 2), area = c('x', 'y'), stock = c("prey_a", "prey_c"), length = c(1,6))
sd_data$number <- floor(runif(length(sd_data$year), min=100, max=999))
attr(sd_data, 'area') <- list(
    x = areas[c('a', 'b')],
    y = areas[c('c')])

# Generate observation data for catch distribution
cd_data <- expand.grid(year = 1999:2000, step = c(1, 2), area = c('x', 'y'), length = c(1,6))
cd_data$number <- floor(runif(length(cd_data$year), min=100, max=999))
attr(cd_data, 'area') <- list(
    x = areas[c('a', 'b')],
    y = areas[c('c')])

# Generate observation data for catch distribution (multinomial)
multinomial_data <- expand.grid(year = 1999:2000, step = c(1, 2), length = c(1,6))
multinomial_data$number <- floor(runif(length(multinomial_data$year), min=100, max=999))

surveyindices_data <- expand.grid(year = 1999:2000, step = c(1, 2))
surveyindices_data$number <- floor(runif(length(surveyindices_data$year), min=100, max=999))

# Generate a step that reports the value of (var_name) into separate variable (steps) times
# (initial_val) provides a definition to use to set variable type
report_step <- function (var_name, steps, initial_val) {
    out <- ~{}
    for (i in seq(steps - 1, 0, by = -1)) {
        step_var_name <- paste0("step", i, "_", var_name)
        assign(step_var_name, initial_val)
        out <- gadget3:::f_substitute(~if (cur_time == i) {
            step_var[] <- var
            g3_report(step_var)
        } else rest, list(
            step_var = as.symbol(step_var_name),
            var = as.symbol(var_name),
            i = i,
            rest = out))
    }
    return(out)
}

actions <- g3_collate(
    g3a_time(1999,2000, steps = c(6, 6)),
    g3a_initialconditions(prey_a, ~10 * prey_a__midlen, ~100 * prey_a__midlen),
    g3a_initialconditions(prey_b, ~10 * prey_b__midlen, ~100 * prey_b__midlen),
    g3a_initialconditions(prey_c, ~10 * prey_c__midlen, ~100 * prey_c__midlen),
    g3a_predate_totalfleet(
        fleet_abc,
        list(prey_a, prey_b, prey_c),
        suitabilities = list(
            prey_a = ~g3_param_vector("fleet_abc_a"),
            prey_b = ~g3_param_vector("fleet_abc_b"),
            prey_c = ~g3_param_vector("fleet_abc_c")),
        amount_f = ~g3_param('amount_ab') * area),
    g3l_catchdistribution(
        'utcd',
        cd_data,
        list(fleet_abc),
        list(prey_a, prey_b, prey_c),
        g3l_catchdistribution_sumofsquares()),
    g3l_catchdistribution(
        'utsd',
        sd_data,
        list(fleet_abc),
        list(prey_a, prey_b, prey_c),
        g3l_catchdistribution_sumofsquares()),
    g3l_catchdistribution(
        'multinom',
        multinomial_data,
        list(fleet_abc),
        list(prey_a),
        g3l_catchdistribution_multinomial()),
    g3l_catchdistribution(
        'surveyindices',
        surveyindices_data,
        list(fleet_abc),
        list(prey_b),
        g3l_catchdistribution_surveyindices('log', alpha = ~g3_param("si_alpha"), beta = ~g3_param("si_beta"))),
    list(
        # Capture data just before final step erases it
        '010:utcd:001:zzzz' = report_step('cdist_utcd_model__num', 4, array(
            # NB: Lift definition from deparse(r$cdist_utcd_model__num)
            dim = c(2L, 2L),
            dimnames = list(
                c("len1", "len6"),
                c("area1", "area3")))),
        '010:utsd:001:zzzz' = report_step('cdist_utsd_model__num', 4, array(
            # NB: Lift definition from deparse(r$cdist_utsd_model__num)
            dim = c(2L, 2L, 2L),
            dimnames = list(
                c("len1", "len6"),
                c("prey_a", "prey_c"),
                c("area1", "area3")))),
        '010:multinom:001:zzzz' = report_step('cdist_multinom_model__num', 4, array(
            # NB: Lift definition from deparse(r$cdist_multinom_model__num)
            dim = c(2L),
            dimnames = list(
                c("len1", "len6")))),
        '010:surveyindices:001:zzzz' = report_step('cdist_surveyindices_model__num', 4, array(
            # NB: Lift definition from deparse(r$cdist_surveyindices_model__num)
            dim = c(1L),
            dimnames = list(
                c("len0")))),
        '990:prey_a__fleet_abc' = report_step('prey_a__fleet_abc', 4, gadget3:::stock_definition(prey_a, 'stock__wgt')),
        '990:prey_b__fleet_abc' = report_step('prey_b__fleet_abc', 4, gadget3:::stock_definition(prey_b, 'stock__wgt')),
        '990:prey_c__fleet_abc' = report_step('prey_c__fleet_abc', 4, gadget3:::stock_definition(prey_c, 'stock__wgt')),
        '990:nll' = report_step('nll', 4, 0.0),
        '999' = ~{
            g3_report(cdist_utcd_model__num)
            g3_report(cdist_utcd_obs__num)
            g3_report(cdist_utsd_model__num)
            g3_report(cdist_utsd_obs__num)
            g3_report(cdist_multinom_model__num)
            g3_report(cdist_multinom_obs__num)
            g3_report(cdist_surveyindices_model__num)
            g3_report(cdist_surveyindices_obs__num)
            g3_report(prey_a__wgt)
            g3_report(prey_b__wgt)
            g3_report(prey_c__wgt)

            # NB: In theory we could inspect the return value, but TMB doesn't give an easy public method for that
            g3_report(nll)
        }))
params <- list(
    fleet_abc_a = c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0),
    fleet_abc_b = c(0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0),
    fleet_abc_c = c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0),
    amount_ab = 100,
    si_alpha = 0,
    si_beta = 0,
    x=1.0)

# Compile model
model_fn <- g3_compile_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_precompile_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
} else {
    writeLines("# skip: not compiling TMB model")
}

ok_group("Likelihood per step", {
    params <- list(
        # Randomly catch, but get something everywhere
        fleet_abc_a = runif(10, min=0.1, max=0.9),
        fleet_abc_b = runif(10, min=0.1, max=0.9),
        fleet_abc_c = runif(10, min=0.1, max=0.9),
        amount_ab = 1000000,
        si_alpha = 4,
        si_beta = 2,
        x=1.0)
    result <- model_fn(params)
    r <- environment(model_fn)$model_report
    # str(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_equal(
        sort(as.vector(r$cdist_utsd_obs__num)),
        sort(sd_data$number)), "cdist_utsd_obs__num: Imported from data.frame, order not necessarily the same")

    ######## cdist_utsd_model__num
    ok(ut_cmp_equal(as.vector(r$step0_cdist_utsd_model__num[,'prey_a', 1]), c(
        sum(r$step0_prey_a__fleet_abc[1:5,] / r$prey_a__wgt[1:5,]),
        sum(r$step0_prey_a__fleet_abc[6:10,] / r$prey_a__wgt[6:10,]))), "step0_cdist_utsd_model__num[,'prey_a',1]: prey_a in area 1")
    ok(ut_cmp_equal(as.vector(r$step0_cdist_utsd_model__num[,'prey_c', 1]), c(
        0,
        0)), "step0_cdist_utsd_model__num[,'prey_c',1]: No prey_c in area 1")
    ok(ut_cmp_equal(as.vector(r$step0_cdist_utsd_model__num[,'prey_a', 2]), c(
        0,
        0)), "step0_cdist_utsd_model__num[,'prey_a',2]: No prey_a in area 2")
    ok(ut_cmp_equal(as.vector(r$step0_cdist_utsd_model__num[,'prey_c', 2]), c(
        sum(r$step0_prey_c__fleet_abc[1:5,] / r$prey_c__wgt[1:5,]),
        sum(r$step0_prey_c__fleet_abc[6:10,] / r$prey_c__wgt[6:10,]))), "step0_cdist_utsd_model__num[,'prey_c',2]: prey_c in area 2")

    ok(ut_cmp_equal(as.vector(r$step1_cdist_utsd_model__num[,'prey_a', 1]), c(
        sum(r$step1_prey_a__fleet_abc[1:5,] / r$prey_a__wgt[1:5,]),
        sum(r$step1_prey_a__fleet_abc[6:10,] / r$prey_a__wgt[6:10,]))), "step1_cdist_utsd_model__num[,'prey_a',1]: prey_a in area 1")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_utsd_model__num[,'prey_c', 1]), c(
        0,
        0)), "step1_cdist_utsd_model__num[,'prey_c',1]: No prey_c in area 1")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_utsd_model__num[,'prey_a', 2]), c(
        0,
        0)), "step1_cdist_utsd_model__num[,'prey_a',2]: No prey_a in area 2")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_utsd_model__num[,'prey_c', 2]), c(
        sum(r$step1_prey_c__fleet_abc[1:5,] / r$prey_c__wgt[1:5,]),
        sum(r$step1_prey_c__fleet_abc[6:10,] / r$prey_c__wgt[6:10,]))), "step1_cdist_utsd_model__num[,'prey_c',2]: prey_c in area 2")

    ok(ut_cmp_equal(as.vector(r$step2_cdist_utsd_model__num[,'prey_a', 1]), c(
        sum(r$step2_prey_a__fleet_abc[1:5,] / r$prey_a__wgt[1:5,]),
        sum(r$step2_prey_a__fleet_abc[6:10,] / r$prey_a__wgt[6:10,]))), "step2_cdist_utsd_model__num[,'prey_a',1]: prey_a in area 1")
    ok(ut_cmp_equal(as.vector(r$step2_cdist_utsd_model__num[,'prey_c', 1]), c(
        0,
        0)), "step2_cdist_utsd_model__num[,'prey_c',1]: No prey_c in area 1")
    ok(ut_cmp_equal(as.vector(r$step2_cdist_utsd_model__num[,'prey_a', 2]), c(
        0,
        0)), "step2_cdist_utsd_model__num[,'prey_a',2]: No prey_a in area 2")
    ok(ut_cmp_equal(as.vector(r$step2_cdist_utsd_model__num[,'prey_c', 2]), c(
        sum(r$step2_prey_c__fleet_abc[1:5,] / r$prey_c__wgt[1:5,]),
        sum(r$step2_prey_c__fleet_abc[6:10,] / r$prey_c__wgt[6:10,]))), "step2_cdist_utsd_model__num[,'prey_c',2]: prey_c in area 2")

    ok(ut_cmp_equal(as.vector(r$step3_cdist_utsd_model__num[,'prey_a', 1]), c(
        sum(r$step3_prey_a__fleet_abc[1:5,] / r$prey_a__wgt[1:5,]),
        sum(r$step3_prey_a__fleet_abc[6:10,] / r$prey_a__wgt[6:10,]))), "step3_cdist_utsd_model__num[,'prey_a',1]: prey_a in area 1")
    ok(ut_cmp_equal(as.vector(r$step3_cdist_utsd_model__num[,'prey_c', 1]), c(
        0,
        0)), "step3_cdist_utsd_model__num[,'prey_c',1]: No prey_c in area 1")
    ok(ut_cmp_equal(as.vector(r$step3_cdist_utsd_model__num[,'prey_a', 2]), c(
        0,
        0)), "step3_cdist_utsd_model__num[,'prey_a',2]: No prey_a in area 2")
    ok(ut_cmp_equal(as.vector(r$step3_cdist_utsd_model__num[,'prey_c', 2]), c(
        sum(r$step3_prey_c__fleet_abc[1:5,] / r$prey_c__wgt[1:5,]),
        sum(r$step3_prey_c__fleet_abc[6:10,] / r$prey_c__wgt[6:10,]))), "step3_cdist_utsd_model__num[,'prey_c',2]: prey_c in area 2")
    ########

    ######## cdist_utcd_model__num
    ok(ut_cmp_equal(as.vector(r$step0_cdist_utcd_model__num[,1]), c(
         sum(
             sum(r$step0_prey_a__fleet_abc[1:5,1] / r$prey_a__wgt[1:5,1]),
             sum(r$step0_prey_b__fleet_abc[1:5,1] / r$prey_b__wgt[1:5,1])),
         sum(
             sum(r$step0_prey_a__fleet_abc[6:10,1] / r$prey_a__wgt[6:10,1]),
             sum(r$step0_prey_b__fleet_abc[6:10,1] / r$prey_b__wgt[6:10,1])),
         NULL)), "step0_cdist_utsd_model__num[,1]: all prey in area a/b")
    ok(ut_cmp_equal(as.vector(r$step0_cdist_utcd_model__num[,2]), c(
         sum(
             sum(r$step0_prey_c__fleet_abc[1:5,1] / r$prey_c__wgt[1:5,1])),
         sum(
             sum(r$step0_prey_c__fleet_abc[6:10,1] / r$prey_c__wgt[6:10,1])),
         NULL)), "step0_cdist_utsd_model__num[,2]: all prey in area c")

    ok(ut_cmp_equal(as.vector(r$step1_cdist_utcd_model__num[,1]), c(
         sum(
             sum(r$step1_prey_a__fleet_abc[1:5,1] / r$prey_a__wgt[1:5,1]),
             sum(r$step1_prey_b__fleet_abc[1:5,1] / r$prey_b__wgt[1:5,1])),
         sum(
             sum(r$step1_prey_a__fleet_abc[6:10,1] / r$prey_a__wgt[6:10,1]),
             sum(r$step1_prey_b__fleet_abc[6:10,1] / r$prey_b__wgt[6:10,1])),
         NULL)), "step1_cdist_utsd_model__num[,1]: all prey in area a/b")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_utcd_model__num[,2]), c(
         sum(
             sum(r$step1_prey_c__fleet_abc[1:5,1] / r$prey_c__wgt[1:5,1])),
         sum(
             sum(r$step1_prey_c__fleet_abc[6:10,1] / r$prey_c__wgt[6:10,1])),
         NULL)), "step1_cdist_utsd_model__num[,2]: all prey in area c")

    ok(ut_cmp_equal(as.vector(r$step2_cdist_utcd_model__num[,1]), c(
         sum(
             sum(r$step2_prey_a__fleet_abc[1:5,1] / r$prey_a__wgt[1:5,1]),
             sum(r$step2_prey_b__fleet_abc[1:5,1] / r$prey_b__wgt[1:5,1])),
         sum(
             sum(r$step2_prey_a__fleet_abc[6:10,1] / r$prey_a__wgt[6:10,1]),
             sum(r$step2_prey_b__fleet_abc[6:10,1] / r$prey_b__wgt[6:10,1])),
         NULL)), "step2_cdist_utsd_model__num[,1]: all prey in area a/b")
    ok(ut_cmp_equal(as.vector(r$step2_cdist_utcd_model__num[,2]), c(
         sum(
             sum(r$step2_prey_c__fleet_abc[1:5,1] / r$prey_c__wgt[1:5,1])),
         sum(
             sum(r$step2_prey_c__fleet_abc[6:10,1] / r$prey_c__wgt[6:10,1])),
         NULL)), "step2_cdist_utsd_model__num[,2]: all prey in area c")

    ok(ut_cmp_equal(as.vector(r$step3_cdist_utcd_model__num[,1]), c(
         sum(
             sum(r$step3_prey_a__fleet_abc[1:5,1] / r$prey_a__wgt[1:5,1]),
             sum(r$step3_prey_b__fleet_abc[1:5,1] / r$prey_b__wgt[1:5,1])),
         sum(
             sum(r$step3_prey_a__fleet_abc[6:10,1] / r$prey_a__wgt[6:10,1]),
             sum(r$step3_prey_b__fleet_abc[6:10,1] / r$prey_b__wgt[6:10,1])),
         NULL)), "step3_cdist_utsd_model__num[,1]: all prey in area a/b")
    ok(ut_cmp_equal(as.vector(r$step3_cdist_utcd_model__num[,2]), c(
         sum(
             sum(r$step3_prey_c__fleet_abc[1:5,1] / r$prey_c__wgt[1:5,1])),
         sum(
             sum(r$step3_prey_c__fleet_abc[6:10,1] / r$prey_c__wgt[6:10,1])),
         NULL)), "step3_cdist_utsd_model__num[,2]: all prey in area c")
    ########

    ok(ut_cmp_equal(r$step0_nll, sum(
        # utsd: stock 1 / area 1
        (r$step0_cdist_utsd_model__num[,1,1] / sum(r$step0_cdist_utsd_model__num[,,1]) -
            r$cdist_utsd_obs__num[,1,1,1] / sum(r$cdist_utsd_obs__num[,,1,1])) ** 2,
        # utsd: stock 2 / area 1
        (r$step0_cdist_utsd_model__num[,2,1] / sum(r$step0_cdist_utsd_model__num[,,1]) -
            r$cdist_utsd_obs__num[,2,1,1] / sum(r$cdist_utsd_obs__num[,,1,1])) ** 2,
        # utsd: stock 1 / area 2
        (r$step0_cdist_utsd_model__num[,1,2] / sum(r$step0_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,1,2,1] / sum(r$cdist_utsd_obs__num[,,2,1])) ** 2,
        # utsd: stock 2 / area 2
        (r$step0_cdist_utsd_model__num[,2,2] / sum(r$step0_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,2,2,1] / sum(r$cdist_utsd_obs__num[,,2,1])) ** 2,
        # utcd: area 1
        (r$step0_cdist_utcd_model__num[,1] / sum(r$step0_cdist_utcd_model__num[,1]) -
            r$cdist_utcd_obs__num[,1,1] / sum(r$cdist_utcd_obs__num[,1,1])) ** 2,
        # utcd: area 2
        (r$step0_cdist_utcd_model__num[,2] / sum(r$step0_cdist_utcd_model__num[,2]) -
            r$cdist_utcd_obs__num[,2,1] / sum(r$cdist_utcd_obs__num[,2,1])) ** 2,
        # multinom:
        (2 * (lgamma(1 + sum( r$cdist_multinom_obs__num[,1] )) -
            sum(lgamma(1 + r$cdist_multinom_obs__num[,1])) +
            sum(r$cdist_multinom_obs__num[,1] * log(
                r$step0_cdist_multinom_model__num / sum(r$step0_cdist_multinom_model__num))))),
        # surveyindices:
        sum(params$si_alpha +
            params$si_beta * log(r$step0_cdist_surveyindices_model__num) -
            log(r$cdist_surveyindices_obs__num[,1])),
        0)), "step0_nll: Sum of squares")

    ok(ut_cmp_equal(r$step1_nll, sum(
        # utsd: stock 1 / area 1
        (r$step1_cdist_utsd_model__num[,1,1] / sum(r$step1_cdist_utsd_model__num[,,1]) -
            r$cdist_utsd_obs__num[,1,1,2] / sum(r$cdist_utsd_obs__num[,,1,2])) ** 2,
        # utsd: stock 2 / area 1
        (r$step1_cdist_utsd_model__num[,2,1] / sum(r$step1_cdist_utsd_model__num[,,1]) -
            r$cdist_utsd_obs__num[,2,1,2] / sum(r$cdist_utsd_obs__num[,,1,2])) ** 2,
        # utsd: stock 1 / area 2
        (r$step1_cdist_utsd_model__num[,1,2] / sum(r$step1_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,1,2,2] / sum(r$cdist_utsd_obs__num[,,2,2])) ** 2,
        # utsd: stock 2 / area 2
        (r$step1_cdist_utsd_model__num[,2,2] / sum(r$step1_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,2,2,2] / sum(r$cdist_utsd_obs__num[,,2,2])) ** 2,
        # utcd: area 1
        (r$step1_cdist_utcd_model__num[,1] / sum(r$step1_cdist_utcd_model__num[,1]) -
            r$cdist_utcd_obs__num[,1,2] / sum(r$cdist_utcd_obs__num[,1,2])) ** 2,
        # utcd: area 2
        (r$step1_cdist_utcd_model__num[,2] / sum(r$step1_cdist_utcd_model__num[,2]) -
            r$cdist_utcd_obs__num[,2,2] / sum(r$cdist_utcd_obs__num[,2,2])) ** 2,
        # multinom:
        (2 * (lgamma(1 + sum( r$cdist_multinom_obs__num[,2] )) -
            sum(lgamma(1 + r$cdist_multinom_obs__num[,2])) +
            sum(r$cdist_multinom_obs__num[,2] * log(
                r$step1_cdist_multinom_model__num / sum(r$step1_cdist_multinom_model__num))))),
        # surveyindices:
        sum(params$si_alpha +
            params$si_beta * log(r$step1_cdist_surveyindices_model__num) -
            log(r$cdist_surveyindices_obs__num[,2])),
        r$step0_nll)), "step1_nll: Sum of squares, including step0_nll")

    ok(ut_cmp_equal(r$step2_nll, sum(
        # utsd: stock 1 / area 1
        (r$step2_cdist_utsd_model__num[,1,1] / sum(r$step2_cdist_utsd_model__num[,,1]) -
            r$cdist_utsd_obs__num[,1,1,3] / sum(r$cdist_utsd_obs__num[,,1,3])) ** 2,
        # utsd: stock 2 / area 1
        (r$step2_cdist_utsd_model__num[,2,1] / sum(r$step2_cdist_utsd_model__num[,,1]) -
            r$cdist_utsd_obs__num[,2,1,3] / sum(r$cdist_utsd_obs__num[,,1,3])) ** 2,
        # utsd: stock 1 / area 2
        (r$step2_cdist_utsd_model__num[,1,2] / sum(r$step2_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,1,2,3] / sum(r$cdist_utsd_obs__num[,,2,3])) ** 2,
        # utsd: stock 2 / area 2
        (r$step2_cdist_utsd_model__num[,2,2] / sum(r$step2_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,2,2,3] / sum(r$cdist_utsd_obs__num[,,2,3])) ** 2,
        # utcd: area 1
        (r$step2_cdist_utcd_model__num[,1] / sum(r$step2_cdist_utcd_model__num[,1]) -
            r$cdist_utcd_obs__num[,1,3] / sum(r$cdist_utcd_obs__num[,1,3])) ** 2,
        # utcd: area 2
        (r$step2_cdist_utcd_model__num[,2] / sum(r$step2_cdist_utcd_model__num[,2]) -
            r$cdist_utcd_obs__num[,2,3] / sum(r$cdist_utcd_obs__num[,2,3])) ** 2,
        # multinom:
        (2 * (lgamma(1 + sum( r$cdist_multinom_obs__num[,3] )) -
            sum(lgamma(1 + r$cdist_multinom_obs__num[,3])) +
            sum(r$cdist_multinom_obs__num[,3] * log(
                r$step2_cdist_multinom_model__num / sum(r$step2_cdist_multinom_model__num))))),
        # surveyindices:
        sum(params$si_alpha +
            params$si_beta * log(r$step2_cdist_surveyindices_model__num) -
            log(r$cdist_surveyindices_obs__num[,3])),
        r$step1_nll)), "step2_nll: Sum of squares, including step1_nll")

    ok(ut_cmp_equal(r$step3_nll, sum(
        # utsd: stock 1 / area 1
        (r$step3_cdist_utsd_model__num[,1,1] / sum(r$step3_cdist_utsd_model__num[,,1]) -
            r$cdist_utsd_obs__num[,1,1,4] / sum(r$cdist_utsd_obs__num[,,1,4])) ** 2,
        # utsd: stock 2 / area 1
        (r$step3_cdist_utsd_model__num[,2,1] / sum(r$step3_cdist_utsd_model__num[,,1]) -
            r$cdist_utsd_obs__num[,2,1,4] / sum(r$cdist_utsd_obs__num[,,1,4])) ** 2,
        # utsd: stock 1 / area 2
        (r$step3_cdist_utsd_model__num[,1,2] / sum(r$step3_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,1,2,4] / sum(r$cdist_utsd_obs__num[,,2,4])) ** 2,
        # utsd: stock 2 / area 2
        (r$step3_cdist_utsd_model__num[,2,2] / sum(r$step3_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,2,2,4] / sum(r$cdist_utsd_obs__num[,,2,4])) ** 2,
        # utcd: area 1
        (r$step3_cdist_utcd_model__num[,1] / sum(r$step3_cdist_utcd_model__num[,1]) -
            r$cdist_utcd_obs__num[,1,4] / sum(r$cdist_utcd_obs__num[,1,4])) ** 2,
        # utcd: area 2
        (r$step3_cdist_utcd_model__num[,2] / sum(r$step3_cdist_utcd_model__num[,2]) -
            r$cdist_utcd_obs__num[,2,4] / sum(r$cdist_utcd_obs__num[,2,4])) ** 2,
        # multinom:
        (2 * (lgamma(1 + sum( r$cdist_multinom_obs__num[,4] )) -
            sum(lgamma(1 + r$cdist_multinom_obs__num[,4])) +
            sum(r$cdist_multinom_obs__num[,4] * log(
                r$step3_cdist_multinom_model__num / sum(r$step3_cdist_multinom_model__num))))),
        # surveyindices:
        sum(params$si_alpha +
            params$si_beta * log(r$step3_cdist_surveyindices_model__num) -
            log(r$cdist_surveyindices_obs__num[,4])),
        r$step2_nll)), "step3_nll: Sum of squares, including step2_nll")

    tmb_r_compare(model_fn, model_tmb, params)
})

ok_group("Likelihood per year", {
    # Change model to aggregate over a year
    # NB: No prey_b, only compare prey_a and prey_c
    # NB: No step column now
    sd_data <- expand.grid(year = 1999:2000, area = c('x', 'y'), stock = c("prey_a", "prey_c"), length = c(1,6))
    sd_data$number <- floor(runif(length(sd_data$year), min=100, max=999))
    attr(sd_data, 'area') <- list(
        x = areas[c('a', 'b')],
        y = areas[c('c')])

    # Change model to aggregate over a year
    # NB: No step column now
    cd_data <- expand.grid(year = 1999:2000, area = c('x', 'y'), length = c(1,6))
    cd_data$number <- floor(runif(length(cd_data$year), min=100, max=999))
    attr(cd_data, 'area') <- list(
        x = areas[c('a', 'b')],
        y = areas[c('c')])

    # Change model to aggregate over a year
    # NB: No step column now
    multinomial_data <- expand.grid(year = 1999:2000, length = c(1,6))
    multinomial_data$number <- floor(runif(length(multinomial_data$year), min=100, max=999))

    # NB: No step column now
    surveyindices_data <- expand.grid(year = 1999:2000)
    surveyindices_data$number <- floor(runif(length(surveyindices_data$year), min=100, max=999))

    actions <- g3_collate(
        actions,
        g3l_catchdistribution(
            'utcd',
            cd_data,
            list(fleet_abc),
            list(prey_a, prey_b, prey_c),
            g3l_catchdistribution_sumofsquares()),
        g3l_catchdistribution(
            'utsd',
            sd_data,
            list(fleet_abc),
            list(prey_a, prey_b, prey_c),
            g3l_catchdistribution_sumofsquares()),
        g3l_catchdistribution(
            'multinom',
            multinomial_data,
            list(fleet_abc),
            list(prey_a),
            g3l_catchdistribution_multinomial()),
        g3l_catchdistribution(
            'surveyindices',
            surveyindices_data,
            list(fleet_abc),
            list(prey_b),
            g3l_catchdistribution_surveyindices('log', alpha = ~g3_param("si_alpha"), beta = ~g3_param("si_beta"))))

    # Compile model
    model_fn <- g3_compile_r(actions, trace = FALSE)
    # model_fn <- edit(model_fn)
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_cpp <- g3_precompile_tmb(actions, trace = FALSE)
        # model_cpp <- edit(model_cpp)
        model_tmb <- g3_tmb_adfun(model_cpp, params)
    } else {
        writeLines("# skip: not compiling TMB model")
    }

    params <- list(
        fleet_abc_a = runif(10, min=0.1, max=0.9),
        fleet_abc_b = runif(10, min=0.1, max=0.9),
        fleet_abc_c = runif(10, min=0.1, max=0.9),
        amount_ab = 1000000,
        si_alpha = 1.82,
        si_beta = 3.74,
        x=1.0)
    result <- model_fn(params)
    r <- environment(model_fn)$model_report
    # str(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_equal(
        as.vector(r$cdist_utsd_obs__num),
        sd_data$number), "cdist_utsd_obs__num: Imported from data.frame")

    ######## cdist_utsd_model__num
    ok(ut_cmp_equal(as.vector(r$step1_cdist_utsd_model__num[,'prey_a', 1]), c(
        sum(
            (r$step0_prey_a__fleet_abc[1:5,] / r$prey_a__wgt[1:5,]),
            (r$step1_prey_a__fleet_abc[1:5,] / r$prey_a__wgt[1:5,])),
        sum(
            sum(r$step0_prey_a__fleet_abc[6:10,] / r$prey_a__wgt[6:10,]),
            sum(r$step1_prey_a__fleet_abc[6:10,] / r$prey_a__wgt[6:10,])),
        NULL)), "step1_cdist_utsd_model__num[,'prey_a',1]: prey_a summed over year")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_utsd_model__num[,'prey_c', 1]), c(
        0,
        0,
        NULL)), "step1_cdist_utsd_model__num[,'prey_c',1]: No prey_c in first area")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_utsd_model__num[,'prey_a', 2]), c(
        0,
        0,
        NULL)), "step1_cdist_utsd_model__num[,'prey_a',2]: No prey_a in second area")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_utsd_model__num[,'prey_c', 2]), c(
        sum(
            (r$step0_prey_c__fleet_abc[1:5,] / r$prey_c__wgt[1:5,]),
            (r$step1_prey_c__fleet_abc[1:5,] / r$prey_c__wgt[1:5,])),
        sum(
            sum(r$step0_prey_c__fleet_abc[6:10,] / r$prey_c__wgt[6:10,]),
            sum(r$step1_prey_c__fleet_abc[6:10,] / r$prey_c__wgt[6:10,])),
        NULL)), "step1_cdist_utsd_model__num[,'prey_c',2]: prey_c summed over year")
    ########

    ######## cdist_utcd_model__num
    ok(ut_cmp_equal(as.vector(r$step1_cdist_utcd_model__num[,1]), c(
         sum(
             sum(r$step0_prey_a__fleet_abc[1:5,1] / r$prey_a__wgt[1:5,1]),
             sum(r$step0_prey_b__fleet_abc[1:5,1] / r$prey_b__wgt[1:5,1]),
             sum(r$step1_prey_a__fleet_abc[1:5,1] / r$prey_a__wgt[1:5,1]),
             sum(r$step1_prey_b__fleet_abc[1:5,1] / r$prey_b__wgt[1:5,1])),
         sum(
             sum(r$step0_prey_a__fleet_abc[6:10,1] / r$prey_a__wgt[6:10,1]),
             sum(r$step0_prey_b__fleet_abc[6:10,1] / r$prey_b__wgt[6:10,1]),
             sum(r$step1_prey_a__fleet_abc[6:10,1] / r$prey_a__wgt[6:10,1]),
             sum(r$step1_prey_b__fleet_abc[6:10,1] / r$prey_b__wgt[6:10,1])),
         NULL)), "step1_cdist_utsd_model__num[,1]: all prey from a/b and steps 0 & 1")

    ok(ut_cmp_equal(as.vector(r$step1_cdist_utcd_model__num[,2]), c(
         sum(
             sum(r$step0_prey_c__fleet_abc[1:5,1] / r$prey_c__wgt[1:5,1]),
             sum(r$step1_prey_c__fleet_abc[1:5,1] / r$prey_c__wgt[1:5,1])),
         sum(
             sum(r$step0_prey_c__fleet_abc[6:10,1] / r$prey_c__wgt[6:10,1]),
             sum(r$step1_prey_c__fleet_abc[6:10,1] / r$prey_c__wgt[6:10,1])),
         NULL)), "step1_cdist_utsd_model__num[,2]: all prey from c and steps 0/1")
    ########

    ok(ut_cmp_equal(r$step0_nll, 0), "step0_nll: nll not calculated yet")

    ok(ut_cmp_equal(r$step1_nll, sum(
        # utsd: stock 1 / area 1
        (r$step1_cdist_utsd_model__num[,1,1] / sum(r$step1_cdist_utsd_model__num[,,1]) -
            # NB: Still using first time data, unlike per-step example
            r$cdist_utsd_obs__num[,1,1,1] / sum(r$cdist_utsd_obs__num[,,1,1])) ** 2,
        # utsd: stock 2 / area 1
        (r$step1_cdist_utsd_model__num[,2,1] / sum(r$step1_cdist_utsd_model__num[,,1]) -
            r$cdist_utsd_obs__num[,2,1,1] / sum(r$cdist_utsd_obs__num[,,1,1])) ** 2,
        # utsd: stock 1 / area 2
        (r$step1_cdist_utsd_model__num[,1,2] / sum(r$step1_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,1,2,1] / sum(r$cdist_utsd_obs__num[,,2,1])) ** 2,
        # utsd: stock 2 / area 2
        (r$step1_cdist_utsd_model__num[,2,2] / sum(r$step1_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,2,2,1] / sum(r$cdist_utsd_obs__num[,,2,1])) ** 2,
        # utcd: area 1
        (r$step1_cdist_utcd_model__num[,1] / sum(r$step1_cdist_utcd_model__num[,1]) -
            r$cdist_utcd_obs__num[,1,1] / sum(r$cdist_utcd_obs__num[,1,1])) ** 2,
        # utcd: area 2
        (r$step1_cdist_utcd_model__num[,2] / sum(r$step1_cdist_utcd_model__num[,2]) -
            r$cdist_utcd_obs__num[,2,1] / sum(r$cdist_utcd_obs__num[,2,1])) ** 2,
        # multinom:
        (2 * (lgamma(1 + sum( r$cdist_multinom_obs__num[,1] )) -
            sum(lgamma(1 + r$cdist_multinom_obs__num[,1])) +
            sum(r$cdist_multinom_obs__num[,1] * log(
                r$step1_cdist_multinom_model__num / sum(r$step1_cdist_multinom_model__num))))),
        # surveyindices:
        sum(params$si_alpha +
            params$si_beta * log(r$step1_cdist_surveyindices_model__num) -
            log(r$cdist_surveyindices_obs__num[,1])),
        0)), "step1_nll: Sum of squares")

    ok(ut_cmp_equal(r$step3_nll, sum(
        # utsd: stock 1 / area 1
        (r$step3_cdist_utsd_model__num[,1,1] / sum(r$step3_cdist_utsd_model__num[,,1]) -
            # NB: Second time data, not 4th as in per-step example
            r$cdist_utsd_obs__num[,1,1,2] / sum(r$cdist_utsd_obs__num[,,1,2])) ** 2,
        # utsd: stock 2 / area 1
        (r$step3_cdist_utsd_model__num[,2,1] / sum(r$step3_cdist_utsd_model__num[,,1]) -
            r$cdist_utsd_obs__num[,2,1,2] / sum(r$cdist_utsd_obs__num[,,1,2])) ** 2,
        # utsd: stock 1 / area 2
        (r$step3_cdist_utsd_model__num[,1,2] / sum(r$step3_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,1,2,2] / sum(r$cdist_utsd_obs__num[,,2,2])) ** 2,
        # utsd: stock 2 / area 2
        (r$step3_cdist_utsd_model__num[,2,2] / sum(r$step3_cdist_utsd_model__num[,,2]) -
            r$cdist_utsd_obs__num[,2,2,2] / sum(r$cdist_utsd_obs__num[,,2,2])) ** 2,
        # utcd: area 1
        (r$step3_cdist_utcd_model__num[,1] / sum(r$step3_cdist_utcd_model__num[,1]) -
            r$cdist_utcd_obs__num[,1,2] / sum(r$cdist_utcd_obs__num[,1,2])) ** 2,
        # utcd: area 2
        (r$step3_cdist_utcd_model__num[,2] / sum(r$step3_cdist_utcd_model__num[,2]) -
            r$cdist_utcd_obs__num[,2,2] / sum(r$cdist_utcd_obs__num[,2,2])) ** 2,
        # multinom:
        (2 * (lgamma(1 + sum( r$cdist_multinom_obs__num[,2] )) -
            sum(lgamma(1 + r$cdist_multinom_obs__num[,2])) +
            sum(r$cdist_multinom_obs__num[,2] * log(
                r$step3_cdist_multinom_model__num / sum(r$step3_cdist_multinom_model__num))))),
        # surveyindices:
        sum(params$si_alpha +
            params$si_beta * log(r$step3_cdist_surveyindices_model__num) -
            log(r$cdist_surveyindices_obs__num[,2])),
        r$step1_nll)), "step3_nll: Sum of squares, including step1_nll")

    tmb_r_compare(model_fn, model_tmb, params)
})
