library(magrittr)
library(unittest)

library(gadget3)

# Zip name/value arguments together into a list
named_list <- function(...) {
    x <- list(...)
    structure(
        x[seq_along(x) %% 2 == 0],
        names = as.character(x[seq_along(x) %% 2 == 1]))
}

cmp_grep <- function (a, ...) {
    re <- paste0('\\Q', c(...), '\\E', collapse = ".*")
    if (grepl(re, a, perl = TRUE)) return(TRUE)
    c(re, "Not found in:", a)
}

# NB: Name has to be different, or it gets sucked into the model
g3_avoid_zero <- g3_env$avoid_zero
g3_logspace_add <- g3_env$logspace_add
g3_logspace_add_vec <- g3_env$logspace_add_vec
g3_lgamma_vec <- lgamma

ok_group("g3_distribution_preview", {
    dat <- expand.grid(year = 1990:1994, step = 2, area = 'IXa')
    dat$number <- seq_len(nrow(dat))
    out <- g3_distribution_preview(dat, area_group = c(IXa = 1))
    ok(ut_cmp_equal(out, structure(
        1:5,
        dim = c(length = 1L, time = 5L, area = 1L),
        dimnames = list(
            length = "0:Inf",
            time = c("1990-02", "1991-02", "1992-02", "1993-02", "1994-02"),
            area = "IXa") )), "Returned number array")

    dat <- expand.grid(year = 1990:1994, step = 2, area = 'IXa')
    dat$weight <- seq_len(nrow(dat)) * 40
    out <- g3_distribution_preview(dat, area_group = c(IXa = 1))
    ok(ut_cmp_equal(out, structure(
        1:5 * 40,
        dim = c(length = 1L, time = 5L, area = 1L),
        dimnames = list(
            length = "0:Inf",
            time = c("1990-02", "1991-02", "1992-02", "1993-02", "1994-02"),
            area = "IXa") )), "Returned weight array")

    dat <- expand.grid(year = 1990:1994, step = 2, area = 'IXa')
    dat$number <- seq_len(nrow(dat)) * 9
    dat$weight <- seq_len(nrow(dat)) * 40
    out <- g3_distribution_preview(dat, area_group = c(IXa = 1))
    ok(ut_cmp_equal(out, structure(
        1:5 * 9,
        dim = c(length = 1L, time = 5L, area = 1L),
        dimnames = list(
            length = "0:Inf",
            time = c("1990-02", "1991-02", "1992-02", "1993-02", "1994-02"),
            area = "IXa") )), "Number array wins if both present")

})

ok_group("g3l_distribution_sumofsquares", {
    ok(cmp_grep(
        deparse1(environment(g3l_distribution_sumofsquares(c('area', 'age')))$modelstock__sstotal),
        'sum(stock_ssinv(modelstock__x, "time", "area", "age"))',
        NULL), "Added custom totals to sumofsquares modelstock__x")
    ok(cmp_grep(
        deparse1(environment(g3l_distribution_sumofsquares(c('area', 'age')))$obsstock__sstotal),
        'sum(stock_ssinv(obsstock__x, "time", "area", "age"))',
        NULL), "Added custom totals to sumofsquares modelstock__x")
    ok(cmp_grep(
        deparse1(g3l_distribution_sumofsquares(c('area', 'length'))),
        'stock_ss(modelstock__x, length = default)',
        'modelstock__sstotal[[modelstock__length_idx]]',
        'stock_ss(obsstock__x, length = default)',
        'obsstock__sstotal[[obsstock__length_idx]]',
        NULL), "Adding length also adds to the stock_ss()")

    # Stratified sumofsquares
    prey_a <- g3_stock('prey_a', seq(1, 5, by = 1)) %>% g3s_age(1,5)
    prey_a__init <- g3_stock_instance(prey_a)
    prey_a__init[] <- runif(length(prey_a__init))
    obsdata <- expand.grid(
        year = 2000,
        length = seq(1, 5, by = 1),
        age = 3:4)  # NB: Only report age 3,4
    obsdata$number <- runif(nrow(obsdata))
    model_fn <- g3_to_r(list(
        # Keep TMB happy
        g3_formula({
            nll <- nll + g3_param("dummy", value = 0) 
            REPORT(prey_a__num)
        }),
        g3a_time(2000, 2001),
        g3a_initialconditions(prey_a,
            num_f = g3_formula(stock_ss(prey_a__init), prey_a__init = prey_a__init),
            wgt_f = 10),
        g3l_abundancedistribution("adist",
            obsdata,
            function_f = g3l_distribution_sumofsquares(over = c("area", "length")),
            stocks = list(prey_a),
            report = TRUE)))
    model_cpp <- g3_to_tmb(attr(model_fn, 'actions'), trace = FALSE)
    if (Sys.getenv('G3_TEST_TMB') == "2") {
        #model_cpp <- edit(model_cpp)
        #writeLines(TMB::gdbsource(g3_tmb_adfun(model_cpp, compile_flags = "-g", output_script = TRUE)))
        model_tmb <- g3_tmb_adfun(model_cpp, trace = TRUE, compile_flags = c("-O0", "-g"))
    }

    params <- attr(model_fn, 'parameter_template')
    r <- model_fn(params)
    modeldata <- attr(r, 'prey_a__num')
    expected_nll <- 0
    for (age in 3:4) for (length in 1:5) {
        # Proportion compared to others in age group, for that length
        expected_nll <- expected_nll + (
            # Proportion compared to others in this length group
            modeldata[length, age] / sum(modeldata[length, c(3,4)]) -
            obsdata[obsdata$age == age & obsdata$length == length, 'number'] / sum(obsdata[obsdata$length == length, 'number'])
        ) ** 2
    }
    ok(ut_cmp_equal(as.numeric(r), expected_nll), "g3l_distribution_sumofsquares statified over length")
    if (Sys.getenv('G3_TEST_TMB') == "2") gadget3:::ut_tmb_r_compare(model_fn, model_tmb, params, model_cpp = model_cpp)
})

ok_group("g3l_distribution:transform_fs", {
    prey_a <- g3_stock('prey_a', seq(1, 5, by = 1)) %>% g3s_age(1,5) %>% g3s_livesonareas(1:2)
    prey_a__init <- g3_stock_instance(prey_a)
    prey_a__init[] <- runif(length(prey_a__init))
    obsdata <- expand.grid(
        year = 2000,
        length = seq(1, 5, by = 1),
        age = 1:5)
    obsdata$number <- runif(nrow(obsdata))
    model_fn <- g3_to_r(list(
        g3a_time(2000, 2001),
        g3a_initialconditions(prey_a,
            num_f = g3_formula(stock_ss(prey_a__init), prey_a__init = prey_a__init),
            wgt_f = 10),
        g3l_abundancedistribution("wt",
            obsdata,
            function_f = g3l_distribution_sumofsquares(),
            stocks = list(prey_a),
            transform_fs = list(
                age = g3_formula( g3_param_array('reader1matrix', value = diag(5))[g3_idx(preage), g3_idx(age)] )),
            report = TRUE),
        g3l_abundancedistribution("len",
            obsdata,
            function_f = g3l_distribution_sumofsquares(),
            stocks = list(prey_a),
            transform_fs = list(
                length = quote(diag(10 * prey_a__midlen)) ),
            report = TRUE),
        g3l_abundancedistribution("wtperstock",
            obsdata,
            function_f = g3l_distribution_sumofsquares(),
            stocks = list(prey_a),
            transform_fs = list(age = list(
                prey_a = g3_formula(g3_param_array('reader1matrix', value = diag(prey_a__maxage - prey_a__minage + 1))[prey_a__preage_idx, prey_a__age_idx] ))),
            report = TRUE),
        g3l_abundancedistribution("nt",
            obsdata,
            function_f = g3l_distribution_sumofsquares(),
            stocks = list(prey_a),
            report = TRUE),
        # Keep TMB happy
        g3_formula( nll <- nll + g3_param("dummy", value = 0) )))
    model_cpp <- g3_to_tmb(attr(model_fn, 'actions'), trace = FALSE)
    if (Sys.getenv('G3_TEST_TMB') == "2") {
        #model_cpp <- edit(model_cpp)
        #writeLines(TMB::gdbsource(g3_tmb_adfun(model_cpp, compile_flags = "-g", output_script = TRUE)))
        model_tmb <- g3_tmb_adfun(model_cpp, trace = TRUE, compile_flags = c("-O0", "-g"))
    }

    # Given results / params, apply matrix manually and make sure the results match
    do_test <- function (r, params, msg) {
        nt <- attr(r, 'adist_sumofsquares_nt_model__num')
        wt <- attr(r, 'adist_sumofsquares_wt_model__num')
        wtperstock <- attr(r, 'adist_sumofsquares_wtperstock_model__num')
        m <- params$reader1matrix
        apply_matrix <- function (destage) {
            nt[,1,] * m[1,destage] +
            nt[,2,] * m[2,destage] +
            nt[,3,] * m[3,destage] +
            nt[,4,] * m[4,destage] +
            nt[,5,] * m[5,destage] +
            0
        }
        ok(ut_cmp_equal(wt, wtperstock), paste0("wt / wtperstock: ", msg))
        ok(ut_cmp_equal(apply_matrix(1), wt[,1,]), paste0("age1: ", msg))
        ok(ut_cmp_equal(apply_matrix(2), wt[,2,]), paste0("age2: ", msg))
        ok(ut_cmp_equal(apply_matrix(3), wt[,3,]), paste0("age3: ", msg))
        ok(ut_cmp_equal(apply_matrix(4), wt[,4,]), paste0("age4: ", msg))
        ok(ut_cmp_equal(apply_matrix(5), wt[,5,]), paste0("age5: ", msg))
    }

    params <- attr(model_fn, 'parameter_template')
    r <- model_fn(params)
    do_test(r, params, "Identity matrix")
    if (Sys.getenv('G3_TEST_TMB') == "2") gadget3:::ut_tmb_r_compare(model_fn, model_tmb, params, model_cpp = model_cpp)

    # Length vector applied for len
    ok(ut_cmp_equal(
        attr(r, "adist_sumofsquares_len_model__num"),
        attr(r, "adist_sumofsquares_nt_model__num") * (10 * g3_stock_def(prey_a, 'midlen')),
        unused = NULL), "r$adist_sumofsquares_len_model__num: Length vector applied")

    params <- attr(model_fn, 'parameter_template')
    # Age 1 smeared over bottom 3 groups
    params$reader1matrix[1,] <- c(0.5, 0.25, 0.25, 0, 0)
    # Age 2 smeared over 2 groups
    params$reader1matrix[2,] <- c(0, 0.75, 0.25, 0, 0)
    r <- model_fn(params)
    do_test(r, params, "age1, age2 smeared")
    if (Sys.getenv('G3_TEST_TMB') == "2") gadget3:::ut_tmb_r_compare(model_fn, model_tmb, params, model_cpp = model_cpp)
})

# g3l_distribution_sumofsquaredlogratios
ok(ut_cmp_identical(
    deparse1(g3l_distribution_sumofsquaredlogratios()),
    "~sum((log(stock_ss(obsstock__x) + 10) - log(stock_ss(modelstock__x) + 10))^2)"), "g3l_distribution_sumofsquaredlogratios: Formula code as expected")
ok(ut_cmp_identical(
    length(environment(g3l_distribution_sumofsquaredlogratios())), 0L), "g3l_distribution_sumofsquaredlogratios: Environment empty")

# NB: Also added some aggregate areas for fleet data
areas <- list(a = 1, b = 2, c = 3, x = 1:2, y = 3)
prey_a <- g3_stock('prey_a', seq(1, 10)) %>% g3s_livesonareas(areas[c('a')])
prey_b <- g3_stock('prey_b', seq(1, 10)) %>% g3s_livesonareas(areas[c('b')])
prey_c <- g3_stock('prey_c', seq(1, 10)) %>% g3s_livesonareas(areas[c('c')])
fleet_abc <- g3_fleet('fleet_abc') %>% g3s_livesonareas(areas[c('a', 'b', 'c')])

# Generate observation data for stock distribution
# NB: No prey_b, only compare prey_a and prey_c
sd_data <- expand.grid(year = 1999:2000, step = c(1, 2), area = c('x', 'y'), stock = c("prey_a", "prey_c"), length = c(1,6))
sd_data$number <- floor(runif(length(sd_data$year), min=100, max=999))

# Generate observation data for catch distribution
cd_data <- expand.grid(year = 1999:2000, step = c(1, 2), area = c('x', 'y'), length = c(1,6))
cd_data$number <- floor(runif(length(cd_data$year), min=100, max=999))

# Generate observation data for catch distribution by biomass
cd_weight_data <- expand.grid(year = 1999:2000, step = c(1, 2), area = c('x', 'y'), length = c(1,6))
cd_weight_data$weight <- floor(runif(length(cd_data$year), min=1000, max=9999))

# Generate observation data for catch distribution (multinomial)
multinomial_data <- expand.grid(year = 1999:2000, step = c(1, 2), length = c(1,6))
multinomial_data$number <- floor(runif(length(multinomial_data$year), min=100, max=999))

surveyindices_data <- expand.grid(year = 1999:2000, step = c(1, 2))
surveyindices_data$number <- floor(runif(length(surveyindices_data$year), min=100, max=999))

# Can't make catchdistribution without fleets
ok(ut_cmp_error(g3l_catchdistribution(
    'utcd',
    cd_data,
    fleets = list(),
    stocks = list(prey_a, prey_b, prey_c),
    area_group = areas,
    g3l_distribution_sumofsquares()), "Fleets must be supplied"), "g3l_catchdistribution: Invalid without fleets")
ok(ut_cmp_error(g3l_abundancedistribution(
    'utcd',
    cd_data,
    fleets = list(fleet_abc),
    stocks = list(prey_a, prey_b, prey_c),
    area_group = areas,
    g3l_distribution_sumofsquares()), "Fleets must not be supplied"), "g3l_abundancedistribution: Invalid with fleets")

# Generate a step that reports the value of (var_name) into separate variable (steps) times
# (initial_val) provides a definition to use to set variable type
report_step <- function (var_name, steps, initial_val) {
    out <- ~{}
    for (i in seq(steps - 1, 0, by = -1)) {
        step_var_name <- paste0("step", i, "_", var_name)
        assign(step_var_name, initial_val)
        out <- gadget3:::f_optimize(gadget3:::f_substitute(~if (cur_time == i) {
            comment(report_comment)
            if (is_nll) {
                # nll is a scalar, and should have attributes stripped
                step_var <- as.numeric(nll)
            } else {
                step_var[] <- var
            }
            REPORT(step_var)
        } else rest, list(
            report_comment = paste0("Reporting on ", var_name, " at step ", i),
            is_nll = var_name == 'nll',
            step_var = as.symbol(step_var_name),
            var = as.symbol(var_name),
            i = i,
            rest = out )))
    }
    return(out)
}

base_actions <- list(
    g3a_time(1999,2000, step_lengths = c(6, 6), project_years = 0),
    g3a_initialconditions(prey_a, ~10 * prey_a__midlen, ~100 * prey_a__midlen),
    g3a_initialconditions(prey_b, ~10 * prey_b__midlen, ~100 * prey_b__midlen),
    g3a_initialconditions(prey_c, ~10 * prey_c__midlen, ~100 * prey_c__midlen),
    g3a_predate_totalfleet(
        fleet_abc,
        list(prey_a, prey_b, prey_c),
        suitabilities = list(
            prey_a = ~g3_param_vector("fleet_abc_a") + 0 * stock__midlen,
            prey_b = ~g3_param_vector("fleet_abc_b") + 0 * stock__midlen,
            prey_c = ~g3_param_vector("fleet_abc_c") + 0 * stock__midlen),
        amount_f = ~g3_param('amount_ab', value = 100) * area),
    named_list(
        # Capture data just before final step erases it
        gadget3:::step_id(10, 'g3l_distribution', 'cdist_sumofsquares_utcd', 1, 'zzzz'), report_step('cdist_sumofsquares_utcd_model__num', 4, array(
            # NB: Lift definition from deparse(r$cdist_sumofsquares_utcd_model__num)
            dim = c(2L, 2L),
            dimnames = list(
                c("1:6", "6:10"),
                c("x", "y")))),
        gadget3:::step_id(10, 'g3l_distribution', 'cdist_sumofsquares_utsd', 1, 'zzzz'), report_step('cdist_sumofsquares_utsd_model__num', 4, array(
            # NB: Lift definition from deparse(r$cdist_sumofsquares_utsd_model__num)
            dim = c(2L, 2L, 2L),
            dimnames = list(
                c("1:6", "6:10"),
                c("prey_a", "prey_c"),
                c("x", "y")))),
        gadget3:::step_id(10, 'g3l_distribution', 'cdist_sumofsquares_utcd_weight', 1, 'zzzz'), report_step('cdist_sumofsquares_utcd_weight_model__wgt', 4, array(
            # NB: Lift definition from deparse(r$cdist_sumofsquares_utcd_weight_model__wgt)
            dim = c(2L, 2L),
            dimnames = list(
                c("1:6", "6:10"),
                c("x", "y")))),
        gadget3:::step_id(10, 'g3l_distribution', 'cdist_multinomial_multinom', 1, 'zzzz'), report_step('cdist_multinomial_multinom_model__num', 4, array(
            # NB: Lift definition from deparse(r$cdist_multinomial_multinom_model__num)
            dim = c(2L),
            dimnames = list(
                c("1:6", "6:10")))),
        gadget3:::step_id(990, 'prey_a__num'), report_step('prey_a__num', 4, g3_stock_instance(prey_a)),
        gadget3:::step_id(990, 'prey_b__num'), report_step('prey_b__num', 4, g3_stock_instance(prey_b)),
        gadget3:::step_id(990, 'prey_c__num'), report_step('prey_c__num', 4, g3_stock_instance(prey_c)),
        gadget3:::step_id(990, 'nll'), report_step('nll', 4, 0.0),
        gadget3:::step_id(999),  ~{
            REPORT(cdist_sumofsquares_utcd_model__num)
            REPORT(cdist_sumofsquares_utcd_obs__num)
            REPORT(cdist_sumofsquares_utcd_weight_model__wgt)
            REPORT(cdist_sumofsquares_utcd_weight_obs__wgt)
            REPORT(cdist_sumofsquares_utsd_model__num)
            REPORT(cdist_sumofsquares_utsd_obs__num)
            REPORT(cdist_multinomial_multinom_model__num)
            REPORT(cdist_multinomial_multinom_obs__num)
            REPORT(prey_a__wgt)
            REPORT(prey_b__wgt)
            REPORT(prey_c__wgt)
            REPORT(prey_a__num)
            REPORT(prey_b__num)
            REPORT(prey_c__num)

            # NB: In theory we could inspect the return value, but TMB doesn't give an easy public method for that
            REPORT(nll)
        }))
actions <- c(base_actions, list(
    g3l_catchdistribution(
        'utcd',
        cd_data,
        list(fleet_abc),
        list(prey_a, prey_b, prey_c),
        area_group = areas,
        g3l_distribution_sumofsquares()),
    g3l_catchdistribution(
        'utcd_weight',
        cd_weight_data,
        list(fleet_abc),
        list(prey_a, prey_b, prey_c),
        area_group = areas,
        g3l_distribution_sumofsquares()),
    g3l_catchdistribution(
        'utsd',
        sd_data,
        list(fleet_abc),
        list(prey_a, prey_b, prey_c),
        area_group = areas,
        g3l_distribution_sumofsquares()),
    g3l_catchdistribution(
        'multinom',
        multinomial_data,
        list(fleet_abc),
        list(prey_a),
        area_group = areas,
        g3l_distribution_multinomial()),
    g3l_abundancedistribution(
        'surveyindices',
        surveyindices_data,
        fleets = list(),
        stocks = list(prey_b),
        area_group = areas,
        report = TRUE,  # NB: Using built-in reporting vs. version hacked in tests
        g3l_distribution_surveyindices_log(alpha = ~g3_param("si_alpha", value = 0), beta = ~g3_param("si_beta", value = 0))),
    NULL))
actions <- c(actions, list(
    # NB: Don't use _detail, since it doesn't play well with report_step()
    #     Ideally we'd remove report_step, but we have to time it exactly between data-collect and reset
    g3a_report_history(actions, '__cons$', out_prefix = "detail_") ))

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    params <- attr(model_fn, 'parameter_template')
    params$fleet_abc_a <- c(0, 0, 0, 0.1, 0.2, 0.1, 0, 0, 0, 0)
    params$fleet_abc_b <- c(0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0, 0)
    params$fleet_abc_c <- c(0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.1, 0)

    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
    model_cpp <- c()
}

ok_group("Likelihood per step", {
    params <- attr(model_fn, 'parameter_template')
    # Randomly catch, but get something everywhere
    params$fleet_abc_a <- runif(10, min=0.1, max=0.9)
    params$fleet_abc_b <- runif(10, min=0.1, max=0.9)
    params$fleet_abc_c <- runif(10, min=0.1, max=0.9)
    params$amount_ab <- 1000000
    params$si_alpha <- 4
    params$si_beta <- 2
    params$cdist_sumofsquares_utcd_weight <- 1
    params$cdist_sumofsquares_utcd_weight_weight <- 1
    params$cdist_sumofsquares_utsd_weight <- 1
    params$cdist_multinomial_multinom_weight <- 1
    params$adist_surveyindices_log_surveyindices_weight <- 1

    result <- model_fn(params)
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_equal(
        sort(as.vector(r$cdist_sumofsquares_utsd_obs__num)),
        sort(sd_data$number)), "cdist_sumofsquares_utsd_obs__num: Imported from data.frame, order not necessarily the same")

    ok(ut_cmp_equal(
        r$adist_surveyindices_log_surveyindices_model__params,
        c(params$si_alpha, params$si_beta)), "adist_surveyindices_log_surveyindices_model__params: Reported our hard-coded linear regression parameters")

    ######## cdist_sumofsquares_utsd_model__num
    ok(ut_cmp_equal(as.vector(r$step0_cdist_sumofsquares_utsd_model__num[,'prey_a', 1]), c(
        sum(r$detail_prey_a_fleet_abc__cons[1:5,,1] / r$prey_a__wgt[1:5,]),
        sum(r$detail_prey_a_fleet_abc__cons[6:10,,1] / r$prey_a__wgt[6:10,]))), "step0_cdist_sumofsquares_utsd_model__num[,'prey_a',1]: prey_a in area 1")
    ok(ut_cmp_equal(as.vector(r$step0_cdist_sumofsquares_utsd_model__num[,'prey_c', 1]), c(
        0,
        0)), "step0_cdist_sumofsquares_utsd_model__num[,'prey_c',1]: No prey_c in area 1")
    ok(ut_cmp_equal(as.vector(r$step0_cdist_sumofsquares_utsd_model__num[,'prey_a', 2]), c(
        0,
        0)), "step0_cdist_sumofsquares_utsd_model__num[,'prey_a',2]: No prey_a in area 2")
    ok(ut_cmp_equal(as.vector(r$step0_cdist_sumofsquares_utsd_model__num[,'prey_c', 2]), c(
        sum(r$detail_prey_c_fleet_abc__cons[1:5,,1] / r$prey_c__wgt[1:5,]),
        sum(r$detail_prey_c_fleet_abc__cons[6:10,,1] / r$prey_c__wgt[6:10,]))), "step0_cdist_sumofsquares_utsd_model__num[,'prey_c',2]: prey_c in area 2")

    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utsd_model__num[,'prey_a', 1]), c(
        sum(r$detail_prey_a_fleet_abc__cons[1:5,,2] / r$prey_a__wgt[1:5,]),
        sum(r$detail_prey_a_fleet_abc__cons[6:10,,2] / r$prey_a__wgt[6:10,]))), "step1_cdist_sumofsquares_utsd_model__num[,'prey_a',1]: prey_a in area 1")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utsd_model__num[,'prey_c', 1]), c(
        0,
        0)), "step1_cdist_sumofsquares_utsd_model__num[,'prey_c',1]: No prey_c in area 1")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utsd_model__num[,'prey_a', 2]), c(
        0,
        0)), "step1_cdist_sumofsquares_utsd_model__num[,'prey_a',2]: No prey_a in area 2")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utsd_model__num[,'prey_c', 2]), c(
        sum(r$detail_prey_c_fleet_abc__cons[1:5,,2] / r$prey_c__wgt[1:5,]),
        sum(r$detail_prey_c_fleet_abc__cons[6:10,,2] / r$prey_c__wgt[6:10,]))), "step1_cdist_sumofsquares_utsd_model__num[,'prey_c',2]: prey_c in area 2")

    ok(ut_cmp_equal(as.vector(r$step2_cdist_sumofsquares_utsd_model__num[,'prey_a', 1]), c(
        sum(r$detail_prey_a_fleet_abc__cons[1:5,,3] / r$prey_a__wgt[1:5,]),
        sum(r$detail_prey_a_fleet_abc__cons[6:10,,3] / r$prey_a__wgt[6:10,]))), "step2_cdist_sumofsquares_utsd_model__num[,'prey_a',1]: prey_a in area 1")
    ok(ut_cmp_equal(as.vector(r$step2_cdist_sumofsquares_utsd_model__num[,'prey_c', 1]), c(
        0,
        0)), "step2_cdist_sumofsquares_utsd_model__num[,'prey_c',1]: No prey_c in area 1")
    ok(ut_cmp_equal(as.vector(r$step2_cdist_sumofsquares_utsd_model__num[,'prey_a', 2]), c(
        0,
        0)), "step2_cdist_sumofsquares_utsd_model__num[,'prey_a',2]: No prey_a in area 2")
    ok(ut_cmp_equal(as.vector(r$step2_cdist_sumofsquares_utsd_model__num[,'prey_c', 2]), c(
        sum(r$detail_prey_c_fleet_abc__cons[1:5,,3] / r$prey_c__wgt[1:5,]),
        sum(r$detail_prey_c_fleet_abc__cons[6:10,,3] / r$prey_c__wgt[6:10,]))), "step2_cdist_sumofsquares_utsd_model__num[,'prey_c',2]: prey_c in area 2")

    ok(ut_cmp_equal(as.vector(r$step3_cdist_sumofsquares_utsd_model__num[,'prey_a', 1]), c(
        sum(r$detail_prey_a_fleet_abc__cons[1:5,,4] / r$prey_a__wgt[1:5,]),
        sum(r$detail_prey_a_fleet_abc__cons[6:10,,4] / r$prey_a__wgt[6:10,]))), "step3_cdist_sumofsquares_utsd_model__num[,'prey_a',1]: prey_a in area 1")
    ok(ut_cmp_equal(as.vector(r$step3_cdist_sumofsquares_utsd_model__num[,'prey_c', 1]), c(
        0,
        0)), "step3_cdist_sumofsquares_utsd_model__num[,'prey_c',1]: No prey_c in area 1")
    ok(ut_cmp_equal(as.vector(r$step3_cdist_sumofsquares_utsd_model__num[,'prey_a', 2]), c(
        0,
        0)), "step3_cdist_sumofsquares_utsd_model__num[,'prey_a',2]: No prey_a in area 2")
    ok(ut_cmp_equal(as.vector(r$step3_cdist_sumofsquares_utsd_model__num[,'prey_c', 2]), c(
        sum(r$detail_prey_c_fleet_abc__cons[1:5,,4] / r$prey_c__wgt[1:5,]),
        sum(r$detail_prey_c_fleet_abc__cons[6:10,,4] / r$prey_c__wgt[6:10,]))), "step3_cdist_sumofsquares_utsd_model__num[,'prey_c',2]: prey_c in area 2")
    ########

    ######## cdist_sumofsquares_utcd_model__num
    ok(ut_cmp_equal(as.vector(r$step0_cdist_sumofsquares_utcd_model__num[,1]), c(
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[1:5,1,1] / r$prey_a__wgt[1:5,1]),
             sum(r$detail_prey_b_fleet_abc__cons[1:5,1,1] / r$prey_b__wgt[1:5,1])),
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[6:10,1,1] / r$prey_a__wgt[6:10,1]),
             sum(r$detail_prey_b_fleet_abc__cons[6:10,1,1] / r$prey_b__wgt[6:10,1])),
         NULL)), "step0_cdist_sumofsquares_utsd_model__num[,1]: all prey in area a/b")
    ok(ut_cmp_equal(as.vector(r$step0_cdist_sumofsquares_utcd_model__num[,2]), c(
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[1:5,1,1] / r$prey_c__wgt[1:5,1])),
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[6:10,1,1] / r$prey_c__wgt[6:10,1])),
         NULL)), "step0_cdist_sumofsquares_utsd_model__num[,2]: all prey in area c")

    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utcd_model__num[,1]), c(
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[1:5,1,2] / r$prey_a__wgt[1:5,1]),
             sum(r$detail_prey_b_fleet_abc__cons[1:5,1,2] / r$prey_b__wgt[1:5,1])),
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[6:10,1,2] / r$prey_a__wgt[6:10,1]),
             sum(r$detail_prey_b_fleet_abc__cons[6:10,1,2] / r$prey_b__wgt[6:10,1])),
         NULL)), "step1_cdist_sumofsquares_utsd_model__num[,1]: all prey in area a/b")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utcd_model__num[,2]), c(
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[1:5,1,2] / r$prey_c__wgt[1:5,1])),
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[6:10,1,2] / r$prey_c__wgt[6:10,1])),
         NULL)), "step1_cdist_sumofsquares_utsd_model__num[,2]: all prey in area c")

    ok(ut_cmp_equal(as.vector(r$step2_cdist_sumofsquares_utcd_model__num[,1]), c(
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[1:5,1,3] / r$prey_a__wgt[1:5,1]),
             sum(r$detail_prey_b_fleet_abc__cons[1:5,1,3] / r$prey_b__wgt[1:5,1])),
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[6:10,1,3] / r$prey_a__wgt[6:10,1]),
             sum(r$detail_prey_b_fleet_abc__cons[6:10,1,3] / r$prey_b__wgt[6:10,1])),
         NULL)), "step2_cdist_sumofsquares_utsd_model__num[,1]: all prey in area a/b")
    ok(ut_cmp_equal(as.vector(r$step2_cdist_sumofsquares_utcd_model__num[,2]), c(
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[1:5,1,3] / r$prey_c__wgt[1:5,1])),
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[6:10,1,3] / r$prey_c__wgt[6:10,1])),
         NULL)), "step2_cdist_sumofsquares_utsd_model__num[,2]: all prey in area c")

    ok(ut_cmp_equal(as.vector(r$step3_cdist_sumofsquares_utcd_model__num[,1]), c(
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[1:5,1,4] / r$prey_a__wgt[1:5,1]),
             sum(r$detail_prey_b_fleet_abc__cons[1:5,1,4] / r$prey_b__wgt[1:5,1])),
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[6:10,1,4] / r$prey_a__wgt[6:10,1]),
             sum(r$detail_prey_b_fleet_abc__cons[6:10,1,4] / r$prey_b__wgt[6:10,1])),
         NULL)), "step3_cdist_sumofsquares_utsd_model__num[,1]: all prey in area a/b")
    ok(ut_cmp_equal(as.vector(r$step3_cdist_sumofsquares_utcd_model__num[,2]), c(
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[1:5,1,4] / r$prey_c__wgt[1:5,1])),
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[6:10,1,4] / r$prey_c__wgt[6:10,1])),
         NULL)), "step3_cdist_sumofsquares_utsd_model__num[,2]: all prey in area c")
    ########

    ######## adist_surveyindices_log_surveyindices_model__num
    ok(ut_cmp_equal(
        as.vector(r$adist_surveyindices_log_surveyindices_model__num),
        c(
            sum(r$step0_prey_b__num),
            sum(r$step1_prey_b__num),
            sum(r$step2_prey_b__num),
            sum(r$step3_prey_b__num),
            NULL)), "adist_surveyindices_log_surveyindices_model__num: Built-in reporting gave us step 0..3 abundance")
    ########

    ok(ut_cmp_equal(r$step0_nll, sum(
        # utsd: stock 1 / area 1
        (r$step0_cdist_sumofsquares_utsd_model__num[,1,1] / sum(r$step0_cdist_sumofsquares_utsd_model__num[,,1]) -
            r$cdist_sumofsquares_utsd_obs__num[,1,1,1] / sum(r$cdist_sumofsquares_utsd_obs__num[,,1,1])) ** 2,
        # utsd: stock 2 / area 1
        (r$step0_cdist_sumofsquares_utsd_model__num[,2,1] / sum(r$step0_cdist_sumofsquares_utsd_model__num[,,1]) -
            r$cdist_sumofsquares_utsd_obs__num[,2,1,1] / sum(r$cdist_sumofsquares_utsd_obs__num[,,1,1])) ** 2,
        # utsd: stock 1 / area 2
        (r$step0_cdist_sumofsquares_utsd_model__num[,1,2] / sum(r$step0_cdist_sumofsquares_utsd_model__num[,,2]) -
            r$cdist_sumofsquares_utsd_obs__num[,1,1,2] / sum(r$cdist_sumofsquares_utsd_obs__num[,,1,2])) ** 2,
        # utsd: stock 2 / area 2
        (r$step0_cdist_sumofsquares_utsd_model__num[,2,2] / sum(r$step0_cdist_sumofsquares_utsd_model__num[,,2]) -
            r$cdist_sumofsquares_utsd_obs__num[,2,1,2] / sum(r$cdist_sumofsquares_utsd_obs__num[,,1,2])) ** 2,
        # utcd: area 1
        (r$step0_cdist_sumofsquares_utcd_model__num[,1] / sum(r$step0_cdist_sumofsquares_utcd_model__num[,1]) -
            r$cdist_sumofsquares_utcd_obs__num[,1,1] / sum(r$cdist_sumofsquares_utcd_obs__num[,1,1])) ** 2,
        # utcd: area 2
        (r$step0_cdist_sumofsquares_utcd_model__num[,2] / sum(r$step0_cdist_sumofsquares_utcd_model__num[,2]) -
            r$cdist_sumofsquares_utcd_obs__num[,1,2] / sum(r$cdist_sumofsquares_utcd_obs__num[,1,2])) ** 2,
        # utcd_weight: area 1
        (r$step0_cdist_sumofsquares_utcd_weight_model__wgt[,1] / g3_avoid_zero(sum(r$step0_cdist_sumofsquares_utcd_weight_model__wgt[,1])) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,1,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,1,1]))) ** 2,
        # utcd_weight: area 2
        (r$step0_cdist_sumofsquares_utcd_weight_model__wgt[,2] / g3_avoid_zero(sum(r$step0_cdist_sumofsquares_utcd_weight_model__wgt[,2])) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,1,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,1,2]))) ** 2,
        # multinom:
        (2 * (-sum(r$cdist_multinomial_multinom_obs__num[,1] * log(g3_logspace_add_vec(r$step0_cdist_multinomial_multinom_model__num/g3_avoid_zero(sum(r$step0_cdist_multinomial_multinom_model__num)) *
            10000, (1/(length(r$cdist_multinomial_multinom_obs__num[,1]) * 10)) * 10000)/10000)) +
                (sum(g3_lgamma_vec(1 + r$cdist_multinomial_multinom_obs__num[,1])) - lgamma(1 +
                        sum(r$cdist_multinomial_multinom_obs__num[,1]))))),
        # surveyindices:
        0,  # NB: We don't calculate until end
        0)), "step0_nll: Sum of squares")

    ok(ut_cmp_equal(r$step1_nll, sum(
        # utsd: stock 1 / area 1
        (r$step1_cdist_sumofsquares_utsd_model__num[,1,1] / g3_avoid_zero(sum(r$step1_cdist_sumofsquares_utsd_model__num[,,1])) -
            r$cdist_sumofsquares_utsd_obs__num[,1,2,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,2,1]))) ** 2,
        # utsd: stock 2 / area 1
        (r$step1_cdist_sumofsquares_utsd_model__num[,2,1] / g3_avoid_zero(sum(r$step1_cdist_sumofsquares_utsd_model__num[,,1])) -
            r$cdist_sumofsquares_utsd_obs__num[,2,2,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,2,1]))) ** 2,
        # utsd: stock 1 / area 2
        (r$step1_cdist_sumofsquares_utsd_model__num[,1,2] / g3_avoid_zero(sum(r$step1_cdist_sumofsquares_utsd_model__num[,,2])) -
            r$cdist_sumofsquares_utsd_obs__num[,1,2,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,2,2]))) ** 2,
        # utsd: stock 2 / area 2
        (r$step1_cdist_sumofsquares_utsd_model__num[,2,2] / g3_avoid_zero(sum(r$step1_cdist_sumofsquares_utsd_model__num[,,2])) -
            r$cdist_sumofsquares_utsd_obs__num[,2,2,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,2,2]))) ** 2,
        # utcd: area 1
        (r$step1_cdist_sumofsquares_utcd_model__num[,1] / g3_avoid_zero(sum(r$step1_cdist_sumofsquares_utcd_model__num[,1])) -
            r$cdist_sumofsquares_utcd_obs__num[,2,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_obs__num[,2,1]))) ** 2,
        # utcd: area 2
        (r$step1_cdist_sumofsquares_utcd_model__num[,2] / g3_avoid_zero(sum(r$step1_cdist_sumofsquares_utcd_model__num[,2])) -
            r$cdist_sumofsquares_utcd_obs__num[,2,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_obs__num[,2,2]))) ** 2,
        # utcd_weight: area 1
        (r$step1_cdist_sumofsquares_utcd_weight_model__wgt[,1] / g3_avoid_zero(sum(r$step1_cdist_sumofsquares_utcd_weight_model__wgt[,1])) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,2,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,2,1]))) ** 2,
        # utcd_weight: area 2
        (r$step1_cdist_sumofsquares_utcd_weight_model__wgt[,2] / g3_avoid_zero(sum(r$step1_cdist_sumofsquares_utcd_weight_model__wgt[,2])) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,2,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,2,2]))) ** 2,
        # multinom:
        (2 * (-sum(r$cdist_multinomial_multinom_obs__num[,2] * log(g3_logspace_add_vec(r$step1_cdist_multinomial_multinom_model__num/g3_avoid_zero(sum(r$step1_cdist_multinomial_multinom_model__num)) *
            10000, (1/(length(r$cdist_multinomial_multinom_obs__num[,2]) * 10)) * 10000)/10000)) +
                (sum(g3_lgamma_vec(1 + r$cdist_multinomial_multinom_obs__num[,2])) - lgamma(1 +
                        sum(r$cdist_multinomial_multinom_obs__num[,2]))))),
        # surveyindices:
        0,  # NB: We don't calculate until end
        r$step0_nll)), "step1_nll: Sum of squares, including step0_nll")

    ok(ut_cmp_equal(r$step2_nll, sum(
        # utsd: stock 1 / area 1
        (r$step2_cdist_sumofsquares_utsd_model__num[,1,1] / g3_avoid_zero(sum(r$step2_cdist_sumofsquares_utsd_model__num[,,1])) -
            r$cdist_sumofsquares_utsd_obs__num[,1,3,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,3,1]))) ** 2,
        # utsd: stock 2 / area 1
        (r$step2_cdist_sumofsquares_utsd_model__num[,2,1] / g3_avoid_zero(sum(r$step2_cdist_sumofsquares_utsd_model__num[,,1])) -
            r$cdist_sumofsquares_utsd_obs__num[,2,3,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,3,1]))) ** 2,
        # utsd: stock 1 / area 2
        (r$step2_cdist_sumofsquares_utsd_model__num[,1,2] / g3_avoid_zero(sum(r$step2_cdist_sumofsquares_utsd_model__num[,,2])) -
            r$cdist_sumofsquares_utsd_obs__num[,1,3,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,3,2]))) ** 2,
        # utsd: stock 2 / area 2
        (r$step2_cdist_sumofsquares_utsd_model__num[,2,2] / g3_avoid_zero(sum(r$step2_cdist_sumofsquares_utsd_model__num[,,2])) -
            r$cdist_sumofsquares_utsd_obs__num[,2,3,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,3,2]))) ** 2,
        # utcd: area 1
        (r$step2_cdist_sumofsquares_utcd_model__num[,1] / g3_avoid_zero(sum(r$step2_cdist_sumofsquares_utcd_model__num[,1])) -
            r$cdist_sumofsquares_utcd_obs__num[,3,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_obs__num[,3,1]))) ** 2,
        # utcd: area 2
        (r$step2_cdist_sumofsquares_utcd_model__num[,2] / g3_avoid_zero(sum(r$step2_cdist_sumofsquares_utcd_model__num[,2])) -
            r$cdist_sumofsquares_utcd_obs__num[,3,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_obs__num[,3,2]))) ** 2,
        # utcd_weight: area 1
        (r$step2_cdist_sumofsquares_utcd_weight_model__wgt[,1] / g3_avoid_zero(sum(r$step2_cdist_sumofsquares_utcd_weight_model__wgt[,1])) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,3,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,3,1]))) ** 2,
        # utcd_weight: area 2
        (r$step2_cdist_sumofsquares_utcd_weight_model__wgt[,2] / g3_avoid_zero(sum(r$step2_cdist_sumofsquares_utcd_weight_model__wgt[,2])) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,3,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,3,2]))) ** 2,
        # multinom:
        (2 * (-sum(r$cdist_multinomial_multinom_obs__num[,3] * log(g3_logspace_add_vec(r$step2_cdist_multinomial_multinom_model__num/g3_avoid_zero(sum(r$step2_cdist_multinomial_multinom_model__num)) *
            10000, (1/(length(r$cdist_multinomial_multinom_obs__num[,3]) * 10)) * 10000)/10000)) +
                (sum(g3_lgamma_vec(1 + r$cdist_multinomial_multinom_obs__num[,3])) - lgamma(1 +
                        sum(r$cdist_multinomial_multinom_obs__num[,3]))))),
        # surveyindices:
        0,  # NB: We don't calculate until end
        r$step1_nll)), "step2_nll: Sum of squares, including step1_nll")

    ok(ut_cmp_equal(r$step3_nll, sum(
        # utsd: stock 1 / area 1
        (r$step3_cdist_sumofsquares_utsd_model__num[,1,1] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utsd_model__num[,,1])) -
            r$cdist_sumofsquares_utsd_obs__num[,1,4,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,4,1]))) ** 2,
        # utsd: stock 2 / area 1
        (r$step3_cdist_sumofsquares_utsd_model__num[,2,1] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utsd_model__num[,,1])) -
            r$cdist_sumofsquares_utsd_obs__num[,2,4,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,4,1]))) ** 2,
        # utsd: stock 1 / area 2
        (r$step3_cdist_sumofsquares_utsd_model__num[,1,2] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utsd_model__num[,,2])) -
            r$cdist_sumofsquares_utsd_obs__num[,1,4,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,4,2]))) ** 2,
        # utsd: stock 2 / area 2
        (r$step3_cdist_sumofsquares_utsd_model__num[,2,2] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utsd_model__num[,,2])) -
            r$cdist_sumofsquares_utsd_obs__num[,2,4,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,4,2]))) ** 2,
        # utcd: area 1
        (r$step3_cdist_sumofsquares_utcd_model__num[,1] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utcd_model__num[,1])) -
            r$cdist_sumofsquares_utcd_obs__num[,4,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_obs__num[,4,1]))) ** 2,
        # utcd: area 2
        (r$step3_cdist_sumofsquares_utcd_model__num[,2] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utcd_model__num[,2])) -
            r$cdist_sumofsquares_utcd_obs__num[,4,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_obs__num[,4,2]))) ** 2,
        # utcd_weight: area 1
        (r$step3_cdist_sumofsquares_utcd_weight_model__wgt[,1] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utcd_weight_model__wgt[,1])) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,4,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,4,1]))) ** 2,
        # utcd_weight: area 2
        (r$step3_cdist_sumofsquares_utcd_weight_model__wgt[,2] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utcd_weight_model__wgt[,2])) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,4,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,4,2]))) ** 2,
        # multinom:
        (2 * (-sum(r$cdist_multinomial_multinom_obs__num[,4] * log(g3_logspace_add_vec(r$step3_cdist_multinomial_multinom_model__num/g3_avoid_zero(sum(r$step3_cdist_multinomial_multinom_model__num)) *
            10000, (1/(length(r$cdist_multinomial_multinom_obs__num[,4]) * 10)) * 10000)/10000)) +
                (sum(g3_lgamma_vec(1 + r$cdist_multinomial_multinom_obs__num[,4])) - lgamma(1 +
                        sum(r$cdist_multinomial_multinom_obs__num[,4]))))),
        # surveyindices:
        sum((params$si_alpha +
            params$si_beta * log(g3_avoid_zero(r$adist_surveyindices_log_surveyindices_model__num[,])) -
            log(g3_avoid_zero(r$adist_surveyindices_log_surveyindices_obs__num[,])))**2),
        r$step2_nll)), "step3_nll: Sum of squares, including step2_nll")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
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
    cd_weight_data <- expand.grid(year = 1999:2000, area = c('x', 'y'), length = c(1,6))
    cd_weight_data$weight <- floor(runif(length(cd_data$year), min=1000, max=9999))

    # Change model to aggregate over a year
    # NB: No step column now
    multinomial_data <- expand.grid(year = 1999:2000, length = c(1,6))
    multinomial_data$number <- floor(runif(length(multinomial_data$year), min=100, max=999))

    # NB: No step column now
    surveyindices_data <- expand.grid(year = 1999:2000)
    surveyindices_data$number <- floor(runif(length(surveyindices_data$year), min=100, max=999))

    actions <- c(base_actions, list(
        g3l_catchdistribution(
            'utcd',
            cd_data,
            list(fleet_abc),
            list(prey_a, prey_b, prey_c),
            area_group = areas,
            g3l_distribution_sumofsquares()),
        g3l_catchdistribution(
            'utcd_weight',
            cd_weight_data,
            list(fleet_abc),
            list(prey_a, prey_b, prey_c),
            area_group = areas,
            g3l_distribution_sumofsquares()),
        g3l_catchdistribution(
            'utsd',
            sd_data,
            list(fleet_abc),
            list(prey_a, prey_b, prey_c),
            area_group = areas,
            g3l_distribution_sumofsquares()),
        g3l_catchdistribution(
            'multinom',
            multinomial_data,
            list(fleet_abc),
            list(prey_a),
            area_group = areas,
            g3l_distribution_multinomial()),
        g3l_abundancedistribution(
            'surveyindices',
            surveyindices_data,
            fleets = list(),
            stocks = list(prey_b),
            area_group = areas,
            report = TRUE,
            g3l_distribution_surveyindices_log(alpha = ~g3_param("si_alpha", value = 0), beta = ~g3_param("si_beta", value = 0)))))
    actions <- c(actions, list(
        g3a_report_history(actions, '__cons$', out_prefix = "detail_") ))

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

    params <- attr(model_fn, 'parameter_template')
    params$fleet_abc_a <- runif(10, min=0.1, max=0.9)
    params$fleet_abc_b <- runif(10, min=0.1, max=0.9)
    params$fleet_abc_c <- runif(10, min=0.1, max=0.9)
    params$amount_ab <- 1000000
    params$si_alpha <- 1.82
    params$si_beta <- 3.74
    params$cdist_sumofsquares_utcd_weight <- 1
    params$cdist_sumofsquares_utcd_weight_weight <- 1
    params$cdist_sumofsquares_utsd_weight <- 1
    params$cdist_multinomial_multinom_weight <- 1
    params$adist_surveyindices_log_surveyindices_weight <- 1

    result <- model_fn(params)
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)

    ok(ut_cmp_equal(
        sort(as.vector(r$cdist_sumofsquares_utsd_obs__num)),
        sort(sd_data$number)), "cdist_sumofsquares_utsd_obs__num: Imported from data.frame, order not necessarily the same")

    ok(ut_cmp_equal(
        r$adist_surveyindices_log_surveyindices_model__params,
        c(params$si_alpha, params$si_beta)), "adist_surveyindices_log_surveyindices_model__params: Reported our hard-coded linear regression parameters")


    ######## cdist_sumofsquares_utsd_model__num
    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utsd_model__num[,'prey_a', 1]), c(
        sum(
            (r$detail_prey_a_fleet_abc__cons[1:5,,1] / r$prey_a__wgt[1:5,]),
            (r$detail_prey_a_fleet_abc__cons[1:5,,2] / r$prey_a__wgt[1:5,])),
        sum(
            sum(r$detail_prey_a_fleet_abc__cons[6:10,,1] / r$prey_a__wgt[6:10,]),
            sum(r$detail_prey_a_fleet_abc__cons[6:10,,2] / r$prey_a__wgt[6:10,])),
        NULL)), "step1_cdist_sumofsquares_utsd_model__num[,'prey_a',1]: prey_a summed over year")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utsd_model__num[,'prey_c', 1]), c(
        0,
        0,
        NULL)), "step1_cdist_sumofsquares_utsd_model__num[,'prey_c',1]: No prey_c in first area")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utsd_model__num[,'prey_a', 2]), c(
        0,
        0,
        NULL)), "step1_cdist_sumofsquares_utsd_model__num[,'prey_a',2]: No prey_a in second area")
    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utsd_model__num[,'prey_c', 2]), c(
        sum(
            (r$detail_prey_c_fleet_abc__cons[1:5,,1] / r$prey_c__wgt[1:5,]),
            (r$detail_prey_c_fleet_abc__cons[1:5,,2] / r$prey_c__wgt[1:5,])),
        sum(
            sum(r$detail_prey_c_fleet_abc__cons[6:10,,1] / r$prey_c__wgt[6:10,]),
            sum(r$detail_prey_c_fleet_abc__cons[6:10,,2] / r$prey_c__wgt[6:10,])),
        NULL)), "step1_cdist_sumofsquares_utsd_model__num[,'prey_c',2]: prey_c summed over year")
    ########

    ######## cdist_sumofsquares_utcd_model__num
    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utcd_model__num[,1]), c(
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[1:5,1,1] / r$prey_a__wgt[1:5,1]),
             sum(r$detail_prey_b_fleet_abc__cons[1:5,1,1] / r$prey_b__wgt[1:5,1]),
             sum(r$detail_prey_a_fleet_abc__cons[1:5,1,2] / r$prey_a__wgt[1:5,1]),
             sum(r$detail_prey_b_fleet_abc__cons[1:5,1,2] / r$prey_b__wgt[1:5,1])),
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[6:10,1,1] / r$prey_a__wgt[6:10,1]),
             sum(r$detail_prey_b_fleet_abc__cons[6:10,1,1] / r$prey_b__wgt[6:10,1]),
             sum(r$detail_prey_a_fleet_abc__cons[6:10,1,2] / r$prey_a__wgt[6:10,1]),
             sum(r$detail_prey_b_fleet_abc__cons[6:10,1,2] / r$prey_b__wgt[6:10,1])),
         NULL)), "step1_cdist_sumofsquares_utcd_model__num[,1]: all prey from a/b and steps 0 & 1")

    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utcd_model__num[,2]), c(
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[1:5,1,1] / r$prey_c__wgt[1:5,1]),
             sum(r$detail_prey_c_fleet_abc__cons[1:5,1,2] / r$prey_c__wgt[1:5,1])),
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[6:10,1,1] / r$prey_c__wgt[6:10,1]),
             sum(r$detail_prey_c_fleet_abc__cons[6:10,1,2] / r$prey_c__wgt[6:10,1])),
         NULL)), "step1_cdist_sumofsquares_utcd_model__num[,2]: all prey from c and steps 0/1")
    ########

    ######## cdist_sumofsquares_utcd_weight_model__wgt
    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utcd_weight_model__wgt[,1]), c(
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[1:5,1,1]),
             sum(r$detail_prey_b_fleet_abc__cons[1:5,1,1]),
             sum(r$detail_prey_a_fleet_abc__cons[1:5,1,2]),
             sum(r$detail_prey_b_fleet_abc__cons[1:5,1,2])),
         sum(
             sum(r$detail_prey_a_fleet_abc__cons[6:10,1,1]),
             sum(r$detail_prey_b_fleet_abc__cons[6:10,1,1]),
             sum(r$detail_prey_a_fleet_abc__cons[6:10,1,2]),
             sum(r$detail_prey_b_fleet_abc__cons[6:10,1,2])),
         NULL)), "step1_cdist_sumofsquares_utcd_weight_model__wgt[,1]: total biomass of prey from a/b and steps 0 & 1")

    ok(ut_cmp_equal(as.vector(r$step1_cdist_sumofsquares_utcd_weight_model__wgt[,2]), c(
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[1:5,1,1]),
             sum(r$detail_prey_c_fleet_abc__cons[1:5,1,2])),
         sum(
             sum(r$detail_prey_c_fleet_abc__cons[6:10,1,1]),
             sum(r$detail_prey_c_fleet_abc__cons[6:10,1,2])),
         NULL)), "step1_cdist_sumofsquares_utcd_weight_model__wgt[,2]: total biomass of prey from c and steps 0/1")
    ########

    ######## adist_surveyindices_surveyindices_model__num
    ok(ut_cmp_equal(
        as.vector(r$adist_surveyindices_log_surveyindices_model__num),
        c(
            sum(r$step0_prey_b__num) + sum(r$step1_prey_b__num),
            sum(r$step2_prey_b__num) + sum(r$step3_prey_b__num),
            NULL)), "adist_surveyindices_log_surveyindices_model__num: Built-in reporting gave us step 0..3 abundance")
    ########

    ok(ut_cmp_equal(r$step0_nll, 0), "step0_nll: nll not calculated yet")

    ok(ut_cmp_equal(r$step1_nll, sum(
        # utsd: stock 1 / area 1
        (r$step1_cdist_sumofsquares_utsd_model__num[,1,1] / sum(r$step1_cdist_sumofsquares_utsd_model__num[,,1]) -
            # NB: Still using first time data, unlike per-step example
            r$cdist_sumofsquares_utsd_obs__num[,1,1,1] / sum(r$cdist_sumofsquares_utsd_obs__num[,,1,1])) ** 2,
        # utsd: stock 2 / area 1
        (r$step1_cdist_sumofsquares_utsd_model__num[,2,1] / sum(r$step1_cdist_sumofsquares_utsd_model__num[,,1]) -
            r$cdist_sumofsquares_utsd_obs__num[,2,1,1] / sum(r$cdist_sumofsquares_utsd_obs__num[,,1,1])) ** 2,
        # utsd: stock 1 / area 2
        (r$step1_cdist_sumofsquares_utsd_model__num[,1,2] / sum(r$step1_cdist_sumofsquares_utsd_model__num[,,2]) -
            r$cdist_sumofsquares_utsd_obs__num[,1,1,2] / sum(r$cdist_sumofsquares_utsd_obs__num[,,1,2])) ** 2,
        # utsd: stock 2 / area 2
        (r$step1_cdist_sumofsquares_utsd_model__num[,2,2] / sum(r$step1_cdist_sumofsquares_utsd_model__num[,,2]) -
            r$cdist_sumofsquares_utsd_obs__num[,2,1,2] / sum(r$cdist_sumofsquares_utsd_obs__num[,,1,2])) ** 2,
        # utcd: area 1
        (r$step1_cdist_sumofsquares_utcd_model__num[,1] / sum(r$step1_cdist_sumofsquares_utcd_model__num[,1]) -
            r$cdist_sumofsquares_utcd_obs__num[,1,1] / sum(r$cdist_sumofsquares_utcd_obs__num[,1,1])) ** 2,
        # utcd: area 2
        (r$step1_cdist_sumofsquares_utcd_model__num[,2] / sum(r$step1_cdist_sumofsquares_utcd_model__num[,2]) -
            r$cdist_sumofsquares_utcd_obs__num[,1,2] / sum(r$cdist_sumofsquares_utcd_obs__num[,1,2])) ** 2,
        # utcd_weight: area 1
        (r$step1_cdist_sumofsquares_utcd_weight_model__wgt[,1] / sum(r$step1_cdist_sumofsquares_utcd_weight_model__wgt[,1]) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,1,1] / sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,1,1])) ** 2,
        # utcd_weight: area 2
        (r$step1_cdist_sumofsquares_utcd_weight_model__wgt[,2] / sum(r$step1_cdist_sumofsquares_utcd_weight_model__wgt[,2]) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,1,2] / sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,1,2])) ** 2,
        # multinom:
        (2 * (-sum(r$cdist_multinomial_multinom_obs__num[,1] * log(g3_logspace_add_vec(r$step1_cdist_multinomial_multinom_model__num/g3_avoid_zero(sum(r$step1_cdist_multinomial_multinom_model__num)) *
            10000, (1/(length(r$cdist_multinomial_multinom_obs__num[,1]) * 10)) * 10000)/10000)) +
                (sum(g3_lgamma_vec(1 + r$cdist_multinomial_multinom_obs__num[,1])) - lgamma(1 +
                        sum(r$cdist_multinomial_multinom_obs__num[,1]))))),
        # surveyindices:
        0,  # NB: We don't calculate until end
        0)), "step1_nll: Sum of squares")

    ok(ut_cmp_equal(r$step3_nll, sum(
        # utsd: stock 1 / area 1
        (r$step3_cdist_sumofsquares_utsd_model__num[,1,1] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utsd_model__num[,,1])) -
            # NB: Second time data, not 4th as in per-step example
            r$cdist_sumofsquares_utsd_obs__num[,1,2,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,2,1]))) ** 2,
        # utsd: stock 2 / area 1
        (r$step3_cdist_sumofsquares_utsd_model__num[,2,1] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utsd_model__num[,,1])) -
            r$cdist_sumofsquares_utsd_obs__num[,2,2,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,2,1]))) ** 2,
        # utsd: stock 1 / area 2
        (r$step3_cdist_sumofsquares_utsd_model__num[,1,2] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utsd_model__num[,,2])) -
            r$cdist_sumofsquares_utsd_obs__num[,1,2,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,2,2]))) ** 2,
        # utsd: stock 2 / area 2
        (r$step3_cdist_sumofsquares_utsd_model__num[,2,2] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utsd_model__num[,,2])) -
            r$cdist_sumofsquares_utsd_obs__num[,2,2,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utsd_obs__num[,,2,2]))) ** 2,
        # utcd: area 1
        (r$step3_cdist_sumofsquares_utcd_model__num[,1] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utcd_model__num[,1])) -
            r$cdist_sumofsquares_utcd_obs__num[,2,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_obs__num[,2,1]))) ** 2,
        # utcd: area 2
        (r$step3_cdist_sumofsquares_utcd_model__num[,2] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utcd_model__num[,2])) -
            r$cdist_sumofsquares_utcd_obs__num[,2,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_obs__num[,2,2]))) ** 2,
        # utcd_weight: area 1
        (r$step3_cdist_sumofsquares_utcd_weight_model__wgt[,1] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utcd_weight_model__wgt[,1])) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,2,1] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,2,1]))) ** 2,
        # utcd_weight: area 2
        (r$step3_cdist_sumofsquares_utcd_weight_model__wgt[,2] / g3_avoid_zero(sum(r$step3_cdist_sumofsquares_utcd_weight_model__wgt[,2])) -
            r$cdist_sumofsquares_utcd_weight_obs__wgt[,2,2] / g3_avoid_zero(sum(r$cdist_sumofsquares_utcd_weight_obs__wgt[,2,2]))) ** 2,
        # multinom:
        (2 * (-sum(r$cdist_multinomial_multinom_obs__num[,2] * log(g3_logspace_add_vec(r$step3_cdist_multinomial_multinom_model__num/g3_avoid_zero(sum(r$step3_cdist_multinomial_multinom_model__num)) *
            10000, (1/(length(r$cdist_multinomial_multinom_obs__num[,2]) * 10)) * 10000)/10000)) +
                (sum(g3_lgamma_vec(1 + r$cdist_multinomial_multinom_obs__num[,2])) - lgamma(1 +
                        sum(r$cdist_multinomial_multinom_obs__num[,2]))))),
        # surveyindices:
        sum((params$si_alpha +
            params$si_beta * log(g3_avoid_zero(r$adist_surveyindices_log_surveyindices_model__num[,])) -
            log(g3_avoid_zero(r$adist_surveyindices_log_surveyindices_obs__num[,])))**2),
        r$step1_nll)), "step3_nll: Sum of squares, including step1_nll")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    }
})
