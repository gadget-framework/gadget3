library(magrittr)
library(unittest)

library(gadget3)

actions <- list(
    g3a_time(
        start_year = 1990,
        end_year = 1994,
        c(3,3,3,3)),
    g3l_random_dnorm('dnorm_log',
        ~g3_param('dnorm_log', value = 0, random = TRUE),
        mean_f = ~g3_param('dnorm_log_mean', value = 0, optimise = FALSE),
        sigma_f = ~g3_param('dnorm_log_sigma', value = 1, optimise = FALSE),
        weight = ~g3_param('dnorm_log_enabled', value = 0, optimise = FALSE)),
    g3l_random_dnorm('dnorm_lin',
        ~g3_param('dnorm_lin', value = 0, random = TRUE),
        mean_f = ~g3_param('dnorm_lin_mean', value = 0, optimise = FALSE),
        sigma_f = ~g3_param('dnorm_lin_sigma', value = 1, optimise = FALSE),
        log_f = FALSE,
        weight = ~g3_param('dnorm_lin_enabled', value = 0, optimise = FALSE)),
    g3l_random_walk('walk_year',
        ~g3_param_table('walk_year', expand.grid(cur_year = seq(start_year,  end_year)), value = 0, random = TRUE),
        sigma_f = ~g3_param('walk_year_sigma', value = 1, optimise = FALSE),
        weight = ~g3_param('walk_year_enabled', value = 0, optimise = FALSE)),
    g3l_random_walk('walk_step',
        ~g3_param_table('walk_step', expand.grid(
            cur_year = seq(start_year,  end_year),
            cur_step = 1:4), value = 0, random = TRUE),
        sigma_f = ~g3_param('walk_step_sigma', value = 1, optimise = FALSE),
        weight = ~g3_param('walk_step_enabled', value = 0, optimise = FALSE)),
    list('999' = ~{}))

model_fn <- g3_to_r(actions)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"))
} else {
    writeLines("# skip: not compiling TMB model")
    model_cpp <- c()
}

ok_group('g3l_random_dnorm', {
    params <- attr(model_fn, 'parameter_template')
    params['dnorm_log'] <- runif(1, 0, 10)
    params['dnorm_log_mean'] <- runif(1, 0, 10)
    params['dnorm_log_sigma'] <- runif(1, 0, 10)
    params['dnorm_log_enabled'] <- 1
    params['dnorm_lin'] <- runif(1, 0, 10)
    params['dnorm_lin_mean'] <- runif(1, 0, 10)
    params['dnorm_lin_sigma'] <- runif(1, 0, 10)
    params['dnorm_lin_enabled'] <- 1
    result <- model_fn(params)

    ok(ut_cmp_equal(
        as.vector(attr(result, "nll_random_dnorm_dnorm_log__dnorm")),
        -dnorm(params$dnorm_log, params$dnorm_log_mean, params$dnorm_log_sigma, TRUE),
        tolerance = 1e-7), "dnorm_log matches dnorm")
    ok(ut_cmp_equal(
        as.vector(attr(result, "nll_random_dnorm_dnorm_lin__dnorm")),
        dnorm(params$dnorm_lin, params$dnorm_lin_mean, params$dnorm_lin_sigma, FALSE),
        tolerance = 1e-7), "dnorm_lin matches dnorm")
    ok(ut_cmp_equal(
        as.vector(result),
        sum(c(
            -dnorm(params$dnorm_log, params$dnorm_log_mean, params$dnorm_log_sigma, TRUE),
            dnorm(params$dnorm_lin, params$dnorm_lin_mean, params$dnorm_lin_sigma, FALSE),
            0)),
        tolerance = 1e-7), "nll matches both (and has been included only once)")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        # Reproduce model to include any unoptimised parameters
        model_tmb <- g3_tmb_adfun(model_cpp, param_template, compile_flags = c("-O0", "-g"))
        ok(ut_cmp_identical(names(model_tmb$env$last.par)[model_tmb$env$random], c(
            'dnorm_lin',
            'dnorm_log',
            'walk_step__1990__1',
            'walk_step__1991__1',
            'walk_step__1992__1',
            'walk_step__1993__1',
            'walk_step__1994__1',
            'walk_step__1990__2',
            'walk_step__1991__2',
            'walk_step__1992__2',
            'walk_step__1993__2',
            'walk_step__1994__2',
            'walk_step__1990__3',
            'walk_step__1991__3',
            'walk_step__1992__3',
            'walk_step__1993__3',
            'walk_step__1994__3',
            'walk_step__1990__4',
            'walk_step__1991__4',
            'walk_step__1992__4',
            'walk_step__1993__4',
            'walk_step__1994__4',
            'walk_year__1990',
            'walk_year__1991',
            'walk_year__1992',
            'walk_year__1993',
            'walk_year__1994',
            NULL)), "env$random: TMB got expected random variables")
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    } else {
        writeLines("# skip: not running TMB tests")
    }
})

ok_group('g3l_random_walk', {
    params <- attr(model_fn, 'parameter_template')
    params['walk_year_enabled'] <- 1
    params[grepl('walk_year\\.199', names(params))] <- runif(5, 1, 10)
    params['walk_step_enabled'] <- 1
    params[grepl('walk_step\\.199', names(params))] <- runif(5 * 4, 1, 10)
    result <- model_fn(params)
    
    # Calculate the sum of a random walk along values
    sum_walk_dnorm <- function (vals, sigma, is_log) {
        vals <- vals[order(names(vals))]  # Sort by name, so 1991.1 is before 1991.2
        lead <- head(vals, -1)
        lag <- tail(vals, -1)
        sum(vapply(
            seq_along(lead),
            function (i) -dnorm(lead[[i]], lag[[i]], sigma, is_log),
            numeric(1)))
    }

    ok(ut_cmp_equal(
        as.vector(attr(result, "nll_random_walk_walk_year__dnorm")),
        sum_walk_dnorm(params[grepl('walk_year\\.199', names(params))],  params$walk_year_sigma, TRUE),
        tolerance = 1e-7), "nll_random_walk_walk_year__dnorm")

    ok(ut_cmp_equal(
        as.vector(attr(result, "nll_random_walk_walk_step__dnorm")),
        sum_walk_dnorm(params[grepl('walk_step\\.199', names(params))],  params$walk_step_sigma, TRUE),
        tolerance = 1e-7), "nll_random_walk_walk_step__dnorm")

    ok(ut_cmp_equal(
        as.vector(result),
        as.vector(sum(
            attr(result, "nll_random_walk_walk_year__dnorm"),
            attr(result, "nll_random_walk_walk_step__dnorm"),
            0)),
        tolerance = 1e-7), "nll matches both")

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        param_template <- attr(model_cpp, "parameter_template")
        param_template$value <- params[param_template$switch]
        # Reproduce model to include any unoptimised parameters
        model_tmb <- g3_tmb_adfun(model_cpp, param_template, compile_flags = c("-O0", "-g"))
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
    } else {
        writeLines("# skip: not running TMB tests")
    }
})

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    # An end-to-end test, too complex to be a vignette example
    in_param <- g3_parameterized('par', by_year = TRUE, random = TRUE, value = 50)
    out_stock <- g3_stock('output', 1) |> g3s_time(year = 1990:2000)

    simple_code <- g3_to_tmb(list(
        g3a_time(1990, 2000),
        gadget3:::g3_step(g3_formula(
            quote( stock_iterate(out_stock, stock_ss(out_stock__val) <- force_type * in_param) ),
            out_stock = out_stock,
            # NB: We need to set an array<Type>
            force_type = as.array(1),
            out_stock__val = g3_stock_instance(out_stock, 0),
            in_param = in_param )),
        g3l_random_dnorm(
            'rdnorm_par',
            param_f = in_param,
            # These need to be Types, not (double), thus g3_formula()
            mean_f = g3_formula(cur_year * m, m = 0.001),
            sigma_f = g3_formula(s, s = 1) ),
        NULL))
    obj.fn <- g3_tmb_adfun(simple_code)
    out <- suppressWarnings(optim(obj.fn$par, obj.fn$fn, obj.fn$gr, method = "BFGS", control = list(maxit = 10)))
    ok(ut_cmp_equal(
        as.vector(obj.fn$report()$output__val),
        c(1.99, 1.991, 1.992, 1.993, 1.994, 1.995, 1.996, 1.997, 1.998, 1.999, 2),
        filter = NULL), "output__val: Found mean for each year")
}
