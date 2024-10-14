library(unittest)

library(gadget3)

ok_group('edit.g3_r - Round-trip through an editor results in working code', {
    nll <- 0.0
    model_fn <- g3_to_r(list(~{
        nll <- g3_param('nll')
        return(nll)
    }))
    model_fn_n <- edit(model_fn, editor = '/bin/true')
    attr(model_fn, 'srcref') <- NULL
    attr(model_fn, 'srcfile') <- NULL
    attr(model_fn_n, 'srcref') <- NULL
    attr(model_fn_n, 'srcfile') <- NULL
    ok(ut_cmp_identical(
        capture.output(str(model_fn)),
        capture.output(str(model_fn_n))), "str() output of 2 functions is the same")

    n <- runif(1)
    ok(ut_cmp_identical(n, model_fn(list(nll = n))), "Original function works")
    ok(ut_cmp_identical(n, model_fn_n(list(nll = n))), "Edited function works too")
})

ok_group('print.g3_r', {
    cap <- function (x) {
        out <- capture.output(x)
        out <- gsub('^<(\\w+): .*>$', '<\\1: xxx>', out)
        return(out)
    }
    model_fn <- g3_to_r(list( g3_formula(
        return(x + sum(y)),
        y = c(4, 5),
        x = g3_parameterized('parp') )))
    ok(ut_cmp_identical(cap(print(model_fn)), c(
        "function (param = parameter_template) ",
        "{",
        "    if (is.data.frame(param)) {",
        "        param_lower <- structure(param$lower, names = param$switch)",
        "        param_upper <- structure(param$upper, names = param$switch)",
        "        param <- structure(param$value, names = param$switch)",
        "    }",
        "    else {",
        "        param_lower <- lapply(param, function(x) NA)",
        "        param_upper <- lapply(param, function(x) NA)",
        "    }",
        "    stopifnot(\"parp\" %in% names(param))",
        "    x <- param[[\"parp\"]]",
        "    while (TRUE) {",
        "        return(x + sum(y))",
        "    }",
        "}",
        "<bytecode: xxx>",
        "<environment: xxx>",
        NULL)), "Default print output has code, no attributes")
    ok(ut_cmp_identical(cap(print(model_fn, with_environment = TRUE, with_template = TRUE)), c(
        "function (param = parameter_template) ",
        "{",
        "    if (is.data.frame(param)) {",
        "        param_lower <- structure(param$lower, names = param$switch)",
        "        param_upper <- structure(param$upper, names = param$switch)",
        "        param <- structure(param$value, names = param$switch)",
        "    }",
        "    else {",
        "        param_lower <- lapply(param, function(x) NA)",
        "        param_upper <- lapply(param, function(x) NA)",
        "    }",
        "    stopifnot(\"parp\" %in% names(param))",
        "    x <- param[[\"parp\"]]",
        "    while (TRUE) {",
        "        return(x + sum(y))",
        "    }",
        "}",
        "<bytecode: xxx>",
        "<environment: xxx>",
        "Environment:",
        " $ y                : num [1:2] 4 5",
        " $ reporting_enabled: int 1",
        "Parameter template:",
        " $ parp: num 0",
        NULL)), "Can add with_environment = TRUE, with_template = TRUE")
})

ok_group("g3_to_r: attr.actions", {
    actions <- list(
        list("001" = ~{ 1 + 1 }, "002" = ~{2 + 2}),
        "003" = ~{3 + 3})
    model_fn <- g3_to_r(actions)
    ok(ut_cmp_identical(attr(model_fn, 'actions'), actions), "actions returned as attribute uncollated")
})

ok_group("g3_to_r: attr.parameter_template", {
    actions <- list(
        list("001" = ~{ 1 + 1 }, "002" = ~{2 + 2}),
        "003" = ~{3 + 3})
    model_fn <- g3_to_r(actions)
    ok(ut_cmp_identical(attr(model_fn, 'parameter_template'), NULL), "Empty parameter template")

    actions <- list(
        "001" = ~{ 1 + 1 },
        "002" = ~{g3_param('moo', value = 4) + g3_param('oink', value = 99)},
        "003" = ~{3 + 3})
    model_fn <- g3_to_r(actions)
    ok(ut_cmp_identical(attr(model_fn, 'parameter_template'), list(moo = 4, oink = 99)), "2 values populated")
})

ok_group("g3_to_r: custom functions", local({
    # Find an unclaimed name in globalenv to use
    fn_name <- NULL
    while (is.null(fn_name) || exists(fn_name, envir = globalenv())) {
        fn_name <- paste0("ut_run_r_fn_", floor(runif(1, 1e6, 1e7)))
    }

    # Set here and in globalenv, neither should appear in our tests
    assign(fn_name, function(x) 98)
    assign(fn_name, function(x) 99, envir = globalenv())

    # Trying to call this without defining it explicitly doesn't work
    step_f <- gadget3:::call_to_formula(
        substitute( return(fn(123)), list( fn = as.symbol(fn_name) )),
        env = new.env(parent = emptyenv()) )
    model_fn <- g3_to_r(list(step_f))
    ok(!(fn_name %in% names(environment(model_fn))), "Function *not* in model environment")
    ok(ut_cmp_error(
        model_fn(),
        fn_name ), "Failed to run function without definition in formula, don't pick up globalenv")

    environment(step_f)[[fn_name]] <- function (x) x * 100
    model_fn <- g3_to_r(list(step_f))
    ok(fn_name %in% names(environment(model_fn)), "Function in model environment")
    ok(ut_cmp_equal(model_fn(), 12300), "Used definition of function from formula, not globalenv")

    # Can use closures within the model too
    closure <- function (base) { base <- base * 1e5 ; function(x) x + base }
    environment(step_f)[[fn_name]] <- closure(89)
    model_fn <- g3_to_r(list(step_f))
    ok(fn_name %in% names(environment(model_fn)), "Function in model environment")
    ok(ut_cmp_equal(model_fn(), 8900123), "Used function, preserved it's closure")

    # Tidy up
    rm(list = fn_name, envir = globalenv())
}))

ok_group("g3_to_r: non-base functions", local({
    model_fn <- g3_to_r(list(g3_formula(quote(
        # NB: tail is part of utils::, not base::
        return(tail(1:10, 5))
    ))))
    ok(!any(grepl("tail", names(environment(model_fn)))), "tail not included in environment")
    ok(ut_cmp_equal(model_fn(), 6:10), "Can use non-base attached namespaces")
}))

ok_group("g3_to_r: namespace-referenced functions", local({
    model_fn <- g3_to_r(list(g3_formula(quote({
        # Explicitly reference dplyr::desc, which (probably) isn't attached.
        return(dplyr::desc(g3_param("v")))
    }))))
    v_in <- runif(1, 1e5, 1e6)
    ok(!any(grepl("desc", names(environment(model_fn)))), "dplyr::desc not included in environment")
    ok(ut_cmp_equal(
        model_fn(list(v = v_in)),
        dplyr::desc(v_in) ), "Called dplyr::desc to generate output")
}))

ok_group('g3_param', {
    param <- attr(g3_to_r(list(g3a_time(2000, 2004, project_years = 0), ~{
        g3_param('a')
        # NB: We don't actually use optimise, but shouldn't error if it's there
        g3_param('b', value = 4, optimise = FALSE)
    })), 'parameter_template')
    ok(ut_cmp_identical(
        param[c('a', 'b')],
        list(a = 0, b = 4)), "Param list included value values")

    # Definitions-as-code should still parse g3_param()s (it's a different path to formulas)
    model_fn <- g3_to_r(list(g3_formula(
        return(y),
        y = quote( 2 + g3_param("peep") ))))
    ok(ut_cmp_identical(model_fn(list(peep = 4)), 6), "Unevaluated code had g3_param() resolved")
})

ok_group('g3_param_table', {
    param <- attr(g3_to_r(list(g3a_time(2000, 2004, step_lengths = rep(3, times = 4), project_years = 0), ~{
        g3_param_table('pt', expand.grid(  # NB: We can use base R
            cur_year = seq(start_year, end_year),  # NB: We can use g3a_time's vars
            cur_step = 2:3))
        g3_param_table('pg', expand.grid(
            cur_year = start_year,
            # NB: We don't actually use optimise, but shouldn't error if it's there
            cur_step = 1:2), value = 4, optimise = FALSE)
    })), 'parameter_template')
    ok(ut_cmp_identical(
        param[c(paste('pt', 2000:2004, 2, sep = '.'), paste('pt', 2000:2004, 3, sep = '.'), 'pg.2000.1', 'pg.2000.2')],
        structure(
            as.list(c(rep(0, 5), rep(0, 5), rep(4, 2))),
            names = c(
                paste('pt', 2000:2004, 2, sep = '.'),
                paste('pt', 2000:2004, 3, sep = '.'),
                'pg.2000.1',
                'pg.2000.2'))), "Param table turned into multiple parameters, value set")

    params.in <- attr(g3_to_r(list( g3a_time(1990, 2000), g3_formula(
        quote(d),
        d = g3_parameterized('par.years', value = 0, by_year = TRUE, exponentiate = TRUE),
        x = NA) )), 'parameter_template')
    ok(ut_cmp_identical(grep('^par', names(params.in), value = TRUE), c(
        paste0('par.years.', 1990:2000, '_exp'),
        NULL)), "exponentiate prefix ends up at the end of parameters")
})

ok_group('parameter_template default', {
    # By default we read arguments from the parameter_template
    model_fn <- g3_to_r(~return(g3_param("archibald", value = 45)))
    ok(ut_cmp_equal(model_fn(), 45), "Used default from parameter_template")
    ok(ut_cmp_equal(model_fn(list(archibald = 99)), 99), "Override default")
})

ok_group('parameter data.frame', {
    # We can also hand model_fn a data.frame, in which case the value column is used
    model_fn <- g3_to_r(~return(g3_param("archibald", value = 45)))

    df <- data.frame(switch = c("archibald"), lower = 0, upper = 100, value = I(list(floor(runif(1, 100, 200)))))
    ok(ut_cmp_equal(as.numeric(model_fn(df)), as.numeric(df$value)), "data.frame accepted as input")
})
