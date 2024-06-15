library(magrittr)
library(unittest)

library(gadget3)

capture_warnings <- function(x, full_object = FALSE) {
    all_warnings <- list()
    rv <- withCallingHandlers(x, warning = function (w) {
        all_warnings <<- c(all_warnings, list(w))
        invokeRestart("muffleWarning")
    })
    if (!full_object) all_warnings <- vapply(all_warnings, function (w) w$message, character(1))
    return(list(rv = rv, warnings = all_warnings))
}

ok(ut_cmp_error({
    invalid_subset <- array(dim = c(2,2,2))
    g3_to_tmb(list(~{invalid_subset[,g3_idx(1),]}))
}, "invalid_subset"), "Complained when trying to subset in middle")
ok(ut_cmp_error({
    invalid_subset <- array(dim = c(2,2,2))
    g3_to_tmb(list(~{invalid_subset[g3_idx(1),,] <- 0}))
}, "invalid_subset"), "Complained when trying to subset by row in an lvalue")

ok(ut_cmp_error({
    g3_to_tmb(list(~{ 2 + NA }))
}, "NA"), "No such thing as NA in C++")

ok(grepl(
    "unknown_func(.*2.*)",
    paste(g3_to_tmb(list(~{
        unknown_func(2)
    })), collapse = "\n"),
    perl = TRUE), "Unknown functions that are valid C++ get included")

ok(ut_cmp_error({
    g3_to_tmb(list(~`0unknown0`(2)))
}, "0unknown0"), "An unknown function has to at least be a valid C++ function")

ok(ut_cmp_error({
    g3_to_tmb(list(~not.a.function(2)))
}, "not\\.a\\.function"), "An unknown function has to at least be a valid C++ function")

ok(ut_cmp_error({
    g3_tmb_adfun(g3_to_tmb(list(~{
        g3_param('randomandoptimise', random = TRUE, optimise = TRUE)
        g3_param('ro2', random = TRUE, optimise = TRUE)
    })))
}, "randomandoptimise,ro2"), "Specifying random and optimise isn't allowed")

ok(ut_cmp_error({
    g3_tmb_adfun(g3_to_tmb(list(~{ g3_param_table('randomandoptimise', expand.grid(cur_step = 2:3), random = TRUE, optimise = TRUE) })))
}, "randomandoptimise"), "Specifying random and optimise isn't allowed")

ok(ut_cmp_error({
    g3_to_tmb(list(~g3_param("camel", optimize = FALSE)))
}, "optimise"), "Optimise is spelt with an s in g3_param()")

ok_group("Exponentiate params")
params.in <- attr(g3_to_tmb(list( g3a_time(1990, 2000), g3_formula(
    quote(d),
    d = g3_parameterized('par.years', value = 0, by_year = TRUE, exponentiate = TRUE),
    x = NA) )), 'parameter_template')
ok(ut_cmp_identical(params.in[grep('^par', params.in$switch), 'switch'], c(
    paste0('par.years.', 1990:2000, '_exp'),
    NULL)), "exponentiate prefix ends up at the end of parameters")

ok_group('g3_tmb_par', {
    param <- attr(g3_to_tmb(list(~{
        g3_param('param.b')
        g3_param_vector('param_vec')
        g3_param('aaparam')
        g3_param('randomparam', random = TRUE)
    })), 'parameter_template')
    param$value <- I(list(
        aaparam = 55,
        param.b = 66,
        randomparam = 2,
        param_vec = 6:10)[rownames(param)])

    ok(ut_cmp_identical(g3_tmb_par(param, include_random = FALSE), c(
        param__b = 66,
        param_vec1 = 6, param_vec2 = 7, param_vec3 = 8, param_vec4 = 9, param_vec5 = 10,
        aaparam = 55)), "g3_tmb_par: Flattened parameters in right order")

    param['param_vec', 'optimise'] <- FALSE
    ok(ut_cmp_identical(g3_tmb_par(param, include_random = FALSE), c(
        param__b = 66,
        aaparam = 55)), "g3_tmb_par: Turning off optimise removed values")

    ok(ut_cmp_identical(g3_tmb_par(param, include_random = TRUE), c(
        param__b = 66,
        aaparam = 55,
        randomparam = 2)), "g3_tmb_par: randomparam visible if include_random on")
})

ok_group('g3_tmb_lower', {
    param <- attr(g3_to_tmb(list(~{
        g3_param('param.b')
        g3_param_vector('param_vec')
        g3_param('aaparam')
        # NB: Never visible
        g3_param('randomparam', random = TRUE)
    })), 'parameter_template')
    param$value <- I(list(
        aaparam = 55,
        param.b = 66,
        randomparam = 2,
        param_vec = 6:10)[rownames(param)])
    param$lower <- c(
        aaparam = 500,
        param.b = 600,
        param_vec = 100)[rownames(param)]

    ok(ut_cmp_identical(g3_tmb_lower(param), c(
        param__b = 600,
        param_vec1 = 100, param_vec2 = 100, param_vec3 = 100, param_vec4 = 100, param_vec5 = 100,
        aaparam = 500)), "g3_tmb_lower: All lower bounds in right order")

    param['param_vec', 'lower'] <- 3
    ok(ut_cmp_identical(g3_tmb_lower(param), c(
        param__b = 600,
        param_vec1 = 3, param_vec2 = 3, param_vec3 = 3, param_vec4 = 3, param_vec5 = 3,
        aaparam = 500)), "g3_tmb_lower: Set all lower values of param_vec in one go")
    ok(ut_cmp_identical(
        names(g3_tmb_par(param, include_random = FALSE)),
        names(g3_tmb_lower(param))), "g3_tmb_lower: Structure matches par after setting param_vec")

    param['param.b', 'optimise'] <- FALSE
    ok(ut_cmp_identical(g3_tmb_lower(param), c(
        param_vec1 = 3, param_vec2 = 3, param_vec3 = 3, param_vec4 = 3, param_vec5 = 3,
        aaparam = 500)), "g3_tmb_lower: Cleared param.b by setting optimise = F")
    ok(ut_cmp_identical(
        names(g3_tmb_par(param, include_random = FALSE)),
        names(g3_tmb_lower(param))), "g3_tmb_lower: Structure matches par after setting param.b")
})

ok_group('g3_tmb_upper', {
    param <- attr(g3_to_tmb(list(~{
        g3_param('param.b')
        g3_param_vector('param_vec')
        g3_param('aaparam')
        # NB: Never visible
        g3_param('randomparam', random = TRUE)
    })), 'parameter_template')
    param$value <- I(list(
        aaparam = 55,
        param.b = 66,
        randomparam = 2,
        param_vec = 6:10)[rownames(param)])
    param$upper <- c(
        aaparam = 500,
        param.b = 600,
        param_vec = 100)[rownames(param)]

    ok(ut_cmp_identical(g3_tmb_upper(param), c(
        param__b = 600,
        param_vec1 = 100, param_vec2 = 100, param_vec3 = 100, param_vec4 = 100, param_vec5 = 100,
        aaparam = 500)), "g3_tmb_upper: All upper bounds in right order")

    param['param_vec', 'upper'] <- 3
    ok(ut_cmp_identical(g3_tmb_upper(param), c(
        param__b = 600,
        param_vec1 = 3, param_vec2 = 3, param_vec3 = 3, param_vec4 = 3, param_vec5 = 3,
        aaparam = 500)), "g3_tmb_upper: Set all lower values of param_vec in one go")
    ok(ut_cmp_identical(
        names(g3_tmb_par(param, include_random = FALSE)),
        names(g3_tmb_lower(param))), "g3_tmb_upper: Structure matches par after setting param_vec")

    param['param.b', 'optimise'] <- FALSE
    ok(ut_cmp_identical(g3_tmb_upper(param), c(
        param_vec1 = 3, param_vec2 = 3, param_vec3 = 3, param_vec4 = 3, param_vec5 = 3,
        aaparam = 500)), "g3_tmb_upper: Cleared param.b by setting optimise = F")
    ok(ut_cmp_identical(
        names(g3_tmb_par(param, include_random = FALSE)),
        names(g3_tmb_lower(param))), "g3_tmb_lower: Structure matches par after setting param.b")
})

ok_group('g3_tmb_parscale', {
    param <- attr(g3_to_tmb(list(~{
        g3_param('param.lu', lower = 4, upper = 8)
        g3_param('param.ps', parscale = 22)
        g3_param('param.lups', lower = 4, upper = 8, parscale = 44)
    })), 'parameter_template')
    ok(ut_cmp_identical(g3_tmb_parscale(param), c(
        param__lu = NA,
        param__ps = 22,
        param__lups = 44)), "We populate parscale")
})

ok_group('g3_tmb_relist', {
    param <- attr(g3_to_tmb(list(~{
        g3_param('param.b')
        g3_param_vector('param_vec')
        g3_param('unopt_param', optimise = FALSE)
        g3_param('randomparam', random = TRUE)
        g3_param('aaparam')
    })), 'parameter_template')
    param$value <- I(list(
        aaparam = 55,
        param.b = 66,
        unopt_param = 95,
        randomparam = 2,
        param_vec = 6:10)[rownames(param)])

    ok(ut_cmp_identical(
        g3_tmb_relist(param, c(
            param__b = 660,
            param_vec1 = 60, param_vec2 = 70, param_vec3 = 80, param_vec4 = 90, param_vec5 = 100,
            aaparam = 550)),
        list(
            "param.b" = 660,
            "param_vec" = c(60, 70, 80, 90, 100),
            "unopt_param" = 95,
            "randomparam" = 2,
            "aaparam" = 550)), "g3_tmb_relist: Put parameters back in right slots, used old unopt_param value")
    ok(ut_cmp_identical(
        g3_tmb_relist(param, c(
            param__b = 660,
            param_vec1 = 60, param_vec2 = 70, param_vec3 = 80, param_vec4 = 90, param_vec5 = 100,
            randomparam = 5,  # NB: randomparam included, so we update it
            aaparam = 550)),
        list(
            "param.b" = 660,
            "param_vec" = c(60, 70, 80, 90, 100),
            "unopt_param" = 95,
            "randomparam" = 5,
            "aaparam" = 550)), "g3_tmb_relist: Put parameters back in right slots, including random")

    param['param.b', 'optimise'] <- FALSE
    ok(ut_cmp_error(
        g3_tmb_relist(param, c(
            param__b = 660,
            param_vec1 = 60, param_vec2 = 70, param_vec3 = 80, param_vec4 = 90, param_vec5 = 100,
            randomparam = 6,  # NB: randomparam included, so we update it
            aaparam = 550)),
        "par"), "g3_tmb_relist: Still included param__b in par, now an error")
    ok(ut_cmp_identical(
        g3_tmb_relist(param, c(
            param_vec1 = 60, param_vec2 = 70, param_vec3 = 80, param_vec4 = 90, param_vec5 = 100,
            aaparam = 550)),
        list(
            "param.b" = 66,
            "param_vec" = c(60, 70, 80, 90, 100),
            "unopt_param" = 95,
            "randomparam" = 2,
            "aaparam" = 550)), "g3_tmb_relist: Works without param.b set, use initial table value")
})

ok_group('g3_param', {
    param <- attr(g3_to_tmb(list(g3a_time(2000, 2004), ~{
        g3_param('a')
        g3_param('b', value = 4, optimise = FALSE, random = TRUE, lower = 5, upper = 10)
    })), 'parameter_template')
    ok(ut_cmp_identical(
        param[c('a', 'b'),],
        data.frame(
            row.names = c('a', 'b'),
            switch = c('a', 'b'),
            type = c("", ""),
            value = I(list(a = 0, b = 4)),
            optimise = c(TRUE, FALSE),
            random = c(FALSE, TRUE),
            lower = c(NA, 5),
            upper = c(NA, 10),
            parscale = as.numeric(c(NA, NA)),  # NB: Not derived yet, only when calling g3_tmb_parscale()
            stringsAsFactors = FALSE)), "Param table included custom values")
})

ok_group('g3_param_table', {
    param <- attr(g3_to_tmb(list(g3a_time(2000, 2004, step_lengths = c(3,3,3,3)), ~{
        g3_param_table('pt', expand.grid(  # NB: We can use base R
            cur_year = seq(start_year, end_year),  # NB: We can use g3a_time's vars
            cur_step = 2:3))
        g3_param_table('pg', expand.grid(
            cur_year = start_year,
            cur_step = 1:2), value = 4, optimise = FALSE, random = TRUE, lower = 5, upper = 10)
    })), 'parameter_template')
    ok(ut_cmp_identical(
        param[c(paste('pt', 2000:2004, 2, sep = '.'), paste('pt', 2000:2004, 3, sep = '.'), 'pg.2000.1', 'pg.2000.2'),],
        data.frame(
            row.names = c(paste('pt', 2000:2004, 2, sep = '.'), paste('pt', 2000:2004, 3, sep = '.'), 'pg.2000.1', 'pg.2000.2'),
            switch = c(paste('pt', 2000:2004, 2, sep = '.'), paste('pt', 2000:2004, 3, sep = '.'), 'pg.2000.1', 'pg.2000.2'),
            type = c("", "", "", "", "", "", "", "", "", "", "", ""),
            value = I(list(
                "pt.2000.2" = 0,
                "pt.2001.2" = 0,
                "pt.2002.2" = 0,
                "pt.2003.2" = 0,
                "pt.2004.2" = 0,
                "pt.2000.3" = 0,
                "pt.2001.3" = 0,
                "pt.2002.3" = 0,
                "pt.2003.3" = 0,
                "pt.2004.3" = 0,
                "pg.2000.1" = 4,
                "pg.2000.2" = 4)),
            optimise = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
            random = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
            lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5, 5),
            upper = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 10, 10),
            # NB: Not derived yet, only when calling g3_tmb_parscale()
            parscale = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
            stringsAsFactors = FALSE)), "Param table included custom values")
    ok(ut_cmp_identical(
        attr(g3_to_tmb(list(~{
            g3_param_table('moo', expand.grid(cur_year=1990:1994))
            g3_param('moo.1990')
            g3_param('oink.2')
        })), 'parameter_template')$switch,
        c("moo.1990", "moo.1991", "moo.1992", "moo.1993", "moo.1994", "oink.2")), "Refering to individual parameters doesn't generate duplicate table entries")
})

ok_group("g3_to_tmb: attr.actions", {
    actions <- list(
        list("001" = ~{ 1 + 1 }, "002" = ~{2 + 2}),
        "003" = ~{3 + 3})
    model_fn <- g3_to_tmb(actions)
    ok(ut_cmp_identical(attr(model_fn, 'actions'), actions), "actions returned as attribute uncollated")
})

ok_group("g3_to_tmb: Can use random parameters without resorting to include_random", local({
    # ./TMB/inst/examples/randomregression.R converted into gadget3
    actions <- local({
        set.seed(123)
        n <- 1
        ng <- 1
        f <- gl(ng, n/ng)
        t <- rnorm(n, mean=2, sd=5)
        a <- rnorm(ng, mean=3, sd=4)
        b <- rnorm(ng, mean=8, sd=7)
        x <- a[f] * t + b[f] + rnorm(n, mean=0, sd=1)
        set.seed(NULL)

        list(
            g3a_time(1990, 1991),
            g3l_random_dnorm("a",
                ~g3_param('a', value=1, random=TRUE),
                ~g3_param('mu.a', value=1),
                ~g3_param('sigma.a', value=1), period="single"),
            g3l_random_dnorm("b",
                ~g3_param('b', value=1, random=TRUE),
                ~g3_param('mu.b', value=1),
                ~g3_param('sigma.b', value=1), period="single"),
            g3l_random_dnorm("x",
                ~x,
                ~g3_param('a', value=1, random=TRUE) * t + g3_param('b', value=1, random=TRUE),
                ~g3_param('sigma0', value=1), period="single" ))
    })
    model_fn <- g3_to_r(actions)

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_cpp <- g3_to_tmb(actions)
        model_tmb <- g3_tmb_adfun(model_cpp, compile_flags = c("-O0", "-g"), inner.control = list(fnscale = -1))
        param_tbl <- attr(model_cpp, 'parameter_template')
        param_tbl[,'lower'] <- -2
        param_tbl[,'upper'] <- 3
        param_tbl[,'parscale'] <- 1

        ok(ut_cmp_identical(names(model_tmb$env$last.par)[model_tmb$env$random], c(
            "a",
            "b",
            NULL)), "env$random: TMB got the random parameters we expected")

        ok(ut_cmp_error({
            nlminb(g3_tmb_par(param_tbl), model_tmb$fn, model_tmb$gr)
        }, "\\$par"), "g3_tmb_par: Not allowed in nlminb() call")
        ok(ut_cmp_error({
            optim(g3_tmb_par(param_tbl), model_tmb$fn, model_tmb$gr)
        }, "\\$par"), "g3_tmb_par: Not allowed in optim() call")

        res <- suppressWarnings({
            nlminb(model_tmb$par, model_tmb$fn, model_tmb$gr,
                upper = g3_tmb_upper(param_tbl),
                lower = g3_tmb_lower(param_tbl),
                control = list(
                    fnscale = -1,
                    parscale = g3_tmb_parscale(param_tbl)))
        })
        ok(res$convergence == 0, "Model ran successfully and converged")

        # The first won't have random parameters, the latter will. Both should work
        value_no_random <- g3_tmb_relist(param_tbl, res$par)
        value_inc_random <- g3_tmb_relist(param_tbl, model_tmb$env$last.par)
        ok(ut_cmp_equal(
            value_no_random[param_tbl[!param_tbl$random, 'switch']],
            value_inc_random[param_tbl[!param_tbl$random, 'switch']]), "Non-random parameters match")
        ok(ut_cmp_equal(
            I(value_no_random[param_tbl[param_tbl$random, 'switch']]),
            param_tbl$value[param_tbl[param_tbl$random, 'switch']]), "value_no_random: Random parameters as default")
        ok(!isTRUE(all.equal(
            I(value_inc_random[param_tbl[param_tbl$random, 'switch']]),
            param_tbl$value[param_tbl[param_tbl$random, 'switch']])), "value_random: Random parameters have been updated")
        param_tbl$value <- value_inc_random

        # NB: ut_tmb_r_compare will be using g3_tmb_par()
        gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_tbl)
    } else {
        writeLines("# skip: not compiling TMB model")
    }
}))

ok_group("cpp_code", {
    ns <- function (s) gsub("\\s+", "", s)
    ok(ut_cmp_identical(
        ns(gadget3:::cpp_code(quote( 4 + 4 ))),
        ns("(double)(4) + (double)(4)")),
        "4 + 4: Defaulted to double")
    ok(ut_cmp_identical(
        ns(gadget3:::cpp_code(quote( (4 + 4) ))),
        ns("((double)(4) + (double)(4))")),
        "(4 + 4): Defaulted to double")
    ok(ut_cmp_identical(
        ns(gadget3:::cpp_code(quote( x[[4]] + 4 ))),
        ns("x(3) + (double)(4)")),
        "x[[4]] + 4: Subtracted one, assumed int")
    # NB: We rely on this in g3_param_table(ifmissing_param_table) below
    ok(ut_cmp_identical(
        ns(gadget3:::cpp_code(quote( x[[(-4 + 1 - 1) / (2*8)]] + 4 ))),
        ns("x( (-4 + 1 - 1) / (2*8) - 1 ) + (double)(4)")),
        "x[[(-4 + 1 - 1) / (2*8)]] + 4: Assumed int passses through +, -, *, /, ( ")
})


###############################################################################
actions <- list()
expecteds <- new.env(parent = emptyenv())
expected_warnings_r <- c()
expected_warnings_tmb <- c()
params <- list(rv=0)

# Check constants can pass through cleanly
constants_integer <- 999
constants_nan <- 999
actions <- c(actions, ~{
    comment('constants')
    constants_integer <- 5L ; REPORT(constants_integer)
    constants_nan <- NaN ; REPORT(constants_nan)
})
expecteds$constants_integer <- 5L
expecteds$constants_nan <- NaN

# Can assign a single value to 1x1 array
assign_to_1_1 <- array(dim = c(1,1))
actions <- c(actions, ~{
    comment('assign_to_1_1')
    assign_to_1_1[g3_idx(1)] <- 100.1
    REPORT(assign_to_1_1)
})
expecteds$assign_to_1_1 <- array(c(100.1), dim = c(1,1))

assign_to_2_1 <- array(dim = c(2,1))
data_to_2_1 <- c(100.1, 200.2)
actions <- c(actions, ~{
    comment('assign_to_2_1')
    assign_to_2_1[,g3_idx(1)] <- data_to_2_1
    REPORT(assign_to_2_1)
})
expecteds$assign_to_2_1 <- array(c(100.1, 200.2), dim = c(2,1))
    
assign_to_2_2 <- array(dim = c(2,2))
data_to_2_2 <- c(110.1, 220.2)
actions <- c(actions, ~{
    comment('assign_to_2_2')
    assign_to_2_2[,g3_idx(1)] <- data_to_2_1
    assign_to_2_2[,g3_idx(2)] <- data_to_2_2
    REPORT(assign_to_2_2)
})
expecteds$assign_to_2_2 <- array(c(100.1, 200.2, 110.1, 220.2), dim = c(2,2))

# Assign single value, horizontally
assign_to_2_2a <- array(dim = c(2,2))
actions <- c(actions, ~{
    comment('assign_to_2_2a')
    assign_to_2_2a[[g3_idx(1),g3_idx(2)]] <- 99
    assign_to_2_2a[[g3_idx(2),g3_idx(1)]] <- 88
    assign_to_2_2a[[g3_idx(2),g3_idx(2)]] <- 0
    assign_to_2_2a[[g3_idx(1),g3_idx(1)]] <- 0
    REPORT(assign_to_2_2a)
})
expecteds$assign_to_2_2a <- array(c(0, 88, 99, 0), dim = c(2,2))

# Assign zero and other scalars to column, arrays
assign_scalar <- array(dim = c(2,4))
params$assign_scalar_const <- runif(1, 100, 200)
actions <- c(actions, ~{
    comment('assign_scalar')
    assign_scalar[] <- 0  # TODO: TMB auto-setZeros, R doesn't
    assign_scalar[g3_idx(1),g3_idx(1)] <- 99  # NB: Overwritten
    assign_scalar[,g3_idx(1)] <- 0
    assign_scalar[,g3_idx(2)] <- 88
    assign_scalar[g3_idx(2),g3_idx(3)] <- 27
    assign_scalar[,g3_idx(4)] <- g3_param("assign_scalar_const")
    REPORT(assign_scalar)
})
expecteds$assign_scalar <- array(c(
    0, 0,
    88, 88,
    0, 27,
    params$assign_scalar_const, params$assign_scalar_const,
    NULL), dim = c(2,4))

# Can subset from left and right, using transpose
assign_subset <- array(
    rep(seq(100, 200, 100), each = 3 * 5) +
    rep(seq(10, 30, 10), each = 5, times = 2) +
    rep(seq(1, 5, 1), each = 1, times = 2),
    dim = c(5, 3, 2))
assign_right <- assign_subset[,2,2] ; assign_right[] <- 0
assign_leftright <- assign_subset[4,,2] ; assign_leftright[] <- 0
actions <- c(actions, ~{
    comment('assign_subset')
    assign_right <- assign_subset[,g3_idx(2),g3_idx(2)]
    REPORT(assign_right)
    assign_leftright <- assign_subset[g3_idx(4),,g3_idx(2)]
    REPORT(assign_leftright)
})
expecteds$assign_right <- assign_subset[,2,2]
expecteds$assign_leftright <- assign_subset[4,,2]

# Arrays with dynamic dimensions
dynamic_dim_array <- array(0, dim = c(2,1))
dynamic_dim_array_na <- array(NA, dim = c(2,1))
attr(dynamic_dim_array, 'dynamic_dim') <- list(2, quote(dynamic_dim_array_dim_2))
attr(dynamic_dim_array_na, 'dynamic_dim') <- list(2, quote(dynamic_dim_array_dim_2))
dynamic_dim_array_dim_2 <- 4L
dynamic_dim_array_dim_na_2 <- 4L
actions <- c(actions, ~{
    comment('assign_scalar')
    # NB: Under TMB this won't be NA, so set it to hide differences.
    dynamic_dim_array_na[] <- 5
    REPORT(dynamic_dim_array)
    REPORT(dynamic_dim_array_na)
})
expecteds$dynamic_dim_array <- array(0, dim = c(2, 4))
expecteds$dynamic_dim_array_na <- array(5, dim = c(2, 4))

# Data variable names are escaped
escaped.data.scalar <- 44
escaped.data.array <- c(1,2,3,4,5)
escaped_data_output <- 0.0
actions <- c(actions, ~{
    comment('escaped.data.array')
    escaped_data_output <- escaped.data.scalar + sum(escaped.data.array)
    REPORT(escaped_data_output)
})
# NB: In theory we could also report the escaped name, but we can't rename
# items in the produced report, so reports will differ TMB/R
expecteds$escaped_data_output <- escaped.data.scalar + sum(escaped.data.array)

# is.nan / is.finite
is_nan_nan_scalar_input <- NaN
is_nan_finite_scalar_input <- 4
is_nan_finite_array_input <- as.array(1:4)
is_nan_nan_array_input <- as.array(rep(NaN, 5))
is_nan_finite_vector_input <- 1:4
is_nan_nan_vector_input <- rep(NaN, 5)
is_nan_finite_single_array_input <- as.array(1)
is_nan_output <- 0L
is_finite_output <- 0L
actions <- c(actions, ~{
    comment('is.nan / is.finite')
    is_nan_output <- is_nan_output + (if (is.nan(is_nan_nan_scalar_input)) 1 else 0)
    is_nan_output <- is_nan_output + (if (is.nan(is_nan_finite_scalar_input)) 2 else 0)
    is_nan_output <- is_nan_output + (if (any(is.nan(is_nan_finite_array_input))) 4 else 0)
    is_nan_output <- is_nan_output + (if (any(is.nan(is_nan_nan_array_input))) 8 else 0)
    is_nan_output <- is_nan_output + (if (any(is.nan(is_nan_nan_vector_input))) 16 else 0)
    is_nan_output <- is_nan_output + (if (any(is.nan(is_nan_finite_single_array_input))) 32 else 0)
    REPORT(is_nan_output)
    is_finite_output <- is_finite_output + (if (is.finite(is_nan_nan_scalar_input)) 1 else 0)
    is_finite_output <- is_finite_output + (if (is.finite(is_nan_finite_scalar_input)) 2 else 0)
    is_finite_output <- is_finite_output + (if (any(is.finite(is_nan_finite_array_input))) 4 else 0)
    is_finite_output <- is_finite_output + (if (any(is.finite(is_nan_nan_array_input))) 8 else 0)
    is_finite_output <- is_finite_output + (if (any(is.finite(is_nan_nan_vector_input))) 16 else 0)
    is_finite_output <- is_finite_output + (if (any(is.finite(is_nan_finite_single_array_input))) 32 else 0)
    REPORT(is_finite_output)
})
expecteds$is_nan_output <- 1 + 8 + 16
expecteds$is_finite_output <- 2 + 4 + 32

# as.vector() --> .vec()
as_vector_array <- array(runif(20), dim=c(10, 2))
as_vector_result1 <- rep(NaN, 10)
as_vector_result2 <- rep(NaN, 10)
as_vector_mean <- 0.5
as_vector_sigma <- 1
actions <- c(actions, ~{
    comment('as_vector')
    as_vector_result1 <- pnorm(as.vector(as_vector_array[,g3_idx(1)]), as_vector_mean, as_vector_sigma)
    as_vector_result2 <- pnorm(as.vector(as_vector_array[,g3_idx(2)]), as_vector_mean, as_vector_sigma)
    REPORT(as_vector_result1)
    REPORT(as_vector_result2)
})
expecteds$as_vector_result1 <- pnorm(as_vector_array[,1], as_vector_mean, as_vector_sigma)
expecteds$as_vector_result2 <- pnorm(as_vector_array[,2], as_vector_mean, as_vector_sigma)

# pow() / .pow()
pow_scalar <- 99
pow_scalar_result <- 0
pow_vector <- c("50:60" = 55, "60:70" = 65, "70:80" = 75, "80:90" = 85, "90:100" = 95, "100:Inf" = 105)
pow_vector_result <- array(0, dim = c(6, 5))
actions <- c(actions, ~{
    comment('pow_vector')
    pow_scalar_result <- pow_scalar^3
    REPORT(pow_scalar_result)
    # NB: This has to use pow_vector.pow(), pow(pow_vector, 2) fails
    g3_with(pv := 5 * pow_vector^2, for (a in seq(1, 5, by = 1)) {
        pow_vector_result[,g3_idx(a)] <- pv
    })
    REPORT(pow_vector_result)
})
expecteds$pow_scalar_result <- pow_scalar^3
expecteds$pow_vector_result <- array(5 * pow_vector^2, dim = c(6, 5))

# mean() --> .mean()
mean_vector <- array(c(1, 2, 88, 99))
mean_vector_result <- 0
actions <- c(actions, ~{
    comment('mean_vector')
    mean_vector_result <- mean(mean_vector)
    REPORT(mean_vector_result)
})
expecteds$mean_vector_result <- mean(mean_vector)

# colsums
colsums_in <- array(1:6, dim = c(3,2))
colsums_result <- c(0L, 0L)
actions <- c(actions, ~{
    comment('colsums')
    colsums_result <- colSums(colsums_in)
    REPORT(colsums_result)
})
expecteds$colsums_result <- colSums(colsums_in)

# rowsums
rowsums_in <- array(c(1,2,3,4,5,6), dim = c(3,2))
rowsums_result <- c(0, 0, 0)
actions <- c(actions, ~{
    comment('rowsums')
    rowsums_result <- rowSums(rowsums_in)
    REPORT(rowsums_result)
})
expecteds$rowsums_result <- rowSums(rowsums_in)

# if statement without braces
if_no_brace_result <- 0.0
actions <- c(actions, ~{
    comment('if_without_braces')
    if (FALSE) if_no_brace_result <- 1 else if_no_brace_result <- 0.2
    REPORT(if_no_brace_result)
})
expecteds$if_no_brace_result <- 0.2

# if expression (not statement) turns into tertiary
tertiary_result_0 <- 3
tertiary_result_1 <- 6
tertiary_result_2 <- 6
ok(ut_cmp_error({
    g3_to_tmb(list(~{ tertiary_result_0 <- if (tertiary_result_0 == 3) 9  }))
}, "tertiary_result_0"), "Complained about missing else for tertiary")
actions <- c(actions, ~{
    comment('tertiary')
    tertiary_result_0 <- if (tertiary_result_0 == 3) 9 else 4
    tertiary_result_1 <- if (tertiary_result_1 == 3) 9 else 4
    tertiary_result_2 <- 100 - if (tertiary_result_0 == -30) 20 else 40
    REPORT(tertiary_result_0)
    REPORT(tertiary_result_1)
    REPORT(tertiary_result_2)
})
expecteds$tertiary_result_0 <- 9
expecteds$tertiary_result_1 <- 4
expecteds$tertiary_result_2 <- 100 - 40  # NB: Operator precedence preserved

# g3_with()
g3_with_result <- 0L
# NB: We don't define g3_with_iterator, it's defined within the block
actions <- c(actions, ~{
    comment('g3_with')
    g3_with(
        g3_with_iterator := g3_idx(2L),  # NB: Tests we can use g3 functions in definition
        g3_with_other_exp := 1L + 2L + 3L,
        {
            g3_with_result <- g3_with_iterator - g3_idx(1)  # NB: Reverse g3_idx from definition
            REPORT(g3_with_result)
            REPORT(g3_with_other_exp)
        })
})
expecteds$g3_with_result <- 1L  # i.e. 2 - 1 in R or 1 - 0 in TMB
expecteds$g3_with_other_exp <- 1L + 2L + 3L

# min() & max()
min_result <- 0.0
max_result <- 0.0
actions <- c(actions, ~{
    comment('min/max')
    min_result <- min(4, 9)
    max_result <- max(sum(mean_vector), 2)  # NB: sum gets cast to Type
    REPORT(min_result)
    REPORT(max_result)
})
expecteds$min_result <- 4
expecteds$max_result <- sum(mean_vector)

# negate single value
negate_x <- 10
actions <- c(actions, ~{
    comment('negate')
    negate_x <- -negate_x
    REPORT(negate_x)
})
expecteds$negate_x <- -10

# Modulo assumes integer
modulo_x <- 10
modulo_y <- 11
actions <- c(actions, ~{
    comment('modulo')
    modulo_x <- as_integer(modulo_x) %% 2
    modulo_y <- as_integer(modulo_y) %% 2
    REPORT(modulo_x)
    REPORT(modulo_y)
})
expecteds$modulo_x <- 0
expecteds$modulo_y <- 1

# sum() & prod()
sumprod_input <- runif(10)
sumprod_sum <- 0
sumprod_prod <- 0
actions <- c(actions, ~{
    comment('sum/prod')
    sumprod_sum <- sum(sumprod_input)
    sumprod_prod <- prod(sumprod_input)
    REPORT(sumprod_sum)
    REPORT(sumprod_prod)
})
expecteds$sumprod_sum <- sum(sumprod_input)
expecteds$sumprod_prod <- prod(sumprod_input)

# g3_param_table()
param_table_out <- array(rep(0, 6))
actions <- c(actions, ~{
    for (pt_a in seq(1, 3, by = 1)) for (pt_b in seq(7, 8, by = 1)) {
        param_table_out[[((pt_a - 1L) * 2L) + (pt_b - 7L) + 1L]] <- g3_param_table('param_table', expand.grid(pt_a = 1:2, pt_b = c(8, 7)))
    }
    REPORT(param_table_out)
})
params[['param_table.1.8']] <- 18
params[['param_table.1.7']] <- 17
params[['param_table.2.8']] <- 28
params[['param_table.2.7']] <- 27
expecteds$param_table_out <- array(c(
    17,
    18,
    27,
    28,
    NaN,
    NaN,
    NULL))
expected_warnings_r <- c(expected_warnings_r,
    "No value found in g3_param_table param_table, ifmissing not specified",
    "No value found in g3_param_table param_table, ifmissing not specified",
    NULL)
expected_warnings_tmb <- c(expected_warnings_tmb,
    "No value found in g3_param_table param_table, ifmissing not specified",
    "No value found in g3_param_table param_table, ifmissing not specified",
    NULL)

# g3_param_table(ifmissing)
param_table_ifmissing_out <- array(c(1,2,3,4,5,6))
actions <- c(actions, ~{
    for (ifmissing in seq(1, 6, by = 1)) {
        param_table_ifmissing_out[[ifmissing]] <- g3_param_table(
            'param_table_ifmissing',
            expand.grid(ifmissing = 2:4), ifmissing = -1)
    }
    REPORT(param_table_ifmissing_out)
})
params[['param_table_ifmissing.2']] <- 27
params[['param_table_ifmissing.3']] <- 47
params[['param_table_ifmissing.4']] <- 22
expecteds$param_table_ifmissing_out <- array(c(-1, 27, 47, 22, -1, -1))

# g3_param_table(ifmissing_param)
param_table_ifmparam_out <- array(c(1,2,3,4,5,6))
actions <- c(actions, ~{
    for (ifmparam in seq(1, 6, by = 1)) {
        param_table_ifmparam_out[[ifmparam]] <- g3_param_table(
            'param_table_ifmparam',
            expand.grid(ifmparam = 2:4), ifmissing = g3_param("param_table_ifmparam.missing"))
    }
    REPORT(param_table_ifmparam_out)
})
params[['param_table_ifmparam.2']] <- floor(runif(1, 100, 200))
params[['param_table_ifmparam.3']] <- floor(runif(1, 100, 200))
params[['param_table_ifmparam.4']] <- floor(runif(1, 100, 200))
params[['param_table_ifmparam.missing']] <- floor(runif(1, 100, 200))
expecteds$param_table_ifmparam_out <- array(c(
    params[['param_table_ifmparam.missing']],
    params[['param_table_ifmparam.2']],
    params[['param_table_ifmparam.3']],
    params[['param_table_ifmparam.4']],
    params[['param_table_ifmparam.missing']],
    params[['param_table_ifmparam.missing']]))

# g3_param_table(ifmissing_param_table)
param_table_ifmpartab_out <- array(c(1,2,3,4,5,6,7,8,9))
actions <- c(actions, ~{
    for (ifmpartab in seq(1, 3, by = 1)) for (ifmpartab_m in seq(1, 3, by = 1)) {
        param_table_ifmpartab_out[[((ifmpartab - 1) * 3) + (ifmpartab_m - 1) + 1]] <- g3_param_table(
            'param_table_ifmpartab',
            expand.grid(ifmpartab = 2:3), ifmissing = g3_param_table(
                "param_table_ifmpartab_m",
                expand.grid(ifmpartab_m = 1:3)))
    }
    REPORT(param_table_ifmpartab_out)
})
params[['param_table_ifmpartab.2']] <- floor(runif(1, 100, 200))
params[['param_table_ifmpartab.3']] <- floor(runif(1, 100, 200))
params[['param_table_ifmpartab_m.1']] <- floor(runif(1, 100, 200))
params[['param_table_ifmpartab_m.2']] <- floor(runif(1, 100, 200))
params[['param_table_ifmpartab_m.3']] <- floor(runif(1, 100, 200))
expecteds$param_table_ifmpartab_out <- array(c(
    params[['param_table_ifmpartab_m.1']],
    params[['param_table_ifmpartab_m.2']],
    params[['param_table_ifmpartab_m.3']],

    params[['param_table_ifmpartab.2']],
    params[['param_table_ifmpartab.2']],
    params[['param_table_ifmpartab.2']],

    params[['param_table_ifmpartab.3']],
    params[['param_table_ifmpartab.3']],
    params[['param_table_ifmpartab.3']],
    NULL))

###############################################################################

nll <- 0.0
actions <- c(actions, ~{
    comment('done')
    nll <- nll + g3_param('rv')
    return(nll)
})

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

# Compare everything we've been told to compare
result <- capture_warnings(model_fn(params))
# str(attributes(result), vec.len = 10000)
for (n in ls(expecteds)) {
    ok(ut_cmp_equal(
        attr(result$rv, n),
        expecteds[[n]], tolerance = 1e-6), n)
}
ok(ut_cmp_identical(result$warnings, expected_warnings_r), "Warnings from R as expected")
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    param_template <- attr(model_cpp, "parameter_template")
    param_template$value <- params[param_template$switch]
    generated_warnings <- capture_warnings(gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template))$warnings
    ok(ut_cmp_identical(generated_warnings, c(expected_warnings_r, expected_warnings_tmb)), "Warnings from R+TMB as expected")
}
