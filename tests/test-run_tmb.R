library(magrittr)
library(unittest)

library(gadget3)

ok(ut_cmp_error({
    invalid_subset <- array(dim = c(2,2))
    g3_to_tmb(list(~{invalid_subset[g3_idx(1),] <- 0}))
}, "invalid_subset"), "Complained when trying to subset by row")

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

    ok(ut_cmp_identical(g3_tmb_par(param), c(
        param__b = 66,
        param_vec1 = 6, param_vec2 = 7, param_vec3 = 8, param_vec4 = 9, param_vec5 = 10,
        aaparam = 55)), "g3_tmb_par: Flattened parameters in right order")

    param['param_vec', 'optimise'] <- FALSE
    ok(ut_cmp_identical(g3_tmb_par(param), c(
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
        names(g3_tmb_par(param)),
        names(g3_tmb_lower(param))), "g3_tmb_lower: Structure matches par after setting param_vec")

    param['param.b', 'optimise'] <- FALSE
    ok(ut_cmp_identical(g3_tmb_lower(param), c(
        param_vec1 = 3, param_vec2 = 3, param_vec3 = 3, param_vec4 = 3, param_vec5 = 3,
        aaparam = 500)), "g3_tmb_lower: Cleared param.b by setting optimise = F")
    ok(ut_cmp_identical(
        names(g3_tmb_par(param)),
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
        names(g3_tmb_par(param)),
        names(g3_tmb_lower(param))), "g3_tmb_upper: Structure matches par after setting param_vec")

    param['param.b', 'optimise'] <- FALSE
    ok(ut_cmp_identical(g3_tmb_upper(param), c(
        param_vec1 = 3, param_vec2 = 3, param_vec3 = 3, param_vec4 = 3, param_vec5 = 3,
        aaparam = 500)), "g3_tmb_upper: Cleared param.b by setting optimise = F")
    ok(ut_cmp_identical(
        names(g3_tmb_par(param)),
        names(g3_tmb_lower(param))), "g3_tmb_lower: Structure matches par after setting param.b")
})

ok_group('g3_tmb_parscale', {
    param <- attr(g3_to_tmb(list(~{
        g3_param('param.lu', lower = 4, upper = 8)
        g3_param('param.ps', parscale = 22)
        g3_param('param.lups', lower = 4, upper = 8, parscale = 44)
    })), 'parameter_template')
    ok(ut_cmp_identical(g3_tmb_parscale(param), c(
        param__lu = 6,
        param__ps = 22,
        param__lups = 44)), "We use mean(lower, upper) when parscale not available")
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

    param['param.b', 'optimise'] <- FALSE
    ok(ut_cmp_error(
        g3_tmb_relist(param, c(
            param__b = 660,
            param_vec1 = 60, param_vec2 = 70, param_vec3 = 80, param_vec4 = 90, param_vec5 = 100,
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
            value = I(list(a = array(0), b = array(4))),
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
                "pt.2000.2" = array(0),
                "pt.2001.2" = array(0),
                "pt.2002.2" = array(0),
                "pt.2003.2" = array(0),
                "pt.2004.2" = array(0),
                "pt.2000.3" = array(0),
                "pt.2001.3" = array(0),
                "pt.2002.3" = array(0),
                "pt.2003.3" = array(0),
                "pt.2004.3" = array(0),
                "pg.2000.1" = array(4),
                "pg.2000.2" = array(4))),
            optimise = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
            random = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
            lower = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5, 5),
            upper = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 10, 10),
            # NB: Not derived yet, only when calling g3_tmb_parscale()
            parscale = as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
            stringsAsFactors = FALSE)), "Param table included custom values")
})

ok_group("g3_to_tmb: attr.actions", {
    actions <- list(
        list("001" = ~{ 1 + 1 }, "002" = ~{2 + 2}),
        "003" = ~{3 + 3})
    model_fn <- g3_to_tmb(actions)
    ok(ut_cmp_identical(attr(model_fn, 'actions'), actions), "actions returned as attribute uncollated")
})

ok_group("g3_to_tmb: Can use random parameters", local({
    stock__prevrec <- gadget3:::g3_global_formula(init_val = 0.0)

    # Make sure we can use random = TRUE in an action, specifying TMB arguments correctly
    actions <- c(
        g3a_time(1990, 1992),
        list("010:g3l_custom" = gadget3:::f_substitute(~if (cur_step_final) {
            nll <- nll + dnorm(x, stock__prevrec, sigma, 1)
            stock__prevrec <- x
        }, list(
            x = ~g3_param_table('rp', expand.grid(cur_year = seq(start_year, end_year)), random = TRUE),
            sigma = ~g3_param('sigma', default = 0.2, optimise = TRUE)
        ))),
        list()
    )

    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    param_tbl <- attr(model_cpp, 'parameter_template')
    param_tbl[grepl('^rp|^sigma$', rownames(param_tbl)), 'value'] <- runif(sum(grepl('^rp|^sigma$', rownames(param_tbl))))

    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        model_tmb <- g3_tmb_adfun(model_cpp, param_tbl, compile_flags = c("-O0", "-g"))
        res <- optim(g3_tmb_par(param_tbl), model_tmb$fn, model_tmb$gr, method = 'BFGS')
        ok(res$convergence == 0, "Model ran successfully and converged")
    } else {
        writeLines("# skip: not compiling TMB model")
    }
}))

###############################################################################
actions <- list()
expecteds <- new.env(parent = emptyenv())
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
    assign_to_2_2a[g3_idx(1),g3_idx(2)] <- 99
    assign_to_2_2a[g3_idx(2),g3_idx(1)] <- 88
    assign_to_2_2a[g3_idx(2),g3_idx(2)] <- 0
    assign_to_2_2a[g3_idx(1),g3_idx(1)] <- 0
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
is_nan_finite_array_input <- 1:4
is_nan_nan_array_input <- rep(NaN, 5)
is_nan_output <- 0L
is_finite_output <- 0L
actions <- c(actions, ~{
    comment('is.nan / is.finite')
    is_nan_output <- is_nan_output + (if (is.nan(is_nan_nan_scalar_input)) 1 else 0)
    is_nan_output <- is_nan_output + (if (is.nan(is_nan_finite_scalar_input)) 2 else 0)
    is_nan_output <- is_nan_output + (if (any(is.nan(is_nan_finite_array_input))) 4 else 0)
    is_nan_output <- is_nan_output + (if (any(is.nan(is_nan_nan_array_input))) 8 else 0)
    REPORT(is_nan_output)
    is_finite_output <- is_finite_output + (if (is.finite(is_nan_nan_scalar_input)) 1 else 0)
    is_finite_output <- is_finite_output + (if (is.finite(is_nan_finite_scalar_input)) 2 else 0)
    is_finite_output <- is_finite_output + (if (any(is.finite(is_nan_finite_array_input))) 4 else 0)
    is_finite_output <- is_finite_output + (if (any(is.finite(is_nan_nan_array_input))) 8 else 0)
    REPORT(is_finite_output)
})
expecteds$is_nan_output <- 1 + 8
expecteds$is_finite_output <- 2 + 4

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
colsums_result <- c(0, 0)
actions <- c(actions, ~{
    comment('colsums')
    colsums_result <- colSums(colsums_in)
    REPORT(colsums_result)
})
expecteds$colsums_result <- colSums(colsums_in)

# rowsums
rowsums_in <- array(1:6, dim = c(3,2))
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
ok(ut_cmp_error({
    g3_to_tmb(list(~{ tertiary_result_0 <- if (tertiary_result_0 == 3) 9  }))
}, "tertiary_result_0"), "Complained about missing else for tertiary")
actions <- c(actions, ~{
    comment('tertiary')
    tertiary_result_0 <- if (tertiary_result_0 == 3) 9 else 4
    tertiary_result_1 <- if (tertiary_result_1 == 3) 9 else 4
    REPORT(tertiary_result_0)
    REPORT(tertiary_result_1)
})
expecteds$tertiary_result_0 <- 9
expecteds$tertiary_result_1 <- 4

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
pt_a <- 2L ; pt_b <- 7L
param_table_out <- 0
actions <- c(actions, ~{
    pt_a ; pt_b
    param_table_out <- g3_param_table('param_table', expand.grid(pt_a = 1:2, pt_b = c(8, 7)))
    REPORT(param_table_out)
})
params[['param_table.1.8']] <- 18
params[['param_table.1.7']] <- 17
params[['param_table.2.8']] <- 28
params[['param_table.2.7']] <- 27
expecteds$param_table_out <- 27

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

###############################################################################

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
result <- model_fn(params)
# str(attributes(result), vec.len = 10000)
for (n in ls(expecteds)) {
    ok(ut_cmp_equal(
        attr(result, n),
        expecteds[[n]], tolerance = 1e-6), n)
}
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    param_template <- attr(model_cpp, "parameter_template")
    param_template$value <- params[param_template$switch]
    gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
}
