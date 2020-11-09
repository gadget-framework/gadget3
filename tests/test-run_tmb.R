library(magrittr)
library(unittest)

library(gadget3)

tmb_r_compare <- function (model_fn, model_tmb, params) {
    if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
        # Reformat params into a single vector in expected order
        par <- unlist(params[attr(model_cpp, 'parameter_template')$switch])
        model_tmb_report <- model_tmb$report(par)
        for (n in ls(environment(model_fn)$model_report)) {
            ok(ut_cmp_equal(
                model_tmb_report[[n]],
                environment(model_fn)$model_report[[n]],
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}

ok(ut_cmp_error({
    invalid_subset <- array(dim = c(2,2))
    g3_to_tmb(list(~{invalid_subset[g3_idx(1),] <- 0}))
}, "invalid_subset"), "Complained when trying to subset by row")

ok(ut_cmp_error({
    camel <- 1998
    g3_to_tmb(list(~{g3_param("moo", camel)}))
}, "g3_param.*moo"), "Complained about dynamic param, we don't support")

ok(grepl(
    "unknown_func(2)",
    paste(g3_to_tmb(list(~{
        unknown_func(2)
    })), collapse = "\n"),
    fixed = TRUE), "Unknown functions that are valid C++ get included")

ok(ut_cmp_error({
    g3_to_tmb(list(~`0unknown0`(2)))
}, "0unknown0"), "An unknown function has to at least be a valid C++ function")

ok_group('g3_tmb_par', {
    param <- attr(g3_to_tmb(list(~{
        g3_param('param.b')
        g3_param_vector('param_vec')
        g3_param('aaparam')
    })), 'parameter_template')
    param$value <- I(list(
        aaparam = 55,
        param.b = 66,
        param_vec = 6:10)[rownames(param)])

    ok(ut_cmp_identical(g3_tmb_par(param), c(
        param__b = 66,
        param_vec1 = 6, param_vec2 = 7, param_vec3 = 8, param_vec4 = 9, param_vec5 = 10,
        aaparam = 55)), "g3_tmb_par: Flattened parameters in right order")

    param['param_vec', 'optimise'] <- FALSE
    ok(ut_cmp_identical(g3_tmb_par(param), c(
        param__b = 66,
        aaparam = 55)), "g3_tmb_par: Turning off optimise removed values")
})

ok_group('g3_tmb_lower', {
    param <- attr(g3_to_tmb(list(~{
        g3_param('param.b')
        g3_param_vector('param_vec')
        g3_param('aaparam')
    })), 'parameter_template')
    param$value <- I(list(
        aaparam = 55,
        param.b = 66,
        param_vec = 6:10)[rownames(param)])
    param$lower <- c(
        aaparam = 500,
        param.b = 600,
        param_vec = 100)[rownames(param)]

    ok(ut_cmp_identical(g3_tmb_lower(param), c(
        param__b = 600,
        param_vec1 = 100, param_vec2 = 100, param_vec3 = 100, param_vec4 = 100, param_vec5 = 100,
        aaparam = 500)), "g3_tmb_lower: All lower bounds in right order")

    param['param_vec', 'lower'] <- NA
    ok(ut_cmp_identical(g3_tmb_lower(param), c(
        param__b = 600,
        aaparam = 500)), "g3_tmb_lower: Cleared param_vec by setting NA")

    param['param.b', 'optimise'] <- FALSE
    ok(ut_cmp_identical(g3_tmb_lower(param), c(
        aaparam = 500)), "g3_tmb_lower: Cleared param.b by setting optimise = F")
})

ok_group('g3_tmb_upper', {
    param <- attr(g3_to_tmb(list(~{
        g3_param('param.b')
        g3_param_vector('param_vec')
        g3_param('aaparam')
    })), 'parameter_template')
    param$value <- I(list(
        aaparam = 55,
        param.b = 66,
        param_vec = 6:10)[rownames(param)])
    param$upper <- c(
        aaparam = 500,
        param.b = 600,
        param_vec = 100)[rownames(param)]

    ok(ut_cmp_identical(g3_tmb_upper(param), c(
        param__b = 600,
        param_vec1 = 100, param_vec2 = 100, param_vec3 = 100, param_vec4 = 100, param_vec5 = 100,
        aaparam = 500)), "g3_tmb_upper: All upper bounds in right order")
    param['param_vec', 'upper'] <- NA
    ok(ut_cmp_identical(g3_tmb_upper(param), c(
        param__b = 600,
        aaparam = 500)), "g3_tmb_upper: Cleared param_vec by setting NA")
    param['param.b', 'optimise'] <- FALSE
    ok(ut_cmp_identical(g3_tmb_upper(param), c(
        aaparam = 500)), "g3_tmb_upper: Cleared param.b by setting optimise = F")
})

ok_group('g3_param_table', {
    param <- attr(g3_to_tmb(list(g3a_time(2000, 2004), ~{
        g3_param_table('pt', expand.grid(  # NB: We can use base R
            cur_year = seq(start_year, end_year),  # NB: We can use g3a_time's vars
            age = 2:3))
    })), 'parameter_template')
    ok(ut_cmp_identical(
        param$switch,
        paste("pt", 2000:2004, rep(2:3, each = 5), sep=".")), "Param table turned into multiple parameters")
})


###############################################################################
actions <- list()
expecteds <- new.env(parent = emptyenv())
params <- list(rv=0)

# Can assign a single value to 1x1 array
assign_to_1_1 <- array(dim = c(1,1))
actions <- c(actions, ~{
    comment('assign_to_1_1')
    assign_to_1_1[g3_idx(1)] <- 100.1
    g3_report(assign_to_1_1)
})
expecteds$assign_to_1_1 <- array(c(100.1), dim = c(1,1))

assign_to_2_1 <- array(dim = c(2,1))
data_to_2_1 <- c(100.1, 200.2)
actions <- c(actions, ~{
    comment('assign_to_2_1')
    assign_to_2_1[,g3_idx(1)] <- data_to_2_1
    g3_report(assign_to_2_1)
})
expecteds$assign_to_2_1 <- array(c(100.1, 200.2), dim = c(2,1))
    
assign_to_2_2 <- array(dim = c(2,2))
data_to_2_2 <- c(110.1, 220.2)
actions <- c(actions, ~{
    comment('assign_to_2_2')
    assign_to_2_2[,g3_idx(1)] <- data_to_2_1
    assign_to_2_2[,g3_idx(2)] <- data_to_2_2
    g3_report(assign_to_2_2)
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
    g3_report(assign_to_2_2a)
})
expecteds$assign_to_2_2a <- array(c(0, 88, 99, 0), dim = c(2,2))

# Assign zero and other scalars to column, arrays
assign_scalar <- array(dim = c(2,3))
actions <- c(actions, ~{
    comment('assign_scalar')
    assign_scalar[] <- 0  # TODO: TMB auto-setZeros, R doesn't
    assign_scalar[g3_idx(1),g3_idx(1)] <- 99  # NB: Overwritten
    assign_scalar[,g3_idx(1)] <- 0
    assign_scalar[,g3_idx(2)] <- 88
    assign_scalar[g3_idx(2),g3_idx(3)] <- 27
    g3_report(assign_scalar)
})
expecteds$assign_scalar <- array(c(0, 0, 88, 88, 0, 27), dim = c(2,3))

# Data variable names are escaped
escaped.data.scalar <- 44
escaped.data.array <- c(1,2,3,4,5)
escaped_data_output <- 0.0
actions <- c(actions, ~{
    comment('escaped.data.array')
    escaped_data_output <- escaped.data.scalar + sum(escaped.data.array)
    g3_report(escaped_data_output)
})
# NB: In theory we could also report the escaped name, but we can't rename
# items in the produced report, so reports will differ TMB/R
expecteds$escaped_data_output <- escaped.data.scalar + sum(escaped.data.array)

# mean() --> .mean()
mean_vector <- array(c(1, 2, 88, 99))
mean_vector_result <- 0
actions <- c(actions, ~{
    comment('mean_vector')
    mean_vector_result <- mean(mean_vector)
    g3_report(mean_vector_result)
})
expecteds$mean_vector_result <- mean(mean_vector)

# if statement without braces
if_no_brace_result <- 0.0
actions <- c(actions, ~{
    comment('if_without_braces')
    if (FALSE) if_no_brace_result <- 1 else if_no_brace_result <- 0.2
    g3_report(if_no_brace_result)
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
    g3_report(tertiary_result_0)
    g3_report(tertiary_result_1)
})
expecteds$tertiary_result_0 <- 9
expecteds$tertiary_result_1 <- 4

# g3_with()
g3_with_result <- 0L
# NB: We don't define g3_with_iterator, it's defined within the block
actions <- c(actions, ~{
    comment('g3_with')
    g3_with(
        g3_with_iterator, g3_idx(2L),  # NB: Tests we can use g3 functions in definition
        {
            g3_with_result <- g3_with_iterator - g3_idx(1)  # NB: Reverse g3_idx from definition
            g3_report(g3_with_result)
        })
})
expecteds$g3_with_result <- 1L  # i.e. 2 - 1 in R or 1 - 0 in TMB

# min() & max()
min_result <- 0.0
max_result <- 0.0
actions <- c(actions, ~{
    comment('min/max')
    min_result <- min(4, 9)
    max_result <- max(sum(mean_vector), 2)  # NB: sum gets cast to Type
    g3_report(min_result)
    g3_report(max_result)
})
expecteds$min_result <- 4
expecteds$max_result <- sum(mean_vector)

# negate single value
negate_x <- 10
actions <- c(actions, ~{
    comment('negate')
    negate_x <- -negate_x
    g3_report(negate_x)
})
expecteds$negate_x <- -10

# g3_param_table()
pt_a <- 2L ; pt_b <- 7L
param_table_out <- 0
actions <- c(actions, ~{
    pt_a ; pt_b
    param_table_out <- g3_param_table('param_table', expand.grid(pt_a = 1:2, pt_b = c(8, 7)))
    g3_report(param_table_out)
})
params[['param_table.1.8']] <- 18
params[['param_table.1.7']] <- 17
params[['param_table.2.8']] <- 28
params[['param_table.2.7']] <- 27
expecteds$param_table_out <- 27

###############################################################################

actions <- c(actions, ~{
    comment('done')
    return(g3_param('rv'))
})

# Compile model
model_fn <- g3_to_r(actions, trace = FALSE)
# model_fn <- edit(model_fn)
if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    model_cpp <- g3_to_tmb(actions, trace = FALSE)
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)
} else {
    writeLines("# skip: not compiling TMB model")
}

# Compare everything we've been told to compare
result <- model_fn(params)
# str(as.list(environment(model_fn)$model_report), vec.len = 10000)
for (n in ls(expecteds)) {
    ok(ut_cmp_equal(
        environment(model_fn)$model_report[[n]],
        expecteds[[n]], tolerance = 1e-6), n)
}
tmb_r_compare(model_fn, model_tmb, params)
