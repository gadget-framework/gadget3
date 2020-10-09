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
                model_tmb_report[[n]],
                environment(model_fn)$model_report[[n]],
                tolerance = 1e-5), paste("TMB and R match", n))
        }
    } else {
        writeLines("# skip: not running TMB tests")
    }
}

actions <- list()
expecteds <- new.env(parent = emptyenv())

ok(ut_cmp_error({
    invalid_subset <- array(dim = c(2,2))
    g3_to_tmb(list(~{invalid_subset[g3_idx(1),] <- 0}))
}, "invalid_subset"), "Complained when trying to subset by row")

ok(ut_cmp_error({
    start_year <- 1998
    g3_to_tmb(list(~{g3_param("moo", start_year)}))
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

###############################################################################

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

# logspace_add()
logspace_add_1 <- 0.0
logspace_add_0 <- 0.0
logspace_inp_1 <- 1.0
logspace_inp_0 <- 0.0
actions <- c(actions, ~{
    comment('logspace_add')
    # NB: Logspace needs Types, not integers
    logspace_add_1 <- logspace_add(logspace_inp_1, logspace_inp_0)
    logspace_add_0 <- logspace_add(logspace_inp_0, logspace_inp_0)
    g3_report(logspace_add_1)
    g3_report(logspace_add_0)
})
expecteds$logspace_add_1 <- 1.313262
expecteds$logspace_add_0 <- 0.6931472

###############################################################################

actions <- c(actions, ~{
    comment('done')
    return(g3_param('rv'))
})
params <- list(rv=0)

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
