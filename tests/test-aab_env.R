library(magrittr)
library(unittest)

library(gadget3)

params <- list()
actions <- list()
expecteds <- new.env(parent = emptyenv())

# matrix_vec
matrix_vec_tf <- matrix(c(0,1,0,1,0,0,0,0,1), nrow = 3)
matrix_vec_vec <- c(10, 100, 1000)
matrix_vec_out <- rep(0, 5)
actions <- c(actions, ~{
    comment('matrix_vec')
    matrix_vec_out <- g3_matrix_vec(matrix_vec_tf, matrix_vec_vec)
    REPORT(matrix_vec_out)
})
expecteds$matrix_vec_out <- c(100, 10, 1000)

# logspace_add()
logspace_add_1 <- 0.0
logspace_add_0 <- 0.0
logspace_inp_1 <- 1.0
logspace_inp_0 <- 0.0
actions <- c(actions, ~{
    comment('logspace_add')
    # NB: We have to cast "0" to a Type for the below to work, but this happens automatically
    logspace_add_1 <- logspace_add(logspace_inp_1, 0)
    logspace_add_0 <- logspace_add(logspace_inp_0, logspace_inp_0)
    REPORT(logspace_add_1)
    REPORT(logspace_add_0)
})
expecteds$logspace_add_1 <- 1.313262
expecteds$logspace_add_0 <- 0.6931472

# logspace_add_vec()
logspace_add_vec_inp <- c(0,0.1,0.2,0.3)
logspace_add_vec_0 <- c(0,0,0,0)
logspace_add_vec_1 <- c(0,0,0,0)
actions <- c(actions, ~{
    comment('logspace_add_vec')
    logspace_add_vec_0 <- logspace_add_vec(logspace_add_vec_inp, 0)
    logspace_add_vec_1 <- logspace_add_vec(logspace_add_vec_inp, 1)
    REPORT(logspace_add_vec_0)
    REPORT(logspace_add_vec_1)
})
expecteds$logspace_add_vec_0 <- c(0.6931472, 0.7443967, 0.7981389, 0.8543552)
expecteds$logspace_add_vec_1 <- c(1.313262, 1.341154, 1.371101, 1.403186)

# ratio_add_vec()
ratio_add_vec_inp_orig_vec <- runif(10) * 100
ratio_add_vec_inp_orig_amount <- floor(runif(10) * 10)
ratio_add_vec_inp_new_vec <- runif(10) * 100
ratio_add_vec_inp_new_amount <- floor(runif(10) * 10)
ratio_add_vec_output <- rep(0, 10)
actions <- c(actions, ~{
    comment('ratio_add_vec')
    ratio_add_vec_output <- ratio_add_vec(
        ratio_add_vec_inp_orig_vec, ratio_add_vec_inp_orig_amount,
        ratio_add_vec_inp_new_vec, ratio_add_vec_inp_new_amount)
    REPORT(ratio_add_vec_output)
})
ratio_add_vec_total <- ratio_add_vec_inp_orig_amount + ratio_add_vec_inp_new_amount
expecteds$ratio_add_vec_output <- ratio_add_vec_inp_orig_vec * (ratio_add_vec_inp_orig_amount / g3_env$avoid_zero_vec(ratio_add_vec_total)) +
    ratio_add_vec_inp_new_vec * (ratio_add_vec_inp_new_amount / g3_env$avoid_zero_vec(ratio_add_vec_total))

###############################################################################

nll <- 0.0
actions <- c(actions, ~{
    comment('done')
    nll <- nll + g3_param('rv')
    return(nll)
})
params <- list(rv=0)

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
