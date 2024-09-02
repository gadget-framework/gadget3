library(magrittr)
library(unittest)

library(gadget3)

params <- list()
actions <- list()
expecteds <- new.env(parent = emptyenv())
tolerances <- new.env(parent = emptyenv())

native_avz <- function (a) ( pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))) ) / 1000

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
expecteds$ratio_add_vec_output <- ratio_add_vec_inp_orig_vec * (ratio_add_vec_inp_orig_amount / native_avz(ratio_add_vec_total)) +
    ratio_add_vec_inp_new_vec * (ratio_add_vec_inp_new_amount / native_avz(ratio_add_vec_total))

# nonconform_mult
nonconform_inp1 <- array(runif(4*3*2), dim = c(4,3,2))
nonconform_inp2 <- array(runif(4*4*5), dim = c(4,4,5))
nonconform_extra <- array(c(1e1, 1e2, 1e3, 1e4, 1e-1, 1e-2, 1e-3, 1e-4), dim = c(4,2))
nonconform_outmult1 <- array(dim = c(4,3,2))
nonconform_outmult2 <- array(dim = c(4,4,5))
nonconform_outmult2a <- array(dim = c(4,4,1))
actions <- c(actions, ~{
    comment('nonconform_mult')
    nonconform_outmult1 <- nonconform_mult(nonconform_inp1, nonconform_extra[,g3_idx(1)])
    nonconform_outmult2 <- nonconform_mult(nonconform_inp2, nonconform_extra[,g3_idx(2)])
    nonconform_outmult2a <- nonconform_mult(nonconform_inp2[,,g3_idx(1)], nonconform_extra[,g3_idx(2)])
    REPORT(nonconform_outmult1)
    REPORT(nonconform_outmult2)
    REPORT(nonconform_outmult2a)
})
expecteds$nonconform_outmult1 <- nonconform_inp1 * as.vector(nonconform_extra[,1])
expecteds$nonconform_outmult2 <- nonconform_inp2 * as.vector(nonconform_extra[,2])
expecteds$nonconform_outmult2a <- nonconform_inp2[,,1] * as.vector(nonconform_extra[,2])

# nonconform_add
nonconform_outadd1 <- array(dim = c(4,3,2))
nonconform_outadd2 <- array(dim = c(4,4,5))
nonconform_outadd2a <- array(dim = c(4,4,1))
actions <- c(actions, ~{
    comment('nonconform_add')
    nonconform_outadd1 <- nonconform_add(nonconform_inp1, nonconform_extra[,g3_idx(1)])
    nonconform_outadd2 <- nonconform_add(nonconform_inp2, nonconform_extra[,g3_idx(2)])
    nonconform_outadd2a <- nonconform_add(nonconform_inp2[,,g3_idx(1)], nonconform_extra[,g3_idx(2)])
    REPORT(nonconform_outadd1)
    REPORT(nonconform_outadd2)
    REPORT(nonconform_outadd2a)
})
expecteds$nonconform_outadd1 <- nonconform_inp1 + as.vector(nonconform_extra[,1])
expecteds$nonconform_outadd2 <- nonconform_inp2 + as.vector(nonconform_extra[,2])
expecteds$nonconform_outadd2a <- nonconform_inp2[,,1] + as.vector(nonconform_extra[,2])

# nonconform_div
nonconform_outdiv1 <- array(dim = c(4,3,2))
nonconform_outdiv2 <- array(dim = c(4,4,5))
nonconform_outdiv2a <- array(dim = c(4,4,1))
actions <- c(actions, ~{
    comment('nonconform_div')
    nonconform_outdiv1 <- nonconform_div(nonconform_inp1, nonconform_extra[,g3_idx(1)])
    nonconform_outdiv2 <- nonconform_div(nonconform_inp2, nonconform_extra[,g3_idx(2)])
    nonconform_outdiv2a <- nonconform_div(nonconform_inp2[,,g3_idx(1)], nonconform_extra[,g3_idx(2)])
    REPORT(nonconform_outdiv1)
    REPORT(nonconform_outdiv2)
    REPORT(nonconform_outdiv2a)
})
expecteds$nonconform_outdiv1 <- nonconform_inp1 / as.vector(nonconform_extra[,1])
expecteds$nonconform_outdiv2 <- nonconform_inp2 / as.vector(nonconform_extra[,2])
expecteds$nonconform_outdiv2a <- nonconform_inp2[,,1] / as.vector(nonconform_extra[,2])

# nonconform_divavz
nonconform_div_avz_extra <- array(c(0, 1e1, 1e2, 1e3, 0, 1e-1, 1e-2, 1e-3), dim = c(4,2))
nonconform_outdiv_avz1 <- array(dim = c(4,3,2))
nonconform_outdiv_avz2 <- array(dim = c(4,4,5))
nonconform_outdiv_avz2a <- array(dim = c(4,4,1))
actions <- c(actions, ~{
    comment('nonconform_div_avz')
    nonconform_outdiv_avz1 <- nonconform_div_avz(nonconform_inp1, nonconform_div_avz_extra[,g3_idx(1)])
    nonconform_outdiv_avz2 <- nonconform_div_avz(nonconform_inp2, nonconform_div_avz_extra[,g3_idx(2)])
    nonconform_outdiv_avz2a <- nonconform_div_avz(nonconform_inp2[,,g3_idx(1)], nonconform_div_avz_extra[,g3_idx(2)])
    REPORT(nonconform_outdiv_avz1)
    REPORT(nonconform_outdiv_avz2)
    REPORT(nonconform_outdiv_avz2a)
})
expecteds$nonconform_outdiv_avz1 <- nonconform_inp1 / native_avz(as.vector(nonconform_div_avz_extra[,1]))
expecteds$nonconform_outdiv_avz2 <- nonconform_inp2 / native_avz(as.vector(nonconform_div_avz_extra[,2]))
expecteds$nonconform_outdiv_avz2a <- nonconform_inp2[,,1] / native_avz(as.vector(nonconform_div_avz_extra[,2]))

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
        expecteds[[n]],
        tolerance = if (is.null(tolerances[[n]])) 1e-6 else tolerances[[n]] ), n)
}

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    param_template <- attr(model_cpp, "parameter_template")
    param_template$value <- params[param_template$switch]
    gadget3:::ut_tmb_r_compare(model_fn, model_tmb, param_template)
}
