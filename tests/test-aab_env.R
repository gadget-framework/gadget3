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

# normalize_vec
normalize_vec_all_zero_inp <- rep(0, 10)
normalize_vec_all_zero_out <- rep(NA, 10)
actions <- c(actions, ~{
    comment("normalize_vec")
    normalize_vec_all_zero_out <- normalize_vec(normalize_vec_all_zero_inp)
    REPORT(normalize_vec_all_zero_out)
})
expecteds$normalize_vec_all_zero_out <- rep(0, 10)

# ratio_add_pop()
ratio_add_pop_inp_orig_ar <- as.array(runif(10) * 100)
ratio_add_pop_inp_orig_amount <- as.array(floor(runif(10) * 10))
ratio_add_pop_inp_new_ar <- as.array(runif(10) * 100)
ratio_add_pop_inp_new_amount <- as.array(floor(runif(10) * 10))
ratio_add_pop_output <- rep(0, 10)
ratio_add_pop_output_derived <- rep(0, 10)
ratio_add_pop_output_scalar <- 0
actions <- c(actions, ~{
    comment('ratio_add_pop')
    ratio_add_pop_output <- ratio_add_pop(
        ratio_add_pop_inp_orig_ar, ratio_add_pop_inp_orig_amount,
        ratio_add_pop_inp_new_ar, ratio_add_pop_inp_new_amount)
    ratio_add_pop_output_derived <- ratio_add_pop(
        ratio_add_pop_inp_orig_ar, ratio_add_pop_inp_orig_amount,
        ratio_add_pop_inp_new_ar, ratio_add_pop_inp_new_amount * ratio_add_pop_inp_orig_amount)
    ratio_add_pop_output_scalar <- ratio_add_pop(
        ratio_add_pop_inp_orig_ar[[2]], ratio_add_pop_inp_orig_amount[[2]],
        ratio_add_pop_inp_new_ar[[2]], ratio_add_pop_inp_new_amount[[2]])
    REPORT(ratio_add_pop_output)
    REPORT(ratio_add_pop_output_derived)
    REPORT(ratio_add_pop_output_scalar)
})
ratio_add_pop_total <- native_avz(ratio_add_pop_inp_orig_amount + ratio_add_pop_inp_new_amount)
derived_total <- native_avz(ratio_add_pop_inp_orig_amount + (ratio_add_pop_inp_new_amount * ratio_add_pop_inp_orig_amount))
expecteds$ratio_add_pop_output <- ratio_add_pop_inp_orig_ar * (ratio_add_pop_inp_orig_amount / ratio_add_pop_total) +
    ratio_add_pop_inp_new_ar * (ratio_add_pop_inp_new_amount / ratio_add_pop_total)
expecteds$ratio_add_pop_output_derived <-
    ratio_add_pop_inp_orig_ar * (ratio_add_pop_inp_orig_amount / derived_total) +
    ratio_add_pop_inp_new_ar * ((ratio_add_pop_inp_new_amount * ratio_add_pop_inp_orig_amount) / derived_total)
expecteds$ratio_add_pop_output_scalar <- ratio_add_pop_inp_orig_ar[[2]] * (ratio_add_pop_inp_orig_amount[[2]] / ratio_add_pop_total[[2]]) +
    ratio_add_pop_inp_new_ar[[2]] * (ratio_add_pop_inp_new_amount[[2]] / ratio_add_pop_total[[2]])

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
actions <- c(actions, gadget3:::g3l_test_dummy_likelihood(), ~{
    comment('done')
    return(nll)
})

# Compile model
model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)
params <- attr(model_cpp, "parameter_template")

# Compare everything we've been told to compare
result <- model_fn(params)
# str(attributes(result), vec.len = 10000)
for (n in ls(expecteds)) {
    ok(ut_cmp_equal(
        attr(result, n),
        expecteds[[n]],
        tolerance = if (is.null(tolerances[[n]])) 1e-6 else tolerances[[n]] ), n)
}

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
