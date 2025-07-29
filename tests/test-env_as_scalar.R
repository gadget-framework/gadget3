library(unittest)

library(gadget3)

actions <- list()

# NB: Should test under both CppAD and TMBAD
# options(gadget3.tmb.framework = "CppAD")

###############################################################################

scalar_in <- runif(1, 0, 100)
vector_in <- gadget3:::force_vector(runif(1, 0, 100))
array_in <- array(runif(1, 0, 100), dim = c(1,1))

actions[['vals_in']] <- g3_formula(
    {
        expect_as_scalar_scalar <- as_scalar(scalar_in)
        expect_as_scalar_vector <- as_scalar(vector_in)
        expect_as_scalar_vector_prod <- as_scalar(vector_in * scalar_in)
        expect_as_scalar_array <- as_scalar(array_in)
        expect_as_scalar_array_prod <- as_scalar(array_in * scalar_in)
    },
    scalar_in = scalar_in,
    vector_in = vector_in,
    array_in = array_in,
    expect_as_scalar_scalar = scalar_in[[1]],
    expect_as_scalar_vector = vector_in[[1]],
    expect_as_scalar_vector_prod = vector_in[[1]] * scalar_in[[1]],
    expect_as_scalar_array =  array_in[[1]],
    expect_as_scalar_array_prod =  array_in[[1]] * scalar_in[[1]],
    end = NULL )

###############################################################################

expecteds <- new.env(parent = emptyenv())

for (i in seq_along(actions)) {
    exp_names <- grep("^expect_", names(environment(actions[[i]])), value = TRUE)

    # For each expect_ variable, move to expecteds
    for (exp_name in exp_names) {
        expecteds[[exp_name]] <- environment(actions[[i]])[[exp_name]]
        environment(actions[[i]])[[exp_name]][] <- 0
    }

    # REPORT every expect_
    reports <- lapply(exp_names, function (exp_name) {
        substitute(REPORT(sym), list(sym = as.symbol(exp_name)))
    })
    # Convert list to { REPORT(x) ; REPORT(y); ... }
    reports <- as.call(c(as.symbol("{"), reports))

    # Top/tail actions with a comment of their name & reports
    actions[[i]] <- gadget3:::f_substitute(quote({
        comment(act_name)
        act_f
        reports
    }), list(
        act_name = names(actions)[[i]],
        act_f = actions[[i]],
        reports = reports))
}

actions <- c(actions, gadget3:::g3l_test_dummy_likelihood())
actions[['z']] <- g3_formula({
    comment('done')
    return(nll)
}, nll = 0.0)

model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)
params <- attr(model_cpp, "parameter_template")
result <- model_fn(params)

# Compare everything we've been told to compare
for (n in ls(expecteds)) {
    tol <- sqrt(.Machine$double.eps)
    if (!is.null(attr(expecteds[[n]], "tol"))) {
        tol <- attr(expecteds[[n]], "tol")
        attr(expecteds[[n]], "tol") <- NULL
    }
    ok(ut_cmp_equal(
        attr(result, n),
        expecteds[[n]],
        tolerance = tol ), n)
}

gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
