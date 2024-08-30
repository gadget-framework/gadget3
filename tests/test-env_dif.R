library(unittest)

library(gadget3)

params <- list()
actions <- list()

# NB: Should test under both CppAD and TMBAD
# options(gadget3.tmb.framework = "CppAD")

###############################################################################

dif_pmax_scl_dbl_in <- runif(1, 0, 100)
dif_pmax_vec_in <- runif(10, 0, 100)
dif_pmax_vec_vec_max <- runif(10, 0, 10) * 10
actions[['dif_pmax_vec']] <- g3_formula(
    {
        expect_dif_pmax_scl_dbl <- dif_pmax(dif_pmax_scl_dbl_in, 40.0, 1e5)
        expect_dif_pmax_vec_typ <- dif_pmax(dif_pmax_vec_in, dif_pmax_vec_typ_max, 1e5)
        expect_dif_pmax_dervec_typ <- dif_pmax(dif_pmax_vec_in * 2, dif_pmax_vec_typ_max, 1e5)
        expect_dif_pmax_vec_dbl <- dif_pmax(dif_pmax_vec_in, 30.0, 1e5)
        expect_dif_pmax_vec_int <- dif_pmax(dif_pmax_vec_in, dif_pmax_vec_int_max, 1e5)
        expect_dif_pmax_vec_vec <- dif_pmax(dif_pmax_vec_in, dif_pmax_vec_vec_max, 1e5)
    },
    dif_pmax_scl_dbl_in = dif_pmax_scl_dbl_in,
    dif_pmax_vec_in = dif_pmax_vec_in,
    dif_pmax_vec_typ_max = 40.0,
    dif_pmax_vec_int_max = 60L,
    dif_pmax_vec_vec_max = dif_pmax_vec_vec_max,
    expect_dif_pmax_scl_dbl = pmax(dif_pmax_scl_dbl_in, 40),
    expect_dif_pmax_vec_typ = pmax(dif_pmax_vec_in, 40.0),
    expect_dif_pmax_dervec_typ = pmax(dif_pmax_vec_in * 2, 40.0),
    expect_dif_pmax_vec_dbl = pmax(dif_pmax_vec_in, 30.0),
    expect_dif_pmax_vec_int = pmax(dif_pmax_vec_in, 60L),
    expect_dif_pmax_vec_vec = pmax(dif_pmax_vec_in, dif_pmax_vec_vec_max),
    end = NULL )

dif_pmax_arr_in <- array(runif(10, 0, 9), dim = c(3, 3))
dif_pmax_arr_vec_max <- runif(3, 0, 10) * 10
actions[['dif_pmax_arr']] <- g3_formula(
    {
        expect_dif_pmax_arr_typ <- dif_pmax(dif_pmax_arr_in, dif_pmax_arr_typ_max, 1e5)
        expect_dif_pmax_dearr_typ <- dif_pmax(dif_pmax_arr_in * 2, dif_pmax_arr_typ_max, 1e5)
        expect_dif_pmax_arr_dbl <- dif_pmax(dif_pmax_arr_in, 30.0, 1e5)
        expect_dif_pmax_arr_int <- dif_pmax(dif_pmax_arr_in, dif_pmax_arr_int_max, 1e5)
        expect_dif_pmax_arr_vec <- dif_pmax(dif_pmax_arr_in, dif_pmax_arr_vec_max, 1e5)
    },
    dif_pmax_arr_in = dif_pmax_arr_in,
    dif_pmax_arr_typ_max = 40.0,
    dif_pmax_arr_int_max = 60L,
    dif_pmax_arr_vec_max = dif_pmax_arr_vec_max,
    expect_dif_pmax_arr_typ = pmax(dif_pmax_arr_in, 40.0),
    expect_dif_pmax_dearr_typ = pmax(dif_pmax_arr_in * 2, 40.0),
    expect_dif_pmax_arr_dbl = pmax(dif_pmax_arr_in, 30.0),
    expect_dif_pmax_arr_int = pmax(dif_pmax_arr_in, 60L),
    expect_dif_pmax_arr_vec = pmax(dif_pmax_arr_in, dif_pmax_arr_vec_max),
    end = NULL )

dif_pmin_vec_in <- runif(10, 0, 100)
dif_pmin_vec_vec_max <- runif(10, 0, 10) * 10
actions[['dif_pmin_vec']] <- g3_formula(
    {
        expect_dif_pmin_vec_typ <- dif_pmin(dif_pmin_vec_in, dif_pmin_vec_typ_max, 1e5)
        expect_dif_pmin_vec_dbl <- dif_pmin(dif_pmin_vec_in, 30.0, 1e5)
        expect_dif_pmin_vec_int <- dif_pmin(dif_pmin_vec_in, dif_pmin_vec_int_max, 1e5)
        expect_dif_pmin_vec_vec <- dif_pmin(dif_pmin_vec_in, dif_pmin_vec_vec_max, 1e5)
    },
    dif_pmin_vec_in = dif_pmin_vec_in,
    dif_pmin_vec_typ_max = 40.0,
    dif_pmin_vec_int_max = 60L,
    dif_pmin_vec_vec_max = dif_pmin_vec_vec_max,
    expect_dif_pmin_vec_typ = pmin(dif_pmin_vec_in, 40.0),
    expect_dif_pmin_vec_dbl = pmin(dif_pmin_vec_in, 30.0),
    expect_dif_pmin_vec_int = pmin(dif_pmin_vec_in, 60L),
    expect_dif_pmin_vec_vec = pmin(dif_pmin_vec_in, dif_pmin_vec_vec_max),
    end = NULL )

dif_pmin_arr_in <- array(runif(10, 0, 9), dim = c(3, 3))
dif_pmin_arr_vec_max <- runif(3, 0, 10) * 10
actions[['dif_pmin_arr']] <- g3_formula(
    {
        expect_dif_pmin_arr_typ <- dif_pmin(dif_pmin_arr_in, dif_pmin_arr_typ_max, 1e5)
        expect_dif_pmin_arr_dbl <- dif_pmin(dif_pmin_arr_in, 30.0, 1e5)
        expect_dif_pmin_arr_int <- dif_pmin(dif_pmin_arr_in, dif_pmin_arr_int_max, 1e5)
        expect_dif_pmin_arr_vec <- dif_pmin(dif_pmin_arr_in, dif_pmin_arr_vec_max, 1e5)
    },
    dif_pmin_arr_in = dif_pmin_arr_in,
    dif_pmin_arr_typ_max = 40.0,
    dif_pmin_arr_int_max = 60L,
    dif_pmin_arr_vec_max = dif_pmin_arr_vec_max,
    expect_dif_pmin_arr_typ = pmin(dif_pmin_arr_in, 40.0),
    expect_dif_pmin_arr_dbl = pmin(dif_pmin_arr_in, 30.0),
    expect_dif_pmin_arr_int = pmin(dif_pmin_arr_in, 60L),
    expect_dif_pmin_arr_vec = pmin(dif_pmin_arr_in, dif_pmin_arr_vec_max),
    end = NULL )

dif_pminmax_vec_in <- runif(10, 0, 100)
dif_pminmax_vec_vec_l <- runif(10, 0, 50)
dif_pminmax_vec_vec_u <- 50 + runif(10, 0, 50)
actions[['dif_pminmax_vec']] <- g3_formula(
    {
        expect_dif_pminmax_vec_dbl <- dif_pminmax(dif_pminmax_vec_in, 30.0, 60.0, 1e5)
        expect_dif_pminmax_vec_vec <- dif_pminmax(dif_pminmax_vec_in, dif_pminmax_vec_vec_l, dif_pminmax_vec_vec_u, 1e5)
    },
    dif_pminmax_vec_in = dif_pminmax_vec_in,
    dif_pminmax_vec_vec_l = dif_pminmax_vec_vec_l,
    dif_pminmax_vec_vec_u = dif_pminmax_vec_vec_u,
    expect_dif_pminmax_vec_dbl = pmin(pmax(dif_pminmax_vec_in, 30.0), 60.0),
    expect_dif_pminmax_vec_vec = pmin(pmax(dif_pminmax_vec_in, dif_pminmax_vec_vec_l), dif_pminmax_vec_vec_u),
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

actions[['z']] <- g3_formula({
    comment('done')
    nll <- nll + g3_param('rv')
    return(nll)
}, nll = 0.0)
params$rv <- 0.0

model_fn <- g3_to_r(actions)
model_cpp <- g3_to_tmb(actions)
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
