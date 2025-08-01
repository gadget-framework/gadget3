library(unittest)

library(gadget3)

read.matrix <- function (x, ...) unname(as.matrix(read.table(text=x, ...)))

teststock <- g3_stock('teststock', seq(10, 35, 5))

ok_group("g3a_grow_lengthvbsimple", {
    # Run g3a_grow_lengthvbsimple for given linf and midlen
    lvs <- function (linf, kappa = 100) {
        as.numeric(g3_eval(
            g3a_grow_lengthvbsimple(linf, kappa),
            stock = g3_stock("s", seq(10, 100, 10) - 5),
            cur_step_size = 1))
    }
    ok(ut_cmp_equal(
        lvs(linf = 150), 
        c(140, 130, 120, 110, 100, 90, 80, 70, 60, 50)), "linf = 150")
    ok(ut_cmp_equal(
        lvs(linf = 55), 
        c(45, 35, 25, 15, 5, 0, 0, 0, 0, 0), tolerance = 1e-7), "linf = 55 (linf doesn't fall negative)")
})

ok_group("g3a_grow_impl_bbinom:zero-growth", {
    # Run g3a_grow_impl_bbinom for given linf and midlen
    gim <- function (linf, kappa = 20, beta = 0.5, maxlengthgroupgrowth = 10) {
        g3_eval(
            g3a_grow_impl_bbinom(g3a_grow_lengthvbsimple(linf, kappa), ~0, beta, maxlengthgroupgrowth)$len,
            stock = g3_stock("s", seq(10, 100, 10) - 5),
            cur_step_size = 1)
    }

    ok(ut_cmp_equal(round(gim(linf = 120, maxlengthgroupgrowth = 5), 2), read.matrix('
        8.29  8.44  0.40  0.17 0.12 0.15
        9.00 10.00  0.00  0.00 0.00 0.00
       11.43 14.29  1.02  0.36 0.22 0.26
       26.56 39.35  7.50  2.00 1.11 1.18
       26.18 50.91 21.82  2.18 0.91 0.82
       21.00 70.00 80.00 32.00 0.00 0.00
        0.00  0.00  0.00  0.00 0.00 1.00
        0.02  0.05  0.08  0.13 0.21 0.51
        0.14  0.12  0.12  0.13 0.17 0.32
        0.37  0.14  0.10  0.10 0.11 0.19
     ')),"linf=120, maxlengthgroupgrowth=5")
     # TODO: rowSums(gim(linf = 120, maxlengthgroupgrowth = 5)) should be 1?

    ok(ut_cmp_equal(round(gim(linf = 40, maxlengthgroupgrowth = 5), 2), read.matrix('
        0.14 0.12 0.12 0.13 0.17 0.32
        0.37 0.14 0.10 0.10 0.11 0.19
        0.66 0.09 0.06 0.05 0.05 0.09
        1.00 0.00 0.00 0.00 0.00 0.00
        1.00 0.00 0.00 0.00 0.00 0.00
        1.00 0.00 0.00 0.00 0.00 0.00
        1.00 0.00 0.00 0.00 0.00 0.00
        1.00 0.00 0.00 0.00 0.00 0.00
        1.00 0.00 0.00 0.00 0.00 0.00
        1.00 0.00 0.00 0.00 0.00 0.00
    ')),"linf=40, maxlengthgroupgrowth=5 - Linf lower than Lmax produces valid output")
    ok(ut_cmp_equal(
        rowSums(gim(linf = 40, maxlengthgroupgrowth = 5)),
        rep(1, 10), tolerance = 1e-2), "linf=40, maxlengthgroupgrowth=5 - Rows sum to 1")
})

ok_group("g3a_grow_impl_bbinom", {
    actions <- list(  # dmu, lengthgrouplen, binn, beta
        g3a_time(2000, 2001, project_years = 0),
        gadget3:::g3a_initialconditions_manual(teststock, ~g3_param_vector("initial", value = c(10, 100, 1000, 1000, 10000, 100000)), ~0 * teststock__minlen),
        g3a_growmature(teststock,
            impl_f = g3a_grow_impl_bbinom(
                delta_len_f = g3a_grow_lengthvbsimple(~g3_param('linf', value = 160), ~g3_param('kappa', value = 90)),
                delta_wgt_f = g3a_grow_weightsimple(~g3_param('walpha', value = 3), ~g3_param('wbeta', value = 2)),
                beta = ~g3_param('beta', value = 30),
                maxlengthgroupgrowth = 4)),
        # NB: Only required for testing
        gadget3:::g3l_test_dummy_likelihood(),
        list(
            "999" = ~{
                REPORT(teststock__growth_l)
                REPORT(teststock__growth_w)
                return(nll)
            }))

    # Compile model
    # NB: Growth not valid, but not testing growth at this point
    model_fn <- g3_to_r(actions, trace = FALSE, strict = FALSE)
    model_cpp <- g3_to_tmb(actions, trace = FALSE, strict = FALSE)

    params <- attr(model_fn, 'parameter_template')
    result <- model_fn(params)
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)

    #writeLines(deparse(r$teststock__growth_l))
    ok(ut_cmp_equal(r$teststock__growth_l, structure(c(12199.8976226175, 9352.23228375502, 7157.83523789257,
    5463.49450549465, 4154.31593595054, 3143.22179218075, 51322.2074676947,
    39560.4630926918, 30458.8733527337, 23399.2087912083, 17917.1342692155,
    13660.121314133, 81087.2009530949, 62860.2639001552, 48695.7187344743,
    37658.1016483506, 29043.7267350904, 22317.4314305221, 57032.8699935755,
    44472.5676572519, 34669.6583623158, 26995.0549450546, 20974.8144063369,
    16247.8860873661, 15068.9788855573, 11821.5345660349, 9275.97774268346,
    7273.66758241751, 5694.90600451131, 4448.35417879727),
        .Dimnames = list(length = c("10:15", "15:20", "20:25", "25:30", "30:35", "35:Inf"), delta = c("0", "1", "2", "3", "4")),
        .Dim = c(length = 6, delta = 5)), tolerance = 1e-5), "Matches baseline")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)

    # Try comparing with a few different inputs
    for (x in 1:10) ok_group("g3a_grow_impl_bbinom random params", {
        params <- attr(model_fn, 'parameter_template')
        params$linf <- runif(1, 100, 200)
        params$kappa <- runif(1, 50, 100)
        params$walpha <- runif(1, 0.1, 3)
        params$wbeta <- runif(1, 0.1, 2)
        params$beta <- runif(1, 10, 30)
        params$initial <- runif(6, 1000, 9000)
        gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
    })
})

ok_group("g3a_growmature", {
    actions <- list(
        g3a_time(2000, 2001, project_years = 0),
        gadget3:::g3a_initialconditions_manual(teststock,
            ~g3_param_vector("initial_num", value = rep(0, 6)),
            ~g3_param_vector("initial_wgt", value = rep(0, 6))),
        g3a_growmature(teststock,
            impl_f = list(
                delta_dim = as.character(0:6),
                len = ~g3_param_array('growth_matrix', value = array(0, dim = c(6,7))),
                wgt = ~g3_param_array('weight_matrix', value = array(0, dim = c(6,7))))),
        list(
            "999" = ~{
                REPORT(teststock__num)
                REPORT(teststock__wgt)
                nll <- nll + g3_param('x', value = 0, optimise = TRUE)
                return(nll)
            }))
    params <- attr(model_fn, 'parameter_template')
    params$initial_num <- c(10, 100, 1000, 1000, 10000, 100000)
    params$initial_wgt <- c(100, 200, 300, 400, 500, 600)
    params$growth_matrix <- array(dim = c(6, 7))
    params$weight_matrix <- array(0, dim = c(6, 7))

    # Compile model
    # NB: Our tests don't preserve counts, so we can't enable strict here
    model_fn <- g3_to_r(actions, trace = FALSE, strict = FALSE)
    model_cpp <- g3_to_tmb(actions, trace = FALSE, strict = FALSE)

    gm <- array(c(
     # 10    15 20   25   30 35
        0, 0.25, 1, 0.5, 0.5, 1,  # 0
        0,    0, 0,   0,   0, 0,  # +1
        1, 0.75, 0,   0,   0, 0,  # +2
        0,    0, 0,   0,   0, 0,  # +3
        0,    0, 0,   0, 0.5, 0,  # +4
        0,    0, 0,   0,   0, 0,  # +5
        0,    0, 0, 0.5,   0, 0), dim = c(6,7))
    params <- attr(model_fn, 'parameter_template')
    params$initial_num <- c(10, 100, 1000, 1000, 10000, 100000)
    params$initial_wgt <- c(100, 200, 300, 400, 500, 600)
    params$growth_matrix <- gm
    params$weight_matrix <- array(0, dim = c(6, 7))
    result <- model_fn(params)
    r <- attributes(result)
    # str(result)
    # str(as.list(r), vec.len = 10000)

    # model_fn <- edit(model_fn)
    result <- model_fn(params)
    ok(ut_cmp_identical(
        as.vector(r$teststock__num),
        c(0, 25, 1010, 575, 5000, 105500)), "Stock individuals have been scaled by matrix, can't escape plus-group")
    ok(ut_cmp_equal(as.vector(r$teststock__wgt), c(
        0 / g3_env$logspace_add(0, 0),
        (200 * 100*0.25) / 25,
        (300 * 1000 + 100 * 10) / 1010,
        (400 * 1000*0.5 + 200 * 100*0.75) / 575,
        (500 * 10000*0.5) / 5000,
        (600 * 100000 + 500 * 10000*0.5 + 400 * 1000*0.5) / 105500,
        NULL), tolerance = 1e-5), "Weight scaled, didn't let weight go to infinity when dividing by zero")

    gadget3:::ut_tmb_r_compare2(model_fn, model_cpp, params)
})
