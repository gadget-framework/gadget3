library(magrittr)
library(unittest)

library(gadget3)

ok_group("cpp_code:subset-assignment", for (allow_break in 1) {
    actions <- list()
    expecteds <- new.env(parent = emptyenv())

    ok(ut_cmp_error({
        invalid_subset <- array(dim = c(2,2))
        g3_precompile_tmb(list(~{invalid_subset[g3_idx(1),] <- 0}))
    }, "invalid_subset"), "Complained when trying to subset by row")

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

    actions <- c(actions, ~{
        comment('done')
        return(g3_param('rv'))
    })
    params <- list(rv=0)

    model_fn <- g3_compile_r(actions)
    # model_fn <- edit(model_fn)
    result <- model_fn(params)

    # Compare everything we've been told to compare
    for (n in ls(expecteds)) {
        ok(ut_cmp_equal(environment(model_fn)$model_report[[n]], expecteds[[n]]), n)
    }

    model_cpp <- g3_precompile_tmb(actions, trace = FALSE)
    if (!nzchar(Sys.getenv('G3_TEST_TMB'))) { writeLines("# skip: not running TMB tests") ; break }
    # model_cpp <- edit(model_cpp)
    model_tmb <- g3_tmb_adfun(model_cpp, params)

    # Compare everything we've been told to compare
    report <- model_tmb$report()
    for (n in ls(expecteds)) {
        ok(ut_cmp_equal(report[[n]], expecteds[[n]]), n)
    }
})
