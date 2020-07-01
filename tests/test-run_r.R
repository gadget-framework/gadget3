library(unittest)

library(gadget3)

ok_group('edit.g3_r - Round-trip through an editor results in working code', {
    model_fn <- g3_compile_r(list(~{
        nll <- g3_param('nll')
        return(nll)
    }))
    model_fn_n <- edit(model_fn, editor = '/bin/true')
    attr(model_fn_n, 'srcref') <- NULL
    attr(model_fn_n, 'srcfile') <- NULL
    ok(ut_cmp_identical(
        capture.output(str(model_fn)),
        capture.output(str(model_fn_n))), "str() output of 2 functions is the same")

    n <- runif(1)
    ok(ut_cmp_identical(n, model_fn(list(nll = n))), "Original function works")
    ok(ut_cmp_identical(n, model_fn_n(list(nll = n))), "Edited function works too")
})
