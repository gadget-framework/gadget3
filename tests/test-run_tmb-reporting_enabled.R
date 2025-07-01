if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(gadget3)

actions <- list(gadget3:::g3l_test_dummy_likelihood(), g3_formula({
    nll <- g3_param('init_nll', value = 1)
    nll <- nll + (1000 * reporting_enabled)
    REPORT(nll)
    return(nll)
}, nll = 0.0) )

fn <- g3_to_r(actions)
ok(ut_cmp_equal(as.vector(fn()), 1001), "nll: reporting_enabled always 1 under R")

if (nzchar(Sys.getenv('G3_TEST_TMB'))) {
    obj.fn <- g3_tmb_adfun(g3_to_tmb(actions))
    ok(ut_cmp_equal(obj.fn$fn(), 1), "nll: reporting_enabled defaults to 0")
    ok(ut_cmp_equal(obj.fn$report()$nll, 1001), "nll: $report() has enabled reporting_enabled")
    ok(ut_cmp_equal(obj.fn$fn(), 1), "nll: reporting_enabled defaults to 0 afterwards (value reset)")
}
