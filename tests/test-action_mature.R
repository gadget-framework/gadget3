library(magrittr)
library(unittest)

library(gadget3)

ok_group('g3a_mature_constant', {
    cmp_code <- function(a, b) ut_cmp_identical(rlang::f_rhs(a), rlang::f_rhs(b))

    ok(cmp_code(
        g3a_mature_constant(),
        ~1/(1 + exp(0)) ), "No arguments")

    # Invalid combinations
    ok(ut_cmp_error(g3a_mature_constant(alpha = 2), "l50"), "Missing l50")
    ok(ut_cmp_error(g3a_mature_constant(beta = 2), "a50"), "Missing a50")
    ok(ut_cmp_error(g3a_mature_constant(gamma = 2), "k50"), "Missing k50")

    # Single alpha/beta/gamma
    ok(cmp_code(
        g3a_mature_constant(alpha = 28, l50 = 24),
        ~1/(1 + exp(0 - 28 * (stock__meanlen - 24))) ), "alpha = 28, l50 = 24")
    ok(cmp_code(
        g3a_mature_constant(beta = 18, a50 = 83),
        ~1/(1 + exp(0 - 18 * (age - 83))) ), "beta = 18, a50 = 83")
    ok(cmp_code(
        g3a_mature_constant(gamma = 82, k50 = 27),
        ~1/(1 + exp(0 - 82 * (stock__wgt[stock__iter] - 27))) ), "gamma = 82, k50 = 27")

    # Can combine parameters
    ok(cmp_code(
        g3a_mature_constant(alpha = 73, l50 = 16, beta = 54, a50 = 32),
        ~1/(1 + exp(0 - 73 * (stock__meanlen - 16)
                      - 54 * (age - 32)
                      ))), "alpha = 73, l50 = 16, beta = 54, a50 = 32")
    ok(cmp_code(
        g3a_mature_constant(beta = 73, a50 = 67, gamma = 39, k50 = 73),
        ~1/(1 + exp(0 - 73 * (age - 67)
                      - 39 * (stock__wgt[stock__iter] - 73)
                      ))), "beta = 73, a50 = 67, gamma = 39, k50 = 73")

})
