library(unittest)

library(gadget3)

cmp_code <- function(a, b) {
    if (rlang::is_formula(a)) a <- rlang::f_rhs(a)
    if (rlang::is_formula(b)) b <- rlang::f_rhs(b)
    attr(a, "srcref") <- NULL
    attr(a, "srcfile") <- NULL
    attr(a, "wholeSrcref") <- NULL
    attr(b, "srcref") <- NULL
    attr(b, "srcfile") <- NULL
    attr(b, "wholeSrcref") <- NULL
    ut_cmp_identical(a, b)
}

ok_group("action_reports", {
    ok(cmp_code(gadget3:::action_reports(list(
        ~{y <- 1},
        ~{y <- 1 ; x <- 2}
        ~{})), quote({
        g3_report(x)
        g3_report(y)
    })), "Don't return duplicates, results are sorted")

    ok(cmp_code(gadget3:::action_reports(list(
        ~g3_with(x := 1, z := 2, {y <- 1 ; x <- 2}),
        ~{})), quote({
        g3_report(y)
    })), "g3_with vars are ignored")
})