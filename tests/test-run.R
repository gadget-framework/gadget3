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
        ~{}), REPORT = '.'), quote({
        REPORT(x)
        REPORT(y)
    })), "Don't return duplicates, results are sorted")

    ok(cmp_code(gadget3:::action_reports(list(
        ~g3_with(x := 1, z := 2, {y <- 1 ; x <- 2}),
        ~{rr <- 1},
        ~{}), REPORT = '.'), quote({
        REPORT(rr)
        REPORT(y)
    })), "g3_with vars are ignored")

    ok(cmp_code(gadget3:::action_reports(list(
        ~{ p_p <- 1 ; p_q <- 1 ; p_r <- 1},
        ~{ q_p <- 1 ; q_q <- 1 ; q_r <- 1},
        ~{}), p_rep = '^p_', q_rep = '^q_', r_rep = 'r', z_rep = 'z'), quote({
        p_rep(p_p)
        p_rep(p_q)
        p_rep(p_r)
        q_rep(q_p)
        q_rep(q_q)
        q_rep(q_r)
        r_rep(p_r)
        r_rep(q_r)
    })), "Can generate multiple report functions with filters")
})
