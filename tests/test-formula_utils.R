library(unittest)

library(gadget3)

cmp_code <- function(a, b) {
    a <- rlang::f_rhs(a)
    b <- rlang::f_rhs(b)
    attr(a, "srcref") <- NULL
    attr(a, "srcfile") <- NULL
    attr(a, "wholeSrcref") <- NULL
    attr(b, "srcref") <- NULL
    attr(b, "srcfile") <- NULL
    attr(b, "wholeSrcref") <- NULL
    ut_cmp_identical(a, b)
}

deep_ls <- function (env) {
    if (environmentName(env) == "R_EmptyEnv") {
        c()
    } else {
        c(ls(env), deep_ls(parent.env(env)))
    }
}

ok_group("call_replace", {
    call_replace <- gadget3:::call_replace
    ok(ut_cmp_equal(
        call_replace(~ 2 + g3_param("woo"), g3_param = function (x) call('$', as.symbol("data"), x[[2]])),
        ~2 + data$woo), "Manipulated params to call as part of function")

    ok(ut_cmp_equal(
        call_replace(~c(moo, oink, baa), moo = function (x) "oink"),
        ~c("oink", oink, baa)), "Can replace bare symbols")

    ok(ut_cmp_equal(
        call_replace(~c(1, potato(c(2, potato(3), 4))), potato = function (x) call("parsnip", x[2])),
        ~c(1, parsnip(c(2, potato(3), 4)()))), "Make no attempt to recurse implictly---replacement function would have to call_replace too")
})

### f_substitute

ok(cmp_code(
    gadget3:::f_substitute(~x / y, list(y = ~1 + 2)),
    ~x / (1 + 2)), "f_substitute: Auto-bracketed an inline replacement")

ok(cmp_code(
    gadget3:::f_substitute(~x / y, list(y = ~fn(2))),
    ~x / fn(2)), "f_substitute: No extra brackets for a function call")

ok(cmp_code(
    gadget3:::f_substitute(~{x ; a}, list(
        x = as.formula(call("~", call("<-", quote(z), 1))),
        a = as.formula(call("~", call("<-", quote(q), 2))))),
    ~{z <- 1 ; q <- 2}), "f_substitute: No extra brackets for assignment")

### f_concatenate

out_f <- gadget3:::f_concatenate(list(
    ~statement_1,
    ~statement_2,
    ~statement_3))
ok(ut_cmp_equal(rlang::f_rhs(out_f), rlang::f_rhs(~{
    statement_1
    statement_2
    statement_3
})), "f_concatenate")

out_f <- gadget3:::f_concatenate(list(
    ~statement_4,
    ~statement_5,
    ~statement_6), wrap_call = call("while", TRUE))
ok(ut_cmp_equal(rlang::f_rhs(out_f), rlang::f_rhs(~while (TRUE) {
    statement_4
    statement_5
    statement_6
})), "f_concatenate:wrap_call")

out_f <- gadget3:::f_concatenate(list(
    as.formula("~woo()", as.environment(list(woo = 3))),
    as.formula("~yay()", as.environment(list(yay = 4, whoah = 9))),
    as.formula("~wow()", as.environment(list(wow = 5)))))
ok(ut_cmp_identical(
    deep_ls(rlang::f_env(out_f)), 
    c("wow", "whoah", "yay", "woo")), "f_concatenate:environment")

out_f <- gadget3:::f_concatenate(list(
    ~statement_1,
    ~statement_2,
    ~statement_3))
ok(ut_cmp_identical(
    environment(out_f),
    environment()), "f_concatenate:Preserve environment where possible")

out_f <- gadget3:::f_concatenate(list(
    as.formula("~woo()", as.environment(list(woo = 3))),
    as.formula("~yay()", as.environment(list(yay = 4))),
    as.formula("~wow()", as.environment(list(wow = 5)))), parent = as.environment(list(a=1, b=2, c=3)))
ok(ut_cmp_identical(
    deep_ls(rlang::f_env(out_f)), 
    c("wow", "yay", "woo", "a", "b", "c")), "f_concatenate:parent")

ok(ut_cmp_identical(
    gadget3:::f_optimize(~{woo; oink; baa}),
    ~{woo; oink; baa}), "f_optimize: Passed through 3 terms")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~{woo; {}; if (TRUE) {oink}; baa}),
    ~{woo; oink; baa}), "f_optimize: Oink's if statement removed")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~{woo; {}; if (!FALSE) {oink}; baa}),
    ~{woo; oink; baa}), "f_optimize: Oink's if statement removed")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~{woo; {}; if (!TRUE) {oink} else { plonk }; baa}),
    ~{woo; plonk; baa}), "f_optimize: Oink's if statement used else part")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~{woo; { x ; {}}; if (TRUE) {oink}; baa}),
    ~{woo; x ; oink; baa}), "f_optimize: Brackets normalised")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~if (x) {woo}),
    ~if (x) woo), "f_optimize: Regular if statement passed through")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~x * 1 + y),
    ~x + y), "f_optimize: Recurse through arithmetic")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~y * 0 + 0 * z + 99),
    ~y * 0 + 0 * z + 99), "f_optimize: Multiplication by zero *doesn't* cancel out")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~0 + x + 0),
    ~x), "f_optimize: Recurse through arithmetic")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~x + 0 + x),
    ~x + x), "f_optimize: Recurse through arithmetic")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~y / 1 + 1 / z),
    ~y + 1/z), "f_optimize: Division only works one way")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~-x),
    ~-x), "f_optimize: Can still negate values")

ok(ut_cmp_identical(
    gadget3:::f_optimize(~x + ((x) + (0))),
    ~x + x), "f_optimize: Recurse through brackets, remove from symbols")

ok(ut_cmp_identical(
    gadget3:::f_optimize(~x + ((x + 4))),
    ~x + (x + 4)), "f_optimize: Double brackets removed")

ok(ut_cmp_identical(
    gadget3:::f_optimize(~x + (x + (f(4))  )),
    ~x + (x + f(4))), "f_optimize: Functions don't need brackets")

ok(ut_cmp_identical(
    gadget3:::f_optimize(~{x <- (2 + 2) ; y <- (4 + 4) * 6}),
    ~{x <- 2 + 2 ; y <- (4 + 4) * 6}), "f_optimize: Remove outer brackets from definition")

ok(ut_cmp_identical(
    gadget3:::f_optimize(~g3_with(x := 4, if (x == 2) moo)),
    ~g3_with(x := 4, if (x == 2) moo)), "f_optimize: No g3_with change when if is dependent")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~g3_with(x := 4, if (y == 2) moo)),
    ~if (y == 2) g3_with(x := 4, moo)), "f_optimize: Swapped g3_with/if when independent")
ok(ut_cmp_identical(
    gadget3:::f_optimize(~g3_with(x := 4, if (y == 2) moo else oink)),
    ~g3_with(x := 4, if (y == 2) moo else oink)), "f_optimize: Don't bother swapping if/else")
