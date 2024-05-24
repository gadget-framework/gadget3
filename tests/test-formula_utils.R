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

cmp_environment <- function (a, b) {
    ordered_list <- function (x) {
        x <- as.list(x)
        # NB: Can't order an empty list
        if (length(x) > 0) x[order(names(x))] else x
    }

    ut_cmp_identical(ordered_list(a), ordered_list(b))
}

deep_ls <- function (env) {
    if (environmentName(env) == "R_EmptyEnv") {
        c()
    } else {
        c(ls(env), deep_ls(parent.env(env)))
    }
}

ok_group("call_to_formula", {
    out <- gadget3:::call_to_formula(quote( x + 2 ), list(x = 9))
    ok(cmp_code(out, ~x + 2), "Turned into formula")
    ok(cmp_environment(environment(out), list(x = 9)), "Used list as environment")
})

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

### g3_formula

ok(cmp_code(
    g3_formula(x + y),
    ~x + y), "g3_formula: Turned raw code into formula")
ok(cmp_code(
    g3_formula(quote( x + y )),
    ~x + y), "g3_formula: Turned quoted code into formula")
ok(cmp_code(
    g3_formula(x),
    ~x), "g3_formula: Turned symbol into formula")
ok(cmp_code(
    g3_formula(2),
    ~2), "g3_formula: Turned constant into formula")

ok(cmp_environment(
    environment(g3_formula( x + y )),
    list()), "g3_formula: Environment of created formula empty by default")

ok(cmp_environment(
    environment(g3_formula( x + y, x = 2 + 2, y = paste0('a', 'b') )),
    list(x = 4, y = 'ab' )), "g3_formula: Environment based on supplied arguments, evaluated versions added")
ok(cmp_environment(
    environment(g3_formula( x + y, list(x = 2 + 2, y = paste0('a', 'b')) )),
    list(x = 4, y = 'ab' )), "g3_formula: Environment can be based on single list")
env <- as.environment(list(x = 2 + 2, y = paste0('a', 'b')))
ok(ut_cmp_identical(
    environment(g3_formula( x + y, env )),
    env), "g3_formula: Environment can be environment, in which case it's referenced")


ok(cmp_code(
    g3_formula(~x + y),
    ~x + y), "g3_formula: Formula still formula")
ok(cmp_environment(
    environment(g3_formula( ~x + y )),
    list()), "g3_formula: Environment of supplied formula replaced")
ok(cmp_environment(
    environment(g3_formula( ~x + y, x = 2 + 2, y = paste0('a', 'b') )),
    list(x = 4, y = 'ab' )), "g3_formula: Environment based on supplied arguments, evaluated versions added")

### f_substitute

out <- gadget3:::f_substitute(quote( x^2 ), list(x = g3_formula(~parrot, parrot = 4)))
ok(cmp_code(out, ~parrot^2), "f_substitute: Substituted formula into call")
ok(cmp_environment(environment(out), list(parrot = 4)), "f_substitute: Resultant formula has parts from replacement")

ok(cmp_code(
    gadget3:::f_substitute(~x / y, list(y = ~1 + 2)),
    ~x / (1 + 2)), "f_substitute: Auto-bracketed an inline replacement")

ok(cmp_code(
    gadget3:::f_substitute(~x / y, list(y = ~1 + 2)),
    ~x / (1 + 2)), "f_substitute: Auto-bracketed an inline replacement")

ok(cmp_code(
    gadget3:::f_substitute(~x / y, list(y = quote( 1 + 2 ))),
    ~x / (1 + 2)), "f_substitute: Auto-bracketed an inline replacement")

ok(cmp_code(
    gadget3:::f_substitute(~x / y, list(y = ~fn(2))),
    ~x / fn(2)), "f_substitute: No extra brackets for a function call")

ok(cmp_code(
    gadget3:::f_substitute(~{x ; a}, list(
        x = as.formula(call("~", call("<-", quote(z), 1))),
        a = as.formula(call("~", call("<-", quote(q), 2))))),
    ~{z <- 1 ; q <- 2}), "f_substitute: No extra brackets for assignment")

### f_chain_conditional

out <- gadget3:::f_chain_conditional(list(g3_formula(x*2, x = 4), ~b), age = c(1,2), area = c(4,5))
ok(cmp_code(out, g3_formula(
    if (area == 4 && age == 1) (x * 2) else if (area == 5 && age == 2) b else NaN
)), "f_chain_conditional: Chained in order")
ok(cmp_environment(environment(out), list(x = 4)), "f_chain_conditional: Definitions from inner formulas in outer env")

out <- gadget3:::f_chain_conditional(list(quote(x), quote(y)), parrot = c(100,200), default = g3_formula(x*2, x = 99))
ok(cmp_code(out, g3_formula(
    if (parrot == 100) x else if (parrot == 200) y else (x * 2)
)), "f_chain_conditional: Can set default value")
ok(cmp_environment(environment(out), list(x = 99)), "f_chain_conditional: Definitions from default in outer env")

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

ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~{woo; oink; baa}),
    ~{woo; oink; baa}), "f_optimize: Passed through 3 terms")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~{woo; {}; if (TRUE) {oink}; baa}),
    ~{woo; oink; baa}), "f_optimize: Oink's if statement removed")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~{woo; {}; if (!FALSE) {oink}; baa}),
    ~{woo; oink; baa}), "f_optimize: Oink's if statement removed")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~{woo; {}; if (!TRUE) {oink} else { plonk }; baa}),
    ~{woo; plonk; baa}), "f_optimize: Oink's if statement used else part")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~{woo; { x ; {}}; if (TRUE) {oink}; baa}),
    ~{woo; x ; oink; baa}), "f_optimize: Brackets normalised")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~if (x) {woo}),
    ~if (x) woo), "f_optimize: Regular if statement passed through")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~if (x > 3) { if (FALSE) 2 }),
    ~{}), "f_optimize:if: Removed statement with pointless output codepaths")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~if (x > 3) { if (FALSE) 2 } else { }),
    ~{}), "f_optimize:if: Removed statement with pointless output codepaths")
ok(ut_cmp_equal(
    gadget3:::f_optimize(~if (x > 3) { if (FALSE) 2 } else { baa }),
    ~if (x > 3) { } else baa), "f_optimize:if: Don't remove if statment if one codepath is nonempty")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~if (x > 3) { 2 } else { if (FALSE) baa }),
    ~if (x > 3) 2), "f_optimize:if: Remove else condition if codepath is empty")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~if ((x > (2 + 0))) { 2 }),
    ~if (x > 2) 2), "f_optimize:if: Remove excess brackets around condition")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~{TRUE && y ; FALSE && y ; x && y ; x && FALSE ; x && TRUE ; x && (TRUE || y) ; (TRUE || x) && y}),
    ~{y ; FALSE ; x && y ; FALSE ; x ; x; y}), "f_optimize:&&")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~{TRUE || y ; FALSE || y ; x || y ; x || FALSE ; x || TRUE ; x || (FALSE && y) ; (FALSE && x) || y}),
    ~{TRUE ; y ; x || y ; x ; TRUE ; x ; y}), "f_optimize:||")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~x * 1 + y),
    ~x + y), "f_optimize: Recurse through arithmetic")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~y * 0 + 0 * z + 99),
    ~y * 0 + 0 * z + 99), "f_optimize: Multiplication by zero *doesn't* cancel out")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~0 + x + 0),
    ~x), "f_optimize: Recurse through arithmetic")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~x + 0 + x),
    ~x + x), "f_optimize: Recurse through arithmetic")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~y / 1 + 1 / z),
    ~y + 1/z), "f_optimize: Division only works one way")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~-x),
    ~-x), "f_optimize: Can still negate values")

ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~x + ((x) + (0))),
    ~x + x), "f_optimize: Recurse through brackets, remove from symbols")

ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~x + ((x + 4))),
    ~x + (x + 4)), "f_optimize: Double brackets removed")

ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~x + (x + (f(4))  )),
    ~x + (x + f(4))), "f_optimize: Functions don't need brackets")

ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~x + (if (y) 2 else 5)),
    ~x + (if (y) 2 else 5)), "f_optimize: If statements do though")

ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~{x <- (2 + 2) ; y <- (4 + 4) * 6}),
    ~{x <- 2 + 2 ; y <- (4 + 4) * 6}), "f_optimize: Remove outer brackets from definition")

ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~g3_with(x := 4, if (x == 2) moo)),
    ~g3_with(x := 4, if (x == 2) moo)), "f_optimize: No g3_with change when if is dependent")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~g3_with(x := 4, if (y == 2) moo)),
    ~if (y == 2) g3_with(x := 4, moo)), "f_optimize: Swapped g3_with/if when independent")
ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~g3_with(x := 4, if (y == 2) moo else oink)),
    ~g3_with(x := 4, if (y == 2) moo else oink)), "f_optimize: Don't bother swapping if/else")

ok(gadget3:::ut_cmp_code(
    gadget3:::f_optimize(~if (x + 0) y / 1),
    ~if (x) y), "f_optimize: Optimize through if condition")

### all_undefined_vars

ok(ut_cmp_identical(
    gadget3:::all_undefined_vars(quote( `pa-rp` * x )),
    c("pa-rp", "x")), "all_undefined_vars: Symbols necessary to escape in R still recognised")
ok(ut_cmp_identical(
    gadget3:::all_undefined_vars(quote( g3_with(`pa-rp` := 1, x) )),
    c("x")), "all_undefined_vars: Symbols necessary to escape in R still recognised")
ok(ut_cmp_identical(
    gadget3:::all_undefined_vars(quote( for(`pa-rp`in 1) x )),
    c("x")), "all_undefined_vars: Symbols necessary to escape in R still recognised")
ok(ut_cmp_identical(
    gadget3:::all_undefined_vars(quote( g3_with(`pa-rp` := 1, `pa-rp`) )),
    as.character(c())), "all_undefined_vars: Symbols necessary to escape in R still recognised")

ok_group("add_dependent_formula") ###########
adf <- function (f, depend_vars = c("block1", "block2")) gadget3:::add_dependent_formula(f, depend_vars)

out <- adf(g3_formula( 4 * x, x = g3_formula(block1**2) ))
ok(gadget3:::ut_cmp_code(out, quote(
    g3_with(`:=`(x, (block1^2)), (4 * x))
), optimize = TRUE), "Included formula mentioning block1")

out <- adf(g3_formula( 4 * x, x = g3_formula(pass1**2) ))


out <- adf(g3_formula(
     4 * total_predsuit + psi,
     total_predsuit = ~block1 + 2 + 3,
     psi = ~total_predsuit ** 5 ))
ok(gadget3:::ut_cmp_code(out, quote(
    g3_with(
        total_predsuit := (block1 + 2 + 3),
        psi := (total_predsuit^5),
        (4 * total_predsuit + psi) )
), optimize = TRUE), "Both total_predsuit & psi included, as psi needs total_predsuit")

########### add_dependent_formula
