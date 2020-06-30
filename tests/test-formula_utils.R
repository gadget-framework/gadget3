library(unittest)

for (f in list.files('R', pattern = '*.R', full.names = TRUE)) source(f)  # TODO: library(g3)

deep_ls <- function (env) {
    if (environmentName(env) == "R_EmptyEnv") {
        c()
    } else {
        c(ls(env), deep_ls(parent.env(env)))
    }
}

out_f <- f_concatenate(list(
    ~statement_1,
    ~statement_2,
    ~statement_3))
ok(ut_cmp_equal(rlang::f_rhs(out_f), rlang::f_rhs(~{
    statement_1
    statement_2
    statement_3
})), "f_concatenate")

out_f <- f_concatenate(list(
    ~statement_4,
    ~statement_5,
    ~statement_6), wrap_call = call("while", TRUE))
ok(ut_cmp_equal(rlang::f_rhs(out_f), rlang::f_rhs(~while (TRUE) {
    statement_4
    statement_5
    statement_6
})), "f_concatenate:wrap_call")

out_f <- f_concatenate(list(
    as.formula("~woo()", as.environment(list(woo = 3))),
    as.formula("~yay()", as.environment(list(yay = 4, whoah = 9))),
    as.formula("~wow()", as.environment(list(wow = 5)))))
ok(ut_cmp_identical(
    deep_ls(rlang::f_env(out_f)), 
    c("wow", "whoah", "yay", "woo")), "f_concatenate:environment")

out_f <- f_concatenate(list(
    as.formula("~woo()", as.environment(list(woo = 3))),
    as.formula("~yay()", as.environment(list(yay = 4))),
    as.formula("~wow()", as.environment(list(wow = 5)))), parent = as.environment(list(a=1, b=2, c=3)))
ok(ut_cmp_identical(
    deep_ls(rlang::f_env(out_f)), 
    c("wow", "yay", "woo", "a", "b", "c")), "f_concatenate:parent")
