open_bracket <- "("  # )

# formula_utils: Tools for manipulating calls/formula

# Turn a call into a formula, with environment env
call_to_formula <- function (c, env = parent.frame()) {
    formula(call("~", c), env = env)
}

# Merge (additions) into (env)
environment_merge <- function (env, additions, var_names = ls(envir = additions), ignore_overlap = FALSE) {
    for (n in var_names) {
        if (exists(n, envir = additions, inherits = FALSE)) {
            if (!exists(n, envir = env, inherits = FALSE)) {
                assign(n, get(n, envir = additions, inherits = FALSE), envir = env)
            } else if (!ignore_overlap) {
                old <- get(n, envir = env, inherits = FALSE)
                new <- get(n, envir = additions, inherits = FALSE)
                if (!isTRUE(all.equal(old, new))) {
                    # writeLines(unittest::ut_cmp_equal(old, new))
                    warning("Replacing a conflicting definition of ", n)
                }
            }
        }
    }
    return(NULL)
}

# Make a formula with explicitly specified environment
g3_formula <- function (code, ...) {
    f <- sys.call()[[2]]  # Get unevaluated argument

    if (is.call(f) && as.character(f[[1]]) == 'quote') {
        # An already-quoted call, change the quote() to a formula tilde
        f[[1]] <- as.symbol("~")
    } else if (!is.call(f) || f[[1]] != as.symbol('~')) {
        # Not a call or hasn't got the formula tilde, add it
        f <- call("~", f)
    }

    formula(f, env = as.environment(list(...)))
}

# Substitute within formulae, merging all environments together
f_substitute <- function (f, env, copy_all_env = FALSE) {
    explicit_bracket <- function (in_code) {
        # NB: Bracket to avoid operator precedence issues:
        #     substitute(1 - recl / 3, list(recl = quote(4 + 16 / 1)))
        #     has bracketed implicitly and not part of the parse_tree.
        #     The C++ transliterator relies on these bracket nodes to avoid
        #     reinventing operator precedence.
        if (is_infix_call(in_code) && !(as.character(in_code[[1]]) %in% c("<-", "==", "!=", ">", "<", "<=", ">="))) {
            in_code <- call(open_bracket, in_code)
        }
        return(in_code)
    }

    # If not a formula, convert to one with empty environment
    if (!rlang::is_formula(f)) f <- call_to_formula(f, env = emptyenv())
    env <- as.environment(env)
    # Copy f's environment to a new environment, ignore it's parent
    combined_env <- new.env(parent = emptyenv())
    environment_merge(combined_env, rlang::f_env(f))

    # Inspect all substitutions...
    for (n in all.vars(f)) {
        o <- mget(n, envir = env, ifnotfound = list(NULL))[[1]]

        if (is.call(o) && !rlang::is_formula(o)) {  # Bare code (no environment)
            # Add brackets if appropriate
            assign(n, explicit_bracket(o), envir = env)
        } else if (rlang::is_formula(o)) {  # Formula
            # Add brackets if appropriate
            rlang::f_rhs(o) <- explicit_bracket(rlang::f_rhs(o))

            # Replace formulae with the inner expression
            if (length(o) == 3) {
                assign(n, call('<-', o[[2]], o[[3]]), envir = env)
            } else {
                assign(n, o[[2]], envir = env)
            }

            # Combine it's environment with ours
            if (copy_all_env) {
                environment_merge(combined_env, rlang::f_env(o), ignore_overlap = TRUE)
            } else {
                # Only copy things the formulae mentions
                vars_to_copy <- all.names(rlang::f_rhs(o), unique = TRUE)
                environment_merge(combined_env, rlang::f_env(o), var_names = vars_to_copy)
            }
        }
    }

    # Make a substitute call out of our unevaluated formulae
    out <- eval(call("substitute", f, env))
    as.formula(out, env = combined_env)
}
# f_a <- (function () { t <- 3 ; y ~ {x + 2 ; parp} })()
# f_b <- (function () { q <- 2 ; z ~ q * 2 })()
# parse_tree(f_substitute(f_a, list(parp = f_b)))

f_find <- function (f, target_symbol) {
    if (is.call(f)) {
        return(c(
             (if (f[[1]] == target_symbol) list(f) else list()),
             do.call(c, lapply(f, function(x) f_find(x, target_symbol)))))
    }
    return(list())
}
# str(f_find(~ (2+(3+1)) * (4+4), as.symbol("+")))

# Descend through call f, when a symbol like key appears, call it's function to modify the call
call_replace <- function (f, ...) {
    modify_call_fns <- list(...)

    if (is.symbol(f)) {
        # Found a lone symbol, check if that needs translating
        modify_fn <- modify_call_fns[[as.character(f)]]
        if (length(modify_fn) > 0) {
            # TODO: To convert this into modify_fn(...) we need to differentiate
            #       modify_fn(quote(x)) and modify_fn(quote(x())) somehow
            # TODO: Do this using function signatures,
            #       "moo" = function (fn, arg1, arg2, ...) { ... }
            #       "moo" = function (sym) { ... }
            f <- modify_fn(f)
        }

        return(f)
    }

    if (!is.call(f)) return(f)

    # If there's a modify_fn that matches the symbol of this call, call it.
    # NB: Use deparse() to generate useful output for, e.g. Matrix::Matrix
    modify_fn <- modify_call_fns[[deparse(f[[1]])]]
    if (length(modify_fn) > 0) {
        f <- modify_fn(f)
        return(f)
    }

    # Recurse through all elements of this call
    out <- as.call(lapply(f, function (x) call_replace(x, ...)))

    # Put back all attributes (i.e. keep formula-ness)
    attributes(out) <- attributes(f)
    return(out)
}

# Concatenate (list_of_f) into a single call, with (parent) as the parent environment,
# cloning formula environments as necessary
f_concatenate <- function (list_of_f, parent = NULL, wrap_call = NULL) {
    # Formula has same env as first item in list
    has_same_env <- function (f) identical(environment(f), environment(list_of_f[[1]]))

    orig_e <- e <- parent
    for (f in list_of_f) {
        if (is.null(orig_e)) {
            # At top, keeping previous environment, no need to change env.
            orig_e <- e <- environment(f)
        } else if (identical(environment(f), orig_e)) {
            # Environment identical to previous, no need to add anything to the stack
        } else if (identical(parent.env(environment(f)), orig_e)) {
            # Environment different, but has the right parent, so can re-cycle
            orig_e <- e <- environment(f)
        } else {
            # Clone environment and change parent to point at previous
            orig_e <- environment(f)
            e <- rlang::env_clone(environment(f), parent = e)
        }
    }

    # Combine all functions into one expression
    out_call <- as.call(c(list(as.symbol("{")), lapply(unname(list_of_f), rlang::f_rhs)))
    if (!is.null(wrap_call)) {
        # Wrap inner call with outer
        out_call <- as.call(c(
            as.list(wrap_call),
            out_call))
    }
    formula(call("~", out_call), env = e)
}

# Perform optimizations on code within formulae, mostly for readability
f_optimize <- function (f) {
    # Simplify Basic arithmetic
    optim_arithmetic <- function (x) {
        if (!is.call(x) || length(x) != 3) return(x)

        op <- as.character(x[[1]])
        lhs <- f_optimize(x[[2]])
        rhs <- f_optimize(x[[3]])

        # Entirely remove any no-op arithmetic
        noop_value <- if (op == "*" || op == "/") 1 else 0
        if (is.numeric(rhs) && isTRUE(all.equal(rhs, noop_value))) {
            # x (op) 0/1 --> x
            return(lhs)
        }
        if (op %in% c("+", "*") && is.numeric(lhs) && isTRUE(all.equal(lhs, noop_value))) {
            # 0/1 (op) x --> x
            return(rhs)
        }
        # NB: Can't cancel 0 * x, since type information will be lost in the process

        call(op, lhs, rhs)
    }

    call_replace(f,
        "if" = function (x) {
            # Is (x) === quote({})?
            is_empty_brace <- function (x) is.call(x) && length(x) == 1 && x[[1]] == quote({})[[1]]

            if (is.call(x) && ( isTRUE(x[[2]]) || identical(x[[2]], quote(!FALSE)) )) {
                # if(TRUE) exp --> exp
                return(f_optimize(x[[3]]))
            }
            if (is.call(x) && ( isFALSE(x[[2]]) || identical(x[[2]], quote(!TRUE)) )) {
                # if(FALSE) exp else exp_2 --> exp_2
                return (if (length(x) > 3) f_optimize(x[[4]]) else quote({}))
            }
            if (is.call(x) && ( is.call(x[[2]]) && x[[2]][[1]] == as.symbol(open_bracket) )) {
                # if (()) ..., can remove one set of brackets.
                x[[2]] <- x[[2]][[2]]
                return(f_optimize(x))
            }
            # Regular if, descend either side of expression
            x[[2]] <- f_optimize(x[[2]])
            x[[3]] <- f_optimize(x[[3]])
            if (length(x) > 3) {
                x[[4]] <- f_optimize(x[[4]])

                # If else condition is empty, remove it
                if (is_empty_brace(x[[4]])) x[[4]] <- NULL
            }

            # If codepaths out of if are empty, then if statement is pointless
            if (length(x) == 3 && is_empty_brace(x[[3]])) return(quote({}))

            return(x)
        },
        "g3_with" = function (x) {
            g3_with_term_names <- function (y) {
                # Extract term assignments, fetch lhs of each
                vapply(
                    g3_with_extract_terms(y),
                    function (t_call) as.character(t_call[[2]]),
                    character(1))
            }

            if (is.call(x) && is.call(x[[length(x)]]) && x[[length(x)]][[1]] == 'if' && length(x[[length(x)]]) == 3) {
                # g3_with -> if, try and swap around if possible.
                if (length(intersect( all.vars(x[[length(x)]][[2]]), g3_with_term_names(x) )) == 0) {
                    # x becomes inner if statement
                    new_x <- x[[length(x)]]

                    # Wrap statement with our g3_with call
                    g3with_call <- x
                    g3with_call[[length(g3with_call)]] <- new_x[[3]]
                    new_x[[3]] <- g3with_call

                    # Return whole thing for another pass to optimise innards
                    return(new_x)
                }
            }

            # Just recurse
            if (is.call(x)) x <- as.call(lapply(x, f_optimize))
            return(x)
        },
        "<-" = function (x) {
            if (!is.call(x)) return(x)
            if (is.call(x[[3]]) && x[[3]][[1]] == "(") {  # )
                # No point wrapping a definition in braces
                x[[3]] <- x[[3]][[2]]
            }
            x[[3]] <- f_optimize(x[[3]])
            return(x)
        },
        "{" = function (x) {
            if (!is.call(x)) return(x)
            # 1-statement braces just return
            if (length(x) == 2) return(f_optimize(x[[2]]))

            # Flatten any nested braces inside this brace
            as.call(do.call(c, lapply(x, function (part) {
                if (is.call(part)) {
                    # Optimize inner parts first
                    part <- f_optimize(part)
                    # NB: Check for symbol again---could have optimized down to a symbol
                    if (is.call(part) && part[[1]] == "{") {
                        # Nested brace operator, flatten this
                        # NB: "{ }" will be removed as a byproduct, since empty lists will dissapear
                        return(tail(as.list(part), -1))
                    }  # } - Match open brace of call
                }
                return(list(part))
            })))
        }, # } - Match open brace of call
        "(" = function (x) {  # ) - Match open bracket in condition
            if (!is.call(x)) return(x)

            # Optimise innards first
            inner <- f_optimize(x[[2]])

            # Remove brackets from symbols, double-bracked expressions and infix operators
            if (!is.call(inner) || inner[[1]] == open_bracket || (!is_infix_call(inner) && inner[[1]] != as.symbol("if"))) {
                return(inner)
            }

            # Preserve the bracket by default
            return(call(open_bracket, inner))
        },
        "&&" = function (x) {
            if (!is.call(x)) return(x)

            # Optimise innards first
            x[[2]] <- f_optimize(x[[2]])
            x[[3]] <- f_optimize(x[[3]])

            # If either half is TRUE, remove &&
            if (identical(x[[2]], TRUE)) return(x[[3]])
            if (identical(x[[3]], TRUE)) return(x[[2]])

            # If either half is FALSE, entire expression is FALSE
            if (identical(x[[2]], FALSE)) return(FALSE)
            if (identical(x[[3]], FALSE)) return(FALSE)

            return(x)
        },
        "||" = function (x) {
            if (!is.call(x)) return(x)

            # Optimise innards first
            x[[2]] <- f_optimize(x[[2]])
            x[[3]] <- f_optimize(x[[3]])

            # If either half is FALSE, remove ||
            if (identical(x[[2]], FALSE)) return(x[[3]])
            if (identical(x[[3]], FALSE)) return(x[[2]])

            # If either half is TRUE, entire expression is TRUE
            if (identical(x[[2]], TRUE)) return(TRUE)
            if (identical(x[[3]], TRUE)) return(TRUE)

            return(x)
        },
        "+" = optim_arithmetic,
        "-" = optim_arithmetic,
        "*" = optim_arithmetic,
        "/" = optim_arithmetic)
}

# Is (x) a call to an infix operator?
is_infix_call <- function (x) {
    if (!is.call(x)) return(FALSE)

    operator <- as.character(x[[1]])
    # https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Infix-and-prefix-operators
    if (operator %in% c(
            '::',
            '$', '@',
            '^',
            '-', '+',
            ':',
            '%xyz%',
            '*', '/',
            '+', '-',
            '>', '>=', '<', '<=', '==', '!=',
            '!',
            '&', '&&',
            '|', '||',
            '~',
            '->', '->>',
            '<-', '<<-',
            '= ')) return(TRUE)
    return(grepl("^%.*%$", operator, perl = TRUE))
}

# Evaluate rhs of (f), using it's environment and (env_extras), using (env_parent) if one supplied
f_eval <- function (f, env_extras = list(), env_parent = g3_global_env) {
    # NB: Don't alter extras if it is an environment
    env <- as.environment(as.list(env_extras))
    parent.env(env) <- rlang::env_clone(rlang::f_env(f))

    # If supplied, replace the formula's parent env
    # NB: g3 formula objects generally don't have a sensible parent until g3_to_*, so
    #     we use g3_global_env by default for semi-sane behaviour
    if (is.environment(env_parent)) {
        parent.env(parent.env(env)) <- env_parent
    }
    eval(rlang::f_rhs(f), env)
}

# Find all vars, minus vars that are defined within (e.g. iterators)
all_undefined_vars <- function (code) {
    g3_with_extract_term_syms <- function (x) {
        lapply(g3_with_extract_terms(x), function (c) as.character(c[[2]]))
    }

    setdiff(all.vars(code), c(
        lapply(f_find(code, as.symbol("for")), function (x) { as.character(x[[2]]) }),
        do.call(c, lapply(f_find(code, as.symbol("g3_with")), g3_with_extract_term_syms)),
        NULL))
}
