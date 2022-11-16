# debug_utils: Useful tools for environment-based development, not directly used

# Display the parse tree of a formulae / language object
parse_tree <- function (o, prefix = "") {
    short_str <- function (x) {
        trimws(capture.output(str(x))[[1]])
    }

    if (missing(o)) {
        # Missing placeholder
        writeLines(paste0(prefix, " (missing)"))
    } else if (inherits(o, "formula")) {
        writeLines(paste0(prefix, "Formula: ", if (length(o) > 2) o[[2]] else ""))
        parse_tree(o[[length(o)]], prefix = prefix)

        envir <- attr(o, '.Environment')
        while (!(environmentName(envir) %in% c("R_GlobalEnv", "R_EmptyEnv"))) {
            writeLines(paste0(prefix, "With environment:"))
            writeLines(paste0(
                prefix, " * ",
                ls(envir),
                " = ",
                vapply(ls(envir), function (n) short_str(get(n, envir)), character(1)),
                ""))
            envir <- parent.env(envir)
        }
    } else if (is.symbol(o) || !is.language(o)) {
        # Write out symbol / number / ... (NB: symbols are also language)
        writeLines(paste0(prefix, short_str(o)))
    } else {
        # Recurse over language object as list
        for (inner in as.list(o)) {
            parse_tree(inner, prefix = paste0(prefix, if (is.call(o) && mode(o) == "(") "( " else "| "))  # ))
        }
    }
}
# parse_tree(~{for (x in c(1,2,3)) {str(x) ; str(x+1)} })

envir_tree <- function (o) {
    if (rlang::is_formula(o)) return(envir_tree(rlang::f_env(o)))

    out <- as.list(o)
    if (!(is_base_envir(parent.env(o)))) {
        attr(out, '_parent') <- envir_tree(parent.env(o))
    }
    return(out)
}

is_base_envir <- function (env) {
    environmentName(env) %in% c("R_GlobalEnv", "R_EmptyEnv")
}
