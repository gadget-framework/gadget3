# Replace the target of this formulae with what we really want.
f_lhs <- function (name, f) {
    if (length(f) < 3) {
        # Shunt RHS over one so there's space for LHS
        f[[3]] <- f[[2]]
    }
    f[[2]] <- as.symbol(name)
    return(f)
}

# Display the parse tree of a formulae / language object
parse_tree <- function (o, prefix = "") {
    short_str <- function (x) {
        trimws(capture.output(str(x))[[1]])
    }

    if ("formula" %in% class(o)) {
        writeLines(paste0(prefix, "Formula: "))  # TODO: LHS
        parse_tree(o[[length(o)]], prefix = prefix)
        writeLines(paste0(prefix, "With environment:"))
        writeLines(paste0(
            prefix, " * ",
            ls(attr(o, '.Environment')),
            " = ",
            vapply(ls(attr(o, '.Environment')), short_str, character(1)),
            ""))
    } else if (is.symbol(o) || !is.language(o)) {
        # Write out symbol / number / ... (NB: symbols are also language)
        writeLines(paste0(prefix, short_str(o)))
    } else {
        # Recurse over language object as list
        for (inner in as.list(o)) {
            parse_tree(inner, prefix = paste0(prefix, "| "))
        }
    }
}
# parse_tree(~{for (x in c(1,2,3)) {str(x) ; str(x+1)} })
