# Pair of constants, so code editors don't get confused
open_curly_bracket <- "{"
close_curly_bracket <- "}"

cpp_escape_varname <- function (x) gsub('\\W', '__', x, perl = TRUE)

cpp_code <- function(in_call, in_envir, indent = "\n    ", statement = FALSE, expecting_int = FALSE) {
    next_indent <- paste0(indent, "    ")

    # Make sure x has a brace or eqivalent call around it
    embrace <- function (x) {
        if (!(is.call(x) && (x[[1]] == open_curly_bracket || x[[1]] == as.symbol("g3_with")))) {
            return(call(open_curly_bracket, x))
        }
        return(x)
    }

    # Does this call produce a scalar value?
    value_is_scalar <- function (c_val, fallback = FALSE) {
        # Scalar numeric values are constants
        if (is.numeric(c_val)) return(!is.array(c_val) && !is_force_vector(c_val))

        # Single parameters are constants
        if (is.call(c_val) && c_val[[1]] == 'g3_cpp_asis' && isTRUE(c_val$scalar)) return(TRUE)

        # If a variable, try fetching it out of environment and inspecting that
        if (is.symbol(c_val) && exists(as.character(c_val), envir = in_envir, inherits = TRUE)) {
            env_defn <- get(as.character(c_val), envir = in_envir, inherits = TRUE)
            if (!is.null(attr(env_defn, "g3_global_init_val"))) {
                # When considering a global formula, consider the init condition
                env_defn <- attr(env_defn, "g3_global_init_val")
            }
            return(is.numeric(env_defn) && !is.array(env_defn) && !is_force_vector(env_defn))
        }

        # If array subset, scalar if there are no missing points
        if (is.call(c_val) && identical(c_val[[1]], as.symbol("["))) {
            cols_defined <- vapply(c_val, function (d) !identical(as.character(d), ""), logical(1))
            return(all(cols_defined))
        }

        # Array / vector lookup
        if (is.call(c_val) && identical(c_val[[1]], as.symbol("[["))) {
            return(TRUE)
        }

        # Dunno.
        return(fallback)
    }

    # Ensure (x) is transformed into a matrix
    to_matrix <- function (x) {
        inner <- cpp_code(x, in_envir, next_indent)
        if (is.symbol(x)) {
            return(paste0(inner, '.matrix()'))
        }
        if (is.call(x) && x[[1]] == "diag") {
            # diag() returns a matrix already
            return(inner)
        }
        return(paste0('(', inner, ').matrix()'))
    }

    if (!is.call(in_call)) {
        # Literals
        if (length(in_call) == 1) {
            if (is.integer(in_call)) {
                return(toString(in_call))
            } else if (is.numeric(in_call) && is.nan(in_call)) {
                return("NAN")
            } else if (is.numeric(in_call) && is.infinite(in_call)) {
                return(if (in_call > 0) "R_PosInf" else "R_NegInf")
            } else if (is.numeric(in_call) && !expecting_int) {
                # Force anything numeric to be double, to avoid accidental integer division
                return(paste0("(double)(", toString(in_call) ,")"))
            } else if (is.logical(in_call)) {
                if (is.na(in_call)) stop("No general equivalent to NA in C++")
                return(if (in_call) 'true' else 'false')
            } else if (is.symbol(in_call)) {
                return(cpp_escape_varname(in_call))
            }
            return(deparse(in_call))
        }
        return(paste0("{", paste(vapply(in_call, function (x) cpp_code(x, in_envir, next_indent), character(1)), collapse = ", "), "}"))
    }

    # Ignore formulae tildes
    if (rlang::is_formula(in_call)) return(cpp_code(rlang::f_rhs(in_call), in_envir, indent))

    call_name <- deparse(in_call[[1]])
    call_args <- tail(in_call, -1)

    if (call_name == open_curly_bracket && !statement) {
        if (length(call_args) == 1) {
            # Single-statement, {}, (probably if(a) ( if (b) x else d ) else e), pass through
            return(cpp_code(call_args[[1]], in_envir, next_indent))
        }
        stop("Cannot include code inside expressions: ", deparse1(in_call))
    }

    if (call_name == open_curly_bracket) {
        # Recurse into code block
        lines <- vapply(call_args, function (x) {
            out <- cpp_code(x, in_envir, next_indent, statement = TRUE)
            # Add semicolon for any line that needs one
            if (!endsWith(out, close_curly_bracket)) out <- paste0(out, ";")
            return(out)
        }, character(1))
        # Join the result together
        out <- sprintf("{%s%s%s}", next_indent, paste0(lines, collapse = next_indent), indent)
        return(out)
    }

    if (call_name %in% c("g3_cast_vector")) {
        # Ensure expression is vector type, invariably a bodge somewhere
        return(paste0(
            "(vector<Type>)(",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ")"))
    }

    if (call_name %in% c("g3_cpp_asis")) {
        # Pass through C++ generated elsewhere
        return(in_call[[2]])
    }

    if (call_name %in% c("g3_idx")) {
        # Indices are 0-based, subtract 1
        if (is.numeric(in_call[[2]])) {
            # Hard-coded integer, so can subtract now (and avoid double conversion)
            return(toString(in_call[[2]] - 1))
        }
        return(paste(cpp_code(in_call[[2]], in_envir, next_indent, expecting_int = TRUE), "- 1"))
    }

    if (call_name %in% c("g3_with")) {
        # Combine the variable definition with the rest of the code
        inner <- cpp_code(in_call[[length(in_call)]], in_envir, next_indent, statement = TRUE)
        if (!endsWith(inner, close_curly_bracket)) inner <- paste0(inner, ";")

        return(paste(c(
            "{",
            vapply(g3_with_extract_terms(in_call), function (c) {
                if (length(c[[2]]) > 1) {
                    stop("Malformed g3_with: ", deparse(c))
                }
                rv <- paste0(
                    next_indent,
                    # i.e. so "s <- 0" defaults to Type, not double for g3a_spawn()
                    if (is.numeric(c[[3]]) && !is.integer(c[[3]])) "Type " else "auto ",
                    cpp_escape_varname(c[[2]]), " = ",
                    cpp_code(c[[3]], in_envir, next_indent), ";\n")
            }, character(1)),
            next_indent, inner,
            indent, "}"), collapse = ""))
    }

    if (call_name == '<-') {
        # Assignment
        assign_lhs <- in_call[[2]]
        assign_rhs <- in_call[[3]]

        # Are we assigning to an array-like object?
        if (is.call(assign_lhs) && assign_lhs[[1]] == '[') {
            if (grepl('.transpose()', cpp_code(assign_lhs, in_envir, next_indent))) {
                stop("Can't assign to this subset under TMB (.transpose() isn't by reference): ", deparse1(assign_lhs))
            }
            # i.e. there is at least one "missing" in the subset, i.e. we're not going to put a (0) on it
            # and turn it into a scalar
            # TODO: Ideally we share something with "Array subsetting" below, instead of working it out again
            lhs_is_array <- any(vapply(tail(assign_lhs, -2), function (x) deparse1(x, collapse = ""), character(1)) == "")
        } else if (is.symbol(assign_lhs)) {
            env_defn <- mget(as.character(assign_lhs), envir = in_envir, inherits = TRUE, ifnotfound = list(NA))[[1]]
            lhs_is_array <- is.array(env_defn)
        } else {
            # No idea, but assume not.
            lhs_is_array <- FALSE
        }

        if (identical(in_call[[3]], 0) && lhs_is_array) {
            # Set to zero
            return(paste0(
                cpp_code(assign_lhs, in_envir, next_indent),
                ".setZero()"))
        }

        if (value_is_scalar(in_call[[3]]) && lhs_is_array) {
            # Set array to a const
            return(paste0(
                cpp_code(assign_lhs, in_envir, next_indent),
                ".setConstant(", cpp_code(in_call[[3]], in_envir, next_indent) , ")"))
        }

        # Add += operators if possible
        assign_op <- "="
        if (is.call(assign_rhs)
                && identical(assign_lhs, assign_rhs[[2]])
                && as.character(assign_rhs[[1]]) %in% c("+", "-", "*", "/")
                && length(assign_rhs) == 3) {
            # Operating on iself, use a += operation
            assign_op <- paste0(assign_rhs[[1]], "=")
            assign_rhs <- assign_rhs[[3]]
        }

        return(paste(
            cpp_code(assign_lhs, in_envir, next_indent),  # NB: Should either be a sybol or a subset
            assign_op,
            cpp_code(assign_rhs, in_envir, next_indent)))
    }

    if (call_name == 'if' && !statement) {
        # if statment outside a statement definition, use a tertiary operator
        if (length(in_call) != 4) stop("if expression (not statement) must have an else clause: ", deparse(in_call))
        return(paste0(
            "(",
            cpp_code(in_call[[2]], in_envir, indent),
            " ? ",
            cpp_code(in_call[[3]], in_envir, indent),
            " : ",
            cpp_code(in_call[[4]], in_envir, indent),
            ")"))
    }

    if (call_name == 'if' && length(in_call) == 4) {
        # Conditional w/else
        return(paste0(
            "if (",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ") ",
            cpp_code(embrace(in_call[[3]]), in_envir, indent, statement = TRUE),
            " else ",
            # NB: An else condition has to have braces, otherwise the output C++ is nonsense
            cpp_code(embrace(in_call[[4]]), in_envir, indent, statement = TRUE)))
    }

    if (call_name == 'if' && length(in_call) == 3) {
        # Conditional
        return(paste(
            "if (",
            cpp_code(in_call[[2]], in_envir, next_indent, statement = TRUE),
            ")",
            cpp_code(embrace(in_call[[3]]), in_envir, indent, statement = TRUE)))
    }

    if (call_name == 'for' && is.call(call_args[[2]]) && as.character(call_args[[2]][[1]]) == "seq") {
        # for(x in seq(..)) loop, can expressed as a 3-part for loop
        seq_call <- call_args[[2]]
        if (is.null(seq_call$by)) stop("'by' is required in a for(seq(0, 10, by = 1)) loop: ", deparse(in_call)[[1]])
        by <- eval(seq_call$by, envir = baseenv())  # Convert code (read: quote(-1) ) back to numeric value
        check_operator <- if (by > 0) " <= " else " >= "
        iterate_operator <- if (by == 1) "++" else if (by == -1) "--" else sprintf(" += %d", cpp_code(by, in_envir, next_indent, expecting_int = TRUE))
        return(paste0(
            "for (",
            "auto ", cpp_escape_varname(call_args[[1]]), " = ", cpp_code(seq_call[[2]], in_envir, next_indent, expecting_int = TRUE), "; ",
            cpp_escape_varname(call_args[[1]]), check_operator, cpp_code(seq_call[[3]], in_envir, next_indent, expecting_int = TRUE), "; ",
            cpp_escape_varname(call_args[[1]]), iterate_operator, ") ",
            cpp_code(in_call[[4]], in_envir, indent, statement = TRUE)))
    }

    if (call_name == 'for' && is.call(call_args[[2]]) && as.character(call_args[[2]][[1]]) == "seq_along") {
        # for(x in seq_along(..)) loop, can expressed as a 3-part for loop
        return(paste0(
            "for (",
            "auto ", cpp_escape_varname(call_args[[1]]), " = 0; ",
            cpp_escape_varname(call_args[[1]]), " < ", cpp_code(call("length", call_args[[2]][[2]]), in_envir, next_indent), "; ",
            cpp_escape_varname(call_args[[1]]), "++) ",
            cpp_code(in_call[[4]], in_envir, indent, statement = TRUE)))
    }

    if (call_name == 'for') {
        # for-range loop
        # NB: TMB vectors and bundled CPPAD vectors don't support iteration,
        # so this will only work with for-range looping over a constant
        iterator <- cpp_code(in_call[[3]], in_envir, next_indent)
        if (is.numeric(in_call[[3]]) && length(in_call[[3]]) == 1) {
            # A single item won't be an iterator
            iterator <- paste0("{", iterator, "}")
        }
        return(paste(
            "for (auto", cpp_escape_varname(in_call[[2]]), ":", iterator, ")",
            cpp_code(in_call[[4]], in_envir, indent, statement = TRUE)))
    }

    if (call_name == 'while') {
        # while loop
        return(paste0(
            "while (", cpp_code(in_call[[2]], in_envir, next_indent), ") ",
            cpp_code(in_call[[3]], in_envir, indent, statement = TRUE)))
    }

    if (call_name %in% c("unname")) {
        # Unname is meaningless in TMB, no names in the first place
        return(cpp_code(in_call[[2]], in_envir, next_indent))
    }

    if (call_name == '[') {
        # Array subsetting

        # Recurse through list of subset, converting to method calls as we go.
        convert_subset <- function (cols) {
            cols_defined <- vapply(cols, function (d) !identical(as.character(d), ""), logical(1))

            if (length(cols) == 0) return("")
            if (all(!cols_defined)) return("")

            if (all(cols_defined)) {
                # Nothing missing, i.e. a value lookup
                # NB: As a byproduct this masks the fact that vec.col(x) == vec,
                #     as col() falls back to the useless Eigen definition for vector<Type>.
                #     To get rid of it, we'd also have to use array<Type> even in 1-dim cases
                return(paste0(
                    '(',
                    paste(vapply(
                        cols,
                        function (d) cpp_code(d, in_envir, next_indent, expecting_int = TRUE),
                        character(1)), collapse = ", "),
                    ')'))
            }

            if (tail(cols_defined, 1)) {
                # Final value defined, we can use .col()
                return(paste0(
                    ".col(", cpp_code(tail(cols, 1)[[1]], in_envir, next_indent, expecting_int = TRUE), ")",
                    convert_subset(head(cols, -1))))
            }

            if (cols_defined[[1]]) {
                # Final missing, but first is defined. Turn array around and carry on
                return(paste0(
                    ".transpose()",
                    convert_subset(rev(cols))))
            }

            # Either side missing, nothing we can do.
            stop("Missing values must be at start of subset, can't restructure array: ", deparse(in_call))
        }

        # Thing to array subset, either a symbol or an expression, which we should probably bracket
        subject <- if (is.symbol(in_call[[2]])) cpp_escape_varname(in_call[[2]]) else paste0(
            "(", cpp_code(in_call[[2]], in_envir, next_indent), ")")

        return(paste0(
            subject,
            convert_subset(tail(in_call, -2))))
    }

    if (call_name == '[[') {
        # Convert indices into corresponding C code
        inds <- lapply(tail(in_call, -2), function(x) {
            if (is.symbol(x) && endsWith(as.character(x), "_idx")) {
                # Already 0-based, nothing to do
                ind <- x
            } else if (is.call(x) && identical(x[[1]], quote(g3_idx))) {
                # Don't need to do anything to g3_idx calls
                ind <- x
            } else if (is.numeric(x)) {
                # Indices are 0-based, subtract from value
                ind <- x - 1
            } else {
                # Add a subtract-1 operator
                ind <- call("-", x, 1L)
            }
            return(cpp_code(ind, in_envir, next_indent, expecting_int = TRUE))
        })
        return(paste(
            cpp_code(in_call[[2]], in_envir, next_indent),
            "(", paste(inds, collapse=","), ")"))
    }

    if (call_name %in% c('break')) {
        # Flow-control
        return(call_name)
    }

    if (call_name %in% c('next')) {
        # Flow-control
        return("continue")
    }

    if (call_name %in% c('return')) {
        # Exiting function, no brackets in C++
        return(paste0(
            "return ",
            cpp_code(in_call[[2]], in_envir, next_indent)))
    }

    if (call_name %in% c('stop')) {
        # Fatal error, drop message and treat as abort
        return("abort()")
    }

    if (call_name == "%/%") {
        # Integer division
        return(paste0(
            "((int) ", cpp_code(in_call[[2]], in_envir, next_indent), ")",
            " / ",
            "((int) ", cpp_code(in_call[[3]], in_envir, next_indent), ")"))
    }

    if (call_name == "^") {
        # Power operator
        if ( value_is_scalar(in_call[[2]], fallback = TRUE) ) {
            return(paste0(
                "pow(",
                cpp_code(in_call[[2]], in_envir, next_indent), ", ",
                # NB: exponent needs to be (Type), regular (int) isn't allowed
                "(Type)", cpp_code(in_call[[3]], in_envir, next_indent), ")"))
        }
        # Use .pow(), as "auto x = 10 * pow(vec, (Type)2)" does odd things
        return(paste0(
            "(", cpp_code(in_call[[2]], in_envir, next_indent), ").pow(",
            cpp_code(in_call[[3]], in_envir, next_indent), ")" ))
    }

    if (call_name == "%*%") {
        # (matrix) multiplication - cast what should be arrays into matrices
        # NB: We have to cast back to TMB matrix<Type> for .vec() to be available
        return(paste0("(matrix<Type>)(", to_matrix(in_call[[2]]), " * ", to_matrix(in_call[[3]]), ")"))
    }

    if (call_name == "diag") {
        # diag(vec) - generate diagonal matrix
        return(paste0(
            # NB: asDiagonal returns an eigen construction, not a real matrix<Type> we can multiply
            "(matrix<Type>)(",
            to_matrix(in_call[[2]]),
            ".asDiagonal())" ))
    }

    if (call_name == "*") {
        # Element-wise multiplication
        return(paste0(
            cpp_code(in_call[[2]], in_envir, expecting_int = expecting_int, next_indent),
            "*",
            cpp_code(in_call[[3]], in_envir, expecting_int = expecting_int, next_indent)))
    }

    if (call_name == "pmin") {
        # TODO: First parameter has to be an array, second could be single value, can we enforce this?
        return(paste0(
            "(",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ").cwiseMin(",
            cpp_code(in_call[[3]], in_envir, next_indent),
            ")"))
    }

    if (call_name == "pmax") {
        # TODO: First parameter has to be an array, second could be single value, can we enforce this?
        return(paste0(
            "(",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ").cwiseMax(",
            cpp_code(in_call[[3]], in_envir, next_indent),
            ")"))
    }

    if (call_name == "mean") {
        # TODO: First parameter has to be an array, second could be single value, can we enforce this?
        # TODO: (integers).mean() -> integer, unlike R.
        return(paste0(
            "(",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ").mean()"))
    }

    if (call_name %in% c("min", "max")) {
        # Use std:: versions to replace min/max
        if (length(in_call) != 3) stop(call_name, " expects 2 arguments")
        return(paste0(
            "std::", call_name, "( (Type)",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ", (Type)",
            cpp_code(in_call[[3]], in_envir, next_indent),
            ")"))
    }

    if (call_name %in% c("floor")) {
        # Use std:: versions to replace floor
        if (length(in_call) != 2) stop(call_name, " expects 1 argument")
        return(paste0(
            "std::", call_name, "(",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ")"))
    }

    if (call_name %in% c("abs")) {
        # Use CppAD:: versions to replace abs
        return(paste0(
            "CppAD::", call_name, "(",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ")"))
    }

    if (call_name %in% c("!")) {
        # Unary operators
        return(paste(
            call_name,
            cpp_code(in_call[[2]], in_envir, next_indent, expecting_int = expecting_int)))
    }

    if (call_name %in% c("-", "+", "/", "==", "!=", ">", "<", ">=", "<=", "%%", "&&", "||")) {
        # Infix operators
        if (call_name == "%%") call_name <- "%"

        if (call_name == "-" && length(in_call) == 2) {
            # Negative numeral, e.g.
            return(paste0("-", cpp_code(in_call[[2]], in_envir, expecting_int = expecting_int, next_indent)))
        }
        return(paste(
            cpp_code(in_call[[2]], in_envir, next_indent, expecting_int = expecting_int),
            call_name,
            cpp_code(in_call[[3]], in_envir, next_indent, expecting_int = expecting_int || (call_name == "==") || (call_name == "!=") || (call_name == "%"))))
    }

    if (call_name == "(") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, expecting_int = expecting_int, next_indent), ")"))
    }

    if (call_name %in% c("is.nan")) {
        if ( value_is_scalar(in_call[[2]]) ) {
            # Use std::isnan for single values, otherwise assume array and use Eigen method.
            return(paste0(
                "std::isnan(asDouble(",
                cpp_code(in_call[[2]], in_envir, next_indent),
                "))"))
        }
        return(paste0(
            "(",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ").isNaN()"))
    }

    if (call_name %in% c("is.finite")) {
        if ( value_is_scalar(in_call[[2]]) ) {
            # Use std::isfinite for single values, otherwise assume array and use Eigen method.
            return(paste0(
                "std::isfinite(asDouble(",
                cpp_code(in_call[[2]], in_envir, next_indent),
                "))"))
        }
        return(paste0(
            "(",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ").isFinite()"))
    }

    if (call_name %in% c("all", "any")) {
        return(paste0(
            "(",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ").", call_name, "()"))
    }

    if (call_name == "as.vector") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ").vec()"))
    }

    if (call_name == "length") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ").size()"))
    }

    if (call_name == "t") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ").transpose()"))
    }

    if (call_name == "ncol") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ").cols()"))
    }

    if (call_name == "nrow") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ").rows()"))
    }

    if (call_name == "sum") {
        # NB: TMB has a sum(), but it doesn't work in all cases and this is all it does anyway.
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ").sum()"))
    }

    if (call_name == "prod") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ").prod()"))
    }

    if (call_name %in% c("colSums")) {
        # NB: colwise/rowwise only works on matrices, TMB arrays are 1-dimensional as far as eigen is concerned, so colwise/rowwise produce useless answers
        return(paste0(to_matrix(in_call[[2]]), ".colwise().sum()"))
    }

    if (call_name %in% c("rowSums")) {
        # NB: colwise/rowwise only works on matrices, TMB arrays are 1-dimensional as far as eigen is concerned, so colwise/rowwise produce useless answers
        return(paste0(to_matrix(in_call[[2]]), ".rowwise().sum()"))
    }

    if (call_name == "rep" && (is.null(names(in_call)[[3]]) || names(in_call)[[3]] == 'times')) {
        # rep(x, times = n)
        return(paste0("((vector<Type>)(", cpp_code(in_call[[2]], in_envir, next_indent), "))",
            ".replicate(", cpp_code(in_call[[3]], in_envir, next_indent),", 1)"))
    }

    if (call_name == "comment") {
        return(paste(c("// ", gsub("\n", " ", in_call[[2]], fixed = TRUE)), collapse = ""))
    }

    env_defn <- mget(call_name, envir = in_envir, inherits = TRUE, ifnotfound = list(NA))[[1]]
    if (inherits(env_defn, 'g3_native')) {
        if (is.list(attr(env_defn, 'g3_native_cpp'))) {
            # cpp is a list, so cast all arguments
            if (length(call_args) != length(attr(env_defn, 'g3_native_cpp')) - 1) {
                stop("Expected arguments ", paste(attr(env_defn, 'g3_native_cpp'), collapse = ", "), " in call ", deparse(in_call))
            }
            return(paste0(attr(env_defn, 'g3_native_cpp')[[1]], "(", paste(vapply(seq_along(call_args), function (i) {
                if (is.null(attr(env_defn, 'g3_native_cpp')[[i + 1]])) {
                    # No casting here
                    cpp_code(call_args[[i]], in_envir, next_indent)
                } else {
                    # Add cast to variable definition
                    paste0("(", attr(env_defn, 'g3_native_cpp')[[i + 1]], ")(", cpp_code(call_args[[i]], in_envir, next_indent), ")")
                }
            }, character(1)), collapse = ", "), ")"))
        }
        return(paste0(
            call_name,
            "(",
            paste(vapply(call_args, function (a) cpp_code(a, in_envir, next_indent), character(1)), collapse = ", "),
            ")"))
    }

    if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", call_name)) {
        # If this looks like a function call, assume TMB has defined an equivalent
        # function. e.g. lgamma, exp, log, seq, seq_along, Rprintf, REprintf
        return(paste0(
            call_name,
            "(",
            paste(vapply(
                call_args,
                function (x) cpp_code(x, in_envir, next_indent),
                character(1)), collapse = ", "),
            ")"))
    }

    # Unknown call that can't be a C++ function, give up.
    stop(
        "Cannot translate ", call_name ," from expression: ",
        paste(deparse(in_call), collapse = "\n"))
}

g3_to_tmb <- function(actions, trace = FALSE, strict = FALSE) {
    collated_actions <- g3_collate(actions)
    all_actions <- f_concatenate(c(
        collated_actions,
        NULL), parent = g3_env, wrap_call = call("while", TRUE))
    model_data <- new.env(parent = emptyenv())
    scope <- list()  # NB: Order is important, can't be an env.

    # Reporting disabled by default, but updatable
    model_data$reporting_enabled <- 0
    scope$reporting_enabled <- 'DATA_SCALAR(reporting_enabled); DATA_UPDATE(reporting_enabled);'

    # Enable / disable strict mode & trace mode
    all_actions <- call_replace(all_actions,
        strict_mode = function (x) { !isFALSE(strict) },
        trace_mode = function (x) { !isFALSE(trace) },
        debug_label = function (x) {
            if (trace) call("Rprintf", paste0(x[[2]], "\n")) else call("comment", x[[2]])
        },
        debug_trace = function (x) {
            if (identical(trace, 'full')) call("Rprintf", paste0(x[[2]], "\n")) else call("comment", x[[2]])
        })

    cpp_definition <- function (cpp_type, cpp_name, cpp_expr, dims = NULL) {
        dim_string <- if (is.null(dims)) "" else paste0("(", paste0(dims, collapse = ","), ")")

        if (cpp_type == 'function') {
            if (!startsWith(cpp_expr, '[')) {
               # Full function / polymorphic template function, add with prefix so we can move it in scope_extract()
               return(paste0('__function:', gsub('__fn__', cpp_name, cpp_expr)))
            }
            # Lambda function, include as usual with auto
            cpp_type <- 'auto'
        }

        if (missing(cpp_expr)) {
            sprintf("%s %s%s;", cpp_type, cpp_escape_varname(cpp_name), dim_string)
        } else {
            sprintf("%s %s%s = %s;", cpp_type, cpp_escape_varname(cpp_name), dim_string, cpp_expr)
        }
    }

    scope_split <- function (s) {
        s <- unlist(s)
        if (is.null(s)) return(list(definition = c(), "function" = c()))

        # Split by whether they start with __function
        s <- split(s, startsWith(s, '__function:'))
        list(
            definition = s[['FALSE']],
            "function" = gsub('^__function:', '', s[['TRUE']]))
    }

    var_defns <- function (code, env) {
        # Rework all g3_param calls
        repl_fn <- function(x) {
            # NB: eval() because -1 won't be a symbol
            find_arg <- function (arg_name, def, do_eval = TRUE) {
                if (!(arg_name %in% names(x))) return(def)
                if (do_eval) return(eval(x[[arg_name]], envir = env))
                return(x[[arg_name]])
            }
            if ('optimize' %in% names(x)) stop("g3_param() optimise parameter should be spelt with an s")

            df_template <- function (name, dims = c(1)) {
                # Extract named args from g3_param() call
                value <- find_arg('value', 0)
                optimise <- find_arg('optimise', !find_arg('random', FALSE))  # i.e. default is opposite of random
                random <- find_arg('random', FALSE)
                lower <- as.numeric(find_arg('lower', NA))
                upper <- as.numeric(find_arg('upper', NA))
                parscale <- as.numeric(find_arg('parscale', NA))
                source <- as.character(find_arg('source', as.character(NA)))

                data.frame(
                    switch = name,  # NB: This should be pre-C++ mangling
                    type = if (x[[1]] == "g3_param_array") "ARRAY" else if (x[[1]] == "g3_param_vector") "VECTOR" else "",
                    value = I(structure(
                        # NB: Has to be a list column because values might be vectors
                        list(if (identical(dims, c(1))) value else array(value, dim = dims)),
                        names = name)),
                    optimise = if (dims[[1]] > 0) optimise else logical(0),
                    random = if (dims[[1]] > 0) random else logical(0),
                    lower = if (dims[[1]] > 0) lower else numeric(0),
                    upper = if (dims[[1]] > 0) upper else numeric(0),
                    parscale = if (dims[[1]] > 0) parscale else numeric(0),
                    source = if (dims[[1]] > 0) source else as.character(NA),
                    row.names = name,
                    stringsAsFactors = FALSE)
            }
            if (length(x) < 2 || !is.character(x[[2]])) stop("You must supply a name for the g3_param in ", deparse(x))
            param_name <- cpp_escape_varname(x[[2]])
            if (x[[1]] == 'g3_param_table') {
                ifmissing <- find_arg('ifmissing', NULL, do_eval = FALSE)
                if (rlang::is_formula(ifmissing)) stop("Formula ifmissing not supported")  # Should f_substitute for this to work
                ifmissing <- call_replace(ifmissing,
                    g3_param_table = repl_fn,
                    g3_param = repl_fn)
                pt_name <- cpp_escape_varname(paste0("pt.", x[[2]]))

                # NB: We eval, so they can be defined in-formulae
                df <- eval(x[[3]], envir = env)

                # Turn table into parameter-setting definition, adding individual PARAMETERs as we go
                init_data <- vapply(seq_len(nrow(df)), function (i) {
                    # NB: as.character()ify each item in row, so we get the name in an area factor
                    sub_param_name <- gen_param_tbl_name(as.character(x[[2]]), vapply(df[i,], as.character, character(1)))
                    sub_param_tuple <- paste0(df[i,], collapse = ",")

                    scope[[cpp_escape_varname(sub_param_name)]] <<- structure(
                        sprintf('PARAMETER(%s);', cpp_escape_varname(sub_param_name)),
                        param_template = df_template(sub_param_name))
                    paste0("{std::make_tuple(", sub_param_tuple ,"), &", cpp_escape_varname(sub_param_name), "}")
                }, character(1))

                # Add definition for overall lookup
                scope[[pt_name]] <<- cpp_definition(
                    paste0('std::map<std::tuple<', paste(rep('int', times = ncol(df)), collapse=","), '>, Type*>'),
                    pt_name,
                    paste0("{", paste0(init_data, collapse=", "), "}"))

                if (!is.null(ifmissing)) {
                    return(call("g3_cpp_asis", paste0(
                        "map_extras::at_def(",
                        pt_name, ", ",
                        "std::make_tuple(", paste(names(df), collapse = ","), "), ",
                        "(Type)(", cpp_code(ifmissing, env), ")",
                        ")")))
                } else {
                    # Replace function call to dereference list
                    return(call("g3_cpp_asis", paste0(
                        "map_extras::at_throw(",
                        pt_name, ", ",
                        "std::make_tuple(", paste(names(df), collapse = ","), "), ",
                        '"', x[[2]], '"',
                        ")")))
                }
            }

            if (x[[1]] == 'g3_param_lower' || x[[1]] == 'g3_param_upper') {
                param_name <- paste0(param_name, if (x[[1]] == 'g3_param_lower') "__lower" else "__upper")
                scope[[param_name]] <<- sprintf(
                    "DATA_SCALAR(%s);",
                    param_name )
                # NB: We'll update these later with real values
                model_data[[param_name]] <<- NaN
                # NB: Tell assignment if we're scalar, so it can use is.finite()
                return(call("g3_cpp_asis", param_name, scalar = TRUE))
            }

            # Add PARAMETER definition for variable
            if (x[[1]] != 'g3_param_nodef') {
                scope[[param_name]] <<- structure(sprintf("PARAMETER%s(%s);",
                    if (x[[1]] == 'g3_param_array') '_ARRAY'
                    else if (x[[1]] == 'g3_param_vector') '_VECTOR'
                    else '',
                    param_name), param_template = df_template(x[[2]]))
            }
            # NB: Tell assignment if we're scalar, so it can use setConstant()
            return(call("g3_cpp_asis", param_name, scalar = (x[[1]] == 'g3_param')))
        }
        code <- call_replace(code,
            g3_param_table = repl_fn,
            g3_param_array = repl_fn,
            g3_param_vector = repl_fn,
            g3_param_lower = repl_fn,
            g3_param_upper = repl_fn,
            g3_param_nodef = repl_fn,
            g3_param = repl_fn)

        # Find all things that have definitions in our environment
        all_defns <- mget(all.names(code, unique = TRUE), envir = env, inherits = TRUE, ifnotfound = list(NA))
        all_defns <- all_defns[!is.na(all_defns)]

        # Find any g3_native functions used, and add them
        for (var_name in names(all_defns)) {
            if (inherits(all_defns[[var_name]], 'g3_native')
                    && is.character(attr(all_defns[[var_name]], 'g3_native_cpp'))  # i.e. it's not a native function here
                    && !(var_name %in% names(scope))) {
                var_defns(attr(all_defns[[var_name]], 'g3_native_depends'), env)
                scope[[var_name]] <<- cpp_definition(
                    'function',
                    var_name,
                    trimws(attr(all_defns[[var_name]], 'g3_native_cpp')))
            }
        }

        # TODO: Should this loop be combined with the above?
        for (var_name in all_undefined_vars(code)) {
            if (var_name %in% names(scope)) {
                # Already init'ed this, ignore it.
                next
            }
            var_val <- tryCatch({
                var_val <- get(var_name, envir = env, inherits = TRUE)
                if (!is.null(attr(var_val, "g3_global_init_val"))) {
                    # When considering a global formula, consider the init condition
                    var_val <- attr(var_val, "g3_global_init_val")
                }
                var_val
            }, error = function (e) {
                lines <- trimws(grep(var_name, deparse(code, width.cutoff = 500), fixed = TRUE, value = TRUE))
                warning(paste(trimws(e), "Used in expression(s):", lines, sep = "\n", collapse = "\n"))
                call("stop", "Incomplete model: No definition for ", var_name)
            })

            if (is.call(var_val)) {  # i.e. either bare-code or a formula
                # Recurse, get definitions for formula, considering it's environment as well as the outer one
                if (rlang::is_formula(var_val)) {
                    var_val_code <- var_defns(rlang::f_rhs(var_val), rlang::env_clone(rlang::f_env(var_val), parent = env))
                } else {
                    var_val_code <- var_defns(var_val, new.env(parent = env))
                }
                if (var_name %in% names(scope)) {
                    # var_name got defined as a side-effect of the above (it's a g3_param)
                    # so don't change anything
                    defn <- scope[[var_name]]
                } else {
                    defn <- cpp_definition('auto', cpp_escape_varname(var_name), cpp_code(var_val_code, env))
                }
            } else {
                # Decide base type
                if (all(is.integer(var_val))) {
                    cpp_type <- 'int'
                } else if (is_force_numeric(var_val)) {
                    cpp_type <- 'double'
                } else if (all(is.numeric(var_val)) || all(is.na(var_val))) {
                    # NB: array(NA) isn't numeric, but that's what we probably want
                    cpp_type <- 'Type'
                } else if (all(is.logical(var_val))) {
                    # NB: bool -> int, as array<bool> doesn't REPORT() (tests/test-action_time.R, R/test_utils.R)
                    cpp_type <- 'int'
                } else {
                    stop("Don't know how to define ", var_name, " = ", paste(capture.output(str(var_val)), collapse = "\n    "))
                }
                if (is.array(var_val)) {
                    cpp_type <- paste0('array<', cpp_type, '>')
                } else if (is_force_vector(var_val)) {
                    cpp_type <- paste0('vector<', cpp_type, '>')
                }

                # Add dimensions
                if (!grepl('<', cpp_type, fixed = TRUE)) {
                    # Scalar
                    defn <- cpp_definition(cpp_type, var_name)
                } else if (is.null(dim(var_val))) {
                    # Vector
                    defn <- cpp_definition(cpp_type, var_name, dims = length(var_val))
                } else if (all(dim(var_val) == 0)) {
                    # Zero-dimensioned array
                    defn <- cpp_definition(cpp_type, var_name)
                } else if (!is.null(attr(var_val, 'dynamic_dim'))) {
                    # Define flexible dimensions
                    # Make sure everything within the dynamic dim is defined first
                    var_defns(as.call(c(as.symbol(open_curly_bracket), attr(var_val, 'dynamic_dim'))), env)
                    defn <- cpp_definition(
                        cpp_type,
                        var_name,
                        dims = vapply(
                            attr(var_val, 'dynamic_dim'),
                            function (d) cpp_code(d, env, expecting_int = TRUE),
                            character(1)))
                } else {
                    # Define fixed dimensions
                    defn <- cpp_definition(
                        cpp_type,
                        var_name,
                        dims = dim(var_val))
                }

                # Initialize value
                if ( length(var_val) < 1 || all(is.na(var_val) & !is.nan(var_val)) ) {
                    # Zero-length or NA, so leave uninitialized
                } else if (!grepl('<', cpp_type, fixed = TRUE)) {
                    # Add initialisation to scalars
                    defn <- cpp_definition(cpp_type, var_name, cpp_code(var_val, env))
                } else if ( all(is.nan(var_val)) ) {
                    # Init all-NaN vector (saving is.finite tests later)
                    defn <- paste0(defn, " ", cpp_escape_varname(var_name), ".setConstant(NAN);")
                } else if (any(var_val != var_val[[1]])) {
                    # Store array in model_data
                    defn <- paste0(
                        c('vector<Type>' = 'DATA_VECTOR', 'vector<int>' = 'DATA_IVECTOR',
                          'array<Type>' = 'DATA_ARRAY', 'array<int>' = 'DATA_IARRAY')[[cpp_type]],
                        '(', cpp_escape_varname(var_name) , ')')
                    attr(var_val, "desc") <- NULL  # The desc break's TMB's type detection
                    assign(cpp_escape_varname(var_name), hide_force_vector(var_val), envir = model_data)
                } else if (is.numeric(var_val[[1]]) && var_val[[1]] == 0) {  # NB: FALSE == 0
                    defn <- paste0(defn, " ", cpp_escape_varname(var_name), ".setZero();")
                } else {
                    defn <- paste0(defn, " ", cpp_escape_varname(var_name), ".setConstant(", cpp_code(var_val[[1]], env),");")
                }
            }

            attr(defn, 'report_names') <- names(var_val)
            attr(defn, 'report_dimnames') <- dimnames(var_val)
            scope[[var_name]] <<- defn
        }
        return(code)
    }  # End of var_defns

    # Define all vars, populating scope in process
    all_actions_code <- var_defns(rlang::f_rhs(all_actions), rlang::f_env(all_actions))

    ss <- scope_split(scope)


    out <- sprintf("
#ifndef TYPE_IS_SCALAR
#ifdef TMBAD_FRAMEWORK
#define TYPE_IS_SCALAR(TestT) typename = std::enable_if_t<std::is_same<TestT, int>::value || std::is_same<TestT, double>::value || std::is_same<TestT, TMBad::global::ad_aug>::value>
#endif // TMBAD_FRAMEWORK
#ifdef CPPAD_FRAMEWORK
#define TYPE_IS_SCALAR(TestT) typename = std::enable_if_t<std::is_same<TestT, int>::value || std::is_same<TestT, double>::value || std::is_same<TestT, CppAD::AD>::value>
#endif // CPPAD_FRAMEWORK
#endif // TYPE_IS_SCALAR

%s

template<class Type>
Type objective_function<Type>::operator() () {
    %s

    %s
}\n", paste(ss[['function']], collapse = "\n"), paste(ss$definition, collapse = "\n    "),
      cpp_code(all_actions_code, rlang::f_env(all_actions), statement = TRUE))
    out <- strsplit(out, "\n")[[1]]

    # Include map_extras namespace if we use it
    if (any(grepl("map_extras::", out, fixed = TRUE))) {
        out <- c(strsplit("namespace map_extras {
    // at(), but throw (err) if item isn't available
    template<class Type, class KeyType>
    Type at_throw(std::map<KeyType, Type*> map_in, KeyType key_in, std::string err) {
            try {
                return *map_in.at(key_in);
            } catch (const std::out_of_range&) {
                Rf_warning(\"No value found in g3_param_table %s, ifmissing not specified\", err.c_str());
                return NAN;
            }
    }

    // at(), but return def if item isn't available
    template<class Type, class KeyType>
    Type at_def(std::map<KeyType, Type*> map_in, KeyType key_in, Type def) {
            try {
                return *map_in.at(key_in);
            } catch (const std::out_of_range&) {
                return def;
            }
    }
}", "\n")[[1]], "", out)
    }

    # Make sure we include TMB
    out <- c("#include <TMB.hpp>", "", out)

    class(out) <- c("g3_cpp", class(out))

    attr(out, 'actions') <- actions
    attr(out, 'parameter_template') <- scope_to_parameter_template(scope, 'data.frame')
    attr(out, 'model_data') <- update_data_bounds(model_data, attr(out, 'parameter_template'))
    attr(out, 'report_renames') <- scope_to_cppnamemap(scope)
    attr(out, 'report_names') <- Filter(Negate(is.null), lapply(scope, function (x) attr(x, 'report_names')))
    attr(out, 'report_dimnames') <- Filter(Negate(is.null), lapply(scope, function (x) attr(x, 'report_dimnames')))
    attr(out, 'report_gen_dimnames') <- mget(
        "gen_dimnames",
        envir = rlang::f_env(all_actions),
        ifnotfound = list(NA),
        inherits = TRUE )[[1]]
    # NB: ifnotfound doesn't work with function output
    if (!is.function(attr(out, 'report_gen_dimnames'))) attr(out, 'report_gen_dimnames') <- function (x) 0
    return(out)
}

# cpp source should be edited without deparsing
edit.g3_cpp <- function(name = NULL, file = "", title = NULL, editor = getOption("editor"), ...) {
    if (file == "") {
        file <- tempfile(fileext = ".cpp")
        on.exit(unlink(file))
    }
    writeLines(name, con = file)
    utils::file.edit(file, title = title, editor = editor)
    out <- readLines(file)
    attributes(out) <- attributes(name)
    environment(out) <- environment(name)
    return(out)
}

# Print all lines directly
print.g3_cpp <- function(x, ...) {
    writeLines(x)
    return(invisible(x))
}

# Turn a g3 TMB bit of code into an adfun
# NB: No -flto for windows since it won't(?) work out of the box
#     https://stackoverflow.com/questions/43152633/invalid-register-for-seh-savexmm-in-cygwin
#     -O1, since -O3 is causing unscruitable problems linking as C++
g3_tmb_adfun <- function(
        cpp_code,
        parameters = attr(cpp_code, 'parameter_template'),
        compile_flags = getOption('gadget3.tmb.compile_flags', default =
            if (.Platform$OS.type == "windows") c("-O1", "-march=native")
            else c("-O3", "-flto=auto", "-march=native") ),
        work_dir = getOption('gadget3.tmb.work_dir', default = tempdir()),
        output_script = FALSE,
        compile_args = list(
            framework = getOption("gadget3.tmb.framework", default = "TMBad") ),
        ...) {
    model_params <- attr(cpp_code, 'parameter_template')

    if (!dir.exists(work_dir)) dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)

    # Combine defaults, compile_args$flags & compile_flags together
    compile_args$flags <- paste(c(
        "-std=gnu++17",  # We need C++17 for function templates returning auto
        "-Wno-ignored-attributes",
        "-DEIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS",
        compile_args$flags,
        compile_flags), collapse = " ")

    # If parameters is a list, merge into our data.frames
    if (!is.data.frame(parameters) && is.list(parameters)) {
        tmp_param <- model_params
        tmp_param$value <- I(parameters[model_params$switch])
        parameters <- tmp_param
    }

    # Make sure required columns are there
    stopifnot(
        is.data.frame(parameters),
        'switch' %in% names(parameters),
        'value' %in% names(parameters))

    # At least param should match
    if (!identical(model_params$switch, parameters$switch)) {
        stop("Parameters not in expected order")
    }

    for (i in seq_len(nrow(parameters))) {
        val <- parameters[i, 'value'][[1]]
        if (parameters[i, 'type'] == "ARRAY" && !is.array(val)) stop("Parameter ", parameters[i, 'switch'], " not an array")
        if (parameters[i, 'type'] == "MATRIX" && !is.matrix(val)) stop("Parameter ", parameters[i, 'switch'], " not a matrix")
        # What can we test if parameters[n, 'type'] == "VECTOR"?
        if (parameters[i, 'type'] == "" && length(val) != 1) stop("Parameter ", parameters[i, 'switch'], " should be a single value")
    }

    if (!any(parameters$optimise) && !any(parameters$random)) {
        stop("No parameters with optimise=TRUE or random=TRUE. Set at least one optimisable parameter to run under TMB")
    }

    tmb_parameters <- structure(
        parameters$value,
        names = cpp_escape_varname(parameters$switch))

    # optimise=F & random=F parameters should be added to fixed map.
    tmb_map <- lapply(parameters[parameters$optimise == FALSE & parameters$random == FALSE, 'switch'], function (n) {
        factor(rep(NA, length(parameters[n, 'value'][[1]])))
    })
    names(tmb_map) <- cpp_escape_varname(parameters[parameters$optimise == FALSE & parameters$random == FALSE, 'switch'])

    # optimise=F & random=T are added to list of random effects
    tmb_extras <- list(...)
    if (identical(tmb_extras$type, "Fun")) {
        # retape() assumes that ADFun has been generated if any parameters are random, so force them off
        tmb_random <- NULL
    } else {
        tmb_random <- cpp_escape_varname(parameters[parameters$random == TRUE, 'switch'])
    }

    if (any(parameters$random & parameters$optimise)) {
        stop("Parameters with random=TRUE & optimise=TRUE doesn't make sense: ", paste(
            parameters[parameters$random & parameters$optimise, 'switch'],
            collapse = ","))
    }

    # Name cpp code based on content, so we will recompile/reload if code edit()ed
    base_name <- paste0(
        # NB: We can't allow "." separators in our library name
        "g3", paste(unlist(utils::packageVersion('gadget3')), collapse = "_"), "_",
        "tmb", paste(unlist(utils::packageVersion('TMB')), collapse = "_"), "_",
        digest::sha1(paste0(
            # NB: as.character() strips attributes, so only use the code to define our digest
            as.character(cpp_code),
            vapply(
                # NB: We shouldn't care about argument order
                sort(names(compile_args)),
                function (n) paste0(n, "=", as.character(compile_args[[n]])),
                character(1) ),
            collapse = "" )))
    cpp_path <- paste0(file.path(work_dir, base_name), '.cpp')
    so_path <- TMB::dynlib(file.path(work_dir, base_name))

    # If not loaded yet, compile & load
    if (!any(vapply(getLoadedDLLs(), function (x) x[['path']] == so_path, logical(1)))) {
        writeLines(cpp_code, con = cpp_path)

        # _R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_ (read: in CMD check) will
        # result in a stray symbols.rds being generated and a NOTE. Turn it off.
        prev_symtbl_val <- Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", unset = NA)
        if (!is.na(prev_symtbl_val)) {
            on.exit(Sys.setenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_" = prev_symtbl_val))
        }
        Sys.unsetenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_")

        # Compile this to an equivalently-named .so
        # NB: Mixed slashes seems to result in g++.exe not finding the file(?)
        if (!file.exists(so_path)) do.call(TMB::compile, c(list(gsub("\\\\", "/", cpp_path)), compile_args ))
        dyn.load(so_path)
    }

    # Update any bounds used by the model
    tmb_data <- as.list(update_data_bounds(attr(cpp_code, 'model_data'), parameters))
    if (output_script) {
        tmp_script_path <- tempfile(fileext = ".R")
        tmp_data_path <- paste0(tmp_script_path, "data")
        save(tmb_data, tmb_parameters, tmb_map, tmb_random, file = tmp_data_path)
        writeLines(c(
            "library(TMB)",
            deparse(call("dyn.load", so_path)),
            deparse(call("load", tmp_data_path)),
            "",
            deparse(call("MakeADFun",
                data = tmb_data,
                parameters = quote(tmb_parameters),
                map = quote(tmb_map),
                random = quote(tmb_random),
                DLL = base_name)),
            ""), con = tmp_script_path)
        return(tmp_script_path)
    }
    fn <- TMB::MakeADFun(
        data = tmb_data,
        parameters = tmb_parameters,
        map = tmb_map,
        random = tmb_random,
        DLL = base_name,
        ...)

    report_names <- attr(cpp_code, 'report_names')
    report_dimnames <- attr(cpp_code, 'report_dimnames')
    report_renames <- attr(cpp_code, 'report_renames')

    # Run gen_dimnames & repopulate any dynamic dims
    # TODO: This should be paying attention to the code within, not just assuming that gen_dimnames was used
    dyndims <- attributes(attr(cpp_code, 'report_gen_dimnames')(tmb_parameters))
    for (dimname in names(dyndims)) {
        for (var_name in names(report_dimnames)) {
            if (dimname %in% names(report_dimnames[[var_name]]) && is.null(report_dimnames[[var_name]][[dimname]])) {
                report_dimnames[[var_name]][[dimname]] <- dyndims[[dimname]]
            }
        }
    }

    fn$orig_report <- fn$report
    fn$report <- function (...) {
        old_reporting_enabled <- fn$env$data$reporting_enabled
        fn$env$data$reporting_enabled <- 1
        on.exit(fn$env$data$reporting_enabled <- old_reporting_enabled)
        out <- fn$orig_report(...)
        # Patch report names back again
        for (dimname in names(report_renames)) {
            if (!(dimname %in% names(out))) next

            out[[report_renames[[dimname]]]] <- out[[dimname]]
            out[[dimname]] <- NULL
        }

        # Patch vector names back again
        for (dimname in names(report_names)) {
            if (!(dimname %in% names(out))) next
            names(out[[dimname]]) <- report_names[[dimname]]
        }

        # Patch report dimensions back again
        for (dimname in names(report_dimnames)) {
            if (!(dimname %in% names(out))) next

            # 1-dimension arrays lose their array-ness, restore it.
            if (!is.array(out[[dimname]])) out[[dimname]] <- array(
                out[[dimname]],
                length(out[[dimname]]))

            names(dim(out[[dimname]])) <- names(report_dimnames[[dimname]])
            dimnames(out[[dimname]]) <- report_dimnames[[dimname]]
        }
        return(out)
    }

    # With Type = "Fun", fn$par sometimes isn't set. Bodge around it
    if (is.null(fn$par)) fn$par <- g3_tmb_par(parameters)
    return(fn)
}

# Turn parameter template into vectors of upper/lower bounds
g3_tmb_bound <- function (parameters, bound, include_random = FALSE) {
    # Get all parameters we're thinking of optimising
    p <- parameters[
        (if (include_random) parameters$random else FALSE) |
        parameters$optimise, c('switch', 'value', bound)]

    if (bound == 'value') {
        out <- p$value
    } else {
        # Get the length of all values
        p$val_len <- vapply(p[['value']], length, integer(1))

        # Turn into a list with same dimensions as each value
        out <- lapply(seq_len(nrow(p)), function (i) rep(p[i, bound], p[i, 'val_len']))
    }
    names(out) <- cpp_escape_varname(p$switch)

    # Unlist the result to condense list back to vector
    unlist(out)
}
# NB: include_random = TRUE so you can do things like obj.fn$report(g3_tmb_par())
g3_tmb_par <- function (parameters, include_random = TRUE) {
    call_stack <- vapply(
        sys.calls(),
        function (x) if (is.call(x)) deparse1(x[[1]]) else "",
        character(1))
    if (length(intersect(call_stack, c('nlminb', 'optim', 'stats::nlminb', 'stats::optim'))) > 0) {
        stop("Don't use g3_tmb_par() with nlminb/optim, use obj.fun$par")
    }
    g3_tmb_bound(parameters, 'value', include_random)
}
# NB: include_random = FALSE as optim()/nlminb() won't expect them
g3_tmb_lower <- function (parameters) g3_tmb_bound(parameters, 'lower')
g3_tmb_upper <- function (parameters) g3_tmb_bound(parameters, 'upper')
g3_tmb_parscale <- function (parameters) g3_tmb_bound(parameters, 'parscale')

g3_tmb_relist <- function (parameters, par) {
    # NB: A fit$par won't have numeric identifiers at the end to keep them unique
    cmp_names <- function (a, b) identical(gsub("\\d+$", "", names(a)), gsub("\\d+$", "", names(b)))

    # Compare names both including and discounting random variables
    if (cmp_names(par, g3_tmb_par(parameters, include_random = TRUE))) {
        include_random <- TRUE
    } else if (cmp_names(par, g3_tmb_par(parameters, include_random = FALSE))) {
        include_random <- FALSE
    } else {
        stop("Names of values in par don't match names of parameters$value")
    }

    # Relist based on table's value
    # NB: Subset should match eqivalent operation in g3_tmb_par()
    out <- utils::relist(par, unclass(parameters$value[
        (if (include_random) parameters$random else FALSE) |
        parameters$optimise]))
    # Copy unoptimised parameters from table
    out <- c(parameters$value[!(
        (if (include_random) parameters$random else FALSE) |
        parameters$optimise)], out)
    # Re-order to match template list
    out <- out[names(parameters$value)]
    return(out)
}
