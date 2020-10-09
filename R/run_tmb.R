# Pair of constants, so code editors don't get confused
open_curly_bracket <- "{"
close_curly_bracket <- "}"

cpp_escape_varname <- function (x) gsub('\\W', '__', x, perl = TRUE)

cpp_code <- function(in_call, in_envir, indent = "\n    ") {
    next_indent <- paste0(indent, "    ")

    if (!is.call(in_call)) {
        # Literals
        if (length(in_call) == 1) {
            if (is.integer(in_call)) {
                return(toString(in_call))
            } else if (is.logical(in_call)) {
                return(if (in_call) 'true' else 'false')
            }
            return(deparse(in_call))
        }
        return(paste0("{", paste(vapply(in_call, function (x) cpp_code(x, in_envir, next_indent), character(1)), collapse = ", "), "}"))
    }

    # Ignore formulae tildes
    if (rlang::is_formula(in_call)) return(cpp_code(rlang::f_rhs(in_call), in_envir, indent))

    call_name <- deparse(in_call[[1]])
    call_args <- tail(in_call, -1)

    if (call_name == open_curly_bracket) {
        # Recurse into code block
        lines <- vapply(call_args, function (x) {
            out <- cpp_code(x, in_envir, next_indent)
            # Add semicolon for any line that needs one
            if (!endsWith(out, close_curly_bracket)) out <- paste0(out, ";")
            return(out)
        }, character(1))
        # Join the result together
        out <- sprintf("{%s%s%s}", next_indent, paste0(lines, collapse = next_indent), indent)
        return(out)
    }

    if (call_name %in% c("g3_param", "g3_param_array", "g3_param_matrix", "g3_param_vector")) {
        # params will end up being a variable
        if (!(is.character(in_call[[2]]) && length(in_call) == 2)) stop("Parameter names need to be a single string, not ", deparse(in_call))
        return(cpp_escape_varname(in_call[[2]]))
    }

    if (call_name %in% c("g3_report")) {
        # A report is a shouty REPORT
        return(paste0("REPORT(", cpp_escape_varname(in_call[[2]]), ")"))
    }

    if (call_name %in% c("g3_idx")) {
        # Indices are 0-based, subtract 1
        if (is.numeric(in_call[[2]])) {
            # Hard-coded integer, so can subtract now (and avoid double conversion)
            return(toString(in_call[[2]] - 1))
        }
        return(paste(cpp_code(in_call[[2]], in_envir, next_indent), "- 1"))
    }

    if (call_name %in% c("g3_with")) {
        # Combine the variable definition with the rest of the code
        return(paste0(
            "{",
            next_indent, "auto ", in_call[[2]], " = ", cpp_code(in_call[[3]], in_envir, next_indent), ";",
            "\n",
            next_indent, cpp_code(in_call[[4]], in_envir, next_indent),
            indent, "}"))
    }

    if (call_name == '<-') {
        # Assignment
        assign_lhs <- in_call[[2]]
        assign_rhs <- in_call[[3]]

        # Are we assigning to an array-like object?
        if (is.call(assign_lhs) && assign_lhs[[1]] == '[') {
            # i.e. there is at least one "missing" in the subset, i.e. we're not going to put a (0) on it
            # and turn it into a scalar
            # TODO: Ideally we share something with "Array subsetting" below, instead of working it out again
            lhs_is_array <- any(vapply(tail(assign_lhs, -2), deparse, character(1)) == "")
        } else if (is.symbol(assign_lhs)) {
            env_defn <- mget(as.character(assign_lhs), envir = in_envir, inherits = TRUE, ifnotfound = list(NA))[[1]]
            lhs_is_array <- is.array(env_defn)
        }

        if (identical(in_call[[3]], 0) && lhs_is_array) {
            # Set to zero
            return(paste0(
                cpp_code(assign_lhs, in_envir, next_indent),
                ".setZero()"))
        }
        if (is.numeric(in_call[[3]]) && length(in_call[[3]]) == 1 && lhs_is_array) {
            # Set array to a const
            # TODO: discover the type of the inner expression, instead of just supporting single values
            return(paste0(
                cpp_code(assign_lhs, in_envir, next_indent),
                ".setConstant(", cpp_code(in_call[[3]], in_envir, next_indent) , ")"))
        }

        # Add += operators if possible
        assign_op <- "="
        if (is.call(assign_rhs) && identical(assign_lhs, assign_rhs[[2]])) {
            # Operating on iself, use a += operation
            assign_op <- paste0(assign_rhs[[1]], "=")
            assign_rhs <- assign_rhs[[3]]
        }

        return(paste(
            cpp_code(assign_lhs, in_envir, next_indent),  # NB: Should either be a sybol or a subset
            assign_op,
            cpp_code(assign_rhs, in_envir, next_indent)))
    }

    if (call_name == 'if' && length(in_call) == 4) {
        # Make sure x has a brace call around it (otherwise our C++ is nonsense)
        embrace <- function (x) {
            if (!is.call(x) || x[[1]] != open_curly_bracket) {
                return(call(open_curly_bracket, x))
            }
            return(x)
        }
        # Conditional w/else
        return(paste0(
            "if (",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ") ",
            cpp_code(embrace(in_call[[3]]), in_envir, indent),
            " else ",
            cpp_code(embrace(in_call[[4]]), in_envir, indent)))
    }

    if (call_name == 'if' && length(in_call) == 3) {
        # Conditional
        return(paste(
            "if (",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ")",
            cpp_code(in_call[[3]], in_envir, indent)))
    }

    if (call_name == 'for' && is.call(call_args[[2]]) && as.character(call_args[[2]][[1]]) == "seq") {
        # for(x in seq(..)) loop, can expressed as a 3-part for loop
        seq_call <- call_args[[2]]
        if (is.null(seq_call$by)) stop("'by' is required in a for(seq(0, 10, by = 1)) loop: ", deparse(in_call)[[1]])
        check_operator <- if (seq_call$by > 0) " <= " else " >= "
        iterate_operator <- if (seq_call$by == 1) "++" else if (seq_call$by == -1) "--" else sprintf(" += %d", seq_call$by)
        return(paste0(
            "for (",
            "auto ", call_args[[1]], " = ", cpp_code(seq_call[[2]], in_envir, next_indent), "; ",
            call_args[[1]], check_operator, cpp_code(seq_call[[3]], in_envir, next_indent), "; ",
            call_args[[1]], iterate_operator, ") ",
            cpp_code(in_call[[4]], in_envir, indent)))
    }

    if (call_name == 'for' && is.call(call_args[[2]]) && as.character(call_args[[2]][[1]]) == "seq_along") {
        # for(x in seq_along(..)) loop, can expressed as a 3-part for loop
        return(paste0(
            "for (",
            "auto ", call_args[[1]], " = 0; ",
            call_args[[1]], " < ", cpp_code(call("length", call_args[[2]][[2]]), in_envir, next_indent), "; ",
            call_args[[1]], "++) ",
            cpp_code(in_call[[4]], in_envir, indent)))
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
            "for (auto", in_call[[2]], ":", iterator, ")",
            cpp_code(in_call[[4]], in_envir, indent)))
    }

    if (call_name == 'while') {
        # while loop
        return(paste0(
            "while (", cpp_code(in_call[[2]], in_envir, next_indent), ") ",
            cpp_code(in_call[[3]], in_envir, indent)))
    }

    if (call_name == '[') {
        # Array subsetting

        # Thing to array subset, either a symbol or an expression, which we should probably bracket
        subject <- if (is.symbol(in_call[[2]])) in_call[[2]] else paste0(
            "(", cpp_code(in_call[[2]], in_envir, next_indent), ")")

        # Which bits of the subset aren't empty values?
        not_missing <- vapply(tail(in_call, -2), function (d) !identical(as.character(d), ""), logical(1))

        if (all(not_missing)) {
            # Nothing missing i.e a value lookup from vector/array
            return(paste0(
                subject, '(',
                paste(vapply(
                    tail(in_call, -2),
                    function (d) cpp_code(d, in_envir, next_indent),
                    character(1)), collapse = ", "),
                ')'))
        }

        if (!identical(not_missing, sort(not_missing))) {
            # We only have the .col() operator to work with, there isn't a .row()
            stop("Missing values must be at start of subset, can't restructure array: ", deparse(in_call))
        }
        
        # Strip off all required dimensions from array
        out <- paste0(c(subject, vapply(rev(tail(in_call, -2)), function (d) {
            if (identical(as.character(d), "")) {
                # Missing symbol
                return("")
            }
            return(paste0(".col(", cpp_code(d, in_envir, next_indent), ")"))
        }, character(1))), collapse = "")
        return(out)
    }

    if (call_name == '[[') {
        if (is.symbol(in_call[[3]]) && endsWith(as.character(in_call[[3]]), "_idx")) {
            # Already 0-based, nothing to do
            ind <- in_call[[3]]
        } else if (is.numeric(in_call[[3]])) {
            # Indices are 0-based, subtract from value
            ind <- in_call[[3]] - 1
        } else {
            # Add a subtract-1 operator
            ind <- call("-", in_call[[3]], 1)
        }
        return(paste(
            cpp_code(in_call[[2]], in_envir, next_indent),
            "(",
            cpp_code(ind, in_envir, next_indent),
            ")"))
    }

    if (call_name %in% c('break', 'next')) {
        # Flow-control
        return(call_name)
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
        return(paste0(
            "pow(",
            cpp_code(in_call[[2]], in_envir, next_indent), ", ",
            # NB: exponent needs to be (Type), regular (int) isn't allowed
            "(Type)", cpp_code(in_call[[3]], in_envir, next_indent), ")"))
    }

    if (call_name == "%*%") {
        # (matrix) multiplication - cast what should be arrays into matrices
        to_matrix <- function (x) {
            inner <- cpp_code(x, in_envir, next_indent)
            if (is.symbol(x)) {
                # If we're multiplying a symbol already defined as a sparse matrix, no need for .matrix()
                env_defn <- mget(as.character(x), envir = in_envir, inherits = TRUE, ifnotfound = list(NA))[[1]]
                if (is(env_defn, 'sparseMatrix')) {
                    return(inner)
                }
                return(paste0(inner, '.matrix()'))
            }
            return(paste0('(', inner, ').matrix()'))
        }
        return(paste0(to_matrix(in_call[[2]]), " * ", to_matrix(in_call[[3]])))
    }

    if (call_name == "*") {
        # Element-wise multiplication
        if (is.symbol(in_call[[2]])) {
            env_defn <- mget(as.character(in_call[[2]]), envir = in_envir, inherits = TRUE, ifnotfound = list(NA))[[1]]
            if (is(env_defn, 'sparseMatrix')) {
                stop("Don't know how to do cwiseProduct for sparse matrix ", in_call[[2]])
            }
            # NB: Would use .cwiseProduct() for dense matrices
        }
        return(paste0(
            cpp_code(in_call[[2]], in_envir, next_indent),
            "*",
            cpp_code(in_call[[3]], in_envir, next_indent)))
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

    if (call_name %in% c("-", "+", "/", "==", ">", "<", ">=", "<=", "%%", "&&", "||")) {
        # Infix operators
        if (call_name == "%%") call_name <- "%"

        if (call_name == "-" && length(in_call) == 2) {
            # Negative numeral, e.g.
            return(paste0("-", cpp_code(in_call[[2]], in_envir, next_indent)))
        }
        return(paste(
            cpp_code(in_call[[2]], in_envir, next_indent),
            call_name,
            cpp_code(in_call[[3]], in_envir, next_indent)))
    }

    if (call_name == "(") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ")"))
    }

    if (call_name == "length") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ").size()"))
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

    if (call_name %in% c("colSums", "Matrix::colSums")) {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ")", ".colwise().sum()"))
    }

    if (call_name == "rep" && (is.null(names(in_call)[[3]]) || names(in_call)[[3]] == 'times')) {
        # rep(x, times = n)
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ")",
            ".replicate(", cpp_code(in_call[[3]], in_envir, next_indent),", 1)"))
    }

    if (call_name == "comment") {
        return(paste(c("// ", gsub("\n", " ", in_call[[2]], fixed = TRUE)), collapse = ""))
    }

    env_defn <- mget(call_name, envir = in_envir, inherits = TRUE, ifnotfound = list(NA))[[1]]
    if ('g3_native' %in% class(env_defn)) {
        return(paste0(
            call_name,
            "(",
            paste(vapply(call_args, function (a) cpp_code(a, in_envir, next_indent), character(1)), collapse = ", "),
            ")"))
    }

    if (grepl("^[a-zA-Z_][a-zA-Z0-9_]*", call_name)) {
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

g3_to_tmb <- function(actions, trace = FALSE) {
    all_actions <- f_concatenate(g3_collate(actions), parent = g3_global_env, wrap_call = call("while", TRUE))
    model_data <- new.env(parent = emptyenv())
    scope <- list()  # NB: Order is important, can't be an env.
    param_lines <- list()  # NB: Order is important, can't be an env.

    if (isTRUE(trace)) {
        all_actions <- call_replace(all_actions, comment = function (x) {
            # Turn comment calls into Rprintf calls
            return(call("Rprintf", paste0(
                "// ",
                gsub("\n", " ", x[[2]], fixed = TRUE),
                "\n")))
        })
    }

    param_lines_to_cpp <- function (pl) {
        vapply(names(pl), function (n) {
            paste0(
                "PARAMETER",
                if (nzchar(pl[[n]])) paste0("_", pl[[n]]),
                "(", cpp_escape_varname(n), ");")
        }, character(1))
    }

    cpp_definition <- function (cpp_type, cpp_name, cpp_expr) {
        if (missing(cpp_expr)) {
            sprintf("%s %s;", cpp_type, cpp_name)
        } else {
            sprintf("%s %s = %s;", cpp_type, cpp_name, cpp_expr)
        }
    }

    var_defns <- function (code, env) {
        # Find all things that have definitions in our environment
        all_defns <- mget(all.names(code, unique = TRUE), envir = env, inherits = TRUE, ifnotfound = list(NA))
        all_defns <- all_defns[!is.na(all_defns)]

        # Find any native functions used, and add them
        for (var_name in names(all_defns)) {
            if ('g3_native' %in% class(all_defns[[var_name]]) && !(var_name %in% names(scope))) {
                scope[[var_name]] <<- cpp_definition(
                    'auto',
                    var_name,
                    trimws(all_defns[[var_name]]$cpp))
            }
        }

        # Find any g3_param stash it in param_lines
        call_replace(code,
            g3_param_array = function (x) param_lines[[x[[2]]]] <<- "ARRAY",
            g3_param_matrix = function (x) param_lines[[x[[2]]]] <<- "MATRIX",
            g3_param_vector = function (x) param_lines[[x[[2]]]] <<- "VECTOR",
            g3_param = function (x) param_lines[[x[[2]]]] <<- "")

        # TODO: Should this loop be combined with the above?
        for (var_name in all.vars(code)) {
            if (var_name %in% names(scope)) {
                # Already init'ed this, ignore it.
                next
            }
            if (var_name %in% lapply(f_find(code, as.symbol("for")), function (x) { x[[2]] }) ) {
                # It's an iterator
                next
            }
            if (var_name %in% lapply(f_find(code, as.symbol("g3_with")), function (x) { x[[2]] }) ) {
                # It's a with variable
                next
            }
            tryCatch({
                var_val <- get(var_name, envir = env, inherits = TRUE)
            }, error = function (e) {
                lines <- trimws(grep(var_name, deparse(code, width.cutoff = 500), fixed = TRUE, value = TRUE))
                stop(paste(trimws(e), "Used in expression(s):", lines, sep = "\n", collapse = "\n"))
            })

            if (rlang::is_formula(var_val)) {
                var_defns(rlang::f_rhs(var_val), env)
                defn <- cpp_definition('auto', var_name, cpp_code(var_val, env))
            } else if (is.call(var_val)) {
                defn <- cpp_definition('auto', var_name, cpp_code(var_val, env))
            } else if (is(var_val, 'sparseMatrix') && Matrix::nnzero(var_val) == 0) {
                # Define empty sparseMatrix
                defn <- cpp_definition(
                    'Eigen::SparseMatrix<Type>',
                    paste0(var_name, "(", paste0(dim(var_val), collapse = ","), ")"))
            } else if (is.array(var_val) && all(is.na(var_val))) {
                if (length(dim(var_val)) == 1) {
                    # NB: vector isn't just an alias, more goodies are available to the vector class
                    cpp_type <- 'vector<Type>'
                } else {
                    # NB: Otherwise array. We only use matrix<> when we know we want matrix multiplication
                    cpp_type <- 'array<Type>'
                }
                if (all(dim(var_val) == 0)) {
                    defn <- cpp_definition(cpp_type, var_name)
                } else {
                    # Define fixed dimensions
                    defn <- cpp_definition(
                        cpp_type,
                        paste0(var_name, "(", paste0(dim(var_val), collapse = ","), ")"))
                }
            } else if (is.array(var_val) && length(dim(var_val)) > 1) {
                # Store array in model_data
                defn <- paste0('DATA_ARRAY(', var_name , ')')
                assign(var_name, var_val, envir = model_data)
            } else if (is.numeric(var_val) || is.character(var_val) || is.logical(var_val)) {
                if (is.integer(var_val)) {
                    cpp_type <- 'int'
                } else if (is.numeric(var_val)) {
                    cpp_type <- 'Type'
                } else {
                    cpp_type <- 'auto'
                }
                if (length(var_val) > 1 || is.array(var_val)) {
                    # Store in DATA
                    if (cpp_type == 'int') {
                        defn <- paste0('DATA_IVECTOR(', var_name , ')')
                    } else {
                        defn <- paste0('DATA_VECTOR(', var_name , ')')
                    }
                    assign(var_name, var_val, envir = model_data)
                } else {
                    # Define as a literal
                    defn <- cpp_definition(cpp_type, var_name, cpp_code(var_val, env))
                }
            } else {
                stop("Don't know how to define ", var_name, " = ", paste(capture.output(str(var_val)), collapse = "\n    "))
            }
            scope[[var_name]] <<- defn
        }

    }
    # Define all vars, populating param_lines and scope
    var_defns(rlang::f_rhs(all_actions), rlang::f_env(all_actions))

    out <- sprintf("#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() () {
    %s

    %s
    abort();  // Should have returned somewhere in the loop
}\n", paste(c(
        param_lines_to_cpp(param_lines),
        "",
        unlist(scope)), collapse = "\n    "),
      cpp_code(rlang::f_rhs(all_actions), rlang::f_env(all_actions)))
    out <- strsplit(out, "\n")[[1]]
    class(out) <- c("g3_cpp", class(out))

    # Attach data to model as closure
    attr(out, 'model_data') <- model_data
    attr(out, 'parameter_template') <- data.frame(
        switch = names(param_lines),
        type = unlist(param_lines),
        value = I(structure(
            # NB: Has to be a list column because values might be vectors
            as.list(rep(NA, length(param_lines))),
            names = names(param_lines))),
        optimise = if (length(param_lines) > 0) TRUE else logical(0),
        random = if (length(param_lines) > 0) FALSE else logical(0),
        row.names = names(param_lines),
        stringsAsFactors = FALSE)
    # TODO: Still need the below?
    environment(out)$model_parameters <- structure(
        as.list(rep(0, length(param_lines))),
        names = cpp_escape_varname(names(param_lines)))
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
g3_tmb_adfun <- function(cpp_code, parameters = attr(cpp_code, 'parameter_template'), cpp_path = tempfile(fileext=".cpp"), ...) {
    model_params <- attr(cpp_code, 'parameter_template')

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

    tmb_parameters <- structure(
        parameters$value,
        names = cpp_escape_varname(parameters$switch))

    tmb_map <- new.env(parent = emptyenv())
    for (n in parameters[parameters$optimise == FALSE, 'switch']) {
        tmb_map[[cpp_escape_varname(n)]] <- factor(NA)
    }
    tmb_random <- cpp_escape_varname(parameters[parameters$random == TRUE, 'switch'])

    cpp_dll <- gsub('\\.cpp$', '', cpp_path)
    writeLines(cpp_code, con = cpp_path)
    out <- TMB::compile(cpp_path, flags = paste(
        "-std=c++1y",
        "-Wno-ignored-attributes",
        ""))
    dyn.load(TMB::dynlib(cpp_dll))

    obj <- TMB::MakeADFun(
        data = as.list(attr(cpp_code, 'model_data')),
        parameters = tmb_parameters,
        map = as.list(tmb_map),
        random = tmb_random,
        DLL = basename(cpp_dll))
    return(obj)
}

# Turn parameter_template table into a vector for TMB
g3_tmb_par <- function (parameters) {
    unlist(structure(
        list(parameters$value),
        names = cpp_escape_varname(parameters$switch)))
}
