library(TMB)

# Pair of constants, so code editors don't get confused
open_curly_bracket <- "{"
close_curly_bracket <- "}"

cpp_escape_varname <- function (x) gsub('\\W', '__', x, perl = TRUE)

cpp_code <- function(in_call, in_envir, indent = "\n    ") {
    next_indent <- paste0(indent, "  ")

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
    if (is.formula(in_call)) return(cpp_code(f_rhs(in_call), in_envir, indent))

    call_name <- as.character(in_call[[1]])
    call_args <- tail(in_call, -1)

    if (call_name == open_curly_bracket) {
        # Recurse into code block
        lines <- vapply(call_args, function (x) {
            out <- cpp_code(x, in_envir, next_indent)
            # Add semicolon for any line that needs one
            if (!endsWith(out, paste0(close_curly_bracket, "\n"))) out <- paste0(out, ";")
            return(out)
        }, character(1))
        # Join the result together
        out <- sprintf("{%s%s%s}\n", next_indent, paste0(lines, collapse = next_indent), indent)
        return(out)
    }

    if (call_name %in% c("g3_param")) {
        # params will end up being a variable
        return(cpp_escape_varname(in_call[[2]]))
    }

    if (call_name %in% c("g3_idx")) {
        # Indices are 0-based, subtract 1
        return(paste(cpp_code(in_call[[2]], in_envir, next_indent), "- 1"))
    }

    if (call_name == '<-') {
        # Assignment
        assign_lhs <- in_call[[2]]
        assign_rhs <- in_call[[3]]

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

    if (call_name == 'if') {
        # Conditional
        return(paste(
            "if (",
            cpp_code(in_call[[2]], in_envir, next_indent),
            ") ",
            cpp_code(in_call[[3]], in_envir, next_indent)))
    }

    if (call_name == 'for' && is.call(call_args[[2]]) && as.character(call_args[[2]][[1]]) == "seq") {
        # for(x in seq(..)) loop, can expressed as a 3-part for loop
        return(paste0(
            "for (",
            "auto ", call_args[[1]], " = ", call_args[[2]][[2]], "; ",
            call_args[[1]], " <= ", call_args[[2]][[3]], "; ",
            call_args[[1]], "++) ",
            cpp_code(in_call[[4]], in_envir, next_indent)))
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
            cpp_code(in_call[[4]], in_envir, next_indent)))
    }

    if (call_name == 'while') {
        # while loop
        return(paste0(
            "while (", cpp_code(in_call[[2]], in_envir, next_indent), ") ",
            cpp_code(in_call[[3]], in_envir, next_indent)))
    }

    if (call_name == '[') {
        # Array subsetting
        missings <- 0
        out <- paste0(c(in_call[[2]], vapply(rev(tail(in_call, -2)), function (d) {
            d <- as.character(d)
            if (d == "") {
                # Missing symbol
                missings <<- missings + 1
                return("")
            }
            return(paste0(".col(", d, ")"))
        }, character(1))), collapse = "")

        if (missings == 1) {
            # Only one dimension left, cast as vector
            out <- paste0(out, ".vec()")
        }
        return(out)
    }

    if (call_name == '[[') {
        # Select value from array
        return(paste(
            cpp_code(in_call[[2]], in_envir, next_indent),
            "(",
            cpp_code(in_call[[3]], in_envir, next_indent),
            ")"))
    }

    if (call_name %in% c('break', 'next')) {
        # Flow-control
        return(call_name)
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
            cpp_code(in_call[[3]], in_envir, next_indent), ")"))
    }

    if (call_name == "%*%") {
        # (matrix) multiplication - cast what should be arrays into matrices
        return(paste0(
            cpp_code(in_call[[2]], in_envir, next_indent), ".matrix()",
            call_name,
            cpp_code(in_call[[3]], in_envir, next_indent), ".matrix()"))
    }

    if (call_name == "*") {
        # Element-wise multiplication
        return(paste0(
            cpp_code(in_call[[2]], in_envir, next_indent),
            "*",
            cpp_code(in_call[[3]], in_envir, next_indent)))
    }

    if (call_name %in% c("-", "+", "/", "==", ">", "<", "%%")) {
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

    if (call_name %in% c("exp", "log", "seq", "seq_along", "sum")) {
        # TMB-defined or built-in functions
        return(paste0(
            call_name,
            "(",
            paste(vapply(
                call_args,
                function (x) cpp_code(x, in_envir, next_indent),
                character(1)), collapse = ", "),
            ")"))
    }

    if (call_name == "(") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ")"))
    }

    if (call_name == "length") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ").size()"))
    }

    if (call_name == "colSums") {
        return(paste0("(", cpp_code(in_call[[2]], in_envir, next_indent), ")", ".colwise().sum()"))
    }

    if (call_name == "comment") {
        return(paste(c("// ", in_call[[2]]), collapse = ""))
    }

    env_defn <- mget(call_name, envir = in_envir, inherits = TRUE, ifnotfound = list(NA))[[1]]
    if ('g3_native' %in% class(env_defn)) {
        return(paste0(
            call_name,
            "(",
            paste(vapply(call_args, function (a) cpp_code(a, in_envir, next_indent), character(1)), collapse = ", "),
            ")"))
    }

    stop(c("TODO:", call_name ,":", deparse(in_call)))
}

g3_precompile_tmb <- function(steps) {
    all_steps <- g3_collate(steps)
    model_data <- new.env(parent = emptyenv())

    cpp_definition <- function (cpp_type, cpp_name, cpp_expr) {
        if (missing(cpp_expr)) {
            sprintf("%s %s;", cpp_type, cpp_name)
        } else {
            sprintf("%s %s = %s;", cpp_type, cpp_name, cpp_expr)
        }
    }

    var_defns <- function (code, env) {
        scope <- list()
        param_lines <- list()

        # Find all things that have definitions in our environment
        all_defns <- mget(all.names(code, unique = TRUE), envir = env, inherits = TRUE, ifnotfound = list(NA))
        all_defns <- all_defns[!is.na(all_defns)]

        # Find any native functions used, and add them
        for (var_name in names(all_defns)) {
            if ('g3_native' %in% class(all_defns[[var_name]])) {
                scope[[var_name]] <- cpp_definition(
                    'auto',
                    var_name,
                    trimws(all_defns[[var_name]]$cpp))
            }
        }

        # Find any g3_param and put it at the top
        # TODO: Not considering type. Work out from example params?
        call_replace(code,
            g3_param = function (x) param_lines[[x[[2]]]] <<- paste0("PARAMETER(", cpp_escape_varname(x[[2]]), ");"))

        # TODO: Should this loop be combined with the above?
        for (var_name in all.vars(code)) {
            if (var_name %in% scope) {
                # Already init'ed this, ignore it.
                next
            }
            if (var_name %in% lapply(f_find(code, as.symbol("for")), function (x) { x[[2]] }) ) {
                # It's an iterator
                next
            }
            var_val <- get(var_name, env = env, inherits = TRUE)

            if (is.formula(var_val)) {
                scope <- c(scope, var_defns(f_rhs(var_val), env))
                defn <- cpp_definition('auto', var_name, cpp_code(var_val, env))
            } else if (is.call(var_val)) {
                defn <- cpp_definition('auto', var_name, cpp_code(var_val, env))
            } else if (is.array(var_val) && all(is.na(var_val))) {
                if (length(dim(var_val)) == 1) {
                    # NB: vector isn't just an alias, more goodies are available to the vector class
                    cpp_type <- 'vector<Type>'
                } else {
                    # NB: Otherwise array. We only use matrix<> when we know we want matrix multiplication
                    cpp_type <- 'array<Type>'
                }
                # Just define dimensions
                defn <- cpp_definition(
                    cpp_type,
                    paste0(var_name, "(", paste0(dim(var_val), collapse = ","), ")"))
            } else if (is.array(var_val)) {
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
                if (length(var_val) > 1 && cpp_type == 'Type') {
                    # Store in DATA
                    defn <- paste0('DATA_VECTOR(', var_name , ')')
                    assign(var_name, var_val, envir = model_data)
                } else if (length(var_val) > 1 && cpp_type == 'int') {
                    # Store in DATA
                    defn <- paste0('DATA_IVECTOR(', var_name , ')')
                    assign(var_name, var_val, envir = model_data)
                } else {
                    # Define as a literal
                    defn <- cpp_definition(cpp_type, var_name, cpp_code(var_val, env))
                }
            } else {
                stop("Don't know how to define ", var_name, " = ", paste(capture.output(str(var_val)), collapse = "\n    "))
            }
            scope[[var_name]] <- defn
        }

        # Merge all groups of lines together
        return(c(param_lines, "", unlist(scope)))
    }

    out <- sprintf("#include <TMB.hpp>
#include <stdio.h>  // For debugf
#include <stdarg.h>  // For debugf

template<class Type>
Type objective_function<Type>::operator() () {
    %s

    %s
    return 0;  // TODO:
}\n", paste(var_defns(f_rhs(all_steps), f_envir(all_steps)), collapse = "\n    "),
      cpp_code(f_rhs(all_steps), f_envir(all_steps)))

    # Attach data to model as closure
    environment(out) <- new.env(parent = emptyenv())
    assign("model_data", model_data, envir = environment(out))
    return(out)
}

g3_compile_tmb <- function(cpp_code, cpp_path = tempfile(fileext=".cpp")) {
    writeLines(cpp_code, con = cpp_path)
    out <- TMB::compile(cpp_path, "-Wno-ignored-attributes")

    # Attach data to model as closure
    environment(out) <- new.env(parent = emptyenv())
    assign("model_data", model_data, envir = environment(out))
    return(out)
}
