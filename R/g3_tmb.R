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
            }
            return(deparse(in_call))
        }
        return(paste0("{", paste(vapply(in_call, function (x) cpp_code(x, in_envir, next_indent), character(1)), collapse = ", "), "}"))
    }

    # Ignore formulae tildes
    if (is.formula(in_call)) return(cpp_code(f_rhs(in_call), env, indent))

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

    if (call_name %in% c("g3_data", "g3_param")) {
        # data/params will end up being a variable
        return(cpp_escape_varname(in_call[[2]]))
    }

    if (call_name %in% c("g3_idx")) {
        # Indices are 0-based, subtract 1
        return(paste(cpp_code(in_call[[2]], env, next_indent), "- 1"))
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
            cpp_code(assign_lhs, env, next_indent),  # NB: Should either be a sybol or a subset
            assign_op,
            cpp_code(assign_rhs, env, next_indent)))
    }

    if (call_name == 'if') {
        # Conditional
        return(paste(
            "if (",
            cpp_code(in_call[[2]], env, next_indent),
            ") ",
            cpp_code(in_call[[3]], env, next_indent)))
    }

    if (call_name == 'for') {
        # for-range loop
        # NB: TMB vectors and bundled CPPAD vectors don't support iteration,
        # so this will only work with for-range looping over a constant
        iterator <- cpp_code(in_call[[3]], env, next_indent)
        if (is.numeric(in_call[[3]]) && length(in_call[[3]]) == 1) {
            # A single item won't be an iterator
            iterator <- paste0("{", iterator, "}")
        }
        return(paste(
            "for (auto", in_call[[2]], ":", iterator, ")",
            cpp_code(in_call[[4]], env, next_indent)))
    }

    if (call_name == 'while') {
        # while loop
        return(paste0(
            "while (", cpp_code(in_call[[2]], env, next_indent), ") ",
            cpp_code(in_call[[3]], env, next_indent)))
    }

    if (call_name == '[') {
        # Array subsetting
        out <- paste0(c(in_call[[2]], vapply(rev(tail(in_call, -2)), function (d) {
            d <- as.character(d)
            if (d == "") return("")  # Missing symbol
            return(paste0(".col(", d, ")"))
        }, character(1))), collapse = "")
        hijack(out)
        return(out)
    }

    if (call_name == '[[') {
        # Select value from array
        return(paste(
            cpp_code(in_call[[2]], env, next_indent),
            "(",
            cpp_code(in_call[[3]], env, next_indent),
            ")"))
    }

    if (call_name %in% c('break', 'next')) {
        # Flow-control
        return(call_name)
    }

    if (call_name == "%/%") {
        # Integer division
        return(paste0(
            "((int) ", cpp_code(in_call[[2]], env, next_indent), ")",
            " / ",
            "((int) ", cpp_code(in_call[[3]], env, next_indent), ")"))
    }

    if (call_name == "^") {
        # Power operator
        return(paste0(
            "pow(",
            cpp_code(in_call[[2]], env, next_indent), ", ",
            cpp_code(in_call[[3]], env, next_indent), ")"))
    }

    if (call_name %in% c("-", "+", "*", "/", "==", ">", "<", "%%")) {
        # Infix operators
        if (call_name == "%%") call_name <- "%"

        if (call_name == "-" && length(in_call) == 2) {
            # Negative numeral, e.g.
            return(paste0("-", cpp_code(in_call[[2]], env, next_indent)))
        }
        # TODO: "matrix * matrix" is matrix multiplication, "matrix.array() * matrix.array()" is element wise
        return(paste(
            cpp_code(in_call[[2]], env, next_indent),
            call_name,
            cpp_code(in_call[[3]], env, next_indent)))
    }

    if (call_name %in% c("exp", "log", "seq", "seq_along", "sum")) {
        # TMB-defined or built-in functions
        return(paste0(
            call_name,
            "(",
            paste(vapply(
                call_args,
                function (x) cpp_code(x, env, next_indent),
                character(1)), collapse = ", "),
            ")"))
    }

    if (call_name == "(") {
        return(paste0("(", cpp_code(in_call[[2]], env, next_indent), ")"))
    }

    if (call_name == "length") {
        return(paste0("(", cpp_code(in_call[[2]], env, next_indent), ").size()"))
    }

    if (call_name == "colSums") {
        return(paste0("(", cpp_code(in_call[[2]], env, next_indent), ")", ".colwise().sum()"))
    }

    if (call_name == "comment") {
        return(paste(c("// ", in_call[[2]]), collapse = ""))
    }

    # TODO: This should be an exception.
    return(paste0(c("TODO:", call_name ,":", deparse(in_call)), collapse = ""))
}

g3_compile_tmb <- function(steps) {
    all_steps <- g3_collate(steps)

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
                scope[[var_name]] <- all_defns[[var_name]]$cpp
            }
        }

        # Find any g3_param / g3_data and put it at the top
        # TODO: Not considering type. Work out from example params?
        call_replace(code,
            g3_param = function (x) param_lines[[x[[2]]]] <<- paste0("PARAM(", cpp_escape_varname(x[[2]]), ");"),
            g3_data = function (x) param_lines[[x[[2]]]] <<- paste0("DATA(", cpp_escape_varname(x[[2]]), ");"))

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
                # Just define dimensions
                defn <- cpp_definition(
                    'vector<Type>',
                    paste0(var_name, "(", paste0(dim(var_val), collapse = ","), ")"))
            } else if (is.array(var_val)) {
                # Generate code to define matrix
                defn <- c(
                    cpp_definition('vector<Type>', var_name, cpp_code(var_val, env)),
                    sprintf('%s.resize(%s);', var_name, paste0(dim(var_val), collapse = ", "),
                    ))
            } else if (is.numeric(var_val) || is.character(var_val) || is.logical(var_val)) {
                if (is.integer(var_val)) {
                    cpp_type <- 'int'
                } else if (is.numeric(var_val)) {
                    cpp_type <- 'Type'
                } else {
                    cpp_type <- 'auto'
                }
                if (length(var_val) > 1) {
                    cpp_type <- paste0('vector<', cpp_type, '>')
                }
                # Defined as a literal
                defn <- cpp_definition(cpp_type, var_name, cpp_code(var_val, env))
            } else {
                stop("Don't know how to define ", var_name, " = ", paste(capture.output(str(var_val)), collapse = "\n    "))
            }
            scope[[var_name]] <- defn
        }

        # Merge all groups of lines together
        return(c(param_lines, "", unlist(scope)))
    }

    sprintf("#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
    %s

    %s
}\n", paste(var_defns(f_rhs(all_steps), f_envir(all_steps)), collapse = "\n    "),
      cpp_code(f_rhs(all_steps), f_env(all_steps)))
    # TODO: Attach data to this code string somehow? Wrap whole thing in a model runner R function?
}
