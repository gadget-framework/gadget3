# Top-level environment for a model, for common functions & definitions to live in
g3_global_env <- new.env(parent = emptyenv())

# Define a function with separate equivalent R and C++ implementations
g3_native <- function(r, cpp) {
    return(structure(list(r = r, cpp = cpp), class = "g3_native"))
}

assign('debugf', g3_native(r = function(...) {
    cat(sprintf(...))
}, cpp = '
   [](const char* format, ...) -> void {
       va_list argptr;
       va_start(argptr, format);
       vprintf(format, argptr);
       va_end(argptr);
   }
'), envir = g3_global_env)
