# Top-level environment for a model, for common functions & definitions to live in
g3_global_env <- new.env(parent = emptyenv())

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
