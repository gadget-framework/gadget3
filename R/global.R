# Top-level environment for a model, for common functions & definitions to live in
g3_global_env <- new.env(parent = emptyenv())

# Define a function with separate equivalent R and C++ implementations
# - r: R function object
# - cpp: C++ lambda function, as a string vector
g3_native <- function(r, cpp) {
    return(structure(list(r = r, cpp = cpp), class = "g3_native"))
}

g3_global_env$debugf <- g3_native(r = function(...) {
    cat(sprintf(...))
}, cpp = '
   [](const char* format, ...) -> void {
       va_list argptr;
       va_start(argptr, format);
       vprintf(format, argptr);
       va_end(argptr);
   }
')

g3_global_env$nll <- 0.0

# Transform a vector using matrix, return vec again
g3_global_env$g3_matrix_vec <- g3_native(r = function(tf, vec) {
    return (tf %*% vec)[,1]
}, cpp = '
   [](array<Type>tf, vector<Type> vec) -> vector<Type> {
       return (tf.matrix() * vec.matrix()).array();
   }
')
