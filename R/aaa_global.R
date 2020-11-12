# Top-level environment for a model, for common functions & definitions to live in
# NB: baseenv() is above this to allow evaluating of g3_param_table expressions
g3_global_env <- new.env(parent = baseenv())

# Define a function with separate equivalent R and C++ implementations
# - r: R function object, or name of base function
# - cpp: C++ lambda function, as a string vector or list('Type', 'Type', NULL) to add casts to native fn or NULL if natively supported
g3_native <- function(r, cpp) {
    # NB: We get away with using "as.numeric" for native functions with native
    #     evaluation since R will be looking for a function, not value.
    #     Ideally we'd be using a reference to base::as.numeric, but that's causing
    #     unfathomable problems in to_tmb land.
    out <- r
    attr(out, "g3_native_cpp") <- cpp
    class(out) <- c("g3_native", class(out))
    return(out)
}

g3_global_env$nll <- 0.0

# Transform a vector using matrix, return vec again
g3_global_env$g3_matrix_vec <- g3_native(r = function(tf, vec) {
    return((tf %*% vec)[,1])
}, cpp = '
   [](array<Type>tf, vector<Type> vec) -> vector<Type> {
       return (tf.matrix() * vec.matrix()).array();
   }
')

# Redefine lgamma for vectors with stricter types
# NB: VECTORIZE1_t-ed lgamma needs a single vector to work (i.e. not
#     an expression). Eigen evaluates lazily, and any expression needs
#     to be evaluated before we decide what type the lgamma function is.
g3_global_env$lgamma_vec <- g3_native(r = "lgamma", cpp = '
   [](vector<Type> vec) -> vector<Type> {
       return lgamma(vec);
   }
')

# Add R definition for TMB-native logspace_add
g3_global_env$logspace_add <- g3_native(r = function(a,b) {
    # https://github.com/kaskr/adcomp/issues/7#issuecomment-642559660
    pmax(a, b) + log1p(exp(pmin(a,b) - pmax(a, b)))
}, cpp = list("logspace_add", "Type", "Type"))  # TMB-native, but arguments have to be cast to Type


# vector<Type> form of logspace_add
g3_global_env$logspace_add_vec <- g3_native(r = function(a,b) {
    # https://github.com/kaskr/adcomp/issues/7#issuecomment-642559660
    pmax(a, b) + log1p(exp(pmin(a,b) - pmax(a, b)))
}, cpp = '[](vector<Type> a, Type b) -> vector<Type> {
    vector<Type> res(a.size());
    for(int i = 0; i < a.size(); i++) {
        res[i] = logspace_add(a[i], b);
    }
    return res;
}')

# vector<Type> form of pow()
g3_global_env$pow_vec <- g3_native(r = function(a, b) { a ^ b }, cpp = list('pow', 'vector<Type>', NULL))

# Rprintf equivalent for R
g3_global_env$Rprintf <- g3_native(r = function(...) {
    cat(sprintf(...))
}, cpp = NULL)


# REprintf equivalent for R
g3_global_env$REprintf <- g3_native(r = function(...) {
    cat(sprintf(...))
}, cpp = NULL)


# stopifnot is assert in C++
g3_global_env$stopifnot <- g3_native(r = "stopifnot", cpp = list("assert", NULL))


# Use TMB's "asDouble" as an equivalent for as.numeric
g3_global_env$as.numeric <- g3_native(r = "as.numeric", cpp = list("asDouble", NULL))
