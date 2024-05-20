# Top-level environment for a model, for common functions & definitions to live in
# NB: baseenv() is above this to allow evaluating of g3_param_table expressions
g3_env <- new.env(parent = baseenv())

# Redefine lgamma for vectors with stricter types
# NB: VECTORIZE1_t-ed lgamma needs a single vector to work (i.e. not
#     an expression). Eigen evaluates lazily, and any expression needs
#     to be evaluated before we decide what type the lgamma function is.
g3_env$lgamma_vec <- g3_native(r = "lgamma", cpp = '
   [](vector<Type> vec) -> vector<Type> {
       return lgamma(vec);
   }
')

# Add R definition for TMB-native logspace_add
g3_env$logspace_add <- g3_native(r = function(a,b) {
    # https://github.com/kaskr/adcomp/issues/7#issuecomment-642559660
    pmax(a, b) + log1p(exp(pmin(a,b) - pmax(a, b)))
}, cpp = list("logspace_add", "Type", "Type"))  # TMB-native, but arguments have to be cast to Type


# vector<Type> form of logspace_add
g3_env$logspace_add_vec <- g3_native(r = function(a,b) {
    # https://github.com/kaskr/adcomp/issues/7#issuecomment-642559660
    pmax(a, b) + log1p(exp(pmin(a,b) - pmax(a, b)))
}, cpp = '[](vector<Type> a, Type b) -> vector<Type> {
    vector<Type> res(a.size());
    for(int i = 0; i < a.size(); i++) {
        res[i] = logspace_add(a[i], b);
    }
    return res;
}')

# differentiable equivalent of pmax(pmin(vec, upper), lower)
g3_env$logspace_minmax_vec <- g3_native(r = function(vec, lower, upper, scale) {
    vec <- -(g3_env$logspace_add(-vec * scale, -upper * scale) / scale)
    vec <- g3_env$logspace_add(vec * scale, lower * scale) / scale
    return(vec)
}, cpp = '[&logspace_add_vec](vector<Type> vec, Type lower, Type upper, double scale) -> vector<Type> {
    vec = -(logspace_add_vec(-vec * scale, -upper * scale) / scale);
    vec = logspace_add_vec(vec * scale, lower * scale) / scale;
    return(vec);
}', depends = c("logspace_add_vec"))

# NB: We have to have avoid_zero in our namespace so CMD check doesn't complain about it's use
#     in surveyindices_linreg(). Maybe g3_env should just go away and use the package
#     namespace instead?
g3_env$avoid_zero <- avoid_zero <- g3_native(r = function (a) {
    # https://github.com/kaskr/adcomp/issues/7#issuecomment-642559660
    ( pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))) ) / 1000
}, cpp = '[](Type a) -> Type {
    return logspace_add(a * 1000.0, (Type)0.0) / 1000.0;
}')

g3_env$avoid_zero_vec <- g3_native(r = function (a) {
    # https://github.com/kaskr/adcomp/issues/7#issuecomment-642559660
    ( pmax(a * 1000, 0) + log1p(exp(pmin(a * 1000, 0) - pmax(a * 1000, 0))) ) / 1000
}, cpp = '[](vector<Type> a) -> vector<Type> {
    vector<Type> res(a.size());
    for(int i = 0; i < a.size(); i++) {
        res[i] = logspace_add(a[i] * 1000.0, (Type)0.0) / 1000.0;
    }
    return res;
}')

# Divide a vector by it's sum, i.e. so it now sums to 1
g3_env$normalize_vec <- g3_native(r = function (a) {
    a / sum(a)
}, cpp = '[](vector<Type> a) -> vector<Type> {
    return a / a.sum();
}')

# Return scalar (x) bounded between (a) and (b)
g3_env$bounded <- g3_native(r = function (x, a, b) {
  a + (b-a)/(1+exp(x))
}, cpp = '[](Type x, Type a, Type b) -> Type {
    return a + (b-a)/(1+exp(x));
}')

# Return vector (x) bounded between (a) and (b)
g3_env$bounded_vec <- g3_native(r = function (x, a, b) {
  a + (b-a)/(1+exp(x))
}, cpp = '[](vector<Type> x, Type a, Type b) -> vector<Type> {
    return a + (b-a)/(1+exp(x));
}')

# vector<Type> form of pow()
g3_env$pow_vec <- g3_native(r = function(a, b) { a ^ b }, cpp = list('pow', 'vector<Type>', NULL))

# Return first non-null argument. Doesn't really make sense in C++
g3_env$nvl <- g3_native(r = function(...) {
    for (i in seq_len(...length())) if (!is.null(...elt(i))) return(...elt(i))
    return(NULL)
}, cpp = "not-implemented")

# REPORT in R attaches attributes to nll
g3_env$REPORT <- g3_native(r = function(var) {
    var_name <- as.character(sys.call()[[2]])
    # Strip attributes from nll, so we don't make recursive structure
    attr(nll, var_name) <<- if (var_name == 'nll') as.vector(var) else var
}, cpp = NULL)

# Placeholder definition for ADREPORT in R
g3_env$ADREPORT <- g3_native(r = function(...) {
    warning("No ADREPORT functionality available in R")
}, cpp = NULL)

# Rprintf equivalent for R
g3_env$Rprintf <- g3_native(r = function(...) {
    cat(sprintf(...))
}, cpp = NULL)


# REprintf equivalent for R
g3_env$REprintf <- g3_native(r = function(...) {
    cat(sprintf(...))
}, cpp = NULL)

# "Rcpp::warning" should be in scope, but on 2024-04-17 r86441 / gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0 :-
# error: there are no arguments to ‘warning’ that depend on a template parameter, so a declaration of ‘warning’ must be available [-fpermissive]
g3_env$warning <- g3_native(r = 'warning', cpp = 'Rf_warning')

g3_env$print_array <- g3_native(r = function(ar_name, ar) {
    writeLines(ar_name)
    print(ar)
}, cpp = '[](const char * ar_name, array<Type> ar) {
    Rprintf(ar_name);
    ar.print();
}')

# Warn with (message) if (expr) is TRUE. NB: Don't actually stop.
# For a fatal condition, do: if (assert_msg(...)) return(NaN)
# Which will stop early without TMB bringing down R session
g3_env$assert_msg <- g3_native(r = function(expr, message) {
    if (isFALSE(expr)) { warning(message) ; return(TRUE) }
    return(FALSE)
}, cpp = '[](bool expr, std::string message) -> bool {
    if (!expr) { Rf_warning(message.c_str()); return TRUE; }
    return FALSE;
}')


# Use TMB's "asDouble" as an equivalent for as.numeric
g3_env$as.numeric <- g3_native(r = "as.numeric", cpp = list("asDouble", NULL))
# NB: It's as*_*integer, since as.integer won't be valid C++
g3_env$as_integer <- g3_native(r = "as.integer", cpp = '[](Type v) -> int {
    return std::floor(asDouble(v));
}')

g3_env$as_numeric_arr <- g3_native(r = function (x) x, cpp = '[](array<Type> x) -> array<double> {
  array<double> out(x.size());
  for(int i=0; i<x.size(); i++)
    out(i) = asDouble(x(i));
  return out;
}')


# Sum (orig_vec) & (new_vec) according to ratio of (orig_amount) & (new_amount)
g3_env$ratio_add_vec <- g3_native(r = function(orig_vec, orig_amount, new_vec, new_amount) {
    (orig_vec * orig_amount + new_vec * new_amount) / avoid_zero_vec(orig_amount + new_amount)
}, cpp = '[&avoid_zero_vec](vector<Type> orig_vec, vector<Type> orig_amount, vector<Type> new_vec, vector<Type> new_amount) -> vector<Type> {
    return (orig_vec * orig_amount + new_vec * new_amount) / avoid_zero_vec(orig_amount + new_amount);
}', depends = c('avoid_zero_vec'))


g3_env$nonconform_add <- g3_native(r = function (base_ar, extra_ar) {
    base_ar + as.vector(extra_ar)
}, cpp = '[](array<Type> base_ar, array<Type> extra_ar) -> array<Type> {
    assert(base_ar.size() % extra_ar.size() == 0);
    return base_ar + (extra_ar.replicate(base_ar.size() / extra_ar.size(), 1));
}')
g3_env$nonconform_mult <- g3_native(r = function (base_ar, extra_ar) {
    base_ar * as.vector(extra_ar)
}, cpp = '[](array<Type> base_ar, array<Type> extra_ar) -> array<Type> {
    assert(base_ar.size() % extra_ar.size() == 0);
    return base_ar * (extra_ar.replicate(base_ar.size() / extra_ar.size(), 1));
}')
g3_env$nonconform_div <- g3_native(r = function (base_ar, extra_ar) {
    base_ar / as.vector(extra_ar)
}, cpp = '[](array<Type> base_ar, array<Type> extra_ar) -> array<Type> {
    assert(base_ar.size() % extra_ar.size() == 0);
    return base_ar / (extra_ar.replicate(base_ar.size() / extra_ar.size(), 1));
}')
g3_env$nonconform_div_avz <- g3_native(r = function (base_ar, extra_ar) {
    extra_ar <- ( pmax(extra_ar * 1000, 0) + log1p(exp(pmin(extra_ar * 1000, 0) - pmax(extra_ar * 1000, 0))) ) / 1000
    base_ar / as.vector(extra_ar)
}, cpp = '[](array<Type> base_ar, array<Type> extra_ar) -> array<Type> {
    vector<Type> extra_vec = extra_ar.vec();
    assert(base_ar.size() % extra_ar.size() == 0);

    for(int i = 0; i < extra_vec.size(); i++) {
        extra_vec[i] = logspace_add(extra_vec[i] * 1000.0, (Type)0.0) / 1000.0;
    }
    return base_ar / (extra_vec.replicate(base_ar.size() / extra_vec.size(), 1));
}')

# Marker to point out we want this value to be cast vector<Type> in TMB, in R ignore
g3_env$g3_cast_vector <- g3_native(r = function (x) x, cpp = NULL)

# Convert c(a = 1, b = 3, ...) to a factor with appropriate labels/levels.
# For internal use by g3_parameterized(by_area = TRUE)
g3_env$named_vec_to_factor <- function (vec) {
    uniq <- vec[!duplicated(vec)]
    # Default level name to number (the fallback should be irrelevant, but preserves position in the factor)
    lvls <- seq_along(names(vec))
    # Assign level names that we do know
    lvls[uniq] <- names(uniq)
    factor(names(vec), levels = lvls)
}
