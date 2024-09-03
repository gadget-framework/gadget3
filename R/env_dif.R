g3_env$dif_pmax <- g3_native(r = function(a, b, scale) {
    # https://github.com/kaskr/adcomp/issues/7#issuecomment-642559660
    logspace_add <- function(a, b) pmax(a, b) + log1p(exp(pmin(a,b) - pmax(a, b)))

    b <- as.vector(b)
    logspace_add(a * scale, b * scale) / scale
}, cpp = '
// Scalar templates
template<typename T, typename LimitT, TYPE_IS_SCALAR(T), TYPE_IS_SCALAR(LimitT)>
T __fn__(T a, LimitT b, double scale) {
    return logspace_add(a * scale, (T)b * scale) / scale;
}
// templates for vector<Type>s & Eigen derived vectors
template<typename LimitT, typename Derived, TYPE_IS_SCALAR(LimitT)>
vector<typename Derived::value_type> __fn__(const Eigen::DenseBase<Derived>& a, LimitT b, double scale) {
    vector<typename Derived::value_type> out(a.size());
    for(int i = 0; i < a.size(); i++) out[i] = logspace_add(a[i] * scale, (typename Derived::value_type)b * scale) / scale;
    return out;
}
template<typename LimitT, typename Derived>
vector<typename Derived::value_type> __fn__(const Eigen::DenseBase<Derived>& a, const Eigen::DenseBase<LimitT>& b, double scale) {
    assert(a.size() % b.size() == 0);

    vector<typename Derived::value_type> out(a.size());
    for(int i = 0; i < a.size(); i++) out[i] = logspace_add(a[i] * scale, (typename Derived::value_type)(b[i % b.size()]) * scale) / scale;
    return out;
}
// Templates for Eigen derived arrays
template<typename LimitT, typename Derived, TYPE_IS_SCALAR(LimitT)>
array<typename Derived::value_type> __fn__(const Eigen::Map<Eigen::DenseBase<Derived>>& a, LimitT b, double scale) {
    array<typename Derived::value_type> out(a.size());
    for(int i = 0; i < a.size(); i++) out[i] = logspace_add(a[i] * scale, (typename Derived::value_type)b * scale) / scale;
    return out;
}
template<typename LimitT, typename Derived>
array<typename Derived::value_type> __fn__(const Eigen::Map<Eigen::DenseBase<Derived>>& a, const Eigen::DenseBase<LimitT>& b, double scale) {
    assert(a.size() % b.size() == 0);

    array<typename Derived::value_type> out(a.size());
    for(int i = 0; i < a.size(); i++) out[i] = logspace_add(a[i] * scale, (typename Derived::value_type)(b[i % b.size()]) * scale) / scale;
    return out;
}
')

# pmin is pmax with a negative scale
g3_env$dif_pmin <- g3_native(r = function(a, b, scale) {
    dif_pmax(a, b, -scale)
}, cpp = '
template<typename X, typename Y>
auto __fn__(X a, Y b, double scale) {
    return dif_pmax(a, b, -scale);
}
', depends = c("dif_pmax"))

# pminmax does both pmin & pmax
g3_env$dif_pminmax <- g3_native(r = function(a, lower, upper, scale) {
    out <- dif_pmax(a, upper, -scale)
    out <- dif_pmax(out, lower, scale)
    return(out)
}, cpp = '
template<typename X, typename Y, typename Z>
auto __fn__(X a, Y lower, Z upper, double scale) {
    auto out = dif_pmax(a, upper, -scale);
    out = dif_pmax(out, lower, scale);
    return out;
}
', depends = c("dif_pmax"))
