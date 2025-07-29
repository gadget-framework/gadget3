g3_env$as_scalar <- g3_native(r = function(a) {
    stopifnot(length(a) == 1)
    return(a[[1]])
}, cpp = '
// Scalar templates
template<typename T, TYPE_IS_SCALAR(T)>
T __fn__(T a) {
    return a;
}
// templates for vector<Type>s & Eigen derived vectors
template<typename T>
typename T::value_type __fn__(const Eigen::DenseBase<T>& a) {
    assert(a.size() == 1);
    return a(0);
}
// Templates for array<Type>s & Eigen derived arrays
template<typename T>
typename T::value_type __fn__(const Eigen::Map<Eigen::DenseBase<T>>& a) {
    assert(a.size() == 1);
    return a(0);
}
')
