#include <TMB.hpp>

namespace map_extras {
    // at(), but throw (err) if item isn't available
    template<class Type, class KeyType>
    Type at_throw(std::map<KeyType, Type*> map_in, KeyType key_in, std::string err) {
            try {
                return *map_in.at(key_in);
            } catch (const std::out_of_range&) {
                Rf_warning("No value found in g3_param_table %s, ifmissing not specified", err.c_str());
                return NAN;
            }
    }

    // at(), but return def if item isn't available
    template<class Type, class KeyType>
    Type at_def(std::map<KeyType, Type*> map_in, KeyType key_in, Type def) {
            try {
                return *map_in.at(key_in);
            } catch (const std::out_of_range&) {
                return def;
            }
    }
}


#ifndef TYPE_IS_SCALAR
#ifdef TMBAD_FRAMEWORK
#define TYPE_IS_SCALAR(TestT) typename = std::enable_if_t<std::is_same<TestT, int>::value || std::is_same<TestT, double>::value || std::is_same<TestT, TMBad::global::ad_aug>::value>
#endif // TMBAD_FRAMEWORK
#ifdef CPPAD_FRAMEWORK
#define TYPE_IS_SCALAR(TestT) typename = std::enable_if_t<std::is_same<TestT, int>::value || std::is_same<TestT, double>::value || std::is_same<TestT, CppAD::AD<double>>::value || std::is_same<TestT, CppAD::AD<CppAD::AD<double>>>::value || std::is_same<TestT, CppAD::AD<CppAD::AD<CppAD::AD<double>>>>::value>
#endif // CPPAD_FRAMEWORK
#endif // TYPE_IS_SCALAR

// Scalar templates
template<typename T, typename LimitT, TYPE_IS_SCALAR(T), TYPE_IS_SCALAR(LimitT)>
T dif_pmax(T a, LimitT b, double scale) {
    return logspace_add(a * scale, (T)b * scale) / scale;
}
// templates for vector<Type>s & Eigen derived vectors
template<typename LimitT, typename Derived, TYPE_IS_SCALAR(LimitT)>
vector<typename Derived::value_type> dif_pmax(const Eigen::DenseBase<Derived>& a, LimitT b, double scale) {
    vector<typename Derived::value_type> out(a.size());
    for(int i = 0; i < a.size(); i++) out[i] = logspace_add(a[i] * scale, (typename Derived::value_type)b * scale) / scale;
    return out;
}
template<typename LimitT, typename Derived>
vector<typename Derived::value_type> dif_pmax(const Eigen::DenseBase<Derived>& a, const Eigen::DenseBase<LimitT>& b, double scale) {
    assert(a.size() % b.size() == 0);

    vector<typename Derived::value_type> out(a.size());
    for(int i = 0; i < a.size(); i++) out[i] = logspace_add(a[i] * scale, (typename Derived::value_type)(b[i % b.size()]) * scale) / scale;
    return out;
}
// Templates for Eigen derived arrays
template<typename LimitT, typename Derived, TYPE_IS_SCALAR(LimitT)>
array<typename Derived::value_type> dif_pmax(const Eigen::Map<Eigen::DenseBase<Derived>>& a, LimitT b, double scale) {
    array<typename Derived::value_type> out(a.size());
    for(int i = 0; i < a.size(); i++) out[i] = logspace_add(a[i] * scale, (typename Derived::value_type)b * scale) / scale;
    return out;
}
template<typename LimitT, typename Derived>
array<typename Derived::value_type> dif_pmax(const Eigen::Map<Eigen::DenseBase<Derived>>& a, const Eigen::DenseBase<LimitT>& b, double scale) {
    assert(a.size() % b.size() == 0);

    array<typename Derived::value_type> out(a.size());
    for(int i = 0; i < a.size(); i++) out[i] = logspace_add(a[i] * scale, (typename Derived::value_type)(b[i % b.size()]) * scale) / scale;
    return out;
}
template<typename X>
auto avoid_zero(X a) {
    return dif_pmax(a, 0.0, 1e3);
}
template<typename T, typename DefT> T intlookup_getdefault(std::map<int, T> lookup, int key, DefT def) {
            return lookup.count(key) > 0 ? lookup[key] : (T)def;
        }
template<typename X, typename Y>
auto dif_pmin(X a, Y b, double scale) {
    return dif_pmax(a, b, -scale);
}
template<typename T> std::map<int, T> intlookup_zip(vector<int> keys, vector<T> values) {
            std::map<int, T> lookup = {};

            assert(keys.size() == values.size());
            for (size_t i = 0; i < keys.size(); ++i) {
                lookup[keys[i]] = values[i];
            }
            return lookup;
        }

template<class Type>
Type objective_function<Type>::operator() () {
    DATA_SCALAR(reporting_enabled); DATA_UPDATE(reporting_enabled);
    PARAMETER(retro_years);
    PARAMETER(fish__Linf);
    PARAMETER(fish__K);
    PARAMETER(fish__t0);
    PARAMETER(fish__lencv);
    PARAMETER(fish__init__scalar);
    PARAMETER(fish__init__1);
    PARAMETER(fish__init__2);
    PARAMETER(fish__init__3);
    PARAMETER(fish__init__4);
    PARAMETER(fish__init__5);
    PARAMETER(fish__init__6);
    PARAMETER(fish__init__7);
    PARAMETER(fish__init__8);
    PARAMETER(fish__init__9);
    PARAMETER(fish__init__10);
    std::map<std::tuple<int>, Type*> pt__fish__init = {{std::make_tuple(1), &fish__init__1}, {std::make_tuple(2), &fish__init__2}, {std::make_tuple(3), &fish__init__3}, {std::make_tuple(4), &fish__init__4}, {std::make_tuple(5), &fish__init__5}, {std::make_tuple(6), &fish__init__6}, {std::make_tuple(7), &fish__init__7}, {std::make_tuple(8), &fish__init__8}, {std::make_tuple(9), &fish__init__9}, {std::make_tuple(10), &fish__init__10}};
    PARAMETER(fish__M__1);
    PARAMETER(fish__M__2);
    PARAMETER(fish__M__3);
    PARAMETER(fish__M__4);
    PARAMETER(fish__M__5);
    PARAMETER(fish__M__6);
    PARAMETER(fish__M__7);
    PARAMETER(fish__M__8);
    PARAMETER(fish__M__9);
    PARAMETER(fish__M__10);
    std::map<std::tuple<int>, Type*> pt__fish__M = {{std::make_tuple(1), &fish__M__1}, {std::make_tuple(2), &fish__M__2}, {std::make_tuple(3), &fish__M__3}, {std::make_tuple(4), &fish__M__4}, {std::make_tuple(5), &fish__M__5}, {std::make_tuple(6), &fish__M__6}, {std::make_tuple(7), &fish__M__7}, {std::make_tuple(8), &fish__M__8}, {std::make_tuple(9), &fish__M__9}, {std::make_tuple(10), &fish__M__10}};
    PARAMETER(init__F);
    PARAMETER(recage);
    PARAMETER(fish__walpha);
    PARAMETER(fish__wbeta);
    PARAMETER(report_detail);
    PARAMETER(fish__comm__alpha);
    PARAMETER(fish__comm__l50);
    PARAMETER(fish__bbin);
    PARAMETER(fish__rec__1990);
    PARAMETER(fish__rec__1991);
    PARAMETER(fish__rec__1992);
    PARAMETER(fish__rec__1993);
    PARAMETER(fish__rec__1994);
    PARAMETER(fish__rec__1995);
    PARAMETER(fish__rec__1996);
    PARAMETER(fish__rec__1997);
    PARAMETER(fish__rec__1998);
    PARAMETER(fish__rec__1999);
    PARAMETER(fish__rec__2000);
    std::map<std::tuple<int>, Type*> pt__fish__rec = {{std::make_tuple(1990), &fish__rec__1990}, {std::make_tuple(1991), &fish__rec__1991}, {std::make_tuple(1992), &fish__rec__1992}, {std::make_tuple(1993), &fish__rec__1993}, {std::make_tuple(1994), &fish__rec__1994}, {std::make_tuple(1995), &fish__rec__1995}, {std::make_tuple(1996), &fish__rec__1996}, {std::make_tuple(1997), &fish__rec__1997}, {std::make_tuple(1998), &fish__rec__1998}, {std::make_tuple(1999), &fish__rec__1999}, {std::make_tuple(2000), &fish__rec__2000}};
    PARAMETER(fish__rec__scalar);
    PARAMETER(adist_surveyindices_log_acoustic_dist_weight);
    PARAMETER(cdist_sumofsquares_comm_ldist_weight);
    auto assert_msg = [](bool expr, std::string message) -> bool {
    if (!expr) { Rf_warning(message.c_str()); return TRUE; }
    return FALSE;
};
    auto normalize_vec = [](vector<Type> a) -> vector<Type> {
    return a / avoid_zero(a.sum());
};
    auto as_numeric_arr = [](array<Type> x) -> array<double> {
  array<double> out(x.size());
  for(int i=0; i<x.size(); i++)
    out(i) = asDouble(x(i));
  return out;
};
    auto nonconform_add = [](array<Type> base_ar, array<Type> extra_ar) -> array<Type> {
    assert(base_ar.size() % extra_ar.size() == 0);
    return base_ar + (extra_ar.replicate(base_ar.size() / extra_ar.size(), 1));
};
    auto nonconform_mult = [](array<Type> base_ar, array<Type> extra_ar) -> array<Type> {
    assert(base_ar.size() % extra_ar.size() == 0);
    return base_ar * (extra_ar.replicate(base_ar.size() / extra_ar.size(), 1));
};
    auto growth_bbinom = [](vector<Type> delt_l, int binn, Type beta) -> array<Type> {
        using namespace Eigen;

        vector<Type> alpha_1 = (beta * delt_l) / (binn - delt_l);

        // possible length growth
        int na = alpha_1.size();
        int n = binn;

        vector<Type> alpha(na * (n + 1));
        // TODO: alpha.replicate(n + 1, 1) should do this, but first entry is zero?
        for (auto i = 0; i < alpha.size(); i++) alpha(i) = alpha_1(i % alpha_1.size());

        vector<Type> x((n + 1) * na);
        for (auto i = 0; i < x.size(); i++) x(i) = i / na;

        // Create a probability matrix where the columns represent the
        // probability of growing x lengthgroups for each lengthgroup
        // length group jumps are distributed according to a beta-binomial
        // distribution
        array<Type> val(na, n + 1);
        val = (lgamma((Type) n + 1) +
            lgamma((vector<Type>)(alpha + beta)) +
            lgamma((vector<Type>)(n - x + beta)) +
            lgamma((vector<Type>)(x + alpha)) -
            lgamma((vector<Type>)(n - x + 1)) -
            lgamma((vector<Type>)(x + 1)) -
            lgamma((vector<Type>)(n + alpha + beta)) -
            lgamma(beta) -
            lgamma(alpha)).exp();
        return(val);
    };
    auto g3a_grow_vec_rotate = [](vector<Type> vec, int a) -> array<Type> {
    array<Type> out(vec.size(), a);
    for (int i = 0 ; i < vec.size(); i++) {
        for (int j = 0 ; j < a; j++) {
            out(i, j) = vec(j + i < vec.size() ? j + i : vec.size() - 1);
        }
    }
    return out;
};
    auto g3a_grow_vec_extrude = [](vector<Type> vec, int a) -> array<Type> {
    array<Type> out(vec.size(), a);
    out = vec.replicate(a, 1);
    return out;
};
    auto g3a_grow_matrix_wgt = [](array<Type> delta_w_ar) {
    // Convert delta_l / delta_w to matrices to get 2 proper dimensions, most of this is row-based.
    matrix<Type> delta_w = delta_w_ar.matrix();
    int total_deltas = delta_w.cols();  // # Length group increases (should be +1 for the no-change group)
    int total_lgs = delta_w.rows(); // # Length groups

    matrix<Type> weight_matrix(total_lgs, total_lgs);
    weight_matrix.setZero();

    for (int lg = 0; lg < total_lgs; lg++) {
        if (lg == total_lgs - 1) {  // Can't grow beyond maximum length group
            weight_matrix.block(lg, lg, 1, total_lgs - lg) = delta_w.block(lg, 0, 1, total_lgs - lg);
        } else if(lg + total_deltas > total_lgs) {
            weight_matrix.block(lg, lg, 1, total_lgs - lg) = delta_w.block(lg, 0, 1, total_lgs - lg);
        } else {
            weight_matrix.block(lg, lg, 1, total_deltas) = delta_w.block(lg, 0, 1, total_deltas);
        }
    }
    return(weight_matrix);
};
    auto g3a_grow_matrix_len = [](array<Type> delta_l_ar) -> matrix<Type> {
    // Convert delta_l / delta_w to matrices to get 2 proper dimensions, most of this is row-based.
    matrix<Type> delta_l = delta_l_ar.matrix();
    int total_deltas = delta_l.cols();  // # Length group increases (should be +1 for the no-change group)
    int total_lgs = delta_l.rows(); // # Length groups

    matrix<Type> growth_matrix(total_lgs, total_lgs);
    growth_matrix.setZero();

    for (int lg = 0; lg < total_lgs; lg++) {
        if (lg == total_lgs - 1) {  // Can't grow beyond maximum length group
            growth_matrix(lg, lg) = delta_l.row(lg).sum();
        } else if(lg + total_deltas > total_lgs) {
            growth_matrix.block(lg, lg, 1, total_lgs - lg) = delta_l.block(lg, 0, 1, total_lgs - lg);
            growth_matrix(lg, total_lgs - 1) = delta_l.row(lg).tail(total_deltas - (total_lgs - lg) + 1).sum();
        } else {
            growth_matrix.block(lg, lg, 1, total_deltas) = delta_l.block(lg, 0, 1, total_deltas);
        }
    }
    return(growth_matrix);
};
    auto g3a_grow_apply = [](matrix<Type> growth_matrix, matrix<Type> weight_matrix, vector<Type> input_num, vector<Type> input_wgt) -> array<Type> {
    int total_lgs = growth_matrix.cols(); // # Length groups

    // Apply matrices to stock
    // NB: Cast to array to get elementwise multiplication
    growth_matrix = growth_matrix.array().colwise() * input_num.array();
    weight_matrix = (weight_matrix.array().colwise() + input_wgt.array()) * growth_matrix.array();

    // Sum together all length group brackets for both length & weight
    array<Type> combined(total_lgs,2);
    combined.col(0) = growth_matrix.colwise().sum();
    combined.col(1) = weight_matrix.colwise().sum().array().rowwise() / avoid_zero(growth_matrix.colwise().sum()).array().transpose();
    return combined;
};
    auto ratio_add_vec = [](vector<Type> orig_vec, vector<Type> orig_amount, vector<Type> new_vec, vector<Type> new_amount) -> vector<Type> {
    return (orig_vec * orig_amount + new_vec * new_amount) / avoid_zero(orig_amount + new_amount);
};
    auto surveyindices_linreg = [](vector<Type> N, vector<Type> I, Type fixed_alpha, Type fixed_beta) -> vector<Type> {
        vector<Type> out(2);

        auto meanI = I.mean();
        auto meanN = N.mean();
        auto beta = std::isnan(asDouble(fixed_beta)) ? ((I - meanI) * (N - meanN)).sum() / avoid_zero((pow(N - meanN, (Type)2)).sum()) : fixed_beta;
        auto alpha = std::isnan(asDouble(fixed_alpha)) ? meanI - beta * meanN : fixed_alpha;
        out(0) = alpha;
        out(1) = beta;
        return out;
    };
    int cur_time = -1;
    PARAMETER(project_years);
    int cur_year = 0;
    int start_year = 1990;
    vector<int> step_lengths(1); step_lengths.setConstant(12);
    auto step_count = (step_lengths).size();
    int cur_year_projection = false;
    int end_year = 2000;
    int cur_step = 0;
    auto cur_step_size = step_lengths ( 0 ) / (double)(12);
    int cur_step_final = false;
    int fish__minage = 1;
    int fish__maxage = 10;
    int fish__area = 1;
    DATA_VECTOR(fish__midlen)
    array<Type> fish__num(6,1,10); fish__num.setZero();
    array<Type> fish__wgt(6,1,10); fish__wgt.setConstant((double)(1));
    auto total_steps = (step_lengths).size()*(end_year - retro_years - start_year + project_years) + (step_lengths).size() - 1;
    auto as_integer = [](Type v) -> int {
    return std::floor(asDouble(v));
};
    array<double> detail_fish__num(6,1,10,as_integer(total_steps + (double)(1))); detail_fish__num.setZero();
    array<double> detail_fish__wgt(6,1,10,as_integer(total_steps + (double)(1))); detail_fish__wgt.setConstant((double)(1));
    array<Type> suit_fish_comm__report(6);
    vector<Type> adist_surveyindices_log_acoustic_dist_model__params(2); adist_surveyindices_log_acoustic_dist_model__params.setZero();
    array<Type> adist_surveyindices_log_acoustic_dist_model__wgt(1,11,1); adist_surveyindices_log_acoustic_dist_model__wgt.setZero();
    DATA_ARRAY(adist_surveyindices_log_acoustic_dist_obs__wgt)
    array<Type> cdist_sumofsquares_comm_ldist_model__wgt(5,11,1); cdist_sumofsquares_comm_ldist_model__wgt.setZero();
    DATA_ARRAY(cdist_sumofsquares_comm_ldist_obs__wgt)
    array<double> detail_fish__predby_comm(6,1,10,as_integer(total_steps + (double)(1)));
    array<double> detail_fish__renewalnum(6,1,10,as_integer(total_steps + (double)(1))); detail_fish__renewalnum.setZero();
    array<double> detail_fish_comm__cons(6,1,10,as_integer(total_steps + (double)(1)));
    Type nll = (double)(0);
    array<Type> nll_adist_surveyindices_log_acoustic_dist__weight(as_integer(total_steps + 1)); nll_adist_surveyindices_log_acoustic_dist__weight.setZero();
    array<Type> nll_adist_surveyindices_log_acoustic_dist__wgt(as_integer(total_steps + 1)); nll_adist_surveyindices_log_acoustic_dist__wgt.setZero();
    array<Type> nll_cdist_sumofsquares_comm_ldist__weight(as_integer(total_steps + 1)); nll_cdist_sumofsquares_comm_ldist__weight.setZero();
    array<Type> nll_cdist_sumofsquares_comm_ldist__wgt(as_integer(total_steps + 1)); nll_cdist_sumofsquares_comm_ldist__wgt.setZero();
    array<Type> nll_understocking__wgt(as_integer(total_steps + 1)); nll_understocking__wgt.setZero();
    array<Type> fish__totalpredate(6,1,10);
    array<Type> comm__totalsuit(1);
    array<Type> fish_comm__suit(6,1,10);
    int comm__area = 1;
    array<Type> fish_comm__cons(6,1,10);
    DATA_IVECTOR(comm_landings_keys)
    DATA_VECTOR(comm_landings_values)
    auto comm_landings = intlookup_zip(comm_landings_keys, comm_landings_values);
    array<Type> fish__consratio(6,1,10);
    Type fish__overconsumption = (double)(0);
    array<Type> fish__consconv(6,1,10);
    array<Type> fish__predby_comm(6,1,10);
    int fish__growth_lastcalc = -1;
    array<Type> fish__growth_l(6,6);
    Type fish__plusdl = (double)(10);
    array<Type> fish__growth_w(6,6);
    Type fish__prevtotal = (double)(0);
    array<Type> fish__renewalnum(6,1,10); fish__renewalnum.setZero();
    array<Type> fish__renewalwgt(6,1,10); fish__renewalwgt.setZero();
    int adist_surveyindices_log_acoustic_dist_model__area = 1;
    DATA_IVECTOR(adist_surveyindices_log_acoustic_dist_model__times_keys)
    DATA_IVECTOR(adist_surveyindices_log_acoustic_dist_model__times_values)
    auto adist_surveyindices_log_acoustic_dist_model__times = intlookup_zip(adist_surveyindices_log_acoustic_dist_model__times_keys, adist_surveyindices_log_acoustic_dist_model__times_values);
    array<Type> fish_adist_surveyindices_log_acoustic_dist_model_lgmatrix(1,6); fish_adist_surveyindices_log_acoustic_dist_model_lgmatrix.setConstant((double)(1));
    int adist_surveyindices_log_acoustic_dist_obs__area = 1;
    int cdist_sumofsquares_comm_ldist_model__area = 1;
    DATA_IVECTOR(cdist_sumofsquares_comm_ldist_model__times_keys)
    DATA_IVECTOR(cdist_sumofsquares_comm_ldist_model__times_values)
    auto cdist_sumofsquares_comm_ldist_model__times = intlookup_zip(cdist_sumofsquares_comm_ldist_model__times_keys, cdist_sumofsquares_comm_ldist_model__times_values);
    DATA_ARRAY(fish_comm_cdist_sumofsquares_comm_ldist_model_lgmatrix)
    int cdist_sumofsquares_comm_ldist_obs__area = 1;
    DATA_IVECTOR(cdist_sumofsquares_comm_ldist_obs__times_keys)
    DATA_IVECTOR(cdist_sumofsquares_comm_ldist_obs__times_values)
    auto cdist_sumofsquares_comm_ldist_obs__times = intlookup_zip(cdist_sumofsquares_comm_ldist_obs__times_keys, cdist_sumofsquares_comm_ldist_obs__times_values);
    Type g3l_understocking_total = (double)(0);
    array<Type> nll_understocking__weight(as_integer(total_steps + 1)); nll_understocking__weight.setZero();

    while (true) {
        {
            // g3a_time: Start of time period;
            cur_time += 1;
            if ( cur_time == 0 && assert_msg(retro_years >= (double)(0), "retro_years must be >= 0") ) {
                return NAN;
            }
            if ( cur_time == 0 && assert_msg(project_years >= (double)(0), "project_years must be >= 0") ) {
                return NAN;
            }
            cur_year = start_year + (((int) cur_time) / ((int) step_count));
            cur_year_projection = cur_year > end_year - retro_years;
            cur_step = (cur_time % step_count) + 1;
            cur_step_size = step_lengths ( cur_step - 1 ) / (double)(12);
            cur_step_final = cur_step == step_count;
        }
        {
            // g3a_initialconditions for fish;
            for (auto age = fish__minage; age <= fish__maxage; age++) if ( cur_time == 0 ) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                auto dnorm = ((fish__midlen - (fish__Linf*((double)(1) - exp(-(double)(1)*fish__K*((age - cur_step_size) - fish__t0))))) / ((fish__Linf*((double)(1) - exp(-(double)(1)*fish__K*((age - cur_step_size) - fish__t0))))*fish__lencv));

                auto factor = (fish__init__scalar*(map_extras::at_throw(pt__fish__init, std::make_tuple(age), "fish.init") + (double)(0)*age)*exp(-(double)(1)*((map_extras::at_throw(pt__fish__M, std::make_tuple(age), "fish.M") + (double)(0)*age) + init__F)*(age - recage)));

                {
                    fish__num.col(fish__age_idx).col(fish__area_idx) = normalize_vec(exp(-((dnorm).pow((double)(2)))*(double)(0.5)))*(double)(10000)*factor;
                    fish__wgt.col(fish__age_idx).col(fish__area_idx) = fish__walpha*(fish__midlen).pow(fish__wbeta);
                }
            }
        }
        if ( (cur_time <= total_steps && report_detail == 1) ) {
            detail_fish__num.col(cur_time + 1 - 1) = as_numeric_arr(fish__num);
        }
        if ( (cur_time <= total_steps && report_detail == 1) ) {
            detail_fish__wgt.col(cur_time + 1 - 1) = as_numeric_arr(fish__wgt);
        }
        if ( cur_time == 0 ) {
            suit_fish_comm__report = (double)(1) / ((double)(1) + exp(-fish__comm__alpha*(fish__midlen - fish__comm__l50)));
            REPORT(suit_fish_comm__report);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(adist_surveyindices_log_acoustic_dist_model__params);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(adist_surveyindices_log_acoustic_dist_model__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(adist_surveyindices_log_acoustic_dist_obs__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(cdist_sumofsquares_comm_ldist_model__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(cdist_sumofsquares_comm_ldist_obs__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(detail_fish__num);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(detail_fish__predby_comm);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(detail_fish__renewalnum);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(detail_fish__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(detail_fish_comm__cons);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_adist_surveyindices_log_acoustic_dist__weight);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_adist_surveyindices_log_acoustic_dist__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_cdist_sumofsquares_comm_ldist__weight);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_cdist_sumofsquares_comm_ldist__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_understocking__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(step_lengths);
        }
        {
            if ( cur_time > total_steps ) {
                return nll;
            }
        }
        fish__totalpredate.setZero();
        comm__totalsuit.setZero();
        {
            auto suitability = (vector<Type>)(suit_fish_comm__report);

            {
                // g3a_predate for comm predating fish;
                fish_comm__suit.setZero();
                for (auto age = fish__minage; age <= fish__maxage; age++) {
                    auto fish__age_idx = age - fish__minage + 1 - 1;

                    auto area = fish__area;

                    auto fish__area_idx = 0;

                    if ( area == comm__area ) {
                        auto catchability = (suitability*fish__num.col(fish__age_idx).col(fish__area_idx));

                        auto comm__area_idx = 0;

                        auto predator_area = area;

                        {
                            // Collect all suitable fish biomass for comm;
                            fish_comm__suit.col(fish__age_idx).col(fish__area_idx) = catchability;
                            comm__totalsuit(comm__area_idx) += (fish_comm__suit.col(fish__age_idx).col(fish__area_idx)).sum();
                        }
                    }
                }
            }
        }
        {
            // Scale comm catch of fish by total expected catch;
            fish_comm__cons.setZero();
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                if ( area == comm__area ) {
                    auto comm__area_idx = 0;

                    auto predator_area = area;

                    auto total_predsuit = comm__totalsuit(comm__area_idx);

                    fish_comm__cons.col(fish__age_idx).col(fish__area_idx) = fish_comm__suit.col(fish__age_idx).col(fish__area_idx)*((area != 1 ? (double)(0) : intlookup_getdefault(comm_landings, cur_year, (double)(0))) / total_predsuit)*fish__wgt.col(fish__age_idx).col(fish__area_idx);
                }
            }
            fish__totalpredate = nonconform_add(fish__totalpredate, fish_comm__cons);
        }
        {
            // Calculate fish overconsumption coefficient;
            // Apply overconsumption to fish;
            fish__consratio = fish__totalpredate / avoid_zero(fish__num*fish__wgt);
            fish__consratio = dif_pmin(fish__consratio, (double)(0.95), (double)(1000));
            fish__overconsumption = (fish__totalpredate).sum();
            fish__consconv = (double)(1) / avoid_zero(fish__totalpredate);
            fish__totalpredate = (fish__num*fish__wgt)*fish__consratio;
            fish__overconsumption -= (fish__totalpredate).sum();
            fish__consconv *= fish__totalpredate;
            fish__num *= ((double)(1) - fish__consratio);
        }
        {
            // Apply overconsumption to fish_comm__cons;
            fish_comm__cons = nonconform_mult(fish_comm__cons, fish__consconv);
        }
        {
            fish__predby_comm.setZero();
            fish__predby_comm = nonconform_add(fish__predby_comm, fish_comm__cons);
        }
        {
            // Natural mortality for fish;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                fish__num.col(fish__age_idx).col(fish__area_idx) *= exp(-((map_extras::at_throw(pt__fish__M, std::make_tuple(age), "fish.M") + (double)(0)*age))*cur_step_size);
            }
        }
        {
            auto growth_delta_l = (fish__growth_lastcalc == std::floor(cur_step_size*12) ? fish__growth_l : (fish__growth_l = growth_bbinom(avoid_zero(avoid_zero((fish__Linf - fish__midlen)*((double)(1) - exp(-(fish__K)*cur_step_size))) / fish__plusdl), 5, avoid_zero(fish__bbin))));

            auto growth_delta_w = (fish__growth_lastcalc == std::floor(cur_step_size*12) ? fish__growth_w : (fish__growth_w = (g3a_grow_vec_rotate((fish__midlen).pow(fish__wbeta), 5 + (double)(1)) - g3a_grow_vec_extrude((fish__midlen).pow(fish__wbeta), 5 + (double)(1)))*fish__walpha));

            auto growthmat_w = g3a_grow_matrix_wgt(growth_delta_w);

            auto growthmat_l = g3a_grow_matrix_len(growth_delta_l);

            {
                // g3a_grow for fish;
                for (auto age = fish__minage; age <= fish__maxage; age++) {
                    auto fish__age_idx = age - fish__minage + 1 - 1;

                    auto area = fish__area;

                    auto fish__area_idx = 0;

                    auto growthresult = g3a_grow_apply(growthmat_l, growthmat_w, fish__num.col(fish__age_idx).col(fish__area_idx), fish__wgt.col(fish__age_idx).col(fish__area_idx));

                    {
                        if ( false ) {
                            fish__prevtotal = (fish__num.col(fish__age_idx).col(fish__area_idx)).sum();
                        }
                        // Update fish using delta matrices;
                        fish__num.col(fish__age_idx).col(fish__area_idx) = growthresult.col(0);
                        fish__wgt.col(fish__age_idx).col(fish__area_idx) = growthresult.col(1);
                        if ( false ) {
                            assert_msg(CppAD::abs(fish__prevtotal - (fish__num.col(fish__age_idx).col(fish__area_idx)).sum()) < (double)(1e-04), "g3a_growmature: fish__num totals are not the same before and after growth");
                        }
                    }
                }
                fish__growth_lastcalc = std::floor(cur_step_size*12);
            }
        }
        {
            auto factor = (map_extras::at_def(pt__fish__rec, std::make_tuple(cur_year), (Type)(NAN))*fish__rec__scalar);

            {
                // g3a_renewal for fish;
                for (auto age = fish__minage; age <= fish__maxage; age++) if ( age == fish__minage && cur_step == 1 && (! cur_year_projection) ) {
                    auto fish__age_idx = age - fish__minage + 1 - 1;

                    auto area = fish__area;

                    auto fish__area_idx = 0;

                    auto dnorm = ((fish__midlen - (fish__Linf*((double)(1) - exp(-(double)(1)*fish__K*(age - fish__t0))))) / ((fish__Linf*((double)(1) - exp(-(double)(1)*fish__K*(age - fish__t0))))*fish__lencv));

                    {
                        fish__renewalnum.col(fish__age_idx).col(fish__area_idx) = normalize_vec(exp(-((dnorm).pow((double)(2)))*(double)(0.5)))*(double)(10000)*factor;
                        fish__renewalwgt.col(fish__age_idx).col(fish__area_idx) = fish__walpha*(fish__midlen).pow(fish__wbeta);
                        // Add result to fish;
                        fish__wgt.col(fish__age_idx).col(fish__area_idx) = ratio_add_vec(fish__wgt.col(fish__age_idx).col(fish__area_idx), fish__num.col(fish__age_idx).col(fish__area_idx), fish__renewalwgt.col(fish__age_idx).col(fish__area_idx), fish__renewalnum.col(fish__age_idx).col(fish__area_idx));
                        fish__num.col(fish__age_idx).col(fish__area_idx) += fish__renewalnum.col(fish__age_idx).col(fish__area_idx);
                    }
                }
            }
        }
        if ( adist_surveyindices_log_acoustic_dist_weight > (double)(0) ) {
            // g3l_abundancedistribution_surveyindices_log: Collect abundance from fish for adist_surveyindices_log_acoustic_dist;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                if ( area == adist_surveyindices_log_acoustic_dist_model__area ) {
                    auto adist_surveyindices_log_acoustic_dist_model__area_idx = 0;

                    auto adist_surveyindices_log_acoustic_dist_model__time_idx = intlookup_getdefault(adist_surveyindices_log_acoustic_dist_model__times, (cur_year*100 + cur_step*0), -1) - 1;

                    if ( adist_surveyindices_log_acoustic_dist_model__time_idx >= 0 ) {
                        // Convert fish to wgt;
                        adist_surveyindices_log_acoustic_dist_model__wgt.col(adist_surveyindices_log_acoustic_dist_model__area_idx).col(adist_surveyindices_log_acoustic_dist_model__time_idx) += ((matrix<Type>)(fish_adist_surveyindices_log_acoustic_dist_model_lgmatrix.matrix() * ((fish__num.col(fish__age_idx).col(fish__area_idx)*fish__wgt.col(fish__age_idx).col(fish__area_idx))).matrix())).vec();
                    }
                }
            }
        }
        {
            auto adist_surveyindices_log_acoustic_dist_model__max_time_idx = 10;

            {
                // g3l_abundancedistribution_surveyindices_log: Compare adist_surveyindices_log_acoustic_dist_model to adist_surveyindices_log_acoustic_dist_obs;
                if ( adist_surveyindices_log_acoustic_dist_weight > (double)(0) && cur_step_final ) {
                    auto area = adist_surveyindices_log_acoustic_dist_model__area;

                    auto adist_surveyindices_log_acoustic_dist_model__area_idx = 0;

                    auto adist_surveyindices_log_acoustic_dist_model__time_idx = intlookup_getdefault(adist_surveyindices_log_acoustic_dist_model__times, (cur_year*100 + cur_step*0), -1) - 1;

                    if ( adist_surveyindices_log_acoustic_dist_model__time_idx >= 0 ) {
                        if ( area == adist_surveyindices_log_acoustic_dist_obs__area ) {
                            auto adist_surveyindices_log_acoustic_dist_obs__area_idx = 0;

                            {
                                adist_surveyindices_log_acoustic_dist_model__params = (adist_surveyindices_log_acoustic_dist_model__time_idx != adist_surveyindices_log_acoustic_dist_model__max_time_idx ? adist_surveyindices_log_acoustic_dist_model__params : surveyindices_linreg(log(avoid_zero(adist_surveyindices_log_acoustic_dist_model__wgt.col(adist_surveyindices_log_acoustic_dist_model__area_idx))), log(avoid_zero(adist_surveyindices_log_acoustic_dist_obs__wgt.col(adist_surveyindices_log_acoustic_dist_obs__area_idx))), NAN, (double)(1)));
                                {
                                    auto cur_cdist_nll = (adist_surveyindices_log_acoustic_dist_model__time_idx != adist_surveyindices_log_acoustic_dist_model__max_time_idx ? (double)(0) : (pow((adist_surveyindices_log_acoustic_dist_model__params ( 0 ) + adist_surveyindices_log_acoustic_dist_model__params ( 1 )*log(avoid_zero(adist_surveyindices_log_acoustic_dist_model__wgt.col(adist_surveyindices_log_acoustic_dist_model__area_idx))) - log(avoid_zero(adist_surveyindices_log_acoustic_dist_obs__wgt.col(adist_surveyindices_log_acoustic_dist_obs__area_idx)))), (Type)(double)(2))).sum());

                                    {
                                        nll += adist_surveyindices_log_acoustic_dist_weight*cur_cdist_nll;
                                        nll_adist_surveyindices_log_acoustic_dist__wgt(cur_time + 1 - 1) += cur_cdist_nll;
                                        nll_adist_surveyindices_log_acoustic_dist__weight(cur_time + 1 - 1) = adist_surveyindices_log_acoustic_dist_weight;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if ( cdist_sumofsquares_comm_ldist_weight > (double)(0) ) {
            // g3l_catchdistribution_sumofsquares: Collect catch from comm/fish for cdist_sumofsquares_comm_ldist;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                if ( area == cdist_sumofsquares_comm_ldist_model__area ) {
                    auto cdist_sumofsquares_comm_ldist_model__area_idx = 0;

                    auto cdist_sumofsquares_comm_ldist_model__time_idx = intlookup_getdefault(cdist_sumofsquares_comm_ldist_model__times, (cur_year*100 + cur_step*0), -1) - 1;

                    if ( cdist_sumofsquares_comm_ldist_model__time_idx >= 0 ) {
                        // Convert fish_comm to wgt;
                        cdist_sumofsquares_comm_ldist_model__wgt.col(cdist_sumofsquares_comm_ldist_model__area_idx).col(cdist_sumofsquares_comm_ldist_model__time_idx) += ((matrix<Type>)(fish_comm_cdist_sumofsquares_comm_ldist_model_lgmatrix.matrix() * (fish_comm__cons.col(fish__age_idx).col(fish__area_idx)).matrix())).vec();
                    }
                }
            }
        }
        {
            // g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_comm_ldist_model to cdist_sumofsquares_comm_ldist_obs;
            if ( cdist_sumofsquares_comm_ldist_weight > (double)(0) && cur_step_final ) {
                auto area = cdist_sumofsquares_comm_ldist_model__area;

                auto cdist_sumofsquares_comm_ldist_model__area_idx = 0;

                auto cdist_sumofsquares_comm_ldist_model__time_idx = intlookup_getdefault(cdist_sumofsquares_comm_ldist_model__times, (cur_year*100 + cur_step*0), -1) - 1;

                if ( cdist_sumofsquares_comm_ldist_model__time_idx >= 0 ) {
                    if ( area == cdist_sumofsquares_comm_ldist_obs__area ) {
                        auto cdist_sumofsquares_comm_ldist_model__sstotal = avoid_zero((cdist_sumofsquares_comm_ldist_model__wgt.col(cdist_sumofsquares_comm_ldist_model__area_idx).col(cdist_sumofsquares_comm_ldist_model__time_idx)).sum());

                        {
                            auto cdist_sumofsquares_comm_ldist_obs__area_idx = 0;

                            auto cdist_sumofsquares_comm_ldist_obs__time_idx = intlookup_getdefault(cdist_sumofsquares_comm_ldist_obs__times, (cur_year*100 + cur_step*0), -1) - 1;

                            if ( cdist_sumofsquares_comm_ldist_obs__time_idx >= 0 ) {
                                auto cdist_sumofsquares_comm_ldist_obs__sstotal = avoid_zero((cdist_sumofsquares_comm_ldist_obs__wgt.col(cdist_sumofsquares_comm_ldist_obs__area_idx).col(cdist_sumofsquares_comm_ldist_obs__time_idx)).sum());

                                auto cur_cdist_nll = ((pow(((cdist_sumofsquares_comm_ldist_model__wgt.col(cdist_sumofsquares_comm_ldist_model__area_idx).col(cdist_sumofsquares_comm_ldist_model__time_idx) / cdist_sumofsquares_comm_ldist_model__sstotal) - (cdist_sumofsquares_comm_ldist_obs__wgt.col(cdist_sumofsquares_comm_ldist_obs__area_idx).col(cdist_sumofsquares_comm_ldist_obs__time_idx) / cdist_sumofsquares_comm_ldist_obs__sstotal)), (Type)(double)(2)))).sum();

                                {
                                    nll += cdist_sumofsquares_comm_ldist_weight*cur_cdist_nll;
                                    nll_cdist_sumofsquares_comm_ldist__wgt(cur_time + 1 - 1) += cur_cdist_nll;
                                    nll_cdist_sumofsquares_comm_ldist__weight(cur_time + 1 - 1) = cdist_sumofsquares_comm_ldist_weight;
                                }
                            }
                        }
                    }
                }
            }
        }
        {
            // Reset understocking total;
            g3l_understocking_total = (double)(0);
        }
        {
            // g3l_understocking for fish;
            // Add understocking from fish as biomass to nll;
            g3l_understocking_total += fish__overconsumption;
        }
        {
            // g3l_understocking: Combine and add to nll;
            g3l_understocking_total = pow(g3l_understocking_total, (Type)(double)(2));
            nll += (double)(1e+08)*g3l_understocking_total;
            nll_understocking__wgt(cur_time + 1 - 1) += g3l_understocking_total;
            nll_understocking__weight(cur_time + 1 - 1) = (double)(1e+08);
        }
        if ( report_detail == 1 ) {
            detail_fish__predby_comm.col(cur_time + 1 - 1) = as_numeric_arr(fish__predby_comm);
        }
        if ( report_detail == 1 ) {
            detail_fish__renewalnum.col(cur_time + 1 - 1) = as_numeric_arr(fish__renewalnum);
        }
        if ( report_detail == 1 ) {
            detail_fish_comm__cons.col(cur_time + 1 - 1) = as_numeric_arr(fish_comm__cons);
        }
        if ( cur_step_final ) {
            // g3a_age for fish;
            for (auto age = fish__maxage; age >= fish__minage; age--) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                {
                    // Check stock has remained finite for this step;
                    if (age == fish__maxage) {
                        // Oldest fish is a plus-group, combine with younger individuals;
                        fish__wgt.col(fish__age_idx) = ratio_add_vec(fish__wgt.col(fish__age_idx), fish__num.col(fish__age_idx), fish__wgt.col(fish__age_idx - 1), fish__num.col(fish__age_idx - 1));
                        fish__num.col(fish__age_idx) += fish__num.col(fish__age_idx - 1);
                    } else {
                        if (age == fish__minage) {
                            // Empty youngest fish age-group;
                            fish__num.col(fish__age_idx).setZero();
                        } else {
                            // Move fish age-group to next one up;
                            fish__num.col(fish__age_idx) = fish__num.col(fish__age_idx - 1);
                            fish__wgt.col(fish__age_idx) = fish__wgt.col(fish__age_idx - 1);
                        }
                    }
                }
            }
        }
    }
}
