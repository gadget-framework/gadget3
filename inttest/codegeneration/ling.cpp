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
    PARAMETER(ling__Linf);
    PARAMETER(ling__K);
    PARAMETER(recage);
    PARAMETER(ling__recl);
    PARAMETER(lingimm__init__scalar);
    PARAMETER(lingimm__M);
    PARAMETER(ling__init__F);
    PARAMETER_VECTOR(lingimm__init);
    PARAMETER(lingimm__walpha);
    PARAMETER(lingimm__wbeta);
    PARAMETER(lingmat__init__scalar);
    PARAMETER(lingmat__M);
    PARAMETER_VECTOR(lingmat__init);
    PARAMETER(lingmat__walpha);
    PARAMETER(lingmat__wbeta);
    PARAMETER(ling__igfs__alpha);
    PARAMETER(ling__igfs__l50);
    PARAMETER(ling__mat1);
    PARAMETER(ling__mat2);
    PARAMETER(ling__bbin);
    PARAMETER(ling__rec__scalar);
    PARAMETER(ling__rec__1994);
    PARAMETER(ling__rec__1995);
    PARAMETER(ling__rec__1996);
    PARAMETER(ling__rec__1997);
    PARAMETER(ling__rec__1998);
    PARAMETER(ling__rec__1999);
    PARAMETER(ling__rec__2000);
    PARAMETER(ling__rec__2001);
    PARAMETER(ling__rec__2002);
    PARAMETER(ling__rec__2003);
    PARAMETER(ling__rec__2004);
    PARAMETER(ling__rec__2005);
    PARAMETER(ling__rec__2006);
    PARAMETER(ling__rec__2007);
    PARAMETER(ling__rec__2008);
    PARAMETER(ling__rec__2009);
    PARAMETER(ling__rec__2010);
    PARAMETER(ling__rec__2011);
    PARAMETER(ling__rec__2012);
    PARAMETER(ling__rec__2013);
    PARAMETER(ling__rec__2014);
    PARAMETER(ling__rec__2015);
    PARAMETER(ling__rec__2016);
    PARAMETER(ling__rec__2017);
    PARAMETER(ling__rec__2018);
    std::map<std::tuple<int>, Type*> pt__ling__rec = {{std::make_tuple(1994), &ling__rec__1994}, {std::make_tuple(1995), &ling__rec__1995}, {std::make_tuple(1996), &ling__rec__1996}, {std::make_tuple(1997), &ling__rec__1997}, {std::make_tuple(1998), &ling__rec__1998}, {std::make_tuple(1999), &ling__rec__1999}, {std::make_tuple(2000), &ling__rec__2000}, {std::make_tuple(2001), &ling__rec__2001}, {std::make_tuple(2002), &ling__rec__2002}, {std::make_tuple(2003), &ling__rec__2003}, {std::make_tuple(2004), &ling__rec__2004}, {std::make_tuple(2005), &ling__rec__2005}, {std::make_tuple(2006), &ling__rec__2006}, {std::make_tuple(2007), &ling__rec__2007}, {std::make_tuple(2008), &ling__rec__2008}, {std::make_tuple(2009), &ling__rec__2009}, {std::make_tuple(2010), &ling__rec__2010}, {std::make_tuple(2011), &ling__rec__2011}, {std::make_tuple(2012), &ling__rec__2012}, {std::make_tuple(2013), &ling__rec__2013}, {std::make_tuple(2014), &ling__rec__2014}, {std::make_tuple(2015), &ling__rec__2015}, {std::make_tuple(2016), &ling__rec__2016}, {std::make_tuple(2017), &ling__rec__2017}, {std::make_tuple(2018), &ling__rec__2018}};
    PARAMETER(cdist_sumofsquares_ldist_lln_weight);
    auto assert_msg = [](bool expr, std::string message) -> bool {
    if (!expr) { Rf_warning(message.c_str()); return TRUE; }
    return FALSE;
};
    auto as_integer = [](Type v) -> int {
    return std::floor(asDouble(v));
};
    auto normalize_vec = [](vector<Type> a) -> vector<Type> {
    return a / avoid_zero(a.sum());
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
    int cur_time = -1;
    int cur_year = 0;
    int start_year = 1994;
    vector<int> step_lengths(4); step_lengths.setConstant(3);
    auto step_count = (step_lengths).size();
    int cur_year_projection = false;
    int end_year = 2018;
    int cur_step = 0;
    auto cur_step_size = step_lengths ( 0 ) / (double)(12);
    int cur_step_final = false;
    int ling_imm__area = 1;
    int ling_imm__minage = 3;
    int ling_imm__maxage = 10;
    DATA_VECTOR(ling_imm__midlen)
    DATA_VECTOR(ling_imm_stddev)
    array<Type> ling_imm__num(35,8,1); ling_imm__num.setZero();
    array<Type> ling_imm__wgt(35,8,1); ling_imm__wgt.setConstant((double)(1));
    int ling_mat__area = 1;
    int ling_mat__minage = 5;
    int ling_mat__maxage = 15;
    DATA_VECTOR(ling_mat__midlen)
    DATA_VECTOR(ling_mat_stddev)
    array<Type> ling_mat__num(35,11,1); ling_mat__num.setZero();
    array<Type> ling_mat__wgt(35,11,1); ling_mat__wgt.setConstant((double)(1));
    array<Type> suit_ling_imm_igfs__report(35);
    array<Type> suit_ling_mat_igfs__report(35);
    auto total_steps = (step_lengths).size()*(end_year - retro_years - start_year + 0) + (step_lengths).size() - 1;
    array<Type> nll_understocking__wgt(as_integer(total_steps + 1)); nll_understocking__wgt.setZero();
    Type nll = (double)(0);
    array<Type> ling_imm__totalpredate(35,8,1);
    array<Type> ling_mat__totalpredate(35,11,1);
    array<Type> igfs__totalsuit(1);
    array<Type> ling_imm_igfs__suit(35,8,1);
    int igfs__area = 1;
    array<Type> ling_mat_igfs__suit(35,11,1);
    array<Type> ling_imm_igfs__cons(35,8,1);
    DATA_IVECTOR(igfs_totaldata_keys)
    DATA_IVECTOR(igfs_totaldata_values)
    auto igfs_totaldata = intlookup_zip(igfs_totaldata_keys, igfs_totaldata_values);
    array<Type> ling_mat_igfs__cons(35,11,1);
    array<Type> ling_imm__consratio(35,8,1);
    Type ling_imm__overconsumption = (double)(0);
    array<Type> ling_imm__consconv(35,8,1);
    array<Type> ling_mat__consratio(35,11,1);
    Type ling_mat__overconsumption = (double)(0);
    array<Type> ling_mat__consconv(35,11,1);
    array<Type> ling_imm__predby_igfs(35,8,1);
    array<Type> ling_mat__predby_igfs(35,11,1);
    array<Type> ling_imm__transitioning_num(35,8,1); ling_imm__transitioning_num.setZero();
    array<Type> ling_imm__transitioning_wgt(35,8,1);
    int ling_imm__growth_lastcalc = -1;
    array<Type> ling_imm__growth_l(35,16);
    Type ling_imm__plusdl = (double)(4);
    array<Type> ling_imm__growth_w(35,16);
    Type ling_imm__prevtotal = (double)(0);
    int ling_mat__growth_lastcalc = -1;
    array<Type> ling_mat__growth_l(35,16);
    Type ling_mat__plusdl = (double)(4);
    array<Type> ling_mat__growth_w(35,16);
    Type ling_mat__prevtotal = (double)(0);
    array<Type> ling_imm__renewalnum(35,8,1); ling_imm__renewalnum.setZero();
    array<Type> ling_imm__renewalwgt(35,8,1); ling_imm__renewalwgt.setZero();
    int cdist_sumofsquares_ldist_lln_model__area = 1;
    array<Type> cdist_sumofsquares_ldist_lln_model__num(35,1); cdist_sumofsquares_ldist_lln_model__num.setZero();
    int cdist_sumofsquares_ldist_lln_obs__area = 1;
    DATA_IVECTOR(cdist_sumofsquares_ldist_lln_obs__times_keys)
    DATA_IVECTOR(cdist_sumofsquares_ldist_lln_obs__times_values)
    auto cdist_sumofsquares_ldist_lln_obs__times = intlookup_zip(cdist_sumofsquares_ldist_lln_obs__times_keys, cdist_sumofsquares_ldist_lln_obs__times_values);
    DATA_ARRAY(cdist_sumofsquares_ldist_lln_obs__num)
    array<Type> nll_cdist_sumofsquares_ldist_lln__num(as_integer(total_steps + 1)); nll_cdist_sumofsquares_ldist_lln__num.setZero();
    array<Type> nll_cdist_sumofsquares_ldist_lln__weight(as_integer(total_steps + 1)); nll_cdist_sumofsquares_ldist_lln__weight.setZero();
    Type g3l_understocking_total = (double)(0);
    array<Type> nll_understocking__weight(as_integer(total_steps + 1)); nll_understocking__weight.setZero();
    array<Type> ling_imm_movement__transitioning_num(35,1,1);
    array<Type> ling_imm_movement__transitioning_wgt(35,1,1);
    int ling_imm_movement__area = 1;
    int ling_imm_movement__minage = 11;
    int ling_imm_movement__maxage = 11;

    while (true) {
        {
            // g3a_time: Start of time period;
            cur_time += 1;
            if ( cur_time == 0 && assert_msg(retro_years >= (double)(0), "retro_years must be >= 0") ) {
                return NAN;
            }
            cur_year = start_year + (((int) cur_time) / ((int) step_count));
            cur_year_projection = cur_year > end_year - retro_years;
            cur_step = (cur_time % step_count) + 1;
            cur_step_size = step_lengths ( cur_step - 1 ) / (double)(12);
            cur_step_final = cur_step == step_count;
        }
        {
            // g3a_initialconditions for ling_imm;
            {
                auto area = ling_imm__area;

                auto ling_imm__area_idx = 0;

                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) if ( cur_time == 0 ) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    auto dnorm = ((ling_imm__midlen - (ling__Linf*((double)(1) - exp(-(double)(1)*ling__K*((age - cur_step_size) - (recage + log((double)(1) - ling__recl / ling__Linf) / ling__K)))))) / ling_imm_stddev ( as_integer((age - cur_step_size)) - 3 + 2 - 1 ));

                    auto factor = (lingimm__init__scalar*exp(-(double)(1)*(lingimm__M + ling__init__F)*age)*lingimm__init ( as_integer(age) - 3 + 1 - 1 ));

                    {
                        ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx) = normalize_vec(exp(-((dnorm).pow((double)(2)))*(double)(0.5)))*(double)(10000)*factor;
                        ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx) = lingimm__walpha*(ling_imm__midlen).pow(lingimm__wbeta);
                    }
                }
            }
        }
        {
            // g3a_initialconditions for ling_mat;
            {
                auto area = ling_mat__area;

                auto ling_mat__area_idx = 0;

                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) if ( cur_time == 0 ) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    auto dnorm = ((ling_mat__midlen - (ling__Linf*((double)(1) - exp(-(double)(1)*ling__K*((age - cur_step_size) - (recage + log((double)(1) - ling__recl / ling__Linf) / ling__K)))))) / ling_mat_stddev ( as_integer((age - cur_step_size)) - 5 + 2 - 1 ));

                    auto factor = (lingmat__init__scalar*exp(-(double)(1)*(lingmat__M + ling__init__F)*age)*lingmat__init ( as_integer(age) - 5 + 1 - 1 ));

                    {
                        ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx) = normalize_vec(exp(-((dnorm).pow((double)(2)))*(double)(0.5)))*(double)(10000)*factor;
                        ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx) = lingmat__walpha*(ling_mat__midlen).pow(lingmat__wbeta);
                    }
                }
            }
        }
        if ( cur_time == 0 ) {
            suit_ling_imm_igfs__report = (double)(1) / ((double)(1) + exp(-ling__igfs__alpha*(ling_imm__midlen - ling__igfs__l50)));
            REPORT(suit_ling_imm_igfs__report);
        }
        if ( cur_time == 0 ) {
            suit_ling_mat_igfs__report = (double)(1) / ((double)(1) + exp(-ling__igfs__alpha*(ling_mat__midlen - ling__igfs__l50)));
            REPORT(suit_ling_mat_igfs__report);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_understocking__wgt);
        }
        {
            if ( cur_time > total_steps ) {
                return nll;
            }
        }
        ling_imm__totalpredate.setZero();
        ling_mat__totalpredate.setZero();
        igfs__totalsuit.setZero();
        {
            auto suitability = (vector<Type>)(suit_ling_imm_igfs__report);

            {
                // g3a_predate for igfs predating ling_imm;
                ling_imm_igfs__suit.setZero();
                {
                    auto area = ling_imm__area;

                    auto ling_imm__area_idx = 0;

                    for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) if ( area == igfs__area ) {
                        auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                        auto catchability = (suitability*ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx)*ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx));

                        auto igfs__area_idx = 0;

                        auto predator_area = area;

                        {
                            // Collect all suitable ling_imm biomass for igfs;
                            ling_imm_igfs__suit.col(ling_imm__area_idx).col(ling_imm__age_idx) = catchability;
                            igfs__totalsuit(igfs__area_idx) += (ling_imm_igfs__suit.col(ling_imm__area_idx).col(ling_imm__age_idx)).sum();
                        }
                    }
                }
            }
        }
        {
            auto suitability = (vector<Type>)(suit_ling_mat_igfs__report);

            {
                // g3a_predate for igfs predating ling_mat;
                ling_mat_igfs__suit.setZero();
                {
                    auto area = ling_mat__area;

                    auto ling_mat__area_idx = 0;

                    for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) if ( area == igfs__area ) {
                        auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                        auto catchability = (suitability*ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx)*ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx));

                        auto igfs__area_idx = 0;

                        auto predator_area = area;

                        {
                            // Collect all suitable ling_mat biomass for igfs;
                            ling_mat_igfs__suit.col(ling_mat__area_idx).col(ling_mat__age_idx) = catchability;
                            igfs__totalsuit(igfs__area_idx) += (ling_mat_igfs__suit.col(ling_mat__area_idx).col(ling_mat__age_idx)).sum();
                        }
                    }
                }
            }
        }
        {
            // Scale igfs catch of ling_imm by total expected catch;
            ling_imm_igfs__cons.setZero();
            {
                auto area = ling_imm__area;

                auto ling_imm__area_idx = 0;

                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) if ( area == igfs__area ) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    auto igfs__area_idx = 0;

                    auto predator_area = area;

                    auto total_predsuit = igfs__totalsuit(igfs__area_idx);

                    ling_imm_igfs__cons.col(ling_imm__area_idx).col(ling_imm__age_idx) = ling_imm_igfs__suit.col(ling_imm__area_idx).col(ling_imm__age_idx)*((area != 1 ? (double)(0) : intlookup_getdefault(igfs_totaldata, (cur_year*100 + cur_step), (double)(0))) / total_predsuit);
                }
            }
            ling_imm__totalpredate = nonconform_add(ling_imm__totalpredate, ling_imm_igfs__cons);
        }
        {
            // Scale igfs catch of ling_mat by total expected catch;
            ling_mat_igfs__cons.setZero();
            {
                auto area = ling_mat__area;

                auto ling_mat__area_idx = 0;

                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) if ( area == igfs__area ) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    auto igfs__area_idx = 0;

                    auto predator_area = area;

                    auto total_predsuit = igfs__totalsuit(igfs__area_idx);

                    ling_mat_igfs__cons.col(ling_mat__area_idx).col(ling_mat__age_idx) = ling_mat_igfs__suit.col(ling_mat__area_idx).col(ling_mat__age_idx)*((area != 1 ? (double)(0) : intlookup_getdefault(igfs_totaldata, (cur_year*100 + cur_step), (double)(0))) / total_predsuit);
                }
            }
            ling_mat__totalpredate = nonconform_add(ling_mat__totalpredate, ling_mat_igfs__cons);
        }
        {
            // Calculate ling_imm overconsumption coefficient;
            // Apply overconsumption to ling_imm;
            ling_imm__consratio = ling_imm__totalpredate / avoid_zero(ling_imm__num*ling_imm__wgt);
            ling_imm__consratio = dif_pmin(ling_imm__consratio, (double)(0.95), (double)(1000));
            ling_imm__overconsumption = (ling_imm__totalpredate).sum();
            ling_imm__consconv = (double)(1) / avoid_zero(ling_imm__totalpredate);
            ling_imm__totalpredate = (ling_imm__num*ling_imm__wgt)*ling_imm__consratio;
            ling_imm__overconsumption -= (ling_imm__totalpredate).sum();
            ling_imm__consconv *= ling_imm__totalpredate;
            ling_imm__num *= ((double)(1) - ling_imm__consratio);
        }
        {
            // Calculate ling_mat overconsumption coefficient;
            // Apply overconsumption to ling_mat;
            ling_mat__consratio = ling_mat__totalpredate / avoid_zero(ling_mat__num*ling_mat__wgt);
            ling_mat__consratio = dif_pmin(ling_mat__consratio, (double)(0.95), (double)(1000));
            ling_mat__overconsumption = (ling_mat__totalpredate).sum();
            ling_mat__consconv = (double)(1) / avoid_zero(ling_mat__totalpredate);
            ling_mat__totalpredate = (ling_mat__num*ling_mat__wgt)*ling_mat__consratio;
            ling_mat__overconsumption -= (ling_mat__totalpredate).sum();
            ling_mat__consconv *= ling_mat__totalpredate;
            ling_mat__num *= ((double)(1) - ling_mat__consratio);
        }
        {
            // Apply overconsumption to ling_imm_igfs__cons;
            ling_imm_igfs__cons = nonconform_mult(ling_imm_igfs__cons, ling_imm__consconv);
        }
        {
            // Apply overconsumption to ling_mat_igfs__cons;
            ling_mat_igfs__cons = nonconform_mult(ling_mat_igfs__cons, ling_mat__consconv);
        }
        {
            ling_imm__predby_igfs.setZero();
            ling_imm__predby_igfs = nonconform_add(ling_imm__predby_igfs, ling_imm_igfs__cons);
        }
        {
            ling_mat__predby_igfs.setZero();
            ling_mat__predby_igfs = nonconform_add(ling_mat__predby_igfs, ling_mat_igfs__cons);
        }
        {
            // Natural mortality for ling_imm;
            {
                auto area = ling_imm__area;

                auto ling_imm__area_idx = 0;

                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx) *= exp(-(lingimm__M)*cur_step_size);
                }
            }
        }
        {
            // Natural mortality for ling_mat;
            {
                auto area = ling_mat__area;

                auto ling_mat__area_idx = 0;

                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx) *= exp(-(lingmat__M)*cur_step_size);
                }
            }
        }
        if ( cur_step_final ) {
            // Reset transitioning arrays;
            ling_imm__transitioning_num.setZero();
            ling_imm__transitioning_wgt = ling_imm__wgt;
        }
        {
            auto maturity_ratio = ((double)(1) / ((double)(1) + exp(((double)(0) - ((double)(0.001)*ling__mat1)*(ling_imm__midlen - ling__mat2)))));

            auto growth_delta_l = (ling_imm__growth_lastcalc == std::floor(cur_step_size*12) ? ling_imm__growth_l : (ling_imm__growth_l = growth_bbinom(avoid_zero(avoid_zero((ling__Linf - ling_imm__midlen)*((double)(1) - exp(-((ling__K*(double)(0.001)))*cur_step_size))) / ling_imm__plusdl), 15, avoid_zero((ling__bbin*(double)(10))))));

            auto growth_delta_w = (ling_imm__growth_lastcalc == std::floor(cur_step_size*12) ? ling_imm__growth_w : (ling_imm__growth_w = (g3a_grow_vec_rotate((ling_imm__midlen).pow(lingimm__wbeta), 15 + (double)(1)) - g3a_grow_vec_extrude((ling_imm__midlen).pow(lingimm__wbeta), 15 + (double)(1)))*lingimm__walpha));

            auto growthmat_w = g3a_grow_matrix_wgt(growth_delta_w);

            auto growthmat_l = g3a_grow_matrix_len(growth_delta_l);

            {
                // g3a_grow for ling_imm;
                {
                    auto area = ling_imm__area;

                    auto ling_imm__area_idx = 0;

                    for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                        auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                        auto growthmatresult = g3a_grow_apply(growthmat_l, growthmat_w, ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx)*maturity_ratio, ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx));

                        auto growthimmresult = g3a_grow_apply(growthmat_l, growthmat_w, ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx)*((double)(1) - maturity_ratio), ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx));

                        auto growthresult = g3a_grow_apply(growthmat_l, growthmat_w, ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx), ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx));

                        {
                            if ( true ) {
                                ling_imm__prevtotal = (ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx)).sum();
                            }
                            if (cur_step_final) {
                                // Grow and separate maturing ling_imm;
                                ling_imm__transitioning_num.col(ling_imm__area_idx).col(ling_imm__age_idx) = growthmatresult.col(0);
                                ling_imm__transitioning_wgt.col(ling_imm__area_idx).col(ling_imm__age_idx) = growthmatresult.col(1);
                                // Grow non-maturing ling_imm;
                                ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx) = growthimmresult.col(0);
                                ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx) = growthimmresult.col(1);
                            } else {
                                // Update ling_imm using delta matrices;
                                ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx) = growthresult.col(0);
                                ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx) = growthresult.col(1);
                            }
                            if ( true ) {
                                if (cur_step_final) {
                                    assert_msg(CppAD::abs(ling_imm__prevtotal - (ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx)).sum() - (ling_imm__transitioning_num.col(ling_imm__area_idx).col(ling_imm__age_idx)).sum()) < (double)(1e-04), "g3a_growmature: ling_imm__num totals are not the same before and after growth (excluding maturation)");
                                } else {
                                    assert_msg(CppAD::abs(ling_imm__prevtotal - (ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx)).sum()) < (double)(1e-04), "g3a_growmature: ling_imm__num totals are not the same before and after growth");
                                }
                            }
                        }
                    }
                }
                ling_imm__growth_lastcalc = std::floor(cur_step_size*12);
            }
        }
        {
            auto growth_delta_l = (ling_mat__growth_lastcalc == std::floor(cur_step_size*12) ? ling_mat__growth_l : (ling_mat__growth_l = growth_bbinom(avoid_zero(avoid_zero((ling__Linf - ling_mat__midlen)*((double)(1) - exp(-((ling__K*(double)(0.001)))*cur_step_size))) / ling_mat__plusdl), 15, avoid_zero((ling__bbin*(double)(10))))));

            auto growth_delta_w = (ling_mat__growth_lastcalc == std::floor(cur_step_size*12) ? ling_mat__growth_w : (ling_mat__growth_w = (g3a_grow_vec_rotate((ling_mat__midlen).pow(lingmat__wbeta), 15 + (double)(1)) - g3a_grow_vec_extrude((ling_mat__midlen).pow(lingmat__wbeta), 15 + (double)(1)))*lingmat__walpha));

            auto growthmat_w = g3a_grow_matrix_wgt(growth_delta_w);

            auto growthmat_l = g3a_grow_matrix_len(growth_delta_l);

            {
                // g3a_grow for ling_mat;
                {
                    auto area = ling_mat__area;

                    auto ling_mat__area_idx = 0;

                    for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                        auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                        auto growthresult = g3a_grow_apply(growthmat_l, growthmat_w, ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx), ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx));

                        {
                            if ( true ) {
                                ling_mat__prevtotal = (ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx)).sum();
                            }
                            // Update ling_mat using delta matrices;
                            ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx) = growthresult.col(0);
                            ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx) = growthresult.col(1);
                            if ( true ) {
                                assert_msg(CppAD::abs(ling_mat__prevtotal - (ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx)).sum()) < (double)(1e-04), "g3a_growmature: ling_mat__num totals are not the same before and after growth");
                            }
                        }
                    }
                }
                ling_mat__growth_lastcalc = std::floor(cur_step_size*12);
            }
        }
        {
            // Move ling_imm to ling_mat;
            {
                auto area = ling_mat__area;

                auto ling_mat__area_idx = 0;

                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) if ( area == ling_imm__area ) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    {
                        auto ling_imm__area_idx = 0;

                        if ( age >= ling_imm__minage && age <= ling_imm__maxage ) {
                            if ( cur_step_final ) {
                                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                                {
                                    ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx) = (ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx)*ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx)) + ling_imm__transitioning_wgt.col(ling_imm__area_idx).col(ling_imm__age_idx)*ling_imm__transitioning_num.col(ling_imm__area_idx).col(ling_imm__age_idx);
                                    ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx) += ling_imm__transitioning_num.col(ling_imm__area_idx).col(ling_imm__age_idx);
                                    ling_imm__transitioning_num.col(ling_imm__area_idx).col(ling_imm__age_idx) -= ling_imm__transitioning_num.col(ling_imm__area_idx).col(ling_imm__age_idx);
                                    ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx) /= avoid_zero(ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx));
                                }
                            }
                        }
                    }
                }
            }
            // Move any unclaimed stock back to ling_imm;
            if ( cur_step_final ) {
                ling_imm__num += ling_imm__transitioning_num;
            }
        }
        {
            auto factor = (ling__rec__scalar*map_extras::at_throw(pt__ling__rec, std::make_tuple(cur_year), "ling.rec"));

            {
                // g3a_renewal for ling_imm;
                {
                    auto area = ling_imm__area;

                    auto ling_imm__area_idx = 0;

                    for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) if ( cur_step == 1 && age == 3 ) {
                        auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                        auto dnorm = ((ling_imm__midlen - (ling__Linf*((double)(1) - exp(-(double)(1)*ling__K*(age - (recage + log((double)(1) - ling__recl / ling__Linf) / ling__K)))))) / ling_imm_stddev ( as_integer(age) - 3 + 1 - 1 ));

                        {
                            ling_imm__renewalnum.col(ling_imm__area_idx).col(ling_imm__age_idx) = normalize_vec(exp(-((dnorm).pow((double)(2)))*(double)(0.5)))*(double)(10000)*factor;
                            ling_imm__renewalwgt.col(ling_imm__area_idx).col(ling_imm__age_idx) = lingimm__walpha*(ling_imm__midlen).pow(lingimm__wbeta);
                            // Add result to ling_imm;
                            ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx) = ratio_add_vec(ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx), ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx), ling_imm__renewalwgt.col(ling_imm__area_idx).col(ling_imm__age_idx), ling_imm__renewalnum.col(ling_imm__area_idx).col(ling_imm__age_idx));
                            ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx) += ling_imm__renewalnum.col(ling_imm__area_idx).col(ling_imm__age_idx);
                        }
                    }
                }
            }
        }
        {
            auto factor = (ling__rec__scalar*map_extras::at_throw(pt__ling__rec, std::make_tuple(cur_year), "ling.rec"));

            {
                // g3a_renewal for ling_imm;
                {
                    auto area = ling_imm__area;

                    auto ling_imm__area_idx = 0;

                    for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) if ( cur_step == 1 && age == 5 ) {
                        auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                        auto dnorm = ((ling_imm__midlen - (ling__Linf*((double)(1) - exp(-(double)(1)*ling__K*(age - (recage + log((double)(1) - ling__recl / ling__Linf) / ling__K)))))) / ling_imm_stddev ( as_integer(age) - 3 + 1 - 1 ));

                        {
                            ling_imm__renewalnum.col(ling_imm__area_idx).col(ling_imm__age_idx) = normalize_vec(exp(-((dnorm).pow((double)(2)))*(double)(0.5)))*(double)(10000)*factor;
                            ling_imm__renewalwgt.col(ling_imm__area_idx).col(ling_imm__age_idx) = lingimm__walpha*(ling_imm__midlen).pow(lingimm__wbeta);
                            // Add result to ling_imm;
                            ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx) = ratio_add_vec(ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx), ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx), ling_imm__renewalwgt.col(ling_imm__area_idx).col(ling_imm__age_idx), ling_imm__renewalnum.col(ling_imm__area_idx).col(ling_imm__age_idx));
                            ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx) += ling_imm__renewalnum.col(ling_imm__area_idx).col(ling_imm__age_idx);
                        }
                    }
                }
            }
        }
        if ( cdist_sumofsquares_ldist_lln_weight > (double)(0) ) {
            // g3l_catchdistribution_sumofsquares: Collect catch from igfs/ling_imm for cdist_sumofsquares_ldist_lln;
            {
                auto area = ling_imm__area;

                auto ling_imm__area_idx = 0;

                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) if ( area == cdist_sumofsquares_ldist_lln_model__area ) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    {
                        auto cdist_sumofsquares_ldist_lln_model__area_idx = 0;

                        {
                            // Convert ling_imm_igfs to num;
                            cdist_sumofsquares_ldist_lln_model__num.col(cdist_sumofsquares_ldist_lln_model__area_idx) += (ling_imm_igfs__cons.col(ling_imm__area_idx).col(ling_imm__age_idx) / avoid_zero(ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx)));
                        }
                    }
                }
            }
        }
        if ( cdist_sumofsquares_ldist_lln_weight > (double)(0) ) {
            // g3l_catchdistribution_sumofsquares: Collect catch from igfs/ling_mat for cdist_sumofsquares_ldist_lln;
            {
                auto area = ling_mat__area;

                auto ling_mat__area_idx = 0;

                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) if ( area == cdist_sumofsquares_ldist_lln_model__area ) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    {
                        auto cdist_sumofsquares_ldist_lln_model__area_idx = 0;

                        {
                            // Convert ling_mat_igfs to num;
                            cdist_sumofsquares_ldist_lln_model__num.col(cdist_sumofsquares_ldist_lln_model__area_idx) += (ling_mat_igfs__cons.col(ling_mat__area_idx).col(ling_mat__age_idx) / avoid_zero(ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx)));
                        }
                    }
                }
            }
        }
        {
            // g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_ldist_lln_model to cdist_sumofsquares_ldist_lln_obs;
            if ( cdist_sumofsquares_ldist_lln_weight > (double)(0) ) {
                {
                    auto area = cdist_sumofsquares_ldist_lln_model__area;

                    auto cdist_sumofsquares_ldist_lln_model__area_idx = 0;

                    auto cdist_sumofsquares_ldist_lln_model__sstotal = avoid_zero((cdist_sumofsquares_ldist_lln_model__num.col(cdist_sumofsquares_ldist_lln_model__area_idx)).sum());

                    if ( area == cdist_sumofsquares_ldist_lln_obs__area ) {
                        auto cdist_sumofsquares_ldist_lln_obs__area_idx = 0;

                        auto cdist_sumofsquares_ldist_lln_obs__time_idx = intlookup_getdefault(cdist_sumofsquares_ldist_lln_obs__times, (cur_year*100 + cur_step), -1) - 1;

                        if ( cdist_sumofsquares_ldist_lln_obs__time_idx >= 0 ) {
                            auto cdist_sumofsquares_ldist_lln_obs__sstotal = avoid_zero((cdist_sumofsquares_ldist_lln_obs__num.col(cdist_sumofsquares_ldist_lln_obs__area_idx).col(cdist_sumofsquares_ldist_lln_obs__time_idx)).sum());

                            auto cur_cdist_nll = ((pow(((cdist_sumofsquares_ldist_lln_model__num.col(cdist_sumofsquares_ldist_lln_model__area_idx) / cdist_sumofsquares_ldist_lln_model__sstotal) - (cdist_sumofsquares_ldist_lln_obs__num.col(cdist_sumofsquares_ldist_lln_obs__area_idx).col(cdist_sumofsquares_ldist_lln_obs__time_idx) / cdist_sumofsquares_ldist_lln_obs__sstotal)), (Type)(double)(2)))).sum();

                            {
                                nll += cdist_sumofsquares_ldist_lln_weight*cur_cdist_nll;
                                nll_cdist_sumofsquares_ldist_lln__num(cur_time + 1 - 1) += cur_cdist_nll;
                                nll_cdist_sumofsquares_ldist_lln__weight(cur_time + 1 - 1) = cdist_sumofsquares_ldist_lln_weight;
                            }
                        }
                    }
                }
                // Zero counters for next reporting period;
                cdist_sumofsquares_ldist_lln_model__num.setZero();
            }
        }
        {
            // Reset understocking total;
            g3l_understocking_total = (double)(0);
        }
        {
            // g3l_understocking for ling_imm;
            // Add understocking from ling_imm as biomass to nll;
            g3l_understocking_total += ling_imm__overconsumption;
        }
        {
            // g3l_understocking for ling_mat;
            // Add understocking from ling_mat as biomass to nll;
            g3l_understocking_total += ling_mat__overconsumption;
        }
        {
            // g3l_understocking: Combine and add to nll;
            g3l_understocking_total = pow(g3l_understocking_total, (Type)(double)(2));
            nll += (double)(1e+08)*g3l_understocking_total;
            nll_understocking__wgt(cur_time + 1 - 1) += g3l_understocking_total;
            nll_understocking__weight(cur_time + 1 - 1) = (double)(1e+08);
        }
        if ( cur_step_final ) {
            auto ling_imm_movement__area_idx = 0;

            auto ling_imm__area_idx = 0;

            {
                // g3a_age for ling_imm;
                for (auto age = ling_imm__maxage; age >= ling_imm__minage; age--) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    {
                        // Check stock has remained finite for this step;
                        if (age == ling_imm__maxage) {
                            // Move oldest ling_imm into ling_imm_movement;
                            ling_imm_movement__transitioning_num.col(ling_imm_movement__area_idx).col(0) = ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx);
                            ling_imm_movement__transitioning_wgt.col(ling_imm_movement__area_idx).col(0) = ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx);
                            ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx) = ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx - 1);
                            ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx) = ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx - 1);
                        } else {
                            if (age == ling_imm__minage) {
                                // Empty youngest ling_imm age-group;
                                ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx).setZero();
                            } else {
                                // Move ling_imm age-group to next one up;
                                ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx) = ling_imm__num.col(ling_imm__area_idx).col(ling_imm__age_idx - 1);
                                ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx) = ling_imm__wgt.col(ling_imm__area_idx).col(ling_imm__age_idx - 1);
                            }
                        }
                    }
                }
            }
        }
        if ( cur_step_final ) {
            auto ling_mat__area_idx = 0;

            {
                // g3a_age for ling_mat;
                for (auto age = ling_mat__maxage; age >= ling_mat__minage; age--) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    {
                        // Check stock has remained finite for this step;
                        if (age == ling_mat__maxage) {
                            // Oldest ling_mat is a plus-group, combine with younger individuals;
                            ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx) = ratio_add_vec(ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx), ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx), ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx - 1), ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx - 1));
                            ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx) += ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx - 1);
                        } else {
                            if (age == ling_mat__minage) {
                                // Empty youngest ling_mat age-group;
                                ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx).setZero();
                            } else {
                                // Move ling_mat age-group to next one up;
                                ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx) = ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx - 1);
                                ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx) = ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx - 1);
                            }
                        }
                    }
                }
            }
        }
        {
            // Move ling_imm_movement to ling_mat;
            {
                auto area = ling_mat__area;

                auto ling_mat__area_idx = 0;

                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) if ( area == ling_imm_movement__area ) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    {
                        auto ling_imm_movement__area_idx = 0;

                        if ( age >= ling_imm_movement__minage && age <= ling_imm_movement__maxage ) {
                            if ( cur_step_final ) {
                                auto ling_imm_movement__age_idx = age - ling_imm_movement__minage + 1 - 1;

                                {
                                    ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx) = (ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx)*ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx)) + ling_imm_movement__transitioning_wgt.col(ling_imm_movement__area_idx).col(ling_imm_movement__age_idx)*ling_imm_movement__transitioning_num.col(ling_imm_movement__area_idx).col(ling_imm_movement__age_idx);
                                    ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx) += ling_imm_movement__transitioning_num.col(ling_imm_movement__area_idx).col(ling_imm_movement__age_idx);
                                    ling_mat__wgt.col(ling_mat__area_idx).col(ling_mat__age_idx) /= avoid_zero(ling_mat__num.col(ling_mat__area_idx).col(ling_mat__age_idx));
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
