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

template<typename T, typename DefT> T intlookup_getdefault(std::map<int, T> lookup, int key, DefT def) {
            return lookup.count(key) > 0 ? lookup[key] : (T)def;
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
    std::map<std::tuple<int>, Type*> pt__fish__init = {{std::make_tuple(1), &fish__init__1}, {std::make_tuple(2), &fish__init__2}, {std::make_tuple(3), &fish__init__3}, {std::make_tuple(4), &fish__init__4}, {std::make_tuple(5), &fish__init__5}};
    PARAMETER(fish__M__1);
    PARAMETER(fish__M__2);
    PARAMETER(fish__M__3);
    PARAMETER(fish__M__4);
    PARAMETER(fish__M__5);
    std::map<std::tuple<int>, Type*> pt__fish__M = {{std::make_tuple(1), &fish__M__1}, {std::make_tuple(2), &fish__M__2}, {std::make_tuple(3), &fish__M__3}, {std::make_tuple(4), &fish__M__4}, {std::make_tuple(5), &fish__M__5}};
    PARAMETER(init__F);
    PARAMETER(recage);
    PARAMETER(fish__walpha);
    PARAMETER(fish__wbeta);
    PARAMETER(report_detail);
    PARAMETER(fish__f_surv__alpha);
    PARAMETER(fish__f_surv__l50);
    PARAMETER(fish__bbin);
    PARAMETER(fish__rec__1979);
    PARAMETER(fish__rec__1980);
    PARAMETER(fish__rec__1981);
    PARAMETER(fish__rec__1982);
    PARAMETER(fish__rec__1983);
    PARAMETER(fish__rec__1984);
    PARAMETER(fish__rec__1985);
    PARAMETER(fish__rec__1986);
    PARAMETER(fish__rec__1987);
    PARAMETER(fish__rec__1988);
    PARAMETER(fish__rec__1989);
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
    PARAMETER(fish__rec__2001);
    PARAMETER(fish__rec__2002);
    PARAMETER(fish__rec__2003);
    PARAMETER(fish__rec__2004);
    PARAMETER(fish__rec__2005);
    PARAMETER(fish__rec__2006);
    PARAMETER(fish__rec__2007);
    PARAMETER(fish__rec__2008);
    PARAMETER(fish__rec__2009);
    PARAMETER(fish__rec__2010);
    PARAMETER(fish__rec__2011);
    PARAMETER(fish__rec__2012);
    PARAMETER(fish__rec__2013);
    PARAMETER(fish__rec__2014);
    PARAMETER(fish__rec__2015);
    PARAMETER(fish__rec__2016);
    PARAMETER(fish__rec__2017);
    PARAMETER(fish__rec__2018);
    PARAMETER(fish__rec__2019);
    PARAMETER(fish__rec__2020);
    PARAMETER(fish__rec__2021);
    PARAMETER(fish__rec__2022);
    PARAMETER(fish__rec__2023);
    std::map<std::tuple<int>, Type*> pt__fish__rec = {{std::make_tuple(1979), &fish__rec__1979}, {std::make_tuple(1980), &fish__rec__1980}, {std::make_tuple(1981), &fish__rec__1981}, {std::make_tuple(1982), &fish__rec__1982}, {std::make_tuple(1983), &fish__rec__1983}, {std::make_tuple(1984), &fish__rec__1984}, {std::make_tuple(1985), &fish__rec__1985}, {std::make_tuple(1986), &fish__rec__1986}, {std::make_tuple(1987), &fish__rec__1987}, {std::make_tuple(1988), &fish__rec__1988}, {std::make_tuple(1989), &fish__rec__1989}, {std::make_tuple(1990), &fish__rec__1990}, {std::make_tuple(1991), &fish__rec__1991}, {std::make_tuple(1992), &fish__rec__1992}, {std::make_tuple(1993), &fish__rec__1993}, {std::make_tuple(1994), &fish__rec__1994}, {std::make_tuple(1995), &fish__rec__1995}, {std::make_tuple(1996), &fish__rec__1996}, {std::make_tuple(1997), &fish__rec__1997}, {std::make_tuple(1998), &fish__rec__1998}, {std::make_tuple(1999), &fish__rec__1999}, {std::make_tuple(2000), &fish__rec__2000}, {std::make_tuple(2001), &fish__rec__2001}, {std::make_tuple(2002), &fish__rec__2002}, {std::make_tuple(2003), &fish__rec__2003}, {std::make_tuple(2004), &fish__rec__2004}, {std::make_tuple(2005), &fish__rec__2005}, {std::make_tuple(2006), &fish__rec__2006}, {std::make_tuple(2007), &fish__rec__2007}, {std::make_tuple(2008), &fish__rec__2008}, {std::make_tuple(2009), &fish__rec__2009}, {std::make_tuple(2010), &fish__rec__2010}, {std::make_tuple(2011), &fish__rec__2011}, {std::make_tuple(2012), &fish__rec__2012}, {std::make_tuple(2013), &fish__rec__2013}, {std::make_tuple(2014), &fish__rec__2014}, {std::make_tuple(2015), &fish__rec__2015}, {std::make_tuple(2016), &fish__rec__2016}, {std::make_tuple(2017), &fish__rec__2017}, {std::make_tuple(2018), &fish__rec__2018}, {std::make_tuple(2019), &fish__rec__2019}, {std::make_tuple(2020), &fish__rec__2020}, {std::make_tuple(2021), &fish__rec__2021}, {std::make_tuple(2022), &fish__rec__2022}, {std::make_tuple(2023), &fish__rec__2023}};
    PARAMETER(fish__rec__scalar);
    PARAMETER(fish__rec__sd);
    DATA_SCALAR(adist_surveyindices_log_dist_si_cpue_weight__lower);
    DATA_SCALAR(adist_surveyindices_log_dist_si_cpue_weight__upper);
    DATA_SCALAR(cdist_sumofsquares_aldist_f_surv_weight__lower);
    DATA_SCALAR(cdist_sumofsquares_aldist_f_surv_weight__upper);
    DATA_SCALAR(cdist_sumofsquares_ldist_f_surv_weight__lower);
    DATA_SCALAR(cdist_sumofsquares_ldist_f_surv_weight__upper);
    DATA_SCALAR(fish__K__lower);
    DATA_SCALAR(fish__K__upper);
    DATA_SCALAR(fish__Linf__lower);
    DATA_SCALAR(fish__Linf__upper);
    DATA_SCALAR(fish__M__1__lower);
    DATA_SCALAR(fish__M__1__upper);
    DATA_SCALAR(fish__M__2__lower);
    DATA_SCALAR(fish__M__2__upper);
    DATA_SCALAR(fish__M__3__lower);
    DATA_SCALAR(fish__M__3__upper);
    DATA_SCALAR(fish__M__4__lower);
    DATA_SCALAR(fish__M__4__upper);
    DATA_SCALAR(fish__M__5__lower);
    DATA_SCALAR(fish__M__5__upper);
    DATA_SCALAR(fish__bbin__lower);
    DATA_SCALAR(fish__bbin__upper);
    DATA_SCALAR(fish__f_surv__alpha__lower);
    DATA_SCALAR(fish__f_surv__alpha__upper);
    DATA_SCALAR(fish__f_surv__l50__lower);
    DATA_SCALAR(fish__f_surv__l50__upper);
    DATA_SCALAR(fish__init__1__lower);
    DATA_SCALAR(fish__init__1__upper);
    DATA_SCALAR(fish__init__2__lower);
    DATA_SCALAR(fish__init__2__upper);
    DATA_SCALAR(fish__init__3__lower);
    DATA_SCALAR(fish__init__3__upper);
    DATA_SCALAR(fish__init__4__lower);
    DATA_SCALAR(fish__init__4__upper);
    DATA_SCALAR(fish__init__5__lower);
    DATA_SCALAR(fish__init__5__upper);
    DATA_SCALAR(fish__init__scalar__lower);
    DATA_SCALAR(fish__init__scalar__upper);
    DATA_SCALAR(fish__lencv__lower);
    DATA_SCALAR(fish__lencv__upper);
    DATA_SCALAR(fish__rec__1979__lower);
    DATA_SCALAR(fish__rec__1979__upper);
    DATA_SCALAR(fish__rec__1980__lower);
    DATA_SCALAR(fish__rec__1980__upper);
    DATA_SCALAR(fish__rec__1981__lower);
    DATA_SCALAR(fish__rec__1981__upper);
    DATA_SCALAR(fish__rec__1982__lower);
    DATA_SCALAR(fish__rec__1982__upper);
    DATA_SCALAR(fish__rec__1983__lower);
    DATA_SCALAR(fish__rec__1983__upper);
    DATA_SCALAR(fish__rec__1984__lower);
    DATA_SCALAR(fish__rec__1984__upper);
    DATA_SCALAR(fish__rec__1985__lower);
    DATA_SCALAR(fish__rec__1985__upper);
    DATA_SCALAR(fish__rec__1986__lower);
    DATA_SCALAR(fish__rec__1986__upper);
    DATA_SCALAR(fish__rec__1987__lower);
    DATA_SCALAR(fish__rec__1987__upper);
    DATA_SCALAR(fish__rec__1988__lower);
    DATA_SCALAR(fish__rec__1988__upper);
    DATA_SCALAR(fish__rec__1989__lower);
    DATA_SCALAR(fish__rec__1989__upper);
    DATA_SCALAR(fish__rec__1990__lower);
    DATA_SCALAR(fish__rec__1990__upper);
    DATA_SCALAR(fish__rec__1991__lower);
    DATA_SCALAR(fish__rec__1991__upper);
    DATA_SCALAR(fish__rec__1992__lower);
    DATA_SCALAR(fish__rec__1992__upper);
    DATA_SCALAR(fish__rec__1993__lower);
    DATA_SCALAR(fish__rec__1993__upper);
    DATA_SCALAR(fish__rec__1994__lower);
    DATA_SCALAR(fish__rec__1994__upper);
    DATA_SCALAR(fish__rec__1995__lower);
    DATA_SCALAR(fish__rec__1995__upper);
    DATA_SCALAR(fish__rec__1996__lower);
    DATA_SCALAR(fish__rec__1996__upper);
    DATA_SCALAR(fish__rec__1997__lower);
    DATA_SCALAR(fish__rec__1997__upper);
    DATA_SCALAR(fish__rec__1998__lower);
    DATA_SCALAR(fish__rec__1998__upper);
    DATA_SCALAR(fish__rec__1999__lower);
    DATA_SCALAR(fish__rec__1999__upper);
    DATA_SCALAR(fish__rec__2000__lower);
    DATA_SCALAR(fish__rec__2000__upper);
    DATA_SCALAR(fish__rec__2001__lower);
    DATA_SCALAR(fish__rec__2001__upper);
    DATA_SCALAR(fish__rec__2002__lower);
    DATA_SCALAR(fish__rec__2002__upper);
    DATA_SCALAR(fish__rec__2003__lower);
    DATA_SCALAR(fish__rec__2003__upper);
    DATA_SCALAR(fish__rec__2004__lower);
    DATA_SCALAR(fish__rec__2004__upper);
    DATA_SCALAR(fish__rec__2005__lower);
    DATA_SCALAR(fish__rec__2005__upper);
    DATA_SCALAR(fish__rec__2006__lower);
    DATA_SCALAR(fish__rec__2006__upper);
    DATA_SCALAR(fish__rec__2007__lower);
    DATA_SCALAR(fish__rec__2007__upper);
    DATA_SCALAR(fish__rec__2008__lower);
    DATA_SCALAR(fish__rec__2008__upper);
    DATA_SCALAR(fish__rec__2009__lower);
    DATA_SCALAR(fish__rec__2009__upper);
    DATA_SCALAR(fish__rec__2010__lower);
    DATA_SCALAR(fish__rec__2010__upper);
    DATA_SCALAR(fish__rec__2011__lower);
    DATA_SCALAR(fish__rec__2011__upper);
    DATA_SCALAR(fish__rec__2012__lower);
    DATA_SCALAR(fish__rec__2012__upper);
    DATA_SCALAR(fish__rec__2013__lower);
    DATA_SCALAR(fish__rec__2013__upper);
    DATA_SCALAR(fish__rec__2014__lower);
    DATA_SCALAR(fish__rec__2014__upper);
    DATA_SCALAR(fish__rec__2015__lower);
    DATA_SCALAR(fish__rec__2015__upper);
    DATA_SCALAR(fish__rec__2016__lower);
    DATA_SCALAR(fish__rec__2016__upper);
    DATA_SCALAR(fish__rec__2017__lower);
    DATA_SCALAR(fish__rec__2017__upper);
    DATA_SCALAR(fish__rec__2018__lower);
    DATA_SCALAR(fish__rec__2018__upper);
    DATA_SCALAR(fish__rec__2019__lower);
    DATA_SCALAR(fish__rec__2019__upper);
    DATA_SCALAR(fish__rec__2020__lower);
    DATA_SCALAR(fish__rec__2020__upper);
    DATA_SCALAR(fish__rec__2021__lower);
    DATA_SCALAR(fish__rec__2021__upper);
    DATA_SCALAR(fish__rec__2022__lower);
    DATA_SCALAR(fish__rec__2022__upper);
    DATA_SCALAR(fish__rec__2023__lower);
    DATA_SCALAR(fish__rec__2023__upper);
    DATA_SCALAR(fish__rec__scalar__lower);
    DATA_SCALAR(fish__rec__scalar__upper);
    DATA_SCALAR(fish__rec__sd__lower);
    DATA_SCALAR(fish__rec__sd__upper);
    DATA_SCALAR(fish__t0__lower);
    DATA_SCALAR(fish__t0__upper);
    DATA_SCALAR(fish__walpha__lower);
    DATA_SCALAR(fish__walpha__upper);
    DATA_SCALAR(fish__wbeta__lower);
    DATA_SCALAR(fish__wbeta__upper);
    DATA_SCALAR(init__F__lower);
    DATA_SCALAR(init__F__upper);
    DATA_SCALAR(project_years__lower);
    DATA_SCALAR(project_years__upper);
    DATA_SCALAR(recage__lower);
    DATA_SCALAR(recage__upper);
    DATA_SCALAR(retro_years__lower);
    DATA_SCALAR(retro_years__upper);
    PARAMETER(adist_surveyindices_log_dist_si_cpue_weight);
    PARAMETER(cdist_sumofsquares_aldist_f_surv_weight);
    PARAMETER(cdist_sumofsquares_ldist_f_surv_weight);
    auto assert_msg = [](bool expr, std::string message) -> bool {
    if (!expr) { Rf_warning(message.c_str()); return TRUE; }
    return FALSE;
};
    auto normalize_vec = [](vector<Type> a) -> vector<Type> {
    return a / a.sum();
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
    auto avoid_zero_vec = [](vector<Type> a) -> vector<Type> {
    vector<Type> res(a.size());
    for(int i = 0; i < a.size(); i++) {
        res[i] = logspace_add(a[i] * 1000.0, (Type)0.0) / 1000.0;
    }
    return res;
};
    auto logspace_add_vec = [](vector<Type> a, Type b) -> vector<Type> {
    vector<Type> res(a.size());
    for(int i = 0; i < a.size(); i++) {
        res[i] = logspace_add(a[i], b);
    }
    return res;
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
    auto avoid_zero = [](Type a) -> Type {
    return logspace_add(a * 1000.0, (Type)0.0) / 1000.0;
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

    auto avoid_zero_vec = [](vector<Type> a) -> vector<Type> {
        vector<Type> res(a.size());
        for(int i = 0; i < a.size(); i++) {
            res[i] = logspace_add(a[i] * 1000.0, (Type)0.0) / 1000.0;
        }
        return res;
    };

    // Apply matrices to stock
    // NB: Cast to array to get elementwise multiplication
    growth_matrix = growth_matrix.array().colwise() * input_num.array();
    weight_matrix = (weight_matrix.array().colwise() + input_wgt.array()) * growth_matrix.array();

    // Sum together all length group brackets for both length & weight
    array<Type> combined(total_lgs,2);
    combined.col(0) = growth_matrix.colwise().sum();
    combined.col(1) = weight_matrix.colwise().sum().array().rowwise() / avoid_zero_vec(growth_matrix.colwise().sum()).array().transpose();
    return combined;
};
    auto ratio_add_vec = [&avoid_zero_vec](vector<Type> orig_vec, vector<Type> orig_amount, vector<Type> new_vec, vector<Type> new_amount) -> vector<Type> {
    return (orig_vec * orig_amount + new_vec * new_amount) / avoid_zero_vec(orig_amount + new_amount);
};
    auto surveyindices_linreg = [&avoid_zero](vector<Type> N, vector<Type> I, Type fixed_alpha, Type fixed_beta) -> vector<Type> {
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
    int start_year = 1979;
    vector<int> step_lengths(4); step_lengths.setConstant(3);
    auto step_count = (step_lengths).size();
    int cur_year_projection = false;
    int end_year = 2023;
    int cur_step = 0;
    auto cur_step_size = step_lengths ( 0 ) / (double)(12);
    int cur_step_final = false;
    int fish__minage = 1;
    int fish__maxage = 5;
    int fish__area = 1;
    DATA_VECTOR(fish__midlen)
    array<Type> fish__num(10,1,5); fish__num.setZero();
    array<Type> fish__wgt(10,1,5); fish__wgt.setConstant((double)(1));
    auto total_steps = (step_lengths).size()*(end_year - retro_years - start_year + project_years) + (step_lengths).size() - 1;
    auto as_integer = [](Type v) -> int {
    return std::floor(asDouble(v));
};
    array<double> detail_fish__num(10,1,5,as_integer(total_steps + (double)(1))); detail_fish__num.setZero();
    array<double> detail_fish__wgt(10,1,5,as_integer(total_steps + (double)(1))); detail_fish__wgt.setConstant((double)(1));
    array<Type> suit_fish_f_surv__report(10);
    vector<Type> adist_surveyindices_log_dist_si_cpue_model__params(2); adist_surveyindices_log_dist_si_cpue_model__params.setZero();
    array<Type> adist_surveyindices_log_dist_si_cpue_model__wgt(1,5,1); adist_surveyindices_log_dist_si_cpue_model__wgt.setZero();
    DATA_ARRAY(adist_surveyindices_log_dist_si_cpue_obs__wgt)
    array<Type> cdist_sumofsquares_aldist_f_surv_model__num(5,4,5); cdist_sumofsquares_aldist_f_surv_model__num.setZero();
    DATA_ARRAY(cdist_sumofsquares_aldist_f_surv_obs__num)
    array<Type> cdist_sumofsquares_ldist_f_surv_model__num(5,5); cdist_sumofsquares_ldist_f_surv_model__num.setZero();
    DATA_ARRAY(cdist_sumofsquares_ldist_f_surv_obs__num)
    array<double> detail_fish__predby_f_surv(10,1,5,as_integer(total_steps + (double)(1)));
    array<double> detail_fish__renewalnum(10,1,5,as_integer(total_steps + (double)(1))); detail_fish__renewalnum.setZero();
    array<double> detail_fish_f_surv__cons(10,1,5,as_integer(total_steps + (double)(1)));
    Type nll = (double)(0);
    array<Type> nll_adist_surveyindices_log_dist_si_cpue__weight(as_integer(total_steps + 1)); nll_adist_surveyindices_log_dist_si_cpue__weight.setZero();
    array<Type> nll_adist_surveyindices_log_dist_si_cpue__wgt(as_integer(total_steps + 1)); nll_adist_surveyindices_log_dist_si_cpue__wgt.setZero();
    array<Type> nll_cdist_sumofsquares_aldist_f_surv__num(as_integer(total_steps + 1)); nll_cdist_sumofsquares_aldist_f_surv__num.setZero();
    array<Type> nll_cdist_sumofsquares_aldist_f_surv__weight(as_integer(total_steps + 1)); nll_cdist_sumofsquares_aldist_f_surv__weight.setZero();
    array<Type> nll_cdist_sumofsquares_ldist_f_surv__num(as_integer(total_steps + 1)); nll_cdist_sumofsquares_ldist_f_surv__num.setZero();
    array<Type> nll_cdist_sumofsquares_ldist_f_surv__weight(as_integer(total_steps + 1)); nll_cdist_sumofsquares_ldist_f_surv__weight.setZero();
    array<Type> nll_understocking__wgt(as_integer(total_steps + 1)); nll_understocking__wgt.setZero();
    array<Type> fish__totalpredate(10,1,5);
    array<Type> f_surv__totalsuit(1);
    array<Type> fish_f_surv__suit(10,1,5);
    int f_surv__area = 1;
    array<Type> fish_f_surv__cons(10,1,5);
    DATA_IVECTOR(landings_f_surv_keys)
    DATA_VECTOR(landings_f_surv_values)
    auto landings_f_surv = intlookup_zip(landings_f_surv_keys, landings_f_surv_values);
    array<Type> fish__consratio(10,1,5);
    Type fish__overconsumption = (double)(0);
    array<Type> fish__consconv(10,1,5);
    array<Type> fish__predby_f_surv(10,1,5);
    int fish__growth_lastcalc = -1;
    array<Type> fish__growth_l(10,5);
    Type fish__plusdl = (double)(10);
    array<Type> fish__growth_w(10,5);
    Type fish__prevtotal = (double)(0);
    array<Type> fish__renewalnum(10,1,5); fish__renewalnum.setZero();
    array<Type> fish__renewalwgt(10,1,5); fish__renewalwgt.setZero();
    int adist_surveyindices_log_dist_si_cpue_model__area = 1;
    DATA_IVECTOR(adist_surveyindices_log_dist_si_cpue_model__times_keys)
    DATA_IVECTOR(adist_surveyindices_log_dist_si_cpue_model__times_values)
    auto adist_surveyindices_log_dist_si_cpue_model__times = intlookup_zip(adist_surveyindices_log_dist_si_cpue_model__times_keys, adist_surveyindices_log_dist_si_cpue_model__times_values);
    array<Type> fish_adist_surveyindices_log_dist_si_cpue_model_lgmatrix(1,10); fish_adist_surveyindices_log_dist_si_cpue_model_lgmatrix.setConstant((double)(1));
    int adist_surveyindices_log_dist_si_cpue_obs__area = 1;
    DATA_IVECTOR(cdist_sumofsquares_aldist_f_surv_model__times_keys)
    DATA_IVECTOR(cdist_sumofsquares_aldist_f_surv_model__times_values)
    auto cdist_sumofsquares_aldist_f_surv_model__times = intlookup_zip(cdist_sumofsquares_aldist_f_surv_model__times_keys, cdist_sumofsquares_aldist_f_surv_model__times_values);
    int cdist_sumofsquares_aldist_f_surv_model__minage = 1;
    int cdist_sumofsquares_aldist_f_surv_model__maxage = 4;
    DATA_ARRAY(fish_f_surv_cdist_sumofsquares_aldist_f_surv_model_lgmatrix)
    DATA_IVECTOR(cdist_sumofsquares_aldist_f_surv_obs__times_keys)
    DATA_IVECTOR(cdist_sumofsquares_aldist_f_surv_obs__times_values)
    auto cdist_sumofsquares_aldist_f_surv_obs__times = intlookup_zip(cdist_sumofsquares_aldist_f_surv_obs__times_keys, cdist_sumofsquares_aldist_f_surv_obs__times_values);
    int cdist_sumofsquares_aldist_f_surv_obs__minage = 1;
    int cdist_sumofsquares_aldist_f_surv_obs__maxage = 4;
    DATA_IVECTOR(cdist_sumofsquares_ldist_f_surv_model__times_keys)
    DATA_IVECTOR(cdist_sumofsquares_ldist_f_surv_model__times_values)
    auto cdist_sumofsquares_ldist_f_surv_model__times = intlookup_zip(cdist_sumofsquares_ldist_f_surv_model__times_keys, cdist_sumofsquares_ldist_f_surv_model__times_values);
    DATA_ARRAY(fish_f_surv_cdist_sumofsquares_ldist_f_surv_model_lgmatrix)
    DATA_IVECTOR(cdist_sumofsquares_ldist_f_surv_obs__times_keys)
    DATA_IVECTOR(cdist_sumofsquares_ldist_f_surv_obs__times_values)
    auto cdist_sumofsquares_ldist_f_surv_obs__times = intlookup_zip(cdist_sumofsquares_ldist_f_surv_obs__times_keys, cdist_sumofsquares_ldist_f_surv_obs__times_values);
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

                auto factor = (fish__init__scalar*map_extras::at_throw(pt__fish__init, std::make_tuple(age), "fish.init")*exp(-(double)(1)*(map_extras::at_throw(pt__fish__M, std::make_tuple(age), "fish.M") + init__F)*(age - recage)));

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
            suit_fish_f_surv__report = (double)(1) / ((double)(1) + exp(-fish__f_surv__alpha*(fish__midlen - fish__f_surv__l50)));
            REPORT(suit_fish_f_surv__report);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(adist_surveyindices_log_dist_si_cpue_model__params);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(adist_surveyindices_log_dist_si_cpue_model__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(adist_surveyindices_log_dist_si_cpue_obs__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(cdist_sumofsquares_aldist_f_surv_model__num);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(cdist_sumofsquares_aldist_f_surv_obs__num);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(cdist_sumofsquares_ldist_f_surv_model__num);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(cdist_sumofsquares_ldist_f_surv_obs__num);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(detail_fish__num);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(detail_fish__predby_f_surv);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(detail_fish__renewalnum);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(detail_fish__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(detail_fish_f_surv__cons);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_adist_surveyindices_log_dist_si_cpue__weight);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_adist_surveyindices_log_dist_si_cpue__wgt);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_cdist_sumofsquares_aldist_f_surv__num);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_cdist_sumofsquares_aldist_f_surv__weight);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_cdist_sumofsquares_ldist_f_surv__num);
        }
        if ( reporting_enabled > 0 && cur_time > total_steps ) {
            REPORT(nll_cdist_sumofsquares_ldist_f_surv__weight);
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
        f_surv__totalsuit.setZero();
        {
            auto suitability = (vector<Type>)(suit_fish_f_surv__report);

            {
                // g3a_predate for f_surv predating fish;
                fish_f_surv__suit.setZero();
                for (auto age = fish__minage; age <= fish__maxage; age++) {
                    auto fish__age_idx = age - fish__minage + 1 - 1;

                    auto area = fish__area;

                    auto fish__area_idx = 0;

                    if ( area == f_surv__area ) {
                        auto catchability = (suitability*fish__num.col(fish__age_idx).col(fish__area_idx)*fish__wgt.col(fish__age_idx).col(fish__area_idx));

                        auto f_surv__area_idx = 0;

                        auto predator_area = area;

                        {
                            // Collect all suitable fish biomass for f_surv;
                            fish_f_surv__suit.col(fish__age_idx).col(fish__area_idx) = catchability;
                            f_surv__totalsuit(f_surv__area_idx) += (fish_f_surv__suit.col(fish__age_idx).col(fish__area_idx)).sum();
                        }
                    }
                }
            }
        }
        {
            // Scale f_surv catch of fish by total expected catch;
            fish_f_surv__cons.setZero();
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                if ( area == f_surv__area ) {
                    auto f_surv__area_idx = 0;

                    auto predator_area = area;

                    auto total_predsuit = f_surv__totalsuit(f_surv__area_idx);

                    fish_f_surv__cons.col(fish__age_idx).col(fish__area_idx) = fish_f_surv__suit.col(fish__age_idx).col(fish__area_idx)*((area != 1 ? (double)(0) : intlookup_getdefault(landings_f_surv, (cur_year*100 + cur_step), (double)(0))) / total_predsuit);
                }
            }
            fish__totalpredate = nonconform_add(fish__totalpredate, fish_f_surv__cons);
        }
        {
            // Calculate fish overconsumption coefficient;
            // Apply overconsumption to fish;
            fish__consratio = fish__totalpredate / avoid_zero_vec(fish__num*fish__wgt);
            fish__consratio = logspace_add_vec(fish__consratio*-(double)(1000), (double)(0.95)*-(double)(1000)) / -(double)(1000);
            fish__overconsumption = (fish__totalpredate).sum();
            fish__consconv = (double)(1) / avoid_zero_vec(fish__totalpredate);
            fish__totalpredate = (fish__num*fish__wgt)*fish__consratio;
            fish__overconsumption -= (fish__totalpredate).sum();
            fish__consconv *= fish__totalpredate;
            fish__num *= ((double)(1) - fish__consratio);
        }
        {
            // Apply overconsumption to fish_f_surv__cons;
            fish_f_surv__cons = nonconform_mult(fish_f_surv__cons, fish__consconv);
        }
        {
            fish__predby_f_surv.setZero();
            fish__predby_f_surv = nonconform_add(fish__predby_f_surv, fish_f_surv__cons);
        }
        {
            // Natural mortality for fish;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                fish__num.col(fish__age_idx).col(fish__area_idx) *= exp(-(map_extras::at_throw(pt__fish__M, std::make_tuple(age), "fish.M"))*cur_step_size);
            }
        }
        {
            auto growth_delta_l = (fish__growth_lastcalc == std::floor(cur_step_size*12) ? fish__growth_l : (fish__growth_l = growth_bbinom(avoid_zero_vec(avoid_zero_vec((fish__Linf - fish__midlen)*((double)(1) - exp(-(fish__K)*cur_step_size))) / fish__plusdl), 4, avoid_zero(fish__bbin))));

            auto growth_delta_w = (fish__growth_lastcalc == std::floor(cur_step_size*12) ? fish__growth_w : (fish__growth_w = (g3a_grow_vec_rotate(pow((vector<Type>)(fish__midlen), fish__wbeta), 4 + (double)(1)) - g3a_grow_vec_extrude(pow((vector<Type>)(fish__midlen), fish__wbeta), 4 + (double)(1)))*fish__walpha));

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
                for (auto age = fish__minage; age <= fish__maxage; age++) if ( age == fish__minage && cur_step == 2 && (! cur_year_projection) ) {
                    auto fish__age_idx = age - fish__minage + 1 - 1;

                    auto area = fish__area;

                    auto fish__area_idx = 0;

                    auto dnorm = ((fish__midlen - (fish__Linf*((double)(1) - exp(-(double)(1)*fish__K*(age - fish__t0))))) / fish__rec__sd);

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
        {
            // g3l_bounds_penalty for adist_surveyindices_log_dist_si_cpue_weight;
            if ( cur_time == 0 && std::isfinite(asDouble(adist_surveyindices_log_dist_si_cpue_weight__lower)) && std::isfinite(asDouble(adist_surveyindices_log_dist_si_cpue_weight__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(adist_surveyindices_log_dist_si_cpue_weight - adist_surveyindices_log_dist_si_cpue_weight__upper) / (adist_surveyindices_log_dist_si_cpue_weight__upper - adist_surveyindices_log_dist_si_cpue_weight__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(adist_surveyindices_log_dist_si_cpue_weight__lower - adist_surveyindices_log_dist_si_cpue_weight) / (adist_surveyindices_log_dist_si_cpue_weight__upper - adist_surveyindices_log_dist_si_cpue_weight__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for cdist_sumofsquares_aldist_f_surv_weight;
            if ( cur_time == 0 && std::isfinite(asDouble(cdist_sumofsquares_aldist_f_surv_weight__lower)) && std::isfinite(asDouble(cdist_sumofsquares_aldist_f_surv_weight__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(cdist_sumofsquares_aldist_f_surv_weight - cdist_sumofsquares_aldist_f_surv_weight__upper) / (cdist_sumofsquares_aldist_f_surv_weight__upper - cdist_sumofsquares_aldist_f_surv_weight__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(cdist_sumofsquares_aldist_f_surv_weight__lower - cdist_sumofsquares_aldist_f_surv_weight) / (cdist_sumofsquares_aldist_f_surv_weight__upper - cdist_sumofsquares_aldist_f_surv_weight__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for cdist_sumofsquares_ldist_f_surv_weight;
            if ( cur_time == 0 && std::isfinite(asDouble(cdist_sumofsquares_ldist_f_surv_weight__lower)) && std::isfinite(asDouble(cdist_sumofsquares_ldist_f_surv_weight__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(cdist_sumofsquares_ldist_f_surv_weight - cdist_sumofsquares_ldist_f_surv_weight__upper) / (cdist_sumofsquares_ldist_f_surv_weight__upper - cdist_sumofsquares_ldist_f_surv_weight__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(cdist_sumofsquares_ldist_f_surv_weight__lower - cdist_sumofsquares_ldist_f_surv_weight) / (cdist_sumofsquares_ldist_f_surv_weight__upper - cdist_sumofsquares_ldist_f_surv_weight__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.K;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__K__lower)) && std::isfinite(asDouble(fish__K__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__K - fish__K__upper) / (fish__K__upper - fish__K__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__K__lower - fish__K) / (fish__K__upper - fish__K__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.Linf;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__Linf__lower)) && std::isfinite(asDouble(fish__Linf__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__Linf - fish__Linf__upper) / (fish__Linf__upper - fish__Linf__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__Linf__lower - fish__Linf) / (fish__Linf__upper - fish__Linf__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.M.1;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__M__1__lower)) && std::isfinite(asDouble(fish__M__1__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__M__1 - fish__M__1__upper) / (fish__M__1__upper - fish__M__1__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__M__1__lower - fish__M__1) / (fish__M__1__upper - fish__M__1__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.M.2;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__M__2__lower)) && std::isfinite(asDouble(fish__M__2__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__M__2 - fish__M__2__upper) / (fish__M__2__upper - fish__M__2__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__M__2__lower - fish__M__2) / (fish__M__2__upper - fish__M__2__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.M.3;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__M__3__lower)) && std::isfinite(asDouble(fish__M__3__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__M__3 - fish__M__3__upper) / (fish__M__3__upper - fish__M__3__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__M__3__lower - fish__M__3) / (fish__M__3__upper - fish__M__3__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.M.4;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__M__4__lower)) && std::isfinite(asDouble(fish__M__4__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__M__4 - fish__M__4__upper) / (fish__M__4__upper - fish__M__4__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__M__4__lower - fish__M__4) / (fish__M__4__upper - fish__M__4__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.M.5;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__M__5__lower)) && std::isfinite(asDouble(fish__M__5__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__M__5 - fish__M__5__upper) / (fish__M__5__upper - fish__M__5__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__M__5__lower - fish__M__5) / (fish__M__5__upper - fish__M__5__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.bbin;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__bbin__lower)) && std::isfinite(asDouble(fish__bbin__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__bbin - fish__bbin__upper) / (fish__bbin__upper - fish__bbin__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__bbin__lower - fish__bbin) / (fish__bbin__upper - fish__bbin__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.f_surv.alpha;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__f_surv__alpha__lower)) && std::isfinite(asDouble(fish__f_surv__alpha__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__f_surv__alpha - fish__f_surv__alpha__upper) / (fish__f_surv__alpha__upper - fish__f_surv__alpha__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__f_surv__alpha__lower - fish__f_surv__alpha) / (fish__f_surv__alpha__upper - fish__f_surv__alpha__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.f_surv.l50;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__f_surv__l50__lower)) && std::isfinite(asDouble(fish__f_surv__l50__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__f_surv__l50 - fish__f_surv__l50__upper) / (fish__f_surv__l50__upper - fish__f_surv__l50__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__f_surv__l50__lower - fish__f_surv__l50) / (fish__f_surv__l50__upper - fish__f_surv__l50__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.init.1;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__init__1__lower)) && std::isfinite(asDouble(fish__init__1__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__init__1 - fish__init__1__upper) / (fish__init__1__upper - fish__init__1__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__init__1__lower - fish__init__1) / (fish__init__1__upper - fish__init__1__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.init.2;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__init__2__lower)) && std::isfinite(asDouble(fish__init__2__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__init__2 - fish__init__2__upper) / (fish__init__2__upper - fish__init__2__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__init__2__lower - fish__init__2) / (fish__init__2__upper - fish__init__2__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.init.3;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__init__3__lower)) && std::isfinite(asDouble(fish__init__3__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__init__3 - fish__init__3__upper) / (fish__init__3__upper - fish__init__3__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__init__3__lower - fish__init__3) / (fish__init__3__upper - fish__init__3__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.init.4;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__init__4__lower)) && std::isfinite(asDouble(fish__init__4__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__init__4 - fish__init__4__upper) / (fish__init__4__upper - fish__init__4__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__init__4__lower - fish__init__4) / (fish__init__4__upper - fish__init__4__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.init.5;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__init__5__lower)) && std::isfinite(asDouble(fish__init__5__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__init__5 - fish__init__5__upper) / (fish__init__5__upper - fish__init__5__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__init__5__lower - fish__init__5) / (fish__init__5__upper - fish__init__5__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.init.scalar;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__init__scalar__lower)) && std::isfinite(asDouble(fish__init__scalar__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__init__scalar - fish__init__scalar__upper) / (fish__init__scalar__upper - fish__init__scalar__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__init__scalar__lower - fish__init__scalar) / (fish__init__scalar__upper - fish__init__scalar__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.lencv;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__lencv__lower)) && std::isfinite(asDouble(fish__lencv__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__lencv - fish__lencv__upper) / (fish__lencv__upper - fish__lencv__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__lencv__lower - fish__lencv) / (fish__lencv__upper - fish__lencv__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1979;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1979__lower)) && std::isfinite(asDouble(fish__rec__1979__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1979 - fish__rec__1979__upper) / (fish__rec__1979__upper - fish__rec__1979__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1979__lower - fish__rec__1979) / (fish__rec__1979__upper - fish__rec__1979__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1980;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1980__lower)) && std::isfinite(asDouble(fish__rec__1980__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1980 - fish__rec__1980__upper) / (fish__rec__1980__upper - fish__rec__1980__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1980__lower - fish__rec__1980) / (fish__rec__1980__upper - fish__rec__1980__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1981;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1981__lower)) && std::isfinite(asDouble(fish__rec__1981__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1981 - fish__rec__1981__upper) / (fish__rec__1981__upper - fish__rec__1981__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1981__lower - fish__rec__1981) / (fish__rec__1981__upper - fish__rec__1981__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1982;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1982__lower)) && std::isfinite(asDouble(fish__rec__1982__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1982 - fish__rec__1982__upper) / (fish__rec__1982__upper - fish__rec__1982__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1982__lower - fish__rec__1982) / (fish__rec__1982__upper - fish__rec__1982__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1983;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1983__lower)) && std::isfinite(asDouble(fish__rec__1983__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1983 - fish__rec__1983__upper) / (fish__rec__1983__upper - fish__rec__1983__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1983__lower - fish__rec__1983) / (fish__rec__1983__upper - fish__rec__1983__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1984;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1984__lower)) && std::isfinite(asDouble(fish__rec__1984__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1984 - fish__rec__1984__upper) / (fish__rec__1984__upper - fish__rec__1984__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1984__lower - fish__rec__1984) / (fish__rec__1984__upper - fish__rec__1984__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1985;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1985__lower)) && std::isfinite(asDouble(fish__rec__1985__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1985 - fish__rec__1985__upper) / (fish__rec__1985__upper - fish__rec__1985__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1985__lower - fish__rec__1985) / (fish__rec__1985__upper - fish__rec__1985__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1986;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1986__lower)) && std::isfinite(asDouble(fish__rec__1986__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1986 - fish__rec__1986__upper) / (fish__rec__1986__upper - fish__rec__1986__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1986__lower - fish__rec__1986) / (fish__rec__1986__upper - fish__rec__1986__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1987;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1987__lower)) && std::isfinite(asDouble(fish__rec__1987__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1987 - fish__rec__1987__upper) / (fish__rec__1987__upper - fish__rec__1987__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1987__lower - fish__rec__1987) / (fish__rec__1987__upper - fish__rec__1987__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1988;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1988__lower)) && std::isfinite(asDouble(fish__rec__1988__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1988 - fish__rec__1988__upper) / (fish__rec__1988__upper - fish__rec__1988__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1988__lower - fish__rec__1988) / (fish__rec__1988__upper - fish__rec__1988__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1989;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1989__lower)) && std::isfinite(asDouble(fish__rec__1989__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1989 - fish__rec__1989__upper) / (fish__rec__1989__upper - fish__rec__1989__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1989__lower - fish__rec__1989) / (fish__rec__1989__upper - fish__rec__1989__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1990;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1990__lower)) && std::isfinite(asDouble(fish__rec__1990__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1990 - fish__rec__1990__upper) / (fish__rec__1990__upper - fish__rec__1990__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1990__lower - fish__rec__1990) / (fish__rec__1990__upper - fish__rec__1990__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1991;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1991__lower)) && std::isfinite(asDouble(fish__rec__1991__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1991 - fish__rec__1991__upper) / (fish__rec__1991__upper - fish__rec__1991__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1991__lower - fish__rec__1991) / (fish__rec__1991__upper - fish__rec__1991__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1992;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1992__lower)) && std::isfinite(asDouble(fish__rec__1992__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1992 - fish__rec__1992__upper) / (fish__rec__1992__upper - fish__rec__1992__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1992__lower - fish__rec__1992) / (fish__rec__1992__upper - fish__rec__1992__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1993;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1993__lower)) && std::isfinite(asDouble(fish__rec__1993__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1993 - fish__rec__1993__upper) / (fish__rec__1993__upper - fish__rec__1993__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1993__lower - fish__rec__1993) / (fish__rec__1993__upper - fish__rec__1993__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1994;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1994__lower)) && std::isfinite(asDouble(fish__rec__1994__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1994 - fish__rec__1994__upper) / (fish__rec__1994__upper - fish__rec__1994__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1994__lower - fish__rec__1994) / (fish__rec__1994__upper - fish__rec__1994__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1995;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1995__lower)) && std::isfinite(asDouble(fish__rec__1995__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1995 - fish__rec__1995__upper) / (fish__rec__1995__upper - fish__rec__1995__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1995__lower - fish__rec__1995) / (fish__rec__1995__upper - fish__rec__1995__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1996;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1996__lower)) && std::isfinite(asDouble(fish__rec__1996__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1996 - fish__rec__1996__upper) / (fish__rec__1996__upper - fish__rec__1996__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1996__lower - fish__rec__1996) / (fish__rec__1996__upper - fish__rec__1996__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1997;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1997__lower)) && std::isfinite(asDouble(fish__rec__1997__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1997 - fish__rec__1997__upper) / (fish__rec__1997__upper - fish__rec__1997__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1997__lower - fish__rec__1997) / (fish__rec__1997__upper - fish__rec__1997__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1998;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1998__lower)) && std::isfinite(asDouble(fish__rec__1998__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1998 - fish__rec__1998__upper) / (fish__rec__1998__upper - fish__rec__1998__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1998__lower - fish__rec__1998) / (fish__rec__1998__upper - fish__rec__1998__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.1999;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__1999__lower)) && std::isfinite(asDouble(fish__rec__1999__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__1999 - fish__rec__1999__upper) / (fish__rec__1999__upper - fish__rec__1999__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__1999__lower - fish__rec__1999) / (fish__rec__1999__upper - fish__rec__1999__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2000;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2000__lower)) && std::isfinite(asDouble(fish__rec__2000__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2000 - fish__rec__2000__upper) / (fish__rec__2000__upper - fish__rec__2000__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2000__lower - fish__rec__2000) / (fish__rec__2000__upper - fish__rec__2000__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2001;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2001__lower)) && std::isfinite(asDouble(fish__rec__2001__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2001 - fish__rec__2001__upper) / (fish__rec__2001__upper - fish__rec__2001__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2001__lower - fish__rec__2001) / (fish__rec__2001__upper - fish__rec__2001__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2002;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2002__lower)) && std::isfinite(asDouble(fish__rec__2002__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2002 - fish__rec__2002__upper) / (fish__rec__2002__upper - fish__rec__2002__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2002__lower - fish__rec__2002) / (fish__rec__2002__upper - fish__rec__2002__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2003;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2003__lower)) && std::isfinite(asDouble(fish__rec__2003__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2003 - fish__rec__2003__upper) / (fish__rec__2003__upper - fish__rec__2003__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2003__lower - fish__rec__2003) / (fish__rec__2003__upper - fish__rec__2003__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2004;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2004__lower)) && std::isfinite(asDouble(fish__rec__2004__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2004 - fish__rec__2004__upper) / (fish__rec__2004__upper - fish__rec__2004__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2004__lower - fish__rec__2004) / (fish__rec__2004__upper - fish__rec__2004__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2005;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2005__lower)) && std::isfinite(asDouble(fish__rec__2005__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2005 - fish__rec__2005__upper) / (fish__rec__2005__upper - fish__rec__2005__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2005__lower - fish__rec__2005) / (fish__rec__2005__upper - fish__rec__2005__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2006;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2006__lower)) && std::isfinite(asDouble(fish__rec__2006__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2006 - fish__rec__2006__upper) / (fish__rec__2006__upper - fish__rec__2006__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2006__lower - fish__rec__2006) / (fish__rec__2006__upper - fish__rec__2006__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2007;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2007__lower)) && std::isfinite(asDouble(fish__rec__2007__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2007 - fish__rec__2007__upper) / (fish__rec__2007__upper - fish__rec__2007__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2007__lower - fish__rec__2007) / (fish__rec__2007__upper - fish__rec__2007__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2008;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2008__lower)) && std::isfinite(asDouble(fish__rec__2008__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2008 - fish__rec__2008__upper) / (fish__rec__2008__upper - fish__rec__2008__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2008__lower - fish__rec__2008) / (fish__rec__2008__upper - fish__rec__2008__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2009;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2009__lower)) && std::isfinite(asDouble(fish__rec__2009__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2009 - fish__rec__2009__upper) / (fish__rec__2009__upper - fish__rec__2009__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2009__lower - fish__rec__2009) / (fish__rec__2009__upper - fish__rec__2009__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2010;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2010__lower)) && std::isfinite(asDouble(fish__rec__2010__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2010 - fish__rec__2010__upper) / (fish__rec__2010__upper - fish__rec__2010__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2010__lower - fish__rec__2010) / (fish__rec__2010__upper - fish__rec__2010__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2011;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2011__lower)) && std::isfinite(asDouble(fish__rec__2011__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2011 - fish__rec__2011__upper) / (fish__rec__2011__upper - fish__rec__2011__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2011__lower - fish__rec__2011) / (fish__rec__2011__upper - fish__rec__2011__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2012;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2012__lower)) && std::isfinite(asDouble(fish__rec__2012__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2012 - fish__rec__2012__upper) / (fish__rec__2012__upper - fish__rec__2012__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2012__lower - fish__rec__2012) / (fish__rec__2012__upper - fish__rec__2012__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2013;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2013__lower)) && std::isfinite(asDouble(fish__rec__2013__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2013 - fish__rec__2013__upper) / (fish__rec__2013__upper - fish__rec__2013__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2013__lower - fish__rec__2013) / (fish__rec__2013__upper - fish__rec__2013__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2014;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2014__lower)) && std::isfinite(asDouble(fish__rec__2014__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2014 - fish__rec__2014__upper) / (fish__rec__2014__upper - fish__rec__2014__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2014__lower - fish__rec__2014) / (fish__rec__2014__upper - fish__rec__2014__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2015;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2015__lower)) && std::isfinite(asDouble(fish__rec__2015__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2015 - fish__rec__2015__upper) / (fish__rec__2015__upper - fish__rec__2015__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2015__lower - fish__rec__2015) / (fish__rec__2015__upper - fish__rec__2015__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2016;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2016__lower)) && std::isfinite(asDouble(fish__rec__2016__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2016 - fish__rec__2016__upper) / (fish__rec__2016__upper - fish__rec__2016__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2016__lower - fish__rec__2016) / (fish__rec__2016__upper - fish__rec__2016__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2017;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2017__lower)) && std::isfinite(asDouble(fish__rec__2017__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2017 - fish__rec__2017__upper) / (fish__rec__2017__upper - fish__rec__2017__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2017__lower - fish__rec__2017) / (fish__rec__2017__upper - fish__rec__2017__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2018;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2018__lower)) && std::isfinite(asDouble(fish__rec__2018__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2018 - fish__rec__2018__upper) / (fish__rec__2018__upper - fish__rec__2018__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2018__lower - fish__rec__2018) / (fish__rec__2018__upper - fish__rec__2018__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2019;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2019__lower)) && std::isfinite(asDouble(fish__rec__2019__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2019 - fish__rec__2019__upper) / (fish__rec__2019__upper - fish__rec__2019__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2019__lower - fish__rec__2019) / (fish__rec__2019__upper - fish__rec__2019__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2020;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2020__lower)) && std::isfinite(asDouble(fish__rec__2020__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2020 - fish__rec__2020__upper) / (fish__rec__2020__upper - fish__rec__2020__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2020__lower - fish__rec__2020) / (fish__rec__2020__upper - fish__rec__2020__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2021;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2021__lower)) && std::isfinite(asDouble(fish__rec__2021__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2021 - fish__rec__2021__upper) / (fish__rec__2021__upper - fish__rec__2021__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2021__lower - fish__rec__2021) / (fish__rec__2021__upper - fish__rec__2021__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2022;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2022__lower)) && std::isfinite(asDouble(fish__rec__2022__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2022 - fish__rec__2022__upper) / (fish__rec__2022__upper - fish__rec__2022__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2022__lower - fish__rec__2022) / (fish__rec__2022__upper - fish__rec__2022__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.2023;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__2023__lower)) && std::isfinite(asDouble(fish__rec__2023__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__2023 - fish__rec__2023__upper) / (fish__rec__2023__upper - fish__rec__2023__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__2023__lower - fish__rec__2023) / (fish__rec__2023__upper - fish__rec__2023__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.scalar;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__scalar__lower)) && std::isfinite(asDouble(fish__rec__scalar__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__scalar - fish__rec__scalar__upper) / (fish__rec__scalar__upper - fish__rec__scalar__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__scalar__lower - fish__rec__scalar) / (fish__rec__scalar__upper - fish__rec__scalar__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.rec.sd;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__rec__sd__lower)) && std::isfinite(asDouble(fish__rec__sd__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__rec__sd - fish__rec__sd__upper) / (fish__rec__sd__upper - fish__rec__sd__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__rec__sd__lower - fish__rec__sd) / (fish__rec__sd__upper - fish__rec__sd__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.t0;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__t0__lower)) && std::isfinite(asDouble(fish__t0__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__t0 - fish__t0__upper) / (fish__t0__upper - fish__t0__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__t0__lower - fish__t0) / (fish__t0__upper - fish__t0__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.walpha;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__walpha__lower)) && std::isfinite(asDouble(fish__walpha__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__walpha - fish__walpha__upper) / (fish__walpha__upper - fish__walpha__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__walpha__lower - fish__walpha) / (fish__walpha__upper - fish__walpha__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for fish.wbeta;
            if ( cur_time == 0 && std::isfinite(asDouble(fish__wbeta__lower)) && std::isfinite(asDouble(fish__wbeta__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(fish__wbeta - fish__wbeta__upper) / (fish__wbeta__upper - fish__wbeta__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(fish__wbeta__lower - fish__wbeta) / (fish__wbeta__upper - fish__wbeta__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for init.F;
            if ( cur_time == 0 && std::isfinite(asDouble(init__F__lower)) && std::isfinite(asDouble(init__F__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(init__F - init__F__upper) / (init__F__upper - init__F__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(init__F__lower - init__F) / (init__F__upper - init__F__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for project_years;
            if ( cur_time == 0 && std::isfinite(asDouble(project_years__lower)) && std::isfinite(asDouble(project_years__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(project_years - project_years__upper) / (project_years__upper - project_years__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(project_years__lower - project_years) / (project_years__upper - project_years__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for recage;
            if ( cur_time == 0 && std::isfinite(asDouble(recage__lower)) && std::isfinite(asDouble(recage__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(recage - recage__upper) / (recage__upper - recage__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(recage__lower - recage) / (recage__upper - recage__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        {
            // g3l_bounds_penalty for retro_years;
            if ( cur_time == 0 && std::isfinite(asDouble(retro_years__lower)) && std::isfinite(asDouble(retro_years__upper)) ) {
                nll += (double)(1)*(pow((logspace_add((Type)((double)(1e+06)*(retro_years - retro_years__upper) / (retro_years__upper - retro_years__lower)), (Type)((double)(0))) + logspace_add((Type)((double)(1e+06)*(retro_years__lower - retro_years) / (retro_years__upper - retro_years__lower)), (Type)((double)(0)))), (Type)(double)(2)));
            }
        }
        if ( adist_surveyindices_log_dist_si_cpue_weight > (double)(0) ) {
            // g3l_abundancedistribution_surveyindices_log: Collect abundance from fish for adist_surveyindices_log_dist_si_cpue;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                if ( area == adist_surveyindices_log_dist_si_cpue_model__area ) {
                    auto adist_surveyindices_log_dist_si_cpue_model__area_idx = 0;

                    auto adist_surveyindices_log_dist_si_cpue_model__time_idx = intlookup_getdefault(adist_surveyindices_log_dist_si_cpue_model__times, (cur_year*100 + cur_step), -1) - 1;

                    if ( adist_surveyindices_log_dist_si_cpue_model__time_idx >= 0 ) {
                        // Convert fish to wgt;
                        adist_surveyindices_log_dist_si_cpue_model__wgt.col(adist_surveyindices_log_dist_si_cpue_model__area_idx).col(adist_surveyindices_log_dist_si_cpue_model__time_idx) += ((matrix<Type>)(fish_adist_surveyindices_log_dist_si_cpue_model_lgmatrix.matrix() * ((fish__num.col(fish__age_idx).col(fish__area_idx)*fish__wgt.col(fish__age_idx).col(fish__area_idx))).matrix())).vec();
                    }
                }
            }
        }
        {
            auto adist_surveyindices_log_dist_si_cpue_model__max_time_idx = 4;

            {
                // g3l_abundancedistribution_surveyindices_log: Compare adist_surveyindices_log_dist_si_cpue_model to adist_surveyindices_log_dist_si_cpue_obs;
                if ( adist_surveyindices_log_dist_si_cpue_weight > (double)(0) ) {
                    auto area = adist_surveyindices_log_dist_si_cpue_model__area;

                    auto adist_surveyindices_log_dist_si_cpue_model__area_idx = 0;

                    auto adist_surveyindices_log_dist_si_cpue_model__time_idx = intlookup_getdefault(adist_surveyindices_log_dist_si_cpue_model__times, (cur_year*100 + cur_step), -1) - 1;

                    if ( adist_surveyindices_log_dist_si_cpue_model__time_idx >= 0 ) {
                        if ( area == adist_surveyindices_log_dist_si_cpue_obs__area ) {
                            auto adist_surveyindices_log_dist_si_cpue_obs__area_idx = 0;

                            {
                                adist_surveyindices_log_dist_si_cpue_model__params = (adist_surveyindices_log_dist_si_cpue_model__time_idx != adist_surveyindices_log_dist_si_cpue_model__max_time_idx ? adist_surveyindices_log_dist_si_cpue_model__params : surveyindices_linreg(log(avoid_zero_vec(adist_surveyindices_log_dist_si_cpue_model__wgt.col(adist_surveyindices_log_dist_si_cpue_model__area_idx))), log(avoid_zero_vec(adist_surveyindices_log_dist_si_cpue_obs__wgt.col(adist_surveyindices_log_dist_si_cpue_obs__area_idx))), NAN, (double)(1)));
                                {
                                    auto cur_cdist_nll = (adist_surveyindices_log_dist_si_cpue_model__time_idx != adist_surveyindices_log_dist_si_cpue_model__max_time_idx ? (double)(0) : (pow((adist_surveyindices_log_dist_si_cpue_model__params ( 0 ) + adist_surveyindices_log_dist_si_cpue_model__params ( 1 )*log(avoid_zero_vec(adist_surveyindices_log_dist_si_cpue_model__wgt.col(adist_surveyindices_log_dist_si_cpue_model__area_idx))) - log(avoid_zero_vec(adist_surveyindices_log_dist_si_cpue_obs__wgt.col(adist_surveyindices_log_dist_si_cpue_obs__area_idx)))), (Type)(double)(2))).sum());

                                    {
                                        nll += adist_surveyindices_log_dist_si_cpue_weight*cur_cdist_nll;
                                        nll_adist_surveyindices_log_dist_si_cpue__wgt(cur_time + 1 - 1) += cur_cdist_nll;
                                        nll_adist_surveyindices_log_dist_si_cpue__weight(cur_time + 1 - 1) = adist_surveyindices_log_dist_si_cpue_weight;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if ( cdist_sumofsquares_aldist_f_surv_weight > (double)(0) ) {
            // g3l_catchdistribution_sumofsquares: Collect catch from f_surv/fish for cdist_sumofsquares_aldist_f_surv;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                auto cdist_sumofsquares_aldist_f_surv_model__time_idx = intlookup_getdefault(cdist_sumofsquares_aldist_f_surv_model__times, (cur_year*100 + cur_step), -1) - 1;

                if ( cdist_sumofsquares_aldist_f_surv_model__time_idx >= 0 ) {
                    if ( age >= cdist_sumofsquares_aldist_f_surv_model__minage && age <= cdist_sumofsquares_aldist_f_surv_model__maxage ) {
                        auto cdist_sumofsquares_aldist_f_surv_model__age_idx = age - cdist_sumofsquares_aldist_f_surv_model__minage + 1 - 1;

                        {
                            // Convert fish_f_surv to num;
                            cdist_sumofsquares_aldist_f_surv_model__num.col(cdist_sumofsquares_aldist_f_surv_model__time_idx).col(cdist_sumofsquares_aldist_f_surv_model__age_idx) += ((matrix<Type>)(fish_f_surv_cdist_sumofsquares_aldist_f_surv_model_lgmatrix.matrix() * ((fish_f_surv__cons.col(fish__age_idx).col(fish__area_idx) / avoid_zero_vec(fish__wgt.col(fish__age_idx).col(fish__area_idx)))).matrix())).vec();
                        }
                    }
                }
            }
        }
        {
            // g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_aldist_f_surv_model to cdist_sumofsquares_aldist_f_surv_obs;
            if ( cdist_sumofsquares_aldist_f_surv_weight > (double)(0) ) {
                auto cdist_sumofsquares_aldist_f_surv_model__time_idx = intlookup_getdefault(cdist_sumofsquares_aldist_f_surv_model__times, (cur_year*100 + cur_step), -1) - 1;

                if ( cdist_sumofsquares_aldist_f_surv_model__time_idx >= 0 ) {
                    for (auto age = cdist_sumofsquares_aldist_f_surv_model__minage; age <= cdist_sumofsquares_aldist_f_surv_model__maxage; age++) {
                        auto cdist_sumofsquares_aldist_f_surv_model__age_idx = age - cdist_sumofsquares_aldist_f_surv_model__minage + 1 - 1;

                        auto cdist_sumofsquares_aldist_f_surv_model__sstotal = avoid_zero((cdist_sumofsquares_aldist_f_surv_model__num.col(cdist_sumofsquares_aldist_f_surv_model__time_idx)).sum());

                        auto cdist_sumofsquares_aldist_f_surv_obs__time_idx = intlookup_getdefault(cdist_sumofsquares_aldist_f_surv_obs__times, (cur_year*100 + cur_step), -1) - 1;

                        if ( cdist_sumofsquares_aldist_f_surv_obs__time_idx >= 0 ) {
                            if ( age >= cdist_sumofsquares_aldist_f_surv_obs__minage && age <= cdist_sumofsquares_aldist_f_surv_obs__maxage ) {
                                auto cdist_sumofsquares_aldist_f_surv_obs__age_idx = age - cdist_sumofsquares_aldist_f_surv_obs__minage + 1 - 1;

                                auto cdist_sumofsquares_aldist_f_surv_obs__sstotal = avoid_zero((cdist_sumofsquares_aldist_f_surv_obs__num.col(cdist_sumofsquares_aldist_f_surv_obs__time_idx)).sum());

                                auto cur_cdist_nll = ((pow(((cdist_sumofsquares_aldist_f_surv_model__num.col(cdist_sumofsquares_aldist_f_surv_model__time_idx).col(cdist_sumofsquares_aldist_f_surv_model__age_idx) / cdist_sumofsquares_aldist_f_surv_model__sstotal) - (cdist_sumofsquares_aldist_f_surv_obs__num.col(cdist_sumofsquares_aldist_f_surv_obs__time_idx).col(cdist_sumofsquares_aldist_f_surv_obs__age_idx) / cdist_sumofsquares_aldist_f_surv_obs__sstotal)), (Type)(double)(2)))).sum();

                                {
                                    nll += cdist_sumofsquares_aldist_f_surv_weight*cur_cdist_nll;
                                    nll_cdist_sumofsquares_aldist_f_surv__num(cur_time + 1 - 1) += cur_cdist_nll;
                                    nll_cdist_sumofsquares_aldist_f_surv__weight(cur_time + 1 - 1) = cdist_sumofsquares_aldist_f_surv_weight;
                                }
                            }
                        }
                    }
                }
            }
        }
        if ( cdist_sumofsquares_ldist_f_surv_weight > (double)(0) ) {
            // g3l_catchdistribution_sumofsquares: Collect catch from f_surv/fish for cdist_sumofsquares_ldist_f_surv;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                auto cdist_sumofsquares_ldist_f_surv_model__time_idx = intlookup_getdefault(cdist_sumofsquares_ldist_f_surv_model__times, (cur_year*100 + cur_step), -1) - 1;

                if ( cdist_sumofsquares_ldist_f_surv_model__time_idx >= 0 ) {
                    // Convert fish_f_surv to num;
                    cdist_sumofsquares_ldist_f_surv_model__num.col(cdist_sumofsquares_ldist_f_surv_model__time_idx) += ((matrix<Type>)(fish_f_surv_cdist_sumofsquares_ldist_f_surv_model_lgmatrix.matrix() * ((fish_f_surv__cons.col(fish__age_idx).col(fish__area_idx) / avoid_zero_vec(fish__wgt.col(fish__age_idx).col(fish__area_idx)))).matrix())).vec();
                }
            }
        }
        {
            // g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_ldist_f_surv_model to cdist_sumofsquares_ldist_f_surv_obs;
            if ( cdist_sumofsquares_ldist_f_surv_weight > (double)(0) ) {
                auto cdist_sumofsquares_ldist_f_surv_model__time_idx = intlookup_getdefault(cdist_sumofsquares_ldist_f_surv_model__times, (cur_year*100 + cur_step), -1) - 1;

                if ( cdist_sumofsquares_ldist_f_surv_model__time_idx >= 0 ) {
                    auto cdist_sumofsquares_ldist_f_surv_model__sstotal = avoid_zero((cdist_sumofsquares_ldist_f_surv_model__num.col(cdist_sumofsquares_ldist_f_surv_model__time_idx)).sum());

                    auto cdist_sumofsquares_ldist_f_surv_obs__time_idx = intlookup_getdefault(cdist_sumofsquares_ldist_f_surv_obs__times, (cur_year*100 + cur_step), -1) - 1;

                    if ( cdist_sumofsquares_ldist_f_surv_obs__time_idx >= 0 ) {
                        auto cdist_sumofsquares_ldist_f_surv_obs__sstotal = avoid_zero((cdist_sumofsquares_ldist_f_surv_obs__num.col(cdist_sumofsquares_ldist_f_surv_obs__time_idx)).sum());

                        auto cur_cdist_nll = ((pow(((cdist_sumofsquares_ldist_f_surv_model__num.col(cdist_sumofsquares_ldist_f_surv_model__time_idx) / cdist_sumofsquares_ldist_f_surv_model__sstotal) - (cdist_sumofsquares_ldist_f_surv_obs__num.col(cdist_sumofsquares_ldist_f_surv_obs__time_idx) / cdist_sumofsquares_ldist_f_surv_obs__sstotal)), (Type)(double)(2)))).sum();

                        {
                            nll += cdist_sumofsquares_ldist_f_surv_weight*cur_cdist_nll;
                            nll_cdist_sumofsquares_ldist_f_surv__num(cur_time + 1 - 1) += cur_cdist_nll;
                            nll_cdist_sumofsquares_ldist_f_surv__weight(cur_time + 1 - 1) = cdist_sumofsquares_ldist_f_surv_weight;
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
            detail_fish__predby_f_surv.col(cur_time + 1 - 1) = as_numeric_arr(fish__predby_f_surv);
        }
        if ( report_detail == 1 ) {
            detail_fish__renewalnum.col(cur_time + 1 - 1) = as_numeric_arr(fish__renewalnum);
        }
        if ( report_detail == 1 ) {
            detail_fish_f_surv__cons.col(cur_time + 1 - 1) = as_numeric_arr(fish_f_surv__cons);
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
