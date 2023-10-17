#include <TMB.hpp>

namespace map_extras {
    // at(), but throw (err) if item isn't available
    template<class Type, class KeyType>
    Type at_throw(std::map<KeyType, Type*> map_in, KeyType key_in, std::string err) {
            try {
                return *map_in.at(key_in);
            } catch (const std::out_of_range&) {
                warning("No value found in g3_param_table %s, ifmissing not specified", err.c_str());
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
    PARAMETER(retro_years);
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
    std::map<std::tuple<int>, Type*> fish__init = {{std::make_tuple(1), &fish__init__1}, {std::make_tuple(2), &fish__init__2}, {std::make_tuple(3), &fish__init__3}, {std::make_tuple(4), &fish__init__4}, {std::make_tuple(5), &fish__init__5}, {std::make_tuple(6), &fish__init__6}, {std::make_tuple(7), &fish__init__7}, {std::make_tuple(8), &fish__init__8}, {std::make_tuple(9), &fish__init__9}, {std::make_tuple(10), &fish__init__10}};
    PARAMETER(fish__M);
    PARAMETER(init__F);
    PARAMETER(recage);
    PARAMETER(fish__Linf);
    PARAMETER(fish__K);
    PARAMETER(fish__t0);
    PARAMETER(fish__lencv);
    PARAMETER(fish__walpha);
    PARAMETER(fish__wbeta);
    PARAMETER(report_detail);
    PARAMETER(fish__comm__alpha);
    PARAMETER(fish__comm__l50);
    PARAMETER(fish__bbin);
    PARAMETER(fish__rec__scalar);
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
    std::map<std::tuple<int>, Type*> fish__rec = {{std::make_tuple(1990), &fish__rec__1990}, {std::make_tuple(1991), &fish__rec__1991}, {std::make_tuple(1992), &fish__rec__1992}, {std::make_tuple(1993), &fish__rec__1993}, {std::make_tuple(1994), &fish__rec__1994}, {std::make_tuple(1995), &fish__rec__1995}, {std::make_tuple(1996), &fish__rec__1996}, {std::make_tuple(1997), &fish__rec__1997}, {std::make_tuple(1998), &fish__rec__1998}, {std::make_tuple(1999), &fish__rec__1999}, {std::make_tuple(2000), &fish__rec__2000}};
    PARAMETER(adist_surveyindices_log_acoustic_dist_weight);
    PARAMETER(cdist_sumofsquares_comm_ldist_weight);
    auto assert_msg = [](bool expr, std::string message) -> bool {
    if (!expr) { warning(message.c_str()); return TRUE; }
    return FALSE;
};
    auto normalize_vec = [](vector<Type> a) -> vector<Type> {
    return a / a.sum();
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
    auto g3a_grow_weightsimple_vec_rotate = [](vector<Type> vec, int a) -> array<Type> {
        array<Type> out(vec.size(), a);
        for (int i = 0 ; i < vec.size(); i++) {
            for (int j = 0 ; j < a; j++) {
                out(i, j) = vec(j + i < vec.size() ? j + i : vec.size() - 1);
            }
        }
        return out;
    };
    auto g3a_grow_weightsimple_vec_extrude = [](vector<Type> vec, int a) -> array<Type> {
        array<Type> out(vec.size(), a);
        for (int i = 0 ; i < vec.size(); i++) {
            for (int j = 0 ; j < a; j++) {
                out(i, j) = vec[i];
            }
        }
        return out;
    };
    auto g3a_grow_apply = [](array<Type> delta_l_ar, array<Type> delta_w_ar, vector<Type> input_num, vector<Type> input_wgt) -> array<Type> {
    // Convert delta_l / delta_w to matrices to get 2 proper dimensions, most of this is row-based.
    matrix<Type> delta_l = delta_l_ar.matrix();
    matrix<Type> delta_w = delta_w_ar.matrix();
    int total_deltas = delta_l.cols();  // # Length group increases (should be +1 for the no-change group)
    int total_lgs = delta_l.rows(); // # Length groups

    auto avoid_zero_vec = [](vector<Type> a) -> vector<Type> {
        vector<Type> res(a.size());
        for(int i = 0; i < a.size(); i++) {
            res[i] = logspace_add(a[i] * 1000.0, (Type)0.0) / 1000.0;
        }
        return res;
    };

    matrix<Type> growth_matrix(total_lgs, total_lgs);
    growth_matrix.setZero();
    matrix<Type> weight_matrix(total_lgs, total_lgs);
    weight_matrix.setZero();

    for (int lg = 0; lg < total_lgs; lg++) {
        if (lg == total_lgs - 1) {  // Can't grow beyond maximum length group
            growth_matrix(lg, lg) = delta_l.row(lg).sum();
            weight_matrix.block(lg, lg, 1, total_lgs - lg) = delta_w.block(lg, 0, 1, total_lgs - lg);
        } else if(lg + total_deltas > total_lgs) {
            growth_matrix.block(lg, lg, 1, total_lgs - lg) = delta_l.block(lg, 0, 1, total_lgs - lg);
            growth_matrix(lg, total_lgs - 1) = delta_l.row(lg).tail(total_deltas - (total_lgs - lg) + 1).sum();
            weight_matrix.block(lg, lg, 1, total_lgs - lg) = delta_w.block(lg, 0, 1, total_lgs - lg);
        } else {
            growth_matrix.block(lg, lg, 1, total_deltas) = delta_l.block(lg, 0, 1, total_deltas);
            weight_matrix.block(lg, lg, 1, total_deltas) = delta_w.block(lg, 0, 1, total_deltas);
        }
    }
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
    auto g3_matrix_vec = [](array<Type>tf, vector<Type> vec) -> vector<Type> {
       return (tf.matrix() * vec.matrix()).array();
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
    Type nll = (double)(0);
    vector<int> step_lengths(1); step_lengths.setConstant(12);
    int end_year = 2000;
    int start_year = 1990;
    auto total_steps = (step_lengths).size()*(end_year - retro_years - start_year + project_years) + (step_lengths).size() - 1;
    int cur_year = 0;
    auto step_count = (step_lengths).size();
    int cur_year_projection = false;
    int cur_step = 0;
    int cur_step_final = false;
    int fish__minage = 1;
    int fish__maxage = 10;
    int fish__area = 1;
    DATA_VECTOR(fish__midlen)
    auto cur_step_size = step_lengths ( 0 ) / (double)(12);
    array<Type> fish__num(6,1,10); fish__num.setZero();
    array<Type> fish__wgt(6,1,10); fish__wgt.setConstant((double)(1));
    int nan_fish__num = false;
    int nan_fish__wgt = false;
    auto as_integer = [](Type v) -> int {
    return std::floor(asDouble(v));
};
    array<Type> detail_fish__num(6,1,10,as_integer(total_steps + (double)(1))); detail_fish__num.setZero();
    array<Type> detail_fish__wgt(6,1,10,as_integer(total_steps + (double)(1))); detail_fish__wgt.setConstant((double)(1));
    array<Type> comm__catch(1);
    array<Type> comm__catchnum(1);
    array<Type> fish__totalpredate(6,1,10);
    array<Type> fish__predby_comm(6,1,10);
    int comm__area = 1;
    array<Type> fish__suit_comm(6,1,10); fish__suit_comm.setZero();
    DATA_IVECTOR(comm_landings__keys)
    DATA_VECTOR(comm_landings__values)
    auto comm_landings__lookup = intlookup_zip(comm_landings__keys, comm_landings__values);
    array<Type> fish__consratio(6,1,10);
    Type fish__overconsumption = (double)(0);
    int fish__growth_lastcalc = -1;
    array<Type> fish__growth_l(6,6);
    Type fish__plusdl = (double)(10);
    array<Type> fish__growth_w(6,6);
    Type fish__prevtotal = (double)(0);
    array<Type> fish__renewalnum(6,1,10); fish__renewalnum.setZero();
    array<Type> fish__renewalwgt(6,1,10); fish__renewalwgt.setZero();
    int adist_surveyindices_log_acoustic_dist_model__area = 1;
    DATA_IVECTOR(times_adist_surveyindices_log_acoustic_dist_model__keys)
    DATA_IVECTOR(times_adist_surveyindices_log_acoustic_dist_model__values)
    auto times_adist_surveyindices_log_acoustic_dist_model__lookup = intlookup_zip(times_adist_surveyindices_log_acoustic_dist_model__keys, times_adist_surveyindices_log_acoustic_dist_model__values);
    array<Type> adist_surveyindices_log_acoustic_dist_model__wgt(1,11,1); adist_surveyindices_log_acoustic_dist_model__wgt.setZero();
    array<Type> fish_adist_surveyindices_log_acoustic_dist_model_lgmatrix(1,6); fish_adist_surveyindices_log_acoustic_dist_model_lgmatrix.setConstant((double)(1));
    int adist_surveyindices_log_acoustic_dist_obs__area = 1;
    vector<Type> adist_surveyindices_log_acoustic_dist_model__params(2); adist_surveyindices_log_acoustic_dist_model__params.setZero();
    DATA_ARRAY(adist_surveyindices_log_acoustic_dist_obs__wgt)
    array<Type> nll_adist_surveyindices_log_acoustic_dist__wgt(as_integer(total_steps + (double)(1))); nll_adist_surveyindices_log_acoustic_dist__wgt.setZero();
    array<Type> nll_adist_surveyindices_log_acoustic_dist__weight(as_integer(total_steps + (double)(1))); nll_adist_surveyindices_log_acoustic_dist__weight.setZero();
    int cdist_sumofsquares_comm_ldist_model__area = 1;
    DATA_IVECTOR(times_cdist_sumofsquares_comm_ldist_model__keys)
    DATA_IVECTOR(times_cdist_sumofsquares_comm_ldist_model__values)
    auto times_cdist_sumofsquares_comm_ldist_model__lookup = intlookup_zip(times_cdist_sumofsquares_comm_ldist_model__keys, times_cdist_sumofsquares_comm_ldist_model__values);
    array<Type> cdist_sumofsquares_comm_ldist_model__wgt(5,11,1); cdist_sumofsquares_comm_ldist_model__wgt.setZero();
    DATA_ARRAY(fish_cdist_sumofsquares_comm_ldist_model_lgmatrix)
    int cdist_sumofsquares_comm_ldist_obs__area = 1;
    DATA_IVECTOR(times_cdist_sumofsquares_comm_ldist_obs__keys)
    DATA_IVECTOR(times_cdist_sumofsquares_comm_ldist_obs__values)
    auto times_cdist_sumofsquares_comm_ldist_obs__lookup = intlookup_zip(times_cdist_sumofsquares_comm_ldist_obs__keys, times_cdist_sumofsquares_comm_ldist_obs__values);
    DATA_ARRAY(cdist_sumofsquares_comm_ldist_obs__wgt)
    array<Type> nll_cdist_sumofsquares_comm_ldist__wgt(as_integer(total_steps + (double)(1))); nll_cdist_sumofsquares_comm_ldist__wgt.setZero();
    array<Type> nll_cdist_sumofsquares_comm_ldist__weight(as_integer(total_steps + (double)(1))); nll_cdist_sumofsquares_comm_ldist__weight.setZero();
    Type g3l_understocking_total = (double)(0);
    array<Type> nll_understocking__wgt(as_integer(total_steps + (double)(1))); nll_understocking__wgt.setZero();
    array<Type> nll_understocking__weight(as_integer(total_steps + (double)(1))); nll_understocking__weight.setZero();
    array<Type> detail_fish__predby_comm(6,1,10,as_integer(total_steps + (double)(1)));
    array<Type> detail_fish__renewalnum(6,1,10,as_integer(total_steps + (double)(1))); detail_fish__renewalnum.setZero();
    array<Type> detail_fish__suit_comm(6,1,10,as_integer(total_steps + (double)(1))); detail_fish__suit_comm.setZero();

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
            if ( false ) {
                assert_msg(std::isfinite(asDouble(nll)), "g3a_time: nll became NaN/Inf in previous timestep");
            }
            if ( cur_time > total_steps ) {
                {
                    REPORT(adist_surveyindices_log_acoustic_dist_model__params);
                    REPORT(adist_surveyindices_log_acoustic_dist_model__wgt);
                    REPORT(cdist_sumofsquares_comm_ldist_model__wgt);
                    REPORT(comm__catch);
                    REPORT(comm__catchnum);
                    REPORT(cur_step);
                    REPORT(cur_step_final);
                    REPORT(cur_time);
                    REPORT(cur_year);
                    REPORT(cur_year_projection);
                    REPORT(detail_fish__num);
                    REPORT(detail_fish__predby_comm);
                    REPORT(detail_fish__renewalnum);
                    REPORT(detail_fish__suit_comm);
                    REPORT(detail_fish__wgt);
                    REPORT(fish__consratio);
                    REPORT(fish__growth_l);
                    REPORT(fish__growth_lastcalc);
                    REPORT(fish__growth_w);
                    REPORT(fish__num);
                    REPORT(fish__overconsumption);
                    REPORT(fish__predby_comm);
                    REPORT(fish__prevtotal);
                    REPORT(fish__renewalnum);
                    REPORT(fish__renewalwgt);
                    REPORT(fish__suit_comm);
                    REPORT(fish__totalpredate);
                    REPORT(fish__wgt);
                    REPORT(g3l_understocking_total);
                    REPORT(nan_fish__num);
                    REPORT(nan_fish__wgt);
                    REPORT(nll);
                    REPORT(nll_adist_surveyindices_log_acoustic_dist__weight);
                    REPORT(nll_adist_surveyindices_log_acoustic_dist__wgt);
                    REPORT(nll_cdist_sumofsquares_comm_ldist__weight);
                    REPORT(nll_cdist_sumofsquares_comm_ldist__wgt);
                    REPORT(nll_understocking__weight);
                    REPORT(nll_understocking__wgt);
                }
                return nll;
            }
            cur_year = start_year + (((int) cur_time) / ((int) step_count));
            cur_year_projection = cur_year > end_year - retro_years;
            cur_step = (cur_time % step_count) + 1;
            cur_step_final = cur_step == step_count;
        }
        {
            // g3a_initialconditions for fish;
            for (auto age = fish__minage; age <= fish__maxage; age++) if ( cur_time == 0 ) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                {
                    auto area = fish__area;

                    auto fish__area_idx = 0;

                    auto factor = (fish__init__scalar*map_extras::at_throw(fish__init, std::make_tuple(age), "fish.init")*exp(-(double)(1)*(fish__M + init__F)*(age - recage)));

                    auto dnorm = ((fish__midlen - (fish__Linf*((double)(1) - exp(-(double)(1)*fish__K*((age - cur_step_size) - fish__t0))))) / ((fish__Linf*((double)(1) - exp(-(double)(1)*fish__K*((age - cur_step_size) - fish__t0))))*fish__lencv));

                    {
                        fish__num.col(fish__age_idx).col(fish__area_idx) = normalize_vec(exp(-(pow(dnorm, (Type)(double)(2)))*(double)(0.5)))*(double)(10000)*factor;
                        fish__wgt.col(fish__age_idx).col(fish__area_idx) = fish__walpha*pow(fish__midlen, (Type)fish__wbeta);
                    }
                }
            }
        }
        {
            // g3a_trace_nan: g3a_initialconditions for fish;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_initialconditions for fish'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_initialconditions for fish'\n", cur_year, cur_step);
            }
        }
        {
            // g3a_trace_nan: g3a_time: Start of time period;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_time: Start of time period'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_time: Start of time period'\n", cur_year, cur_step);
            }
        }
        if ( report_detail == 1 ) {
            detail_fish__num.col(cur_time + 1 - 1) = fish__num;
        }
        if ( report_detail == 1 ) {
            detail_fish__wgt.col(cur_time + 1 - 1) = fish__wgt;
        }
        {
            // Zero biomass-caught counter for comm;
            comm__catch.setZero();
            comm__catchnum.setZero();
        }
        {
            // g3a_trace_nan: Zero biomass-caught counter for comm;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'Zero biomass-caught counter for comm'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'Zero biomass-caught counter for comm'\n", cur_year, cur_step);
            }
        }
        {
            // Zero total predation counter for fish;
            fish__totalpredate.setZero();
        }
        {
            // g3a_trace_nan: Zero total predation counter for fish;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'Zero total predation counter for fish'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'Zero total predation counter for fish'\n", cur_year, cur_step);
            }
        }
        {
            // g3a_predate_fleet for fish;
            // Zero comm-fish biomass-consuming counter;
            fish__predby_comm.setZero();
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                if ( area == comm__area ) {
                    auto comm__area_idx = 0;

                    auto fleet_area = area;

                    {
                        // Collect all suitable fish biomass for comm;
                        fish__suit_comm.col(fish__age_idx).col(fish__area_idx) = (double)(1) / ((double)(1) + exp(-fish__comm__alpha*(fish__midlen - fish__comm__l50)));
                        fish__predby_comm.col(fish__age_idx).col(fish__area_idx) = fish__suit_comm.col(fish__age_idx).col(fish__area_idx)*fish__num.col(fish__age_idx).col(fish__area_idx)*fish__wgt.col(fish__age_idx).col(fish__area_idx);
                        comm__catch(comm__area_idx) += (fish__predby_comm.col(fish__age_idx).col(fish__area_idx)).sum();
                        comm__catchnum(comm__area_idx) += (fish__suit_comm.col(fish__age_idx).col(fish__area_idx)*fish__num.col(fish__age_idx).col(fish__area_idx)).sum();
                    }
                }
            }
        }
        {
            // g3a_trace_nan: g3a_predate_fleet for fish;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_predate_fleet for fish'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_predate_fleet for fish'\n", cur_year, cur_step);
            }
        }
        {
            // Scale comm catch of fish by total expected catch;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                if ( area == comm__area ) {
                    auto comm__area_idx = 0;

                    auto fleet_area = area;

                    {
                        fish__predby_comm.col(fish__age_idx).col(fish__area_idx) = (fish__predby_comm.col(fish__age_idx).col(fish__area_idx) / avoid_zero_vec(fish__wgt.col(fish__age_idx).col(fish__area_idx)))*((area != 1 ? (double)(0) : intlookup_getdefault(comm_landings__lookup, cur_year, (double)(0))) / comm__catchnum(comm__area_idx));
                        fish__totalpredate.col(fish__age_idx).col(fish__area_idx) += fish__predby_comm.col(fish__age_idx).col(fish__area_idx);
                    }
                }
            }
        }
        {
            // g3a_trace_nan: Scale comm catch of fish by total expected catch;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'Scale comm catch of fish by total expected catch'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'Scale comm catch of fish by total expected catch'\n", cur_year, cur_step);
            }
        }
        {
            // Temporarily convert to being proportion of totalpredate;
            fish__predby_comm /= avoid_zero_vec(fish__totalpredate);
        }
        {
            // g3a_trace_nan: Temporarily convert to being proportion of totalpredate;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'Temporarily convert to being proportion of totalpredate'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'Temporarily convert to being proportion of totalpredate'\n", cur_year, cur_step);
            }
        }
        {
            // Calculate fish overconsumption coefficient;
            fish__consratio = fish__totalpredate / avoid_zero_vec(fish__num*fish__wgt);
            fish__consratio = logspace_add_vec(fish__consratio*-(double)(1000), (double)(0.95)*-(double)(1000)) / -(double)(1000);
            if ( false ) {
                assert_msg((fish__consratio <= (double)(1)).all(), "g3a_predate_fleet: fish__consratio <= 1, can't consume more fish than currently exist");
            }
            // Apply overconsumption to prey;
            fish__overconsumption = (fish__totalpredate).sum();
            fish__totalpredate = (fish__num*fish__wgt)*fish__consratio;
            fish__overconsumption -= (fish__totalpredate).sum();
            fish__num *= ((double)(1) - fish__consratio);
        }
        {
            // g3a_trace_nan: Calculate fish overconsumption coefficient;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'Calculate fish overconsumption coefficient'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'Calculate fish overconsumption coefficient'\n", cur_year, cur_step);
            }
        }
        {
            // Zero comm catch before working out post-adjustment value;
            comm__catch.setZero();
            comm__catchnum.setZero();
        }
        {
            // Revert to being total biomass (applying overconsumption in process);
            fish__predby_comm *= fish__totalpredate;
            // Update total catch;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                if ( area == comm__area ) {
                    auto comm__area_idx = 0;

                    auto fleet_area = area;

                    {
                        comm__catch(comm__area_idx) += (fish__predby_comm.col(fish__age_idx).col(fish__area_idx)).sum();
                        comm__catchnum(comm__area_idx) += (fish__predby_comm.col(fish__age_idx).col(fish__area_idx) / fish__wgt.col(fish__age_idx).col(fish__area_idx)).sum();
                    }
                }
            }
        }
        {
            // g3a_trace_nan: Revert to being total biomass (applying overconsumption in process);
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'Revert to being total biomass (applying overconsumption in process)'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'Revert to being total biomass (applying overconsumption in process)'\n", cur_year, cur_step);
            }
        }
        {
            // g3a_trace_nan: Zero comm catch before working out post-adjustment value;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'Zero comm catch before working out post-adjustment value'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'Zero comm catch before working out post-adjustment value'\n", cur_year, cur_step);
            }
        }
        {
            // Natural mortality for fish;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                fish__num.col(fish__age_idx).col(fish__area_idx) *= exp(-(fish__M)*cur_step_size);
            }
        }
        {
            // g3a_trace_nan: Natural mortality for fish;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'Natural mortality for fish'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'Natural mortality for fish'\n", cur_year, cur_step);
            }
        }
        {
            // g3a_grow for fish;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                {
                    if ( fish__growth_lastcalc != std::floor(cur_step_size*12) ) {
                        // Calculate length/weight delta matrices for current lengthgroups;
                        fish__growth_l = growth_bbinom(avoid_zero_vec(avoid_zero_vec((fish__Linf - fish__midlen)*((double)(1) - exp(-(fish__K)*cur_step_size))) / fish__plusdl), 5, avoid_zero(fish__bbin));
                        fish__growth_w = (g3a_grow_weightsimple_vec_rotate(pow((vector<Type>)(fish__midlen), fish__wbeta), 5 + (double)(1)) - g3a_grow_weightsimple_vec_extrude(pow((vector<Type>)(fish__midlen), fish__wbeta), 5 + (double)(1)))*fish__walpha;
                        // Don't recalculate until cur_step_size changes;
                        fish__growth_lastcalc = std::floor(cur_step_size*12);
                    }
                    if ( false ) {
                        fish__prevtotal = (fish__num.col(fish__age_idx).col(fish__area_idx)).sum();
                    }
                    // Update fish using delta matrices;
                    {
                        auto growthresult = g3a_grow_apply(fish__growth_l, fish__growth_w, fish__num.col(fish__age_idx).col(fish__area_idx), fish__wgt.col(fish__age_idx).col(fish__area_idx));

                        {
                            fish__num.col(fish__age_idx).col(fish__area_idx) = growthresult.col(0);
                            fish__wgt.col(fish__age_idx).col(fish__area_idx) = growthresult.col(1);
                        }
                    }
                    if ( false ) {
                        assert_msg(CppAD::abs(fish__prevtotal - (fish__num.col(fish__age_idx).col(fish__area_idx)).sum()) < (double)(1e-04), "g3a_growmature: fish__num totals are not the same before and after growth");
                    }
                }
            }
        }
        {
            // g3a_trace_nan: g3a_grow for fish;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_grow for fish'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_grow for fish'\n", cur_year, cur_step);
            }
        }
        {
            auto factor = (fish__rec__scalar*map_extras::at_def(fish__rec, std::make_tuple(cur_year), (Type)(NAN)));

            {
                // g3a_renewal for fish;
                for (auto age = fish__minage; age <= fish__maxage; age++) if ( age == fish__minage && cur_step == 1 && (! cur_year_projection) ) {
                    auto fish__age_idx = age - fish__minage + 1 - 1;

                    auto area = fish__area;

                    auto fish__area_idx = 0;

                    auto dnorm = ((fish__midlen - (fish__Linf*((double)(1) - exp(-(double)(1)*fish__K*(age - fish__t0))))) / ((fish__Linf*((double)(1) - exp(-(double)(1)*fish__K*(age - fish__t0))))*fish__lencv));

                    {
                        fish__renewalnum.col(fish__age_idx).col(fish__area_idx) = normalize_vec(exp(-(pow(dnorm, (Type)(double)(2)))*(double)(0.5)))*(double)(10000)*factor;
                        fish__renewalwgt.col(fish__age_idx).col(fish__area_idx) = fish__walpha*pow(fish__midlen, (Type)fish__wbeta);
                        // Add result to fish;
                        fish__wgt.col(fish__age_idx).col(fish__area_idx) = ratio_add_vec(fish__wgt.col(fish__age_idx).col(fish__area_idx), fish__num.col(fish__age_idx).col(fish__area_idx), fish__renewalwgt.col(fish__age_idx).col(fish__area_idx), fish__renewalnum.col(fish__age_idx).col(fish__area_idx));
                        fish__num.col(fish__age_idx).col(fish__area_idx) += fish__renewalnum.col(fish__age_idx).col(fish__area_idx);
                    }
                }
            }
        }
        {
            // g3a_trace_nan: g3a_renewal for fish;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_renewal for fish'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_renewal for fish'\n", cur_year, cur_step);
            }
        }
        {
            // g3l_abundancedistribution_surveyindices_log: Collect abundance from fish for adist_surveyindices_log_acoustic_dist;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                if ( area == adist_surveyindices_log_acoustic_dist_model__area ) {
                    auto adist_surveyindices_log_acoustic_dist_model__area_idx = 0;

                    auto adist_surveyindices_log_acoustic_dist_model__time_idx = intlookup_getdefault(times_adist_surveyindices_log_acoustic_dist_model__lookup, (cur_year*100 + cur_step*0), -1) - 1;

                    if ( adist_surveyindices_log_acoustic_dist_model__time_idx >= 0 ) {
                        // Take fish total biomass to our count;
                        adist_surveyindices_log_acoustic_dist_model__wgt.col(adist_surveyindices_log_acoustic_dist_model__area_idx).col(adist_surveyindices_log_acoustic_dist_model__time_idx) += g3_matrix_vec(fish_adist_surveyindices_log_acoustic_dist_model_lgmatrix, (fish__num.col(fish__age_idx).col(fish__area_idx)*fish__wgt.col(fish__age_idx).col(fish__area_idx)));
                    }
                }
            }
        }
        {
            // g3a_trace_nan: g3l_abundancedistribution_surveyindices_log: Collect abundance from fish for adist_surveyindices_log_acoustic_dist;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_abundancedistribution_surveyindices_log: Collect abundance from fish for adist_surveyindices_log_acoustic_dist'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_abundancedistribution_surveyindices_log: Collect abundance from fish for adist_surveyindices_log_acoustic_dist'\n", cur_year, cur_step);
            }
        }
        {
            auto adist_surveyindices_log_acoustic_dist_model__max_time_idx = 10;

            {
                // g3l_abundancedistribution_surveyindices_log: Compare adist_surveyindices_log_acoustic_dist_model to adist_surveyindices_log_acoustic_dist_obs;
                if ( cur_step_final ) {
                    auto area = adist_surveyindices_log_acoustic_dist_model__area;

                    auto adist_surveyindices_log_acoustic_dist_model__area_idx = 0;

                    auto adist_surveyindices_log_acoustic_dist_model__time_idx = intlookup_getdefault(times_adist_surveyindices_log_acoustic_dist_model__lookup, (cur_year*100 + cur_step*0), -1) - 1;

                    if ( adist_surveyindices_log_acoustic_dist_model__time_idx >= 0 ) {
                        if ( area == adist_surveyindices_log_acoustic_dist_obs__area ) {
                            auto adist_surveyindices_log_acoustic_dist_obs__area_idx = 0;

                            {
                                adist_surveyindices_log_acoustic_dist_model__params = (adist_surveyindices_log_acoustic_dist_model__time_idx != adist_surveyindices_log_acoustic_dist_model__max_time_idx ? adist_surveyindices_log_acoustic_dist_model__params : surveyindices_linreg(log(avoid_zero_vec(adist_surveyindices_log_acoustic_dist_model__wgt.col(adist_surveyindices_log_acoustic_dist_model__area_idx))), log(avoid_zero_vec(adist_surveyindices_log_acoustic_dist_obs__wgt.col(adist_surveyindices_log_acoustic_dist_obs__area_idx))), NAN, (double)(1)));
                                {
                                    auto cur_cdist_nll = (adist_surveyindices_log_acoustic_dist_model__time_idx != adist_surveyindices_log_acoustic_dist_model__max_time_idx ? (double)(0) : (pow((adist_surveyindices_log_acoustic_dist_model__params ( 0 ) + adist_surveyindices_log_acoustic_dist_model__params ( 1 )*log(avoid_zero_vec(adist_surveyindices_log_acoustic_dist_model__wgt.col(adist_surveyindices_log_acoustic_dist_model__area_idx))) - log(avoid_zero_vec(adist_surveyindices_log_acoustic_dist_obs__wgt.col(adist_surveyindices_log_acoustic_dist_obs__area_idx)))), (Type)(double)(2))).sum());

                                    {
                                        nll += adist_surveyindices_log_acoustic_dist_weight*cur_cdist_nll;
                                        nll_adist_surveyindices_log_acoustic_dist__wgt(cur_time + 1 - 1) += cur_cdist_nll;
                                        nll_adist_surveyindices_log_acoustic_dist__weight(cur_time + 1 - 1) = adist_surveyindices_log_acoustic_dist_weight;
                                        REPORT(adist_surveyindices_log_acoustic_dist_obs__wgt);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        {
            // g3a_trace_nan: g3l_abundancedistribution_surveyindices_log: Compare adist_surveyindices_log_acoustic_dist_model to adist_surveyindices_log_acoustic_dist_obs;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_abundancedistribution_surveyindices_log: Compare adist_surveyindices_log_acoustic_dist_model to adist_surveyindices_log_acoustic_dist_obs'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_abundancedistribution_surveyindices_log: Compare adist_surveyindices_log_acoustic_dist_model to adist_surveyindices_log_acoustic_dist_obs'\n", cur_year, cur_step);
            }
        }
        {
            // g3l_catchdistribution_sumofsquares: Collect catch from comm/fish for cdist_sumofsquares_comm_ldist;
            for (auto age = fish__minage; age <= fish__maxage; age++) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                auto area = fish__area;

                auto fish__area_idx = 0;

                if ( area == cdist_sumofsquares_comm_ldist_model__area ) {
                    auto cdist_sumofsquares_comm_ldist_model__area_idx = 0;

                    auto cdist_sumofsquares_comm_ldist_model__time_idx = intlookup_getdefault(times_cdist_sumofsquares_comm_ldist_model__lookup, (cur_year*100 + cur_step*0), -1) - 1;

                    if ( cdist_sumofsquares_comm_ldist_model__time_idx >= 0 ) {
                        // Take prey_stock__predby_predstock weight, add to our count;
                        cdist_sumofsquares_comm_ldist_model__wgt.col(cdist_sumofsquares_comm_ldist_model__area_idx).col(cdist_sumofsquares_comm_ldist_model__time_idx) += g3_matrix_vec(fish_cdist_sumofsquares_comm_ldist_model_lgmatrix, fish__predby_comm.col(fish__age_idx).col(fish__area_idx));
                    }
                }
            }
        }
        {
            // g3a_trace_nan: g3l_catchdistribution_sumofsquares: Collect catch from comm/fish for cdist_sumofsquares_comm_ldist;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_catchdistribution_sumofsquares: Collect catch from comm/fish for cdist_sumofsquares_comm_ldist'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_catchdistribution_sumofsquares: Collect catch from comm/fish for cdist_sumofsquares_comm_ldist'\n", cur_year, cur_step);
            }
        }
        {
            // g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_comm_ldist_model to cdist_sumofsquares_comm_ldist_obs;
            if ( cur_step_final ) {
                auto area = cdist_sumofsquares_comm_ldist_model__area;

                auto cdist_sumofsquares_comm_ldist_model__area_idx = 0;

                auto cdist_sumofsquares_comm_ldist_model__time_idx = intlookup_getdefault(times_cdist_sumofsquares_comm_ldist_model__lookup, (cur_year*100 + cur_step*0), -1) - 1;

                if ( cdist_sumofsquares_comm_ldist_model__time_idx >= 0 ) {
                    if ( area == cdist_sumofsquares_comm_ldist_obs__area ) {
                        auto cdist_sumofsquares_comm_ldist_model__sstotal = avoid_zero((cdist_sumofsquares_comm_ldist_model__wgt.col(cdist_sumofsquares_comm_ldist_model__area_idx).col(cdist_sumofsquares_comm_ldist_model__time_idx)).sum());

                        auto cdist_sumofsquares_comm_ldist_obs__area_idx = 0;

                        auto cdist_sumofsquares_comm_ldist_obs__time_idx = intlookup_getdefault(times_cdist_sumofsquares_comm_ldist_obs__lookup, (cur_year*100 + cur_step*0), -1) - 1;

                        if ( cdist_sumofsquares_comm_ldist_obs__time_idx >= 0 ) {
                            auto cdist_sumofsquares_comm_ldist_obs__sstotal = avoid_zero((cdist_sumofsquares_comm_ldist_obs__wgt.col(cdist_sumofsquares_comm_ldist_obs__area_idx).col(cdist_sumofsquares_comm_ldist_obs__time_idx)).sum());

                            auto cur_cdist_nll = ((pow(((cdist_sumofsquares_comm_ldist_model__wgt.col(cdist_sumofsquares_comm_ldist_model__area_idx).col(cdist_sumofsquares_comm_ldist_model__time_idx) / cdist_sumofsquares_comm_ldist_model__sstotal) - (cdist_sumofsquares_comm_ldist_obs__wgt.col(cdist_sumofsquares_comm_ldist_obs__area_idx).col(cdist_sumofsquares_comm_ldist_obs__time_idx) / cdist_sumofsquares_comm_ldist_obs__sstotal)), (Type)(double)(2)))).sum();

                            {
                                nll += cdist_sumofsquares_comm_ldist_weight*cur_cdist_nll;
                                nll_cdist_sumofsquares_comm_ldist__wgt(cur_time + 1 - 1) += cur_cdist_nll;
                                nll_cdist_sumofsquares_comm_ldist__weight(cur_time + 1 - 1) = cdist_sumofsquares_comm_ldist_weight;
                                REPORT(cdist_sumofsquares_comm_ldist_obs__wgt);
                            }
                        }
                    }
                }
            }
        }
        {
            // g3a_trace_nan: g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_comm_ldist_model to cdist_sumofsquares_comm_ldist_obs;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_comm_ldist_model to cdist_sumofsquares_comm_ldist_obs'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_catchdistribution_sumofsquares: Compare cdist_sumofsquares_comm_ldist_model to cdist_sumofsquares_comm_ldist_obs'\n", cur_year, cur_step);
            }
        }
        {
            // Reset understocking total;
            g3l_understocking_total = (double)(0);
        }
        {
            // g3a_trace_nan: Reset understocking total;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'Reset understocking total'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'Reset understocking total'\n", cur_year, cur_step);
            }
        }
        {
            // g3l_understocking for fish;
            // Add understocking from fish as biomass to nll;
            g3l_understocking_total += fish__overconsumption;
        }
        {
            // g3a_trace_nan: g3l_understocking for fish;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_understocking for fish'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_understocking for fish'\n", cur_year, cur_step);
            }
        }
        {
            // g3l_understocking: Combine and add to nll;
            g3l_understocking_total = pow(g3l_understocking_total, (Type)(double)(2));
            nll += (double)(1e+08)*g3l_understocking_total;
            nll_understocking__wgt(cur_time + 1 - 1) += g3l_understocking_total;
            nll_understocking__weight(cur_time + 1 - 1) = (double)(1e+08);
        }
        {
            // g3a_trace_nan: g3l_understocking: Combine and add to nll;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3l_understocking: Combine and add to nll'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3l_understocking: Combine and add to nll'\n", cur_year, cur_step);
            }
        }
        if ( report_detail == 1 ) {
            detail_fish__predby_comm.col(cur_time + 1 - 1) = fish__predby_comm;
        }
        if ( report_detail == 1 ) {
            detail_fish__renewalnum.col(cur_time + 1 - 1) = fish__renewalnum;
        }
        if ( report_detail == 1 ) {
            detail_fish__suit_comm.col(cur_time + 1 - 1) = fish__suit_comm;
        }
        if ( cur_step_final ) {
            // g3a_age for fish;
            for (auto age = fish__maxage; age >= fish__minage; age--) {
                auto fish__age_idx = age - fish__minage + 1 - 1;

                {
                    // Check stock has remained finite for this step;
                    if ( false ) {
                        assert_msg(((fish__num.col(fish__age_idx)).isFinite()).all(), "fish__num became NaN/Inf in this timestep");
                    }
                    if ( false ) {
                        assert_msg(((fish__wgt.col(fish__age_idx)).isFinite()).all(), "fish__wgt became NaN/Inf in this timestep");
                    }
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
        {
            // g3a_trace_nan: g3a_age for fish;
            if ( ! nan_fish__num && ((fish__num).isNaN()).any() ) {
                nan_fish__num = true;
                Rprintf("fish__num became NaN at %d-%d, after 'g3a_age for fish'\n", cur_year, cur_step);
            }
            if ( ! nan_fish__wgt && ((fish__wgt).isNaN()).any() ) {
                nan_fish__wgt = true;
                Rprintf("fish__wgt became NaN at %d-%d, after 'g3a_age for fish'\n", cur_year, cur_step);
            }
        }
    }
    abort();  // Should have returned somewhere in the loop
}
