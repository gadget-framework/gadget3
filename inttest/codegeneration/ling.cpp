#include <TMB.hpp>

namespace map_extras {
    // at(), but throw (err) if item isn't available
    template<class Type, class KeyType>
    Type at_throw(std::map<KeyType, Type> map_in, KeyType key_in, std::string err) {
            try {
                return map_in.at(key_in);
            } catch (const std::out_of_range&) {
                throw std::runtime_error("Out of range: " + err);
            }
    }

    // at(), but return def if item isn't available
    template<class Type, class KeyType>
    Type at_def(std::map<KeyType, Type> map_in, KeyType key_in, Type def) {
            try {
                return map_in.at(key_in);
            } catch (const std::out_of_range&) {
                return def;
            }
    }
}

template<class Type>
Type objective_function<Type>::operator() () {
    PARAMETER(ling__Linf);
    PARAMETER(ling__k);
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
    PARAMETER(ling__bbin);
    PARAMETER(ling__mat1);
    PARAMETER(ling__mat2);
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
    std::map<std::tuple<int>, Type*> ling__rec = {{std::make_tuple(1994), &ling__rec__1994}, {std::make_tuple(1995), &ling__rec__1995}, {std::make_tuple(1996), &ling__rec__1996}, {std::make_tuple(1997), &ling__rec__1997}, {std::make_tuple(1998), &ling__rec__1998}, {std::make_tuple(1999), &ling__rec__1999}, {std::make_tuple(2000), &ling__rec__2000}, {std::make_tuple(2001), &ling__rec__2001}, {std::make_tuple(2002), &ling__rec__2002}, {std::make_tuple(2003), &ling__rec__2003}, {std::make_tuple(2004), &ling__rec__2004}, {std::make_tuple(2005), &ling__rec__2005}, {std::make_tuple(2006), &ling__rec__2006}, {std::make_tuple(2007), &ling__rec__2007}, {std::make_tuple(2008), &ling__rec__2008}, {std::make_tuple(2009), &ling__rec__2009}, {std::make_tuple(2010), &ling__rec__2010}, {std::make_tuple(2011), &ling__rec__2011}, {std::make_tuple(2012), &ling__rec__2012}, {std::make_tuple(2013), &ling__rec__2013}, {std::make_tuple(2014), &ling__rec__2014}, {std::make_tuple(2015), &ling__rec__2015}, {std::make_tuple(2016), &ling__rec__2016}, {std::make_tuple(2017), &ling__rec__2017}, {std::make_tuple(2018), &ling__rec__2018}};
    PARAMETER(ldist_lln_weight);
    auto assert_msg = [](bool expr, std::string message) -> void {
    if (!expr) {
        std::cerr << "Check failed: " << message << "\n";
        abort();
    }
};
    auto inttypelookup_getdefault = [](std::map<int, Type> lookup, int key, Type def) -> Type {
            return lookup.count(key) > 0 ? lookup[key] : def;
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
    auto intintlookup_getdefault = [](std::map<int, int> lookup, int key, int def) -> int {
            return lookup.count(key) > 0 ? lookup[key] : def;
        };
    int cur_time = -1;
    Type nll = (double)(0);
    DATA_IVECTOR(step_lengths)
    int end_year = 2018;
    int start_year = 1994;
    auto total_steps = (step_lengths).size()*(end_year - start_year) + (step_lengths).size() - 1;
    int cur_year = 0;
    auto step_count = (step_lengths).size();
    int cur_step = 0;
    auto cur_step_final = false;
    int ling_imm__minage = 3;
    int ling_imm__maxage = 10;
    int ling_imm__area = 1;
    array<Type> ling_imm__num(35,1,8);
    DATA_VECTOR(ling_imm__midlen)
    DATA_VECTOR(ling_imm_stddev)
    array<Type> ling_imm__wgt(35,1,8);
    int ling_mat__minage = 5;
    int ling_mat__maxage = 15;
    int ling_mat__area = 1;
    array<Type> ling_mat__num(35,1,11);
    DATA_VECTOR(ling_mat__midlen)
    DATA_VECTOR(ling_mat_stddev)
    array<Type> ling_mat__wgt(35,1,11);
    vector<Type> igfs__catch(1);
    array<Type> ling_imm__totalpredate(35,1,8);
    array<Type> ling_mat__totalpredate(35,1,11);
    array<Type> ling_imm__igfs(35,1,8);
    int igfs__area = 1;
    array<Type> ling_imm__suit_igfs(35,1,8); ling_imm__suit_igfs.setZero();
    array<Type> ling_mat__igfs(35,1,11);
    array<Type> ling_mat__suit_igfs(35,1,11); ling_mat__suit_igfs.setZero();
    auto inttypelookup_zip = [](vector<int> keys, vector<Type> values) -> std::map<int, Type> {
            std::map<int, Type> lookup = {};

            assert(keys.size() == values.size());
            for (size_t i = 0; i < keys.size(); ++i) {
                lookup[keys[i]] = values[i];
            }
            return lookup;
        };
    DATA_IVECTOR(igfs_totaldata__keys)
    DATA_VECTOR(igfs_totaldata__values)
    auto igfs_totaldata__lookup = inttypelookup_zip(igfs_totaldata__keys, igfs_totaldata__values);
    array<Type> ling_imm__consratio(35,1,8);
    Type ling_imm__overconsumption = (double)(0);
    array<Type> ling_mat__consratio(35,1,11);
    Type ling_mat__overconsumption = (double)(0);
    auto cur_step_size = step_lengths ( 0 ) / (double)(12);
    array<Type> ling_imm__transitioning_num(35,1,8); ling_imm__transitioning_num.setZero();
    array<Type> ling_imm__transitioning_wgt(35,1,8);
    int ling_imm__growth_lastcalc = -1;
    array<Type> ling_imm__growth_l;
    Type ling_imm__plusdl = (double)(4);
    array<Type> ling_imm__growth_w;
    Type ling_imm__prevtotal = (double)(0);
    int ling_mat__growth_lastcalc = -1;
    array<Type> ling_mat__growth_l;
    Type ling_mat__plusdl = (double)(4);
    array<Type> ling_mat__growth_w;
    Type ling_mat__prevtotal = (double)(0);
    array<Type> ling_imm__renewalnum(35,1,8); ling_imm__renewalnum.setZero();
    array<Type> ling_imm__renewalwgt(35,1,8); ling_imm__renewalwgt.setZero();
    vector<Type> cdist_ldist_lln_model__num(35); cdist_ldist_lln_model__num.setZero();
    auto intintlookup_zip = [](vector<int> keys, vector<int> values) -> std::map<int, int> {
            std::map<int, int> lookup = {};

            assert(keys.size() == values.size());
            for (size_t i = 0; i < keys.size(); ++i) {
                lookup[keys[i]] = values[i];
            }
            return lookup;
        };
    DATA_IVECTOR(times_cdist_ldist_lln_obs__keys)
    DATA_IVECTOR(times_cdist_ldist_lln_obs__values)
    auto times_cdist_ldist_lln_obs__lookup = intintlookup_zip(times_cdist_ldist_lln_obs__keys, times_cdist_ldist_lln_obs__values);
    DATA_ARRAY(cdist_ldist_lln_obs__num)
    vector<Type> nll_cdist_ldist_lln__num(total_steps + 1); nll_cdist_ldist_lln__num.setZero();
    vector<Type> nll_cdist_ldist_lln__weight(total_steps + 1); nll_cdist_ldist_lln__weight.setZero();
    Type g3l_understocking_total = (double)(0);
    vector<Type> nll_understocking__wgt(total_steps + 1); nll_understocking__wgt.setZero();
    vector<Type> nll_understocking__weight(total_steps + 1); nll_understocking__weight.setZero();
    array<Type> ling_imm_movement__transitioning_num(35,1,1);
    array<Type> ling_imm_movement__transitioning_wgt(35,1,1);
    int ling_imm_movement__minage = 11;
    int ling_imm_movement__maxage = 11;
    int ling_imm_movement__area = 1;

    while (true) {
        {
            // g3a_time;
            cur_time += 1;
            if ( true ) {
                assert_msg(std::isfinite(asDouble(nll)), "g3a_time: nll became NaN/Inf in previous timestep");
            }
            if ( cur_time > total_steps ) {
                {
                    REPORT(cdist_ldist_lln_model__num);
                    REPORT(cur_step);
                    REPORT(cur_step_final);
                    REPORT(cur_time);
                    REPORT(cur_year);
                    REPORT(g3l_understocking_total);
                    REPORT(igfs__catch);
                    REPORT(ling_imm__consratio);
                    REPORT(ling_imm__growth_l);
                    REPORT(ling_imm__growth_lastcalc);
                    REPORT(ling_imm__growth_w);
                    REPORT(ling_imm__igfs);
                    REPORT(ling_imm__num);
                    REPORT(ling_imm__overconsumption);
                    REPORT(ling_imm__prevtotal);
                    REPORT(ling_imm__renewalnum);
                    REPORT(ling_imm__renewalwgt);
                    REPORT(ling_imm__suit_igfs);
                    REPORT(ling_imm__totalpredate);
                    REPORT(ling_imm__transitioning_num);
                    REPORT(ling_imm__transitioning_wgt);
                    REPORT(ling_imm__wgt);
                    REPORT(ling_imm_movement__transitioning_num);
                    REPORT(ling_imm_movement__transitioning_wgt);
                    REPORT(ling_mat__consratio);
                    REPORT(ling_mat__growth_l);
                    REPORT(ling_mat__growth_lastcalc);
                    REPORT(ling_mat__growth_w);
                    REPORT(ling_mat__igfs);
                    REPORT(ling_mat__num);
                    REPORT(ling_mat__overconsumption);
                    REPORT(ling_mat__prevtotal);
                    REPORT(ling_mat__suit_igfs);
                    REPORT(ling_mat__totalpredate);
                    REPORT(ling_mat__wgt);
                    REPORT(nll);
                    REPORT(nll_cdist_ldist_lln__num);
                    REPORT(nll_cdist_ldist_lln__weight);
                    REPORT(nll_understocking__weight);
                    REPORT(nll_understocking__wgt);
                }
                return nll;
            }
            cur_year = start_year + (((int) cur_time) / ((int) step_count));
            cur_step = (cur_time % step_count) + 1;
            cur_step_final = cur_step == step_count;
            if ( false ) {
                Rprintf("** Tick: %d-%d\n", cur_year, cur_step);
            }
        }
        {
            auto ling_imm__area_idx = 0;

            {
                // g3a_initialconditions_normalparam for ling_imm;
                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) if ( cur_time == 0 ) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    {
                        auto area = ling_imm__area;

                        {
                            // Calculate exp(-(dnorm**2) * 0.5);
                            ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) = exp(-(pow(((ling_imm__midlen - ((ling__Linf*((double)(1) - exp(-(double)(1)*((double)(0.001)*ling__k)*(age - ((double)(1) + log((double)(1) - ling__recl / ling__Linf) / ((double)(0.001)*ling__k))))))))*((double)(1) / (ling_imm_stddev ( age - 3 + 1 - 1 )))), (Type)(double)(2)))*(double)(0.5));
                            // scale results;
                            ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) *= ((double)(10000) / (ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum());
                            ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (lingimm__init__scalar*exp(-(double)(1)*(lingimm__M + ling__init__F)*age)*lingimm__init ( age - 3 + 1 - 1 ));
                            // Generate corresponding mean weight;
                            ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = lingimm__walpha*pow(ling_imm__midlen, (Type)lingimm__wbeta);
                        }
                    }
                }
            }
        }
        {
            auto ling_mat__area_idx = 0;

            {
                // g3a_initialconditions_normalparam for ling_mat;
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) if ( cur_time == 0 ) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    {
                        auto area = ling_mat__area;

                        {
                            // Calculate exp(-(dnorm**2) * 0.5);
                            ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) = exp(-(pow(((ling_mat__midlen - ((ling__Linf*((double)(1) - exp(-(double)(1)*((double)(0.001)*ling__k)*(age - ((double)(1) + log((double)(1) - ling__recl / ling__Linf) / ((double)(0.001)*ling__k))))))))*((double)(1) / (ling_mat_stddev ( age - 5 + 1 - 1 )))), (Type)(double)(2)))*(double)(0.5));
                            // scale results;
                            ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) *= ((double)(10000) / (ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)).sum());
                            ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) *= (lingmat__init__scalar*exp(-(double)(1)*(lingmat__M + ling__init__F)*age)*lingmat__init ( age - 5 + 1 - 1 ));
                            // Generate corresponding mean weight;
                            ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) = lingmat__walpha*pow(ling_mat__midlen, (Type)lingmat__wbeta);
                        }
                    }
                }
            }
        }
        {
            // Zero biomass-caught counter for igfs;
            igfs__catch.setZero();
        }
        {
            // Zero total predation counter for ling_imm;
            ling_imm__totalpredate.setZero();
        }
        {
            // Zero total predation counter for ling_mat;
            ling_mat__totalpredate.setZero();
        }
        {
            auto igfs__area_idx = 0;

            auto ling_imm__area_idx = 0;

            {
                // g3a_predate_fleet for ling_imm;
                // Zero igfs-ling_imm biomass-consuming counter;
                ling_imm__igfs.setZero();
                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    auto area = ling_imm__area;

                    if ( area == igfs__area ) {
                        // Collect all suitable ling_imm biomass for igfs;
                        ling_imm__suit_igfs.col(ling_imm__age_idx).col(ling_imm__area_idx) = (double)(1) / ((double)(1) + exp(-ling__igfs__alpha*(ling_imm__midlen - ling__igfs__l50)));
                        ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx) = ling_imm__suit_igfs.col(ling_imm__age_idx).col(ling_imm__area_idx)*ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)*ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx);
                        igfs__catch(igfs__area_idx) += (ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum();
                    }
                }
            }
        }
        {
            auto igfs__area_idx = 0;

            auto ling_mat__area_idx = 0;

            {
                // g3a_predate_fleet for ling_mat;
                // Zero igfs-ling_mat biomass-consuming counter;
                ling_mat__igfs.setZero();
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    auto area = ling_mat__area;

                    if ( area == igfs__area ) {
                        // Collect all suitable ling_mat biomass for igfs;
                        ling_mat__suit_igfs.col(ling_mat__age_idx).col(ling_mat__area_idx) = (double)(1) / ((double)(1) + exp(-ling__igfs__alpha*(ling_mat__midlen - ling__igfs__l50)));
                        ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx) = ling_mat__suit_igfs.col(ling_mat__age_idx).col(ling_mat__area_idx)*ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)*ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx);
                        igfs__catch(igfs__area_idx) += (ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx)).sum();
                    }
                }
            }
        }
        {
            auto igfs__area_idx = 0;

            auto ling_imm__area_idx = 0;

            {
                // Scale igfs catch of ling_imm by total expected catch;
                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    auto area = ling_imm__area;

                    if ( area == igfs__area ) {
                        ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (inttypelookup_getdefault(igfs_totaldata__lookup, (area*1000000 + cur_year*100 + cur_step), (double)(0)) / igfs__catch(igfs__area_idx));
                        ling_imm__totalpredate.col(ling_imm__age_idx).col(ling_imm__area_idx) += ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx);
                    }
                }
            }
        }
        {
            auto igfs__area_idx = 0;

            auto ling_mat__area_idx = 0;

            {
                // Scale igfs catch of ling_mat by total expected catch;
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    auto area = ling_mat__area;

                    if ( area == igfs__area ) {
                        ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx) *= (inttypelookup_getdefault(igfs_totaldata__lookup, (area*1000000 + cur_year*100 + cur_step), (double)(0)) / igfs__catch(igfs__area_idx));
                        ling_mat__totalpredate.col(ling_mat__age_idx).col(ling_mat__area_idx) += ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx);
                    }
                }
            }
        }
        {
            // Temporarily convert to being proportion of totalpredate;
            ling_imm__igfs /= avoid_zero_vec(ling_imm__totalpredate);
        }
        {
            // Temporarily convert to being proportion of totalpredate;
            ling_mat__igfs /= avoid_zero_vec(ling_mat__totalpredate);
        }
        {
            // Calculate ling_imm overconsumption coefficient;
            ling_imm__consratio = ling_imm__totalpredate / avoid_zero_vec(ling_imm__num*ling_imm__wgt);
            ling_imm__consratio = (double)(0.96) - logspace_add_vec(((double)(0.96) - ling_imm__consratio)*(double)(100), (double)(0.96)) / (double)(100);
            if ( true ) {
                assert_msg((ling_imm__consratio <= (double)(1)).all(), "g3a_predate_fleet: ling_imm__consratio <= 1, can't consume more fish than currently exist");
            }
            // Apply overconsumption to prey;
            ling_imm__overconsumption = (ling_imm__totalpredate).sum();
            ling_imm__totalpredate = (ling_imm__num*ling_imm__wgt)*ling_imm__consratio;
            ling_imm__overconsumption -= (ling_imm__totalpredate).sum();
            ling_imm__num *= ((double)(1) - ling_imm__consratio);
        }
        {
            // Calculate ling_mat overconsumption coefficient;
            ling_mat__consratio = ling_mat__totalpredate / avoid_zero_vec(ling_mat__num*ling_mat__wgt);
            ling_mat__consratio = (double)(0.96) - logspace_add_vec(((double)(0.96) - ling_mat__consratio)*(double)(100), (double)(0.96)) / (double)(100);
            if ( true ) {
                assert_msg((ling_mat__consratio <= (double)(1)).all(), "g3a_predate_fleet: ling_mat__consratio <= 1, can't consume more fish than currently exist");
            }
            // Apply overconsumption to prey;
            ling_mat__overconsumption = (ling_mat__totalpredate).sum();
            ling_mat__totalpredate = (ling_mat__num*ling_mat__wgt)*ling_mat__consratio;
            ling_mat__overconsumption -= (ling_mat__totalpredate).sum();
            ling_mat__num *= ((double)(1) - ling_mat__consratio);
        }
        {
            // Zero igfs catch before working out post-adjustment value;
            igfs__catch.setZero();
        }
        {
            auto ling_imm__area_idx = 0;

            auto igfs__area_idx = 0;

            {
                // Revert to being total biomass (applying overconsumption in process);
                ling_imm__igfs *= ling_imm__totalpredate;
                // Update total catch;
                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    auto area = ling_imm__area;

                    if ( area == igfs__area ) {
                        igfs__catch(igfs__area_idx) += (ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum();
                    }
                }
            }
        }
        {
            auto ling_mat__area_idx = 0;

            auto igfs__area_idx = 0;

            {
                // Revert to being total biomass (applying overconsumption in process);
                ling_mat__igfs *= ling_mat__totalpredate;
                // Update total catch;
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    auto area = ling_mat__area;

                    if ( area == igfs__area ) {
                        igfs__catch(igfs__area_idx) += (ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx)).sum();
                    }
                }
            }
        }
        {
            auto ling_imm__area_idx = 0;

            {
                // Natural mortality for ling_imm;
                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    auto area = ling_imm__area;

                    ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) *= exp(-(lingimm__M)*cur_step_size);
                }
            }
        }
        {
            auto ling_mat__area_idx = 0;

            {
                // Natural mortality for ling_mat;
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    auto area = ling_mat__area;

                    ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) *= exp(-(lingmat__M)*cur_step_size);
                }
            }
        }
        if ( cur_step_final ) {
            // Reset transitioning arrays;
            ling_imm__transitioning_num.setZero();
            ling_imm__transitioning_wgt = ling_imm__wgt;
        }
        {
            auto ling_imm__area_idx = 0;

            {
                // g3a_grow for ling_imm;
                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    auto area = ling_imm__area;

                    {
                        if ( ling_imm__growth_lastcalc != std::floor(cur_step_size*12) ) {
                            // Calculate length/weight delta matrices for current lengthgroups;
                            ling_imm__growth_l = growth_bbinom(avoid_zero_vec(avoid_zero_vec((ling__Linf - ling_imm__midlen)*((double)(1) - exp(-((ling__k*(double)(0.001)))*cur_step_size))) / ling_imm__plusdl), (double)(15), avoid_zero((ling__bbin*(double)(10))));
                            ling_imm__growth_w = (g3a_grow_weightsimple_vec_rotate(pow((vector<Type>)(ling_imm__midlen), lingimm__wbeta), (double)(15) + (double)(1)) - g3a_grow_weightsimple_vec_extrude(pow((vector<Type>)(ling_imm__midlen), lingimm__wbeta), (double)(15) + (double)(1)))*lingimm__walpha;
                            // Don't recalculate until cur_step_size changes;
                            ling_imm__growth_lastcalc = std::floor(cur_step_size*12);
                        }
                        if ( true ) {
                            ling_imm__prevtotal = (ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum();
                        }
                        if (cur_step_final) {
                            auto maturity_ratio = ((double)(1) / ((double)(1) + exp(((double)(0) - ((double)(0.001)*ling__mat1)*(ling_imm__midlen - ling__mat2)))));

                            {
                                // Grow and separate maturing ling_imm;
                                {
                                    auto growthresult = g3a_grow_apply(ling_imm__growth_l, ling_imm__growth_w, ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)*maturity_ratio, ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx));

                                    {
                                        ling_imm__transitioning_num.col(ling_imm__age_idx).col(ling_imm__area_idx) = growthresult.col(0);
                                        ling_imm__transitioning_wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = growthresult.col(1);
                                    }
                                }
                                // Grow non-maturing ling_imm;
                                {
                                    auto growthresult = g3a_grow_apply(ling_imm__growth_l, ling_imm__growth_w, ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)*((double)(1) - maturity_ratio), ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx));

                                    {
                                        ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) = growthresult.col(0);
                                        ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = growthresult.col(1);
                                    }
                                }
                            }
                        } else {
                            // Update ling_imm using delta matrices;
                            {
                                auto growthresult = g3a_grow_apply(ling_imm__growth_l, ling_imm__growth_w, ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx), ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx));

                                {
                                    ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) = growthresult.col(0);
                                    ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = growthresult.col(1);
                                }
                            }
                        }
                        if ( true ) {
                            if (cur_step_final) {
                                assert_msg(CppAD::abs(ling_imm__prevtotal - (ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum() - (ling_imm__transitioning_num.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum()) < (double)(1e-04), "g3a_growmature: ling_imm__num totals are not the same before and after growth (excluding maturation)");
                            } else {
                                assert_msg(CppAD::abs(ling_imm__prevtotal - (ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum()) < (double)(1e-04), "g3a_growmature: ling_imm__num totals are not the same before and after growth");
                            }
                        }
                    }
                }
            }
        }
        {
            auto ling_mat__area_idx = 0;

            {
                // g3a_grow for ling_mat;
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    auto area = ling_mat__area;

                    {
                        if ( ling_mat__growth_lastcalc != std::floor(cur_step_size*12) ) {
                            // Calculate length/weight delta matrices for current lengthgroups;
                            ling_mat__growth_l = growth_bbinom(avoid_zero_vec(avoid_zero_vec((ling__Linf - ling_mat__midlen)*((double)(1) - exp(-((ling__k*(double)(0.001)))*cur_step_size))) / ling_mat__plusdl), (double)(15), avoid_zero((ling__bbin*(double)(10))));
                            ling_mat__growth_w = (g3a_grow_weightsimple_vec_rotate(pow((vector<Type>)(ling_mat__midlen), lingmat__wbeta), (double)(15) + (double)(1)) - g3a_grow_weightsimple_vec_extrude(pow((vector<Type>)(ling_mat__midlen), lingmat__wbeta), (double)(15) + (double)(1)))*lingmat__walpha;
                            // Don't recalculate until cur_step_size changes;
                            ling_mat__growth_lastcalc = std::floor(cur_step_size*12);
                        }
                        if ( true ) {
                            ling_mat__prevtotal = (ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)).sum();
                        }
                        // Update ling_mat using delta matrices;
                        {
                            auto growthresult = g3a_grow_apply(ling_mat__growth_l, ling_mat__growth_w, ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx), ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx));

                            {
                                ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) = growthresult.col(0);
                                ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) = growthresult.col(1);
                            }
                        }
                        if ( true ) {
                            assert_msg(CppAD::abs(ling_mat__prevtotal - (ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)).sum()) < (double)(1e-04), "g3a_growmature: ling_mat__num totals are not the same before and after growth");
                        }
                    }
                }
            }
        }
        {
            auto ling_imm__area_idx = 0;

            auto ling_mat__area_idx = 0;

            {
                // Move ling_imm to ling_mat;
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) if ( age >= ling_imm__minage && age <= ling_imm__maxage ) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    {
                        auto area = ling_mat__area;

                        if ( area == ling_imm__area ) {
                            auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                            if ( cur_step_final ) {
                                ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) = (ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx)*ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)) + ling_imm__transitioning_wgt.col(ling_imm__age_idx).col(ling_imm__area_idx)*ling_imm__transitioning_num.col(ling_imm__age_idx).col(ling_imm__area_idx);
                                ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) += ling_imm__transitioning_num.col(ling_imm__age_idx).col(ling_imm__area_idx);
                                ling_imm__transitioning_num.col(ling_imm__age_idx).col(ling_imm__area_idx) -= ling_imm__transitioning_num.col(ling_imm__age_idx).col(ling_imm__area_idx);
                                ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) /= avoid_zero_vec(ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx));
                            }
                        }
                    }
                }
                // Move any unclaimed stock back to ling_imm;
                if ( cur_step_final ) {
                    ling_imm__num += ling_imm__transitioning_num;
                }
            }
        }
        {
            auto ling_imm__area_idx = 0;

            {
                // g3a_renewal_normalparam for ling_imm;
                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) if ( (cur_step == 1 && age == 5) ) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    {
                        auto area = ling_imm__area;

                        {
                            // Calculate exp(-(dnorm**2) * 0.5);
                            ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) = exp(-(pow(((ling_imm__midlen - ((ling__Linf*((double)(1) - exp(-(double)(1)*((double)(0.001)*ling__k)*(age - ((double)(1) + log((double)(1) - ling__recl / ling__Linf) / ((double)(0.001)*ling__k))))))))*((double)(1) / (ling_imm_stddev ( age - 3 + 1 - 1 )))), (Type)(double)(2)))*(double)(0.5));
                            // scale results;
                            ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) *= ((double)(10000) / (ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum());
                            ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (ling__rec__scalar* *map_extras::at_throw(ling__rec, std::make_tuple(cur_year), "ling.rec"));
                            // Generate corresponding mean weight;
                            ling_imm__renewalwgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = lingimm__walpha*pow(ling_imm__midlen, (Type)lingimm__wbeta);
                            // Add result to ling_imm;
                            ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = ratio_add_vec(ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx), ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx), ling_imm__renewalwgt.col(ling_imm__age_idx).col(ling_imm__area_idx), ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx));
                            ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) += ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx);
                        }
                    }
                }
            }
        }
        {
            auto ling_imm__area_idx = 0;

            {
                // g3a_renewal_normalparam for ling_imm;
                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) if ( (cur_step == 1 && age == 3) ) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    {
                        auto area = ling_imm__area;

                        {
                            // Calculate exp(-(dnorm**2) * 0.5);
                            ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) = exp(-(pow(((ling_imm__midlen - ((ling__Linf*((double)(1) - exp(-(double)(1)*((double)(0.001)*ling__k)*(age - ((double)(1) + log((double)(1) - ling__recl / ling__Linf) / ((double)(0.001)*ling__k))))))))*((double)(1) / (ling_imm_stddev ( age - 3 + 1 - 1 )))), (Type)(double)(2)))*(double)(0.5));
                            // scale results;
                            ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) *= ((double)(10000) / (ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum());
                            ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (ling__rec__scalar* *map_extras::at_throw(ling__rec, std::make_tuple(cur_year), "ling.rec"));
                            // Generate corresponding mean weight;
                            ling_imm__renewalwgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = lingimm__walpha*pow(ling_imm__midlen, (Type)lingimm__wbeta);
                            // Add result to ling_imm;
                            ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = ratio_add_vec(ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx), ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx), ling_imm__renewalwgt.col(ling_imm__age_idx).col(ling_imm__area_idx), ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx));
                            ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) += ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx);
                        }
                    }
                }
            }
        }
        {
            auto ling_imm__area_idx = 0;

            {
                // g3l_catchdistribution: Collect catch from igfs/ling_imm for ldist_lln;
                for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                    auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                    auto area = ling_imm__area;

                    {
                        // Take prey_stock__fleet_stock weight, convert to individuals, add to our count;
                        cdist_ldist_lln_model__num += ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx) / avoid_zero_vec(ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx));
                    }
                }
            }
        }
        {
            auto ling_mat__area_idx = 0;

            {
                // g3l_catchdistribution: Collect catch from igfs/ling_mat for ldist_lln;
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    auto area = ling_mat__area;

                    {
                        // Take prey_stock__fleet_stock weight, convert to individuals, add to our count;
                        cdist_ldist_lln_model__num += ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx) / avoid_zero_vec(ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx));
                    }
                }
            }
        }
        {
            // g3l_catchdistribution: Compare cdist_ldist_lln_model to cdist_ldist_lln_obs;
            {
                auto cdist_ldist_lln_obs__time_idx = intintlookup_getdefault(times_cdist_ldist_lln_obs__lookup, (cur_year*1000 + cur_step), -1) - 1;

                if ( cdist_ldist_lln_obs__time_idx >= 0 ) {
                    auto cur_cdist_nll = (pow((cdist_ldist_lln_model__num / avoid_zero((cdist_ldist_lln_model__num).sum()) - cdist_ldist_lln_obs__num.col(cdist_ldist_lln_obs__time_idx) / avoid_zero((cdist_ldist_lln_obs__num.col(cdist_ldist_lln_obs__time_idx)).sum())), (Type)(double)(2))).sum();

                    {
                        nll += ldist_lln_weight*cur_cdist_nll;
                        nll_cdist_ldist_lln__num(cur_time + 1 - 1) += cur_cdist_nll;
                        nll_cdist_ldist_lln__weight(cur_time + 1 - 1) = ldist_lln_weight;
                    }
                }
            }
            // Zero counters for next reporting period;
            cdist_ldist_lln_model__num.setZero();
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
            nll += g3l_understocking_total;
            nll_understocking__wgt(cur_time + 1 - 1) += g3l_understocking_total;
            nll_understocking__weight(cur_time + 1 - 1) = (double)(1);
        }
        if ( cur_step_final ) {
            // g3a_age for ling_imm;
            for (auto age = ling_imm__maxage; age >= ling_imm__minage; age--) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    // Check stock has remained finite for this step;
                    if ( true ) {
                        assert_msg(((ling_imm__num.col(ling_imm__age_idx)).isFinite()).all(), "ling_imm__num became NaN/Inf in this timestep");
                    }
                    if ( true ) {
                        assert_msg(((ling_imm__wgt.col(ling_imm__age_idx)).isFinite()).all(), "ling_imm__wgt became NaN/Inf in this timestep");
                    }
                    if (age == ling_imm__maxage) {
                        // Move oldest ling_imm into ling_imm_movement;
                        ling_imm_movement__transitioning_num.col(0) = ling_imm__num.col(ling_imm__age_idx);
                        ling_imm_movement__transitioning_wgt.col(0) = ling_imm__wgt.col(ling_imm__age_idx);
                        ling_imm__num.col(ling_imm__age_idx) = ling_imm__num.col(ling_imm__age_idx - 1);
                        ling_imm__wgt.col(ling_imm__age_idx) = ling_imm__wgt.col(ling_imm__age_idx - 1);
                    } else {
                        if (age == ling_imm__minage) {
                            // Empty youngest ling_imm age-group;
                            ling_imm__num.col(ling_imm__age_idx).setZero();
                            ling_imm__wgt.col(ling_imm__age_idx).setZero();
                        } else {
                            // Move ling_imm age-group to next one up;
                            ling_imm__num.col(ling_imm__age_idx) = ling_imm__num.col(ling_imm__age_idx - 1);
                            ling_imm__wgt.col(ling_imm__age_idx) = ling_imm__wgt.col(ling_imm__age_idx - 1);
                        }
                    }
                }
            }
        }
        if ( cur_step_final ) {
            // g3a_age for ling_mat;
            for (auto age = ling_mat__maxage; age >= ling_mat__minage; age--) {
                auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                {
                    // Check stock has remained finite for this step;
                    if ( true ) {
                        assert_msg(((ling_mat__num.col(ling_mat__age_idx)).isFinite()).all(), "ling_mat__num became NaN/Inf in this timestep");
                    }
                    if ( true ) {
                        assert_msg(((ling_mat__wgt.col(ling_mat__age_idx)).isFinite()).all(), "ling_mat__wgt became NaN/Inf in this timestep");
                    }
                    if (age == ling_mat__maxage) {
                        // Oldest ling_mat is a plus-group, combine with younger individuals;
                        ling_mat__wgt.col(ling_mat__age_idx) = ratio_add_vec(ling_mat__wgt.col(ling_mat__age_idx), ling_mat__num.col(ling_mat__age_idx), ling_mat__wgt.col(ling_mat__age_idx - 1), ling_mat__num.col(ling_mat__age_idx - 1));
                        ling_mat__num.col(ling_mat__age_idx) += ling_mat__num.col(ling_mat__age_idx - 1);
                    } else {
                        if (age == ling_mat__minage) {
                            // Empty youngest ling_mat age-group;
                            ling_mat__num.col(ling_mat__age_idx).setZero();
                            ling_mat__wgt.col(ling_mat__age_idx).setZero();
                        } else {
                            // Move ling_mat age-group to next one up;
                            ling_mat__num.col(ling_mat__age_idx) = ling_mat__num.col(ling_mat__age_idx - 1);
                            ling_mat__wgt.col(ling_mat__age_idx) = ling_mat__wgt.col(ling_mat__age_idx - 1);
                        }
                    }
                }
            }
        }
        {
            auto ling_imm_movement__area_idx = 0;

            auto ling_mat__area_idx = 0;

            {
                // Move ling_imm_movement to ling_mat;
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) if ( age >= ling_imm_movement__minage && age <= ling_imm_movement__maxage ) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    {
                        auto area = ling_mat__area;

                        if ( area == ling_imm_movement__area ) {
                            auto ling_imm_movement__age_idx = age - ling_imm_movement__minage + 1 - 1;

                            if ( cur_step_final ) {
                                ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) = (ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx)*ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)) + ling_imm_movement__transitioning_wgt.col(ling_imm_movement__age_idx).col(ling_imm_movement__area_idx)*ling_imm_movement__transitioning_num.col(ling_imm_movement__age_idx).col(ling_imm_movement__area_idx);
                                ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) += ling_imm_movement__transitioning_num.col(ling_imm_movement__age_idx).col(ling_imm_movement__area_idx);
                                ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) /= avoid_zero_vec(ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx));
                            }
                        }
                    }
                }
            }
        }
    }
    abort();  // Should have returned somewhere in the loop
}
