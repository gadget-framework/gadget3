#include <TMB.hpp>
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
    PARAMETER_VECTOR(ling__rec);
    
    auto inttypelookup_getdefault = [](std::map<int, Type> lookup, int key, Type def) -> Type {
            return lookup.count(key) > 0 ? lookup[key] : def;
        };
    auto logspace_add_vec = [](vector<Type> a, Type b) -> vector<Type> {
    vector<Type> res(a.size());
    for(int i = 0; i < a.size(); i++) {
        res[i] = logspace_add(a[i], b);
    }
    return res;
};
    auto growth_bbinom = [](vector<Type> dmu, vector<Type> lengthgrouplen, int binn, Type beta) -> array<Type> {
        using namespace Eigen;

        vector<Type> delt_l = dmu / lengthgrouplen;  // i.e. width of length groups
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
    auto g3a_grow_apply = [](array<Type> delta_l, array<Type> delta_w, vector<Type> input_num, vector<Type> input_wgt) -> array<Type> {
    delta_l = delta_l.transpose();
    delta_w = delta_w.transpose();
    int total_deltas = delta_l.rows();  // # Length group increases (should be +1 for the no-change group)
    int total_lgs = delta_l.cols(); // # Length groups

    auto logspace_add_vec = [](vector<Type> a, Type b) -> vector<Type> {
        vector<Type> res(a.size());
        for(int i = 0; i < a.size(); i++) {
            res[i] = logspace_add(a[i], b);
        }
        return res;
    };

    matrix<Type> growth_matrix(total_lgs, total_lgs);
    growth_matrix.setZero();
    matrix<Type> weight_matrix(total_lgs, total_lgs);
    weight_matrix.setZero();

    for (int lg = 0; lg < total_lgs; lg++) {
        if (lg == total_lgs - 1) {  // Can't grow beyond maximum length group
            growth_matrix(lg, lg) = 1;
            weight_matrix(lg, lg) = 0;
        } else if(lg + total_deltas > total_lgs) {
            growth_matrix.block(lg, lg, 1, total_lgs - lg) = delta_l.col(lg).head(total_lgs - lg).transpose();
            growth_matrix(lg, total_lgs - 1) = delta_l.col(lg).tail(total_deltas - (total_lgs - lg) + 1).sum();
            weight_matrix.block(lg, lg, 1, total_lgs - lg) = delta_w.col(lg).head(total_lgs - lg).transpose();
        } else {
            growth_matrix.block(lg, lg, 1, total_deltas) = delta_l.col(lg).head(total_deltas).transpose();
            weight_matrix.block(lg, lg, 1, total_deltas) = delta_w.col(lg).head(total_deltas).transpose();
        }
    }
    // Apply matrices to stock
    // NB: Cast to array to get elementwise multiplication
    growth_matrix = growth_matrix.array().colwise() * input_num.array();
    weight_matrix = (weight_matrix.array().colwise() + input_wgt.array()) * growth_matrix.array();

    // Sum together all length group brackets for both length & weight
    array<Type> combined(total_lgs,2);
    combined.col(0) = growth_matrix.colwise().sum();
    combined.col(1) = weight_matrix.colwise().sum().array().rowwise() / logspace_add_vec(growth_matrix.colwise().sum(), 0).array().transpose();
    return combined;
};
    auto intintlookup_getdefault = [](std::map<int, int> lookup, int key, int def) -> int {
            return lookup.count(key) > 0 ? lookup[key] : def;
        };
    int cur_time = -1;
    DATA_IVECTOR(step_lengths)
    int end_year = 2018;
    int start_year = 1994;
    auto total_steps = (step_lengths).size()*(end_year - start_year) + (step_lengths).size() - 1;
    Type nll = 0;
    int cur_year = 0;
    int step_count = 4;
    int cur_step = 0;
    Type cur_step_size = 0;
    auto cur_step_final = false;
    int ling_imm__minage = 3;
    int ling_imm__maxage = 10;
    int ling_imm__area = 1;
    array<Type> ling_imm__num(35,1,8);
    auto ling_imm__area_idx = 0;
    DATA_VECTOR(ling_imm__midlen)
    DATA_VECTOR(ling_imm_stddev)
    array<Type> ling_imm__wgt(35,1,8);
    int ling_mat__minage = 5;
    int ling_mat__maxage = 15;
    int ling_mat__area = 1;
    array<Type> ling_mat__num(35,1,11);
    auto ling_mat__area_idx = 0;
    DATA_VECTOR(ling_mat__midlen)
    DATA_VECTOR(ling_mat_stddev)
    array<Type> ling_mat__wgt(35,1,11);
    vector<Type> igfs__catch(1);
    array<Type> ling_imm__totalpredate(35,1,8);
    array<Type> ling_mat__totalpredate(35,1,11);
    array<Type> ling_imm__igfs(35,1,8);
    int igfs__area = 1;
    auto igfs__area_idx = 0;
    array<Type> ling_mat__igfs(35,1,11);
    Type predate_totalfleet_E = 0;
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
    array<Type> ling_imm__overconsumption(35,1,8);
    array<Type> ling_mat__overconsumption(35,1,11);
    array<Type> ling_imm__transitioning_num(35,1,8);
    array<Type> ling_imm__transitioning_wgt(35,1,8);
    array<Type> ling_imm__growth_l;
    DATA_VECTOR(ling_imm__dl)
    array<Type> ling_imm__growth_w;
    Type ling_imm__prevtotal = 0;
    array<Type> ling_mat__growth_l;
    DATA_VECTOR(ling_mat__dl)
    array<Type> ling_mat__growth_w;
    Type ling_mat__prevtotal = 0;
    array<Type> ling_imm__renewalnum(35,1,8);
    array<Type> ling_imm__renewalwgt(35,1,8);
    array<Type> cdist_ldist_lln_obs__num(35,92);
    DATA_ARRAY(ldist_lln_number)
    vector<Type> cdist_ldist_lln_model__num(35);
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
    Type g3l_understocking_total = 0;
    array<Type> ling_imm_movement__transitioning_num(35,1,1);
    array<Type> ling_imm_movement__transitioning_wgt(35,1,1);
    int ling_imm_movement__minage = 11;
    int ling_imm_movement__maxage = 11;
    int ling_imm_movement__area = 1;
    auto ling_imm_movement__area_idx = 0;

    while (true) {
        {
            // g3a_time;
            cur_time += 1;
            if ( cur_time > total_steps ) return nll;
            cur_year = start_year + (((int) cur_time) / ((int) step_count));
            cur_step = (cur_time % step_count) + 1;
            cur_step_size = step_lengths ( cur_step - 1 ) / asDouble(12);
            cur_step_final = cur_step == step_count;
            if ( false ) Rprintf("** Tick: %d-%d\n", cur_year, cur_step);
        }
        {
            // g3a_initialconditions_normalparam for ling_imm;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    if ( cur_time == 0 ) {
                        // Calculate exp(-(dnorm**2) * 0.5);
                        ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) = exp(-(pow(((ling_imm__midlen - (ling__Linf*(1 - exp(-1*(0.001*ling__k)*(age - (1 + log(1 - ling__recl / ling__Linf) / (0.001*ling__k)))))))*(1 / (ling_imm_stddev ( age - 3 + 1 - 1 )))), (Type)2))*0.5);
                        // scale results;
                        ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (10000 / (ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum());
                        ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (lingimm__init__scalar*exp(-1*(lingimm__M + ling__init__F)*age)*lingimm__init ( age - 3 + 1 - 1 ));
                        // Generate corresponding mean weight;
                        ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = (lingimm__walpha)*pow(ling_imm__midlen, (Type)(lingimm__wbeta));
                    }
                }
            }
        }
        {
            // g3a_initialconditions_normalparam for ling_mat;
            for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                {
                    auto area = ling_mat__area;

                    if ( cur_time == 0 ) {
                        // Calculate exp(-(dnorm**2) * 0.5);
                        ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) = exp(-(pow(((ling_mat__midlen - (ling__Linf*(1 - exp(-1*(0.001*ling__k)*(age - (1 + log(1 - ling__recl / ling__Linf) / (0.001*ling__k)))))))*(1 / (ling_mat_stddev ( age - 5 + 1 - 1 )))), (Type)2))*0.5);
                        // scale results;
                        ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) *= (10000 / (ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)).sum());
                        ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) *= (lingmat__init__scalar*exp(-1*(lingmat__M + ling__init__F)*age)*lingmat__init ( age - 5 + 1 - 1 ));
                        // Generate corresponding mean weight;
                        ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) = (lingmat__walpha)*pow(ling_mat__midlen, (Type)(lingmat__wbeta));
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
            // g3a_predate_totalfleet for ling_imm;
            // Zero igfs-ling_imm biomass-consuming counter;
            ling_imm__igfs.setZero();
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    if ( area == igfs__area ) {
                        {
                            // Collect all suitable ling_imm biomass for igfs;
                            ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx) = ((1 / (1 + exp(-ling__igfs__alpha*(ling_imm__midlen - ling__igfs__l50))))*ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)*ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx));
                            igfs__catch(igfs__area_idx) = (igfs__catch(igfs__area_idx) + (ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum());
                        }
                    }
                }
            }
        }
        {
            // g3a_predate_totalfleet for ling_mat;
            // Zero igfs-ling_mat biomass-consuming counter;
            ling_mat__igfs.setZero();
            for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                {
                    auto area = ling_mat__area;

                    if ( area == igfs__area ) {
                        {
                            // Collect all suitable ling_mat biomass for igfs;
                            ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx) = ((1 / (1 + exp(-ling__igfs__alpha*(ling_mat__midlen - ling__igfs__l50))))*ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)*ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx));
                            igfs__catch(igfs__area_idx) = (igfs__catch(igfs__area_idx) + (ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx)).sum());
                        }
                    }
                }
            }
        }
        {
            // Scale igfs catch of ling_imm by total expected catch;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    if ( area == igfs__area ) {
                        {
                            predate_totalfleet_E = (inttypelookup_getdefault(igfs_totaldata__lookup, area*1000000 + cur_year*100 + cur_step, 0));
                            ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx) = predate_totalfleet_E*ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx) / igfs__catch(igfs__area_idx);
                            ling_imm__totalpredate.col(ling_imm__age_idx).col(ling_imm__area_idx) += ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx);
                        }
                    }
                }
            }
        }
        {
            // Scale igfs catch of ling_mat by total expected catch;
            for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                {
                    auto area = ling_mat__area;

                    if ( area == igfs__area ) {
                        {
                            predate_totalfleet_E = (inttypelookup_getdefault(igfs_totaldata__lookup, area*1000000 + cur_year*100 + cur_step, 0));
                            ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx) = predate_totalfleet_E*ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx) / igfs__catch(igfs__area_idx);
                            ling_mat__totalpredate.col(ling_mat__age_idx).col(ling_mat__area_idx) += ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx);
                        }
                    }
                }
            }
        }
        {
            // Zero igfs catch before working out post-adjustment value;
            igfs__catch.setZero();
        }
        {
            // Calculate ling_imm overconsumption coefficient;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    {
                        ling_imm__overconsumption.col(ling_imm__age_idx).col(ling_imm__area_idx) = logspace_add_vec(-200*(ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)*ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx)*0.95) / logspace_add_vec(ling_imm__totalpredate.col(ling_imm__age_idx).col(ling_imm__area_idx), 0), -200) / -200;
                        ling_imm__totalpredate.col(ling_imm__age_idx).col(ling_imm__area_idx) *= ling_imm__overconsumption.col(ling_imm__age_idx).col(ling_imm__area_idx);
                        ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) -= (ling_imm__totalpredate.col(ling_imm__age_idx).col(ling_imm__area_idx) / logspace_add_vec(ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx), 0));
                    }
                }
            }
        }
        {
            // Calculate ling_mat overconsumption coefficient;
            for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                {
                    auto area = ling_mat__area;

                    {
                        ling_mat__overconsumption.col(ling_mat__age_idx).col(ling_mat__area_idx) = logspace_add_vec(-200*(ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)*ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx)*0.95) / logspace_add_vec(ling_mat__totalpredate.col(ling_mat__age_idx).col(ling_mat__area_idx), 0), -200) / -200;
                        ling_mat__totalpredate.col(ling_mat__age_idx).col(ling_mat__area_idx) *= ling_mat__overconsumption.col(ling_mat__age_idx).col(ling_mat__area_idx);
                        ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) -= (ling_mat__totalpredate.col(ling_mat__age_idx).col(ling_mat__area_idx) / logspace_add_vec(ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx), 0));
                    }
                }
            }
        }
        {
            // Scale caught amount by overconsumption, update variables;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    if ( area == igfs__area ) {
                        {
                            ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx) *= ling_imm__overconsumption.col(ling_imm__age_idx).col(ling_imm__area_idx);
                            igfs__catch(igfs__area_idx) = (igfs__catch(igfs__area_idx) + (ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum());
                        }
                    }
                }
            }
        }
        {
            // Scale caught amount by overconsumption, update variables;
            for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                {
                    auto area = ling_mat__area;

                    if ( area == igfs__area ) {
                        {
                            ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx) *= ling_mat__overconsumption.col(ling_mat__age_idx).col(ling_mat__area_idx);
                            igfs__catch(igfs__area_idx) = (igfs__catch(igfs__area_idx) + (ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx)).sum());
                        }
                    }
                }
            }
        }
        if ( true ) {
            // Natural mortality for ling_imm;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    {
                        ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (exp(-(lingimm__M)*cur_step_size));
                    }
                }
            }
        }
        if ( true ) {
            // Natural mortality for ling_mat;
            for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                {
                    auto area = ling_mat__area;

                    {
                        ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) *= (exp(-(lingmat__M)*cur_step_size));
                    }
                }
            }
        }
        if ( cur_step_final ) {
            // Reset transitioning arrays;
            ling_imm__transitioning_num.setZero();
            ling_imm__transitioning_wgt = ling_imm__wgt;
        }
        {
            // g3a_grow for ling_imm;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    if ( true ) {
                        // Calculate length/weight delta matrices for current lengthgroups;
                        ling_imm__growth_l = growth_bbinom(((ling__Linf) - ling_imm__midlen)*(1 - exp(-(ling__k*0.001)*cur_step_size)), ling_imm__dl, 15, ling__bbin*10);
                        ling_imm__growth_w = (g3a_grow_weightsimple_vec_rotate(pow((vector<Type>)(ling_imm__midlen), lingimm__wbeta), 15 + 1) - g3a_grow_weightsimple_vec_extrude(pow((vector<Type>)(ling_imm__midlen), lingimm__wbeta), 15 + 1))*(lingimm__walpha);
                        if ( cur_step_final ) {
                            ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) -= (ling_imm__transitioning_num.col(ling_imm__age_idx).col(ling_imm__area_idx) += ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)*(1 / (1 + exp(0 - (0.001*ling__mat1)*(ling_imm__midlen - (ling__mat2))))));
                            // NB: Mean __wgt unchanged;
                        }
                        if ( false ) ling_imm__prevtotal = (ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum();
                        // Update ling_imm using delta matrices;
                        {
                            auto growthresult = g3a_grow_apply(ling_imm__growth_l, ling_imm__growth_w, ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx), ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx));

                            {
                                ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) = growthresult.col(0);
                                ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = growthresult.col(1);
                            }
                        }
                        if ( false ) assert(ling_imm__prevtotal - (ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum() < 1e-04);
                    }
                }
            }
        }
        {
            // g3a_grow for ling_mat;
            for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                {
                    auto area = ling_mat__area;

                    if ( true ) {
                        // Calculate length/weight delta matrices for current lengthgroups;
                        ling_mat__growth_l = growth_bbinom(((ling__Linf) - ling_mat__midlen)*(1 - exp(-(ling__k*0.001)*cur_step_size)), ling_mat__dl, 15, ling__bbin*10);
                        ling_mat__growth_w = (g3a_grow_weightsimple_vec_rotate(pow((vector<Type>)(ling_mat__midlen), lingmat__wbeta), 15 + 1) - g3a_grow_weightsimple_vec_extrude(pow((vector<Type>)(ling_mat__midlen), lingmat__wbeta), 15 + 1))*(lingmat__walpha);
                        {
                            
                        }
                        if ( false ) ling_mat__prevtotal = (ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)).sum();
                        // Update ling_mat using delta matrices;
                        {
                            auto growthresult = g3a_grow_apply(ling_mat__growth_l, ling_mat__growth_w, ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx), ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx));

                            {
                                ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) = growthresult.col(0);
                                ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) = growthresult.col(1);
                            }
                        }
                        if ( false ) assert(ling_mat__prevtotal - (ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)).sum() < 1e-04);
                    }
                }
            }
        }
        {
            {
                // Move ling_imm to ling_mat;
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    {
                        auto area = ling_mat__area;

                        if ( age >= ling_imm__minage && age <= ling_imm__maxage ) {
                            auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                            if ( area == ling_imm__area ) {
                                if ( cur_step_final ) {
                                    ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) = (ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx)*ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)) + ling_imm__transitioning_wgt.col(ling_imm__age_idx).col(ling_imm__area_idx)*ling_imm__transitioning_num.col(ling_imm__age_idx).col(ling_imm__area_idx)*1;
                                    ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) += ling_imm__transitioning_num.col(ling_imm__age_idx).col(ling_imm__area_idx)*1;
                                    ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) /= logspace_add_vec(ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx), 0);
                                }
                            }
                        }
                    }
                }
            }
        }
        {
            // g3a_renewal_normalparam for ling_imm;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    if ( cur_step == 1 && age == 3 ) {
                        // Calculate exp(-(dnorm**2) * 0.5);
                        ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) = exp(-(pow(((ling_imm__midlen - (ling__Linf*(1 - exp(-1*(0.001*ling__k)*(age - (1 + log(1 - ling__recl / ling__Linf) / (0.001*ling__k)))))))*(1 / (ling_imm_stddev ( age - 3 + 1 - 1 )))), (Type)2))*0.5);
                        // scale results;
                        ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (10000 / (ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum());
                        ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (ling__rec__scalar*ling__rec ( cur_year - start_year + 1 - 1 ));
                        // Generate corresponding mean weight;
                        ling_imm__renewalwgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = (lingimm__walpha)*pow(ling_imm__midlen, (Type)(lingimm__wbeta));
                        // Convert to total biomass;
                        ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) *= ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx);
                        // Add renewal numbers to ling_imm;
                        ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) += ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx);
                        ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) += (ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx)*ling_imm__renewalwgt.col(ling_imm__age_idx).col(ling_imm__area_idx));
                        // Back to mean weight;
                        ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) /= logspace_add_vec(ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx), 0);
                    }
                }
            }
        }
        {
            // g3a_renewal_normalparam for ling_imm;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    if ( cur_step == 1 && age == 5 ) {
                        // Calculate exp(-(dnorm**2) * 0.5);
                        ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) = exp(-(pow(((ling_imm__midlen - (ling__Linf*(1 - exp(-1*(0.001*ling__k)*(age - (1 + log(1 - ling__recl / ling__Linf) / (0.001*ling__k)))))))*(1 / (ling_imm_stddev ( age - 3 + 1 - 1 )))), (Type)2))*0.5);
                        // scale results;
                        ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (10000 / (ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx)).sum());
                        ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx) *= (ling__rec__scalar*ling__rec ( cur_year - start_year + 1 - 1 ));
                        // Generate corresponding mean weight;
                        ling_imm__renewalwgt.col(ling_imm__age_idx).col(ling_imm__area_idx) = (lingimm__walpha)*pow(ling_imm__midlen, (Type)(lingimm__wbeta));
                        // Convert to total biomass;
                        ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) *= ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx);
                        // Add renewal numbers to ling_imm;
                        ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx) += ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx);
                        ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) += (ling_imm__renewalnum.col(ling_imm__age_idx).col(ling_imm__area_idx)*ling_imm__renewalwgt.col(ling_imm__age_idx).col(ling_imm__area_idx));
                        // Back to mean weight;
                        ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx) /= logspace_add_vec(ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx), 0);
                    }
                }
            }
        }
        {
            // Initial data / reset observations for ldist_lln;
            if ( cur_time == 0 ) {
                cdist_ldist_lln_obs__num = ldist_lln_number;
                cdist_ldist_lln_model__num.setZero();
            }
        }
        {
            // g3l_catchdistribution: Collect catch from igfs/ling_imm for ldist_lln;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    {
                        cdist_ldist_lln_model__num += ling_imm__igfs.col(ling_imm__age_idx).col(ling_imm__area_idx) / logspace_add_vec(ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx), 0);
                    }
                }
            }
        }
        {
            // g3l_catchdistribution: Collect catch from igfs/ling_mat for ldist_lln;
            for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                {
                    auto area = ling_mat__area;

                    {
                        cdist_ldist_lln_model__num += ling_mat__igfs.col(ling_mat__age_idx).col(ling_mat__area_idx) / logspace_add_vec(ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx), 0);
                    }
                }
            }
        }
        {
            // g3l_catchdistribution: Compare cdist_ldist_lln_model to cdist_ldist_lln_obs;
            if ( true ) {
                {
                    auto cdist_ldist_lln_obs__time_idx = intintlookup_getdefault(times_cdist_ldist_lln_obs__lookup, cur_year*1000 + cur_step, -1) - 1;

                    if ( cdist_ldist_lln_obs__time_idx >= 0 ) {
                        nll += (1)*((pow((cdist_ldist_lln_model__num / logspace_add((Type)((cdist_ldist_lln_model__num).sum()), (Type)(0)) - cdist_ldist_lln_obs__num.col(cdist_ldist_lln_obs__time_idx) / logspace_add((Type)((cdist_ldist_lln_obs__num.col(cdist_ldist_lln_obs__time_idx)).sum()), (Type)(0))), (Type)2)).sum());
                    }
                }
                cdist_ldist_lln_model__num.setZero();
            }
        }
        {
            // g3l_understocking for ling_imm;
            g3l_understocking_total = 0;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    auto area = ling_imm__area;

                    {
                        // Add understocking from ling_imm as biomass to nll;
                        g3l_understocking_total += pow(((ling_imm__totalpredate.col(ling_imm__age_idx).col(ling_imm__area_idx) / ling_imm__overconsumption.col(ling_imm__age_idx).col(ling_imm__area_idx))*(1 - ling_imm__overconsumption.col(ling_imm__age_idx).col(ling_imm__area_idx))).sum(), (Type)(2));
                    }
                }
            }
            nll += (1)*g3l_understocking_total;
        }
        {
            // g3l_understocking for ling_mat;
            g3l_understocking_total = 0;
            for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                {
                    auto area = ling_mat__area;

                    {
                        // Add understocking from ling_mat as biomass to nll;
                        g3l_understocking_total += pow(((ling_mat__totalpredate.col(ling_mat__age_idx).col(ling_mat__area_idx) / ling_mat__overconsumption.col(ling_mat__age_idx).col(ling_mat__area_idx))*(1 - ling_mat__overconsumption.col(ling_mat__age_idx).col(ling_mat__area_idx))).sum(), (Type)(2));
                    }
                }
            }
            nll += (1)*g3l_understocking_total;
        }
        if ( cur_step_final ) {
            // g3a_age for ling_imm;
            for (auto age = ling_imm__maxage; age >= ling_imm__minage; age--) {
                auto ling_imm__age_idx = age - ling_imm__minage + 1 - 1;

                {
                    if (age == ling_imm__maxage) {
                        {
                            // Move oldest ling_imm into ling_imm_movement;
                            ling_imm_movement__transitioning_num.col(0) = ling_imm__num.col(ling_imm__age_idx);
                            ling_imm_movement__transitioning_wgt.col(0) = ling_imm__wgt.col(ling_imm__age_idx);
                            ling_imm__num.col(ling_imm__age_idx) = ling_imm__num.col(ling_imm__age_idx - 1);
                            ling_imm__wgt.col(ling_imm__age_idx) = ling_imm__wgt.col(ling_imm__age_idx - 1);
                        }
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
                    if (age == ling_mat__maxage) {
                        {
                            // Oldest ling_mat is a plus-group, combine with younger individuals;
                            ling_mat__wgt.col(ling_mat__age_idx) *= ling_mat__num.col(ling_mat__age_idx);
                            ling_mat__num.col(ling_mat__age_idx) += ling_mat__num.col(ling_mat__age_idx - 1);
                            ling_mat__wgt.col(ling_mat__age_idx) += (ling_mat__wgt.col(ling_mat__age_idx - 1)*ling_mat__num.col(ling_mat__age_idx - 1));
                            ling_mat__wgt.col(ling_mat__age_idx) /= logspace_add_vec(ling_mat__num.col(ling_mat__age_idx), 0);
                        }
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
            {
                // Move ling_imm_movement to ling_mat;
                for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                    auto ling_mat__age_idx = age - ling_mat__minage + 1 - 1;

                    {
                        auto area = ling_mat__area;

                        if ( age >= ling_imm_movement__minage && age <= ling_imm_movement__maxage ) {
                            auto ling_imm_movement__age_idx = age - ling_imm_movement__minage + 1 - 1;

                            if ( area == ling_imm_movement__area ) {
                                if ( cur_step_final ) {
                                    ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) = (ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx)*ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx)) + ling_imm_movement__transitioning_wgt.col(ling_imm_movement__age_idx).col(ling_imm_movement__area_idx)*ling_imm_movement__transitioning_num.col(ling_imm_movement__age_idx).col(ling_imm_movement__area_idx)*1;
                                    ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx) += ling_imm_movement__transitioning_num.col(ling_imm_movement__age_idx).col(ling_imm_movement__area_idx)*1;
                                    ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx) /= logspace_add_vec(ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx), 0);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    abort();  // Should have returned somewhere in the loop
}
