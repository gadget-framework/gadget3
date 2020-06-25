#include <TMB.hpp>
#include <stdio.h>  // For debugf
#include <stdarg.h>  // For debugf

template<class Type>
Type objective_function<Type>::operator() () {
    PARAMETER(ling__Linf);
    PARAMETER(ling__k);
    PARAMETER(ling__recl);
    PARAMETER(lingimm__init__scalar);
    PARAMETER(lingimm__M);
    PARAMETER(ling__init__F);
    PARAMETER(lingimm__init__);
    PARAMETER(lingimm__walpha);
    PARAMETER(lingimm__wbeta);
    PARAMETER(lingmat__init__scalar);
    PARAMETER(lingmat__M);
    PARAMETER(lingmat__init__);
    PARAMETER(lingmat__walpha);
    PARAMETER(lingmat__wbeta);
    PARAMETER(ling__igfs__alpha);
    PARAMETER(ling__igfs__l50);
    PARAMETER(ling__bbin);
    
    auto debugf = [](const char* format, ...) -> void {
       va_list argptr;
       va_start(argptr, format);
       vprintf(format, argptr);
       va_end(argptr);
   };
    auto intlookup_get = [](std::map<int, Type> lookup, int key) -> Type {
        assert(lookup.count(key) > 0);
        return lookup[key];
    };
    auto growth_bbinom = [](vector<Type> dmu, int lengthgrouplen, int binn, Type beta) -> Eigen::SparseMatrix<Type> {
        using namespace Eigen;

        vector<Type> delt_l = dmu / lengthgrouplen;  // i.e. width of length groups
        vector<Type> alpha = (beta * delt_l) / (binn - delt_l);

        // possible length growth
        int na = alpha.size();
        int n = binn;
        alpha = alpha.replicate(n + 1, 1);

        vector<Type> x((n + 1) * na);
        for (auto i = 0; i < x.size(); i++) x(i) = i / alpha.size();

        // Create a probability matrix where the columns represent the
        // probability of growing x lengthgroups for each lengthgroup
        // length group jumps are distributed according to a beta-binomial
        // distribution
        vector<Type> val_vec(na * (n + 1));
        vector<Type> lgamma_arg(na * (n + 1));

        // NB: VECTORIZE1_t-ed lgamma needs a single symbol to work
        val_vec = lgamma((Type) n);
        lgamma_arg = alpha + beta; val_vec = val_vec + lgamma(lgamma_arg);
        lgamma_arg = n - x + beta; val_vec = val_vec + lgamma(lgamma_arg);
        lgamma_arg = x + alpha; val_vec = val_vec + lgamma(lgamma_arg);
        lgamma_arg = n - x + 1; val_vec = val_vec - lgamma(lgamma_arg);
        lgamma_arg = x + 1; val_vec = val_vec - lgamma(lgamma_arg);
        lgamma_arg = n + alpha + beta; val_vec = val_vec - lgamma(lgamma_arg);
        val_vec = val_vec - lgamma(beta);
        // NB: Straight lgamma(alpha) segfaults
        lgamma_arg = alpha + 0; val_vec = val_vec - lgamma(lgamma_arg);

        // Map val_vec into a matrix
        Eigen::Map<Eigen::Matrix<Type, Dynamic, Dynamic>> val(val_vec.vec().data(), na, n);

        Eigen::SparseMatrix<Type> growth_matrix(na, na);
        for(int lg = 0; lg < na ; lg++) {
          if(lg == (na - 1)){
            growth_matrix.coeffRef(lg, lg) = 1;
          } else if(lg + n > na){
            for (int i = 0 ; i < (na - lg); i++) growth_matrix.coeffRef(lg, lg + i) = val(lg, i);
            growth_matrix.coeffRef(lg, na - 1) = val.block(lg, na - lg, 1, n - (na - lg)).sum();
          } else {
            for (int i = 0; i < n; i++) growth_matrix.coeffRef(lg, i) = val(lg, i);
          }
        }
        growth_matrix.makeCompressed();

        return growth_matrix;
    };
    int cur_time = 0;
    
    DATA_IVECTOR(steps)
    int end_year = 1985;
    int start_year = 1983;
    auto total_steps = (steps).size()*(end_year - start_year) + (steps).size() - 1;
    int cur_year = 0;
    int step_count = 4;
    int cur_step = 0;
    int cur_step_len = 0;
    auto cur_step_final = false;
    int ling_imm__minage = 3;
    int ling_imm__maxage = 10;
    int ling_imm__age_idx = 0;
    DATA_IVECTOR(ling_imm__areas)
    int area = 1;
    vector<Type> initcond_dnorm(35);
    DATA_VECTOR(ling_imm__meanlen)
    DATA_VECTOR(ling_imm_stddev)
    array<Type> ling_imm__num(35,1,8);
    Type initcond_scaler = 0;
    array<Type> ling_imm__wgt(35,1,8);
    int ling_mat__minage = 5;
    int ling_mat__maxage = 15;
    int ling_mat__age_idx = 0;
    DATA_IVECTOR(ling_mat__areas)
    DATA_VECTOR(ling_mat__meanlen)
    DATA_VECTOR(ling_mat_stddev)
    array<Type> ling_mat__num(35,2,11);
    array<Type> ling_mat__wgt(35,2,11);
    vector<Type> igfs__catch(1);
    array<Type> ling_imm__totalpredate(35,1,8);
    array<Type> ling_mat__totalpredate(35,2,11);
    array<Type> igfs__ling_imm(35,1,8);
    DATA_IVECTOR(igfs__areas)
    int igfs__area_idx = 0;
    array<Type> igfs__ling_mat(35,2,11);
    Type predate_totalfleet_E = 0;
    
    auto intlookup_zip = [](vector<int> keys, vector<Type> values) -> std::map<int, Type> {
        std::map<int, Type> lookup = {};

        assert(keys.size() == values.size());
        for (size_t i = 0; i < keys.size(); ++i) {
            lookup[keys[i]] = values[i];
        }
        return lookup;
    };
    DATA_IVECTOR(igfs_totaldata__keys)
    DATA_VECTOR(igfs_totaldata__values)
    auto igfs_totaldata__lookup = intlookup_zip(igfs_totaldata__keys, igfs_totaldata__values);
    array<Type> ling_mat__overconsumption(35,2,11);
    array<Type> ling_imm__overconsumption(35,1,8);
    vector<Type> ling_imm__grow_l(35);
    Eigen::SparseMatrix<Type> ling_imm__growth_ratio(35,35);
    int ling_imm__dl = 4;
    int ling_imm__countlen = 35;
    vector<Type> ling_mat__grow_l(35);
    Eigen::SparseMatrix<Type> ling_mat__growth_ratio(35,35);
    int ling_mat__dl = 4;
    int ling_mat__countlen = 35;
    int matured_ling_imm__maxage = 10;
    int matured_ling_imm__age_idx = 0;
    int matured_ling_imm__minage = 3;
    DATA_IVECTOR(matured_ling_imm__areas)
    int matured_ling_imm__area_idx = 0;
    array<Type> matured_ling_imm__num(35,1,8);
    array<Type> matured_ling_imm__wgt(35,1,8);

    while (true) {
        {
          // g3a_time;
          if ( cur_time > total_steps )  break;
          cur_year = start_year + (((int) cur_time) / ((int) step_count));
          cur_step = (cur_time % step_count) + 1;
          cur_step_len = steps ( cur_step - 1 );
          cur_step_final = cur_step == step_count;
          debugf("** Tick: %d-%d\n", cur_year, cur_step);
        }

        if ( cur_time == 0 )  {
            // g3a_initialconditions for ling_imm;
            for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
                ling_imm__age_idx = age - ling_imm__minage + 1 - 1;
                for (auto ling_imm__area_idx = 0; ling_imm__area_idx < (ling_imm__areas).size(); ling_imm__area_idx++) {
                    area = ling_imm__areas ( 1 - 1 );
                    {
                      initcond_dnorm = (ling_imm__meanlen - ling__Linf*(1 - exp(-1*(0.001*ling__k)*(age - (1 + log(1 - ling__recl / ling__Linf) / (0.001*ling__k))))))*(1 / ling_imm_stddev ( ling_imm__age_idx ));
                      ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() = exp(-(pow(initcond_dnorm, (Type)2))*0.5);
                      initcond_scaler = 10000 / sum(ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx).vec());
                      ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() = ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx).vec()*initcond_scaler*lingimm__init__scalar*exp(-1*(lingimm__M + ling__init__F)*age)*lingimm__init__;
                      ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() = lingimm__walpha*pow(ling_imm__meanlen, (Type)lingimm__wbeta);
                    }

                  }

              }

          }

        if ( cur_time == 0 )  {
            // g3a_initialconditions for ling_mat;
            for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
                ling_mat__age_idx = age - ling_mat__minage + 1 - 1;
                for (auto ling_mat__area_idx = 0; ling_mat__area_idx < (ling_mat__areas).size(); ling_mat__area_idx++) {
                    area = ling_mat__areas ( ling_mat__area_idx );
                    {
                      initcond_dnorm = (ling_mat__meanlen - ling__Linf*(1 - exp(-1*(0.001*ling__k)*(age - (1 + log(1 - ling__recl / ling__Linf) / (0.001*ling__k))))))*(1 / ling_mat_stddev ( ling_mat__age_idx ));
                      ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() = exp(-(pow(initcond_dnorm, (Type)2))*0.5);
                      initcond_scaler = 10000 / sum(ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx).vec());
                      ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() = ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx).vec()*initcond_scaler*lingmat__init__scalar*exp(-1*(lingmat__M + ling__init__F)*age)*lingmat__init__;
                      ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() = lingmat__walpha*pow(ling_mat__meanlen, (Type)lingmat__wbeta);
                    }

                  }

              }

          }

        {
          // g3a_predate_totalfleet for igfs;
          {
            igfs__catch.vec().setZero();
          }

        }

        {
          // g3a_predate_totalfleet for ling_imm;
          {
            ling_imm__totalpredate.vec().setZero();
          }

        }

        {
          // g3a_predate_totalfleet for ling_mat;
          {
            ling_mat__totalpredate.vec().setZero();
          }

        }

        {
          // g3a_predate_totalfleet for ling_imm;
          {
            // Zero counter of biomass caught for this fleet;
            igfs__ling_imm.vec().setZero();
          }

          for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
              ling_imm__age_idx = age - ling_imm__minage + 1 - 1;
              for (auto ling_imm__area_idx = 0; ling_imm__area_idx < (ling_imm__areas).size(); ling_imm__area_idx++) {
                  area = ling_imm__areas ( 1 - 1 );
                  {
                    for (auto possible_area = 0; possible_area < (igfs__areas).size(); possible_area++) {
                        if ( igfs__areas ( possible_area ) == area )  {
                            igfs__area_idx = possible_area;
                            {
                              // Collect all suitable biomass for fleet;
                              igfs__ling_imm.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() = (1 / (1 + exp(-ling__igfs__alpha*(ling_imm__meanlen - ling__igfs__l50)))*ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx).vec()*ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx).vec());
                              igfs__catch.col(igfs__area_idx)(0) = (igfs__catch.col(igfs__area_idx)(0) + sum(igfs__ling_imm.col(ling_imm__age_idx).col(ling_imm__area_idx).vec()));
                            }

                            break;
                          }

                      }

                  }

                }

            }

        }

        {
          // g3a_predate_totalfleet for ling_mat;
          {
            // Zero counter of biomass caught for this fleet;
            igfs__ling_mat.vec().setZero();
          }

          for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
              ling_mat__age_idx = age - ling_mat__minage + 1 - 1;
              for (auto ling_mat__area_idx = 0; ling_mat__area_idx < (ling_mat__areas).size(); ling_mat__area_idx++) {
                  area = ling_mat__areas ( ling_mat__area_idx );
                  {
                    for (auto possible_area = 0; possible_area < (igfs__areas).size(); possible_area++) {
                        if ( igfs__areas ( possible_area ) == area )  {
                            igfs__area_idx = possible_area;
                            {
                              // Collect all suitable biomass for fleet;
                              igfs__ling_mat.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() = (1 / (1 + exp(-ling__igfs__alpha*(ling_mat__meanlen - ling__igfs__l50)))*ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx).vec()*ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx).vec());
                              igfs__catch.col(igfs__area_idx)(0) = (igfs__catch.col(igfs__area_idx)(0) + sum(igfs__ling_mat.col(ling_mat__age_idx).col(ling_mat__area_idx).vec()));
                            }

                            break;
                          }

                      }

                  }

                }

            }

        }

        {
          // g3a_predate_totalfleet for ling_imm;
          for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
              ling_imm__age_idx = age - ling_imm__minage + 1 - 1;
              for (auto ling_imm__area_idx = 0; ling_imm__area_idx < (ling_imm__areas).size(); ling_imm__area_idx++) {
                  area = ling_imm__areas ( 1 - 1 );
                  {
                    for (auto possible_area = 0; possible_area < (igfs__areas).size(); possible_area++) {
                        if ( igfs__areas ( possible_area ) == area )  {
                            igfs__area_idx = possible_area;
                            {
                              // Scale fleet amount by total expected catch;
                              predate_totalfleet_E = (intlookup_get(igfs_totaldata__lookup, area*1000000 + cur_year*100 + cur_step));
                              igfs__ling_imm.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() = predate_totalfleet_E*igfs__ling_imm.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() / igfs__catch.col(igfs__area_idx)(0);
                              ling_imm__totalpredate.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() += igfs__ling_imm.col(ling_imm__age_idx).col(ling_imm__area_idx).vec();
                            }

                            break;
                          }

                      }

                  }

                }

            }

        }

        {
          // g3a_predate_totalfleet for ling_mat;
          for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
              ling_mat__age_idx = age - ling_mat__minage + 1 - 1;
              for (auto ling_mat__area_idx = 0; ling_mat__area_idx < (ling_mat__areas).size(); ling_mat__area_idx++) {
                  area = ling_mat__areas ( ling_mat__area_idx );
                  {
                    for (auto possible_area = 0; possible_area < (igfs__areas).size(); possible_area++) {
                        if ( igfs__areas ( possible_area ) == area )  {
                            igfs__area_idx = possible_area;
                            {
                              // Scale fleet amount by total expected catch;
                              predate_totalfleet_E = (intlookup_get(igfs_totaldata__lookup, area*1000000 + cur_year*100 + cur_step));
                              igfs__ling_mat.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() = predate_totalfleet_E*igfs__ling_mat.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() / igfs__catch.col(igfs__area_idx)(0);
                              ling_mat__totalpredate.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() += igfs__ling_mat.col(ling_mat__age_idx).col(ling_mat__area_idx).vec();
                            }

                            break;
                          }

                      }

                  }

                }

            }

        }

        {
          // g3a_predate_totalfleet for ling_mat;
          for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
              ling_mat__age_idx = age - ling_mat__minage + 1 - 1;
              for (auto ling_mat__area_idx = 0; ling_mat__area_idx < (ling_mat__areas).size(); ling_mat__area_idx++) {
                  area = ling_mat__areas ( ling_mat__area_idx );
                  {
                    // Prey overconsumption coefficient;
                    ling_mat__overconsumption.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() = ((ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx).vec()*ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx).vec()*0.95) / ling_mat__totalpredate.col(ling_mat__age_idx).col(ling_mat__area_idx).vec()).cwiseMin(1);
                    ling_mat__totalpredate.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() *= ling_mat__overconsumption.col(ling_mat__age_idx).col(ling_mat__area_idx).vec();
                  }

                }

            }

        }

        {
          // g3a_predate_totalfleet for ling_imm;
          for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
              ling_imm__age_idx = age - ling_imm__minage + 1 - 1;
              for (auto ling_imm__area_idx = 0; ling_imm__area_idx < (ling_imm__areas).size(); ling_imm__area_idx++) {
                  area = ling_imm__areas ( 1 - 1 );
                  {
                    for (auto possible_area = 0; possible_area < (igfs__areas).size(); possible_area++) {
                        if ( igfs__areas ( possible_area ) == area )  {
                            igfs__area_idx = possible_area;
                            {
                              // Scale caught amount by overconsumption, update variables;
                              igfs__ling_imm.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() *= ling_imm__overconsumption.col(ling_imm__age_idx).col(ling_imm__area_idx).vec();
                              igfs__catch.col(igfs__area_idx)(0) = (igfs__catch.col(igfs__area_idx)(0) + sum(igfs__ling_imm.col(ling_imm__age_idx).col(ling_imm__area_idx).vec()));
                              ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() -= (ling_imm__totalpredate.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() / ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx).vec());
                            }

                            break;
                          }

                      }

                  }

                }

            }

        }

        {
          // g3a_predate_totalfleet for ling_mat;
          for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
              ling_mat__age_idx = age - ling_mat__minage + 1 - 1;
              for (auto ling_mat__area_idx = 0; ling_mat__area_idx < (ling_mat__areas).size(); ling_mat__area_idx++) {
                  area = ling_mat__areas ( ling_mat__area_idx );
                  {
                    for (auto possible_area = 0; possible_area < (igfs__areas).size(); possible_area++) {
                        if ( igfs__areas ( possible_area ) == area )  {
                            igfs__area_idx = possible_area;
                            {
                              // Scale caught amount by overconsumption, update variables;
                              igfs__ling_mat.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() *= ling_mat__overconsumption.col(ling_mat__age_idx).col(ling_mat__area_idx).vec();
                              igfs__catch.col(igfs__area_idx)(0) = (igfs__catch.col(igfs__area_idx)(0) + sum(igfs__ling_mat.col(ling_mat__age_idx).col(ling_mat__area_idx).vec()));
                              ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() -= (ling_mat__totalpredate.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() / ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx).vec());
                            }

                            break;
                          }

                      }

                  }

                }

            }

        }

        {
          // g3a_grow for ling_imm;
          for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
              ling_imm__age_idx = age - ling_imm__minage + 1 - 1;
              for (auto ling_imm__area_idx = 0; ling_imm__area_idx < (ling_imm__areas).size(); ling_imm__area_idx++) {
                  area = ling_imm__areas ( 1 - 1 );
                  {
                    ling_imm__grow_l = (ling__Linf - ling_imm__meanlen)*(1 - exp(-ling__k*0.001*cur_step_len));
                    ling_imm__growth_ratio = growth_bbinom(ling_imm__grow_l, ling_imm__dl, ling_imm__countlen, ling__bbin*10);
                    ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() = (ling_imm__growth_ratio * (ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx).vec()).matrix()).colwise().sum();
                  }

                }

            }

        }

        {
          // g3a_grow for ling_mat;
          for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
              ling_mat__age_idx = age - ling_mat__minage + 1 - 1;
              for (auto ling_mat__area_idx = 0; ling_mat__area_idx < (ling_mat__areas).size(); ling_mat__area_idx++) {
                  area = ling_mat__areas ( ling_mat__area_idx );
                  {
                    ling_mat__grow_l = (ling__Linf - ling_mat__meanlen)*(1 - exp(-ling__k*0.001*cur_step_len));
                    ling_mat__growth_ratio = growth_bbinom(ling_mat__grow_l, ling_mat__dl, ling_mat__countlen, ling__bbin*10);
                    ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() = (ling_mat__growth_ratio * (ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx).vec()).matrix()).colwise().sum();
                  }

                }

            }

        }

        {
          // g3a_mature for ling_imm;
          for (auto age = ling_imm__minage; age <= ling_imm__maxage; age++) {
              ling_imm__age_idx = age - ling_imm__minage + 1 - 1;
              for (auto ling_imm__area_idx = 0; ling_imm__area_idx < (ling_imm__areas).size(); ling_imm__area_idx++) {
                  area = ling_imm__areas ( 1 - 1 );
                  if ( age <= matured_ling_imm__maxage )  {
                      matured_ling_imm__age_idx = age - matured_ling_imm__minage + 1 - 1;
                      {
                        for (auto possible_area = 0; possible_area < (matured_ling_imm__areas).size(); possible_area++) {
                            if ( matured_ling_imm__areas ( possible_area ) == area )  {
                                matured_ling_imm__area_idx = possible_area;
                                {
                                  // Move matured ling_imm into temporary storage;
                                  ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() -= (matured_ling_imm__num.col(matured_ling_imm__age_idx).col(matured_ling_imm__area_idx).vec() = ling_imm__num.col(ling_imm__age_idx).col(ling_imm__area_idx).vec()*1);
                                  ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx).vec() -= (matured_ling_imm__wgt.col(matured_ling_imm__age_idx).col(matured_ling_imm__area_idx).vec() = ling_imm__wgt.col(ling_imm__age_idx).col(ling_imm__area_idx).vec()*1);
                                }

                                break;
                              }

                          }

                      }

                    }

                }

            }

        }

        {
          // assign for ling_mat;
          for (auto age = ling_mat__minage; age <= ling_mat__maxage; age++) {
              ling_mat__age_idx = age - ling_mat__minage + 1 - 1;
              for (auto ling_mat__area_idx = 0; ling_mat__area_idx < (ling_mat__areas).size(); ling_mat__area_idx++) {
                  area = ling_mat__areas ( ling_mat__area_idx );
                  if ( age <= matured_ling_imm__maxage )  {
                      matured_ling_imm__age_idx = age - matured_ling_imm__minage + 1 - 1;
                      {
                        for (auto possible_area = 0; possible_area < (matured_ling_imm__areas).size(); possible_area++) {
                            if ( matured_ling_imm__areas ( possible_area ) == area )  {
                                matured_ling_imm__area_idx = possible_area;
                                {
                                  // Move matured ling_imm to ling_mat;
                                  ling_mat__num.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() += matured_ling_imm__num.col(matured_ling_imm__age_idx).col(matured_ling_imm__area_idx).vec()*1;
                                  ling_mat__wgt.col(ling_mat__age_idx).col(ling_mat__area_idx).vec() += matured_ling_imm__wgt.col(matured_ling_imm__age_idx).col(matured_ling_imm__area_idx).vec()*1;
                                }

                                break;
                              }

                          }

                      }

                    }

                }

            }

        }

        if ( cur_step_final )  {
            // g3a_age for ling_imm;
            for (auto age = ling_imm__maxage; age <= ling_imm__minage; age++) {
                ling_imm__age_idx = age - ling_imm__minage + 1 - 1;
                if (age == ling_imm__maxage) {
                    // TODO: Plus group migration shenanigans;
                  }
 else {
                    ling_imm__num.col(ling_imm__age_idx + 1) += ling_imm__num.col(ling_imm__age_idx);
                    ling_imm__num.col(ling_imm__age_idx).setZero();
                  }

              }

          }

        {
          cur_time += 1;
        }

      }

    return 0;  // TODO:
}
