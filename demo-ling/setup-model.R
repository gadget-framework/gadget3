

## stock actions



## setup the immature stock first

ling_imm_actions <- list(
  g3a_initialconditions_normalparam(ling_imm,
                                    # NB: area & age factor together (gadget2 just multiplied them)
                                    factor_f = ~g3_param("lingimm.init.scalar") * exp(-1 * (g3_param_table("lingimm.M", data.frame(age = seq(ling_imm__minage, ling_imm__maxage) )) + g3_param("ling.init.F")) * age) * g3_param_table("lingimm.init", data.frame(age = seq(ling_imm__minage, ling_imm__maxage))),
                                    mean_f = ~g3_param("ling.Linf") * (1 - exp(-1 * (0.001 * g3_param("ling.k")) * (age - (1 + log(1 - g3_param("ling.recl")/g3_param("ling.Linf"))/(0.001 * g3_param("ling.k")))))),
                                    stddev_f = ~g3_param_vector("ling.init.sd")[[age]],
                                    alpha_f = ~g3_param("lingimm.walpha"),
                                    beta_f = ~g3_param("lingimm.wbeta")),
  g3a_renewal_normalparam(ling_imm,
                          factor_f = ~g3_param("ling.rec.scalar") * g3_param_table("ling.rec", data.frame(cur_year = seq(start_year, end_year))),
                          mean_f = ~g3_param("ling.Linf") * (1 - exp(-1 * (0.001 * g3_param("ling.k")) * (age - (1 + log(1 - g3_param("ling.recl")/g3_param("ling.Linf"))/(0.001 * g3_param("ling.k")))))),
                          stddev_f = ~g3_param_vector('ling.init.sd')[[age]],
                          alpha_f = ~g3_param("lingimm.walpha"),
                          beta_f = ~g3_param("lingimm.wbeta"),
                          run_f = ~cur_step == 1 && age == 3),
  g3a_growmature(ling_imm,
      impl_f = g3a_grow_impl_bbinom(
          g3a_grow_lengthvbsimple(~g3_param("ling.Linf"), ~g3_param("ling.k") * 0.001),
          g3a_grow_weightsimple(~g3_param("lingimm.walpha"), ~g3_param("lingimm.wbeta")),
          beta_f = ~g3_param("ling.bbin") * 10,
          maxlengthgroupgrowth = 15),
                 maturity_f = g3a_mature_constant( # TODO: Check constant vs. constant
                   alpha = ~0.001 * g3_param("ling.mat1"),
                   l50 = ~g3_param("ling.mat2")),
                   #run_f = ~cur_step == 1,
                 output_stocks = list(ling_mat)),
  g3a_naturalmortality(ling_imm,
                       g3a_naturalmortality_exp(~g3_param_table("lingimm.M", data.frame(age = seq(ling_imm__minage, ling_imm__maxage))))),
  g3a_age(ling_imm,
          output_stocks = list(ling_mat)),
  list())

ling_mat_actions <- list(
  g3a_initialconditions_normalparam(ling_mat,
                                    # NB: area & age factor together (gadget2 just multiplied them)
                                    factor_f = ~g3_param("lingmat.init.scalar") * exp(-1 * (g3_param_table("lingmat.M", data.frame(age = seq(ling_mat__minage, ling_mat__maxage))) + g3_param("ling.init.F")) * age) * g3_param_table("lingmat.init", data.frame(age = seq(ling_mat__minage, ling_mat__maxage))),
                                    mean_f = ~g3_param("ling.Linf") * (1 - exp(-1 * (0.001 * g3_param("ling.k")) * (age - (1 + log(1 - g3_param("ling.recl")/g3_param("ling.Linf"))/(0.001 * g3_param("ling.k")))))),
                                    stddev_f = ~g3_param_vector('ling.init.sd')[[age]],
                                    alpha_f = ~g3_param("lingmat.walpha"),
                                    beta_f = ~g3_param("lingmat.wbeta")),
  g3a_growmature(ling_mat,
      impl_f = g3a_grow_impl_bbinom(
          g3a_grow_lengthvbsimple(~g3_param("ling.Linf"), ~g3_param("ling.k") * 0.001),
          g3a_grow_weightsimple(~g3_param("lingmat.walpha"), ~g3_param("lingmat.wbeta")),
          beta_f = ~g3_param("ling.bbin") * 10,
          maxlengthgroupgrowth = 15)),
  g3a_naturalmortality(ling_mat,
                       g3a_naturalmortality_exp(~g3_param_table("lingmat.M", data.frame(age = seq(ling_mat__minage, ling_mat__maxage))))),
  g3a_age(ling_mat),
  list())

