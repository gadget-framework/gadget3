

## stock actions
bounded <- gadget3:::g3_native(r = function (x, a, b) {
  a + (b-a)/(1+exp(x))
}, cpp = '[](Type x, Type a, Type b) -> Type {
    return a + (b-a)/(1+exp(x));
}')




## setup the immature stock first


## recruitment and initial numbers at age come from the same distribution..
ling_init_abund <-
  ~bounded(g3_param("ling.scalar"), 1, 100) *
  bounded(g3_param_table("ling.init",
                         expand.grid(cur_year = seq(start_year, end_year),
                                     age = seq(
                                       min(ling_imm__minage, ling_mat__minage),
                                       max(ling_imm__maxage, ling_mat__maxage)))),
          0.001,200)

#
# ling_init_abund <- ~bounded(g3_param("ling.scalar"), 1, 100) * bounded(
#   if (cur_time == 0)
#     g3_param_table("ling.init", expand.grid(
#       age = seq(
#         min(ling_imm__minage, ling_mat__minage),
#         max(ling_imm__maxage, ling_mat__maxage))))
#   else
#     g3_param_table("ling.renew", expand.grid(
#       cur_year = seq(start_year, end_year)))
# , 0.001, 200)



## main a50 between bounds
a50 <-
  ~bounded(g3_param('ling.mat.a50'),min(ling_imm__minage, ling_mat__minage),max(ling_imm__maxage, ling_mat__maxage))

## ensure that old fish are not immature
prop_m_age <-
  gadget3:::f_substitute(~ 1/(1 + exp(-bounded(g3_param("ling.mat.a"),0.5,2)*(age - a50))),
                         list(a50 = a50))

## ensure recruitment length at age 1 is within the appropriate range
recl <-
  ~bounded(g3_param('ling.recl'),-5,30)

## Linf and K should be within bounds
linf <-
  ~bounded(g3_param("ling.Linf"),150,200)

K <-
  ~0.001 * bounded(g3_param("ling.k"),40,120)



## mean length is estimated based on a Von B relationship used for immature and mature
mean_l <-
  gadget3:::f_substitute(~linf * (1 - exp(-1 * (0.001 * K) *
                                            (age - (1 + log(1 - recl/linf)/K)))),
                         list(recl = recl, linf = linf, K = K))
init_F <-
  ~bounded(g3_param("ling.init.F"),0.2,0.8)





ling_imm_actions <- list(
  g3a_initialconditions_normalparam(ling_imm,
                                    # NB: area & age factor together (gadget2 just multiplied them)
                                    # initial abundance at age is 1e4 * q
                                    factor_f = gadget3:::f_substitute(~ling_init_abund *
                                                                        exp(-1 * (g3_param_table("lingimm.M",
                                                                                                 data.frame(age = seq(ling_imm__minage, ling_imm__maxage) )) +
                                                                                    init_F) * (age - ling_imm__minage)) * (1 - prop_m_age ),
                                                                      list(ling_init_abund = ling_init_abund, prop_m_age = prop_m_age, init_F = init_F)),
                                    mean_f = mean_l,
                                    stddev_f = ~g3_param_table('lingimm.init.sd',data.frame(age = seq(ling_imm__minage, ling_imm__maxage))),
                                    alpha_f = ~g3_param("lingimm.walpha"),
                                    beta_f = ~g3_param("lingimm.wbeta")),
  g3a_renewal_normalparam(ling_imm,
                          factor_f = ling_init_abund,
                          # gadget3:::f_substitute(~rec*exp(-g3_param_table("lingimm.M",
                          #                                                         data.frame(age = seq(ling_imm__minage, ling_imm__maxage)))),
                          #                                list(rec=ling_init_abund)),
                          mean_f = mean_l,
                          stddev_f = ~bounded(g3_param("ling.rec.sd"),1,5),
                          alpha_f = ~g3_param("lingimm.walpha"),
                          beta_f = ~g3_param("lingimm.wbeta"),
                          run_f = ~cur_step == 1 && age == 3 && cur_time > 0),
  g3a_growmature(ling_imm,
                 impl_f = g3a_grow_impl_bbinom(
                   g3a_grow_lengthvbsimple(linf, K),
                   g3a_grow_weightsimple(~g3_param("lingimm.walpha"),
                                         ~g3_param("lingimm.wbeta")),
                   beta_f = ~bounded(g3_param("ling.bbin"),1,1000),
                   maxlengthgroupgrowth = 5),
                 maturity_f = g3a_mature_continuous(
                   alpha = ~(0.001 * exp(g3_param("ling.mat1"))),
                   l50 = ~bounded(g3_param("ling.mat2"),20,120)),
                 output_stocks = list(ling_mat)),
  g3a_naturalmortality(ling_imm,
                       g3a_naturalmortality_exp(~g3_param_table("lingimm.M", data.frame(age = seq(ling_imm__minage, ling_imm__maxage))))),
  g3a_age(ling_imm,
          output_stocks = list(ling_mat)),
  list())

ling_mat_actions <- list(
  g3a_initialconditions_normalparam(ling_mat,
                                    # NB: area & age factor together (gadget2 just multiplied them)
                                    factor_f =
                                      gadget3:::f_substitute(~ling_init_abund *
                                                               exp(-1 * (g3_param_table("lingmat.M",
                                                                                        data.frame(age = seq(ling_imm__minage, ling_mat__maxage) )) +
                                                                           init_F) * (age - ling_imm__minage)) *prop_m_age,
                                                             list(ling_init_abund = ling_init_abund, prop_m_age = prop_m_age, init_F = init_F)),
                                    mean_f = mean_l,
                                    stddev_f = ~g3_param_table('lingmat.init.sd',data.frame(age = seq(ling_mat__minage, ling_mat__maxage))),
                                    alpha_f = ~g3_param("lingmat.walpha"),
                                    beta_f = ~g3_param("lingmat.wbeta")),
  g3a_growmature(ling_mat,
                 impl_f = g3a_grow_impl_bbinom(
                   g3a_grow_lengthvbsimple(linf, K),
                   g3a_grow_weightsimple(~g3_param("lingmat.walpha"),
                                         ~g3_param("lingmat.wbeta")),
                   beta_f = ~bounded(g3_param("ling.bbin"),1,1000),
                   maxlengthgroupgrowth = 5)),
  g3a_naturalmortality(ling_mat,
                       g3a_naturalmortality_exp(~g3_param_table("lingmat.M", data.frame(age = seq(ling_mat__minage, ling_mat__maxage))))),
  g3a_age(ling_mat),
  list())

