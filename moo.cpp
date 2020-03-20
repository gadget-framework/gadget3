#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
    PARAMETER(growth_rate);
    DATA_INTEGER(run_years);
    DATA_INTEGER(run_steps);

    array<Type> ling_imm_num(10, 10, 2);
    ling_imm_num.col(0).col(0) = rnorm(10, 0, 10).cast<Type>();
    ling_imm_num.col(1).col(0) = rnorm(10, 0, 100).cast<Type>();
    ling_imm_num.col(1).col(0) *= 10;
    REPORT(ling_imm_num);
    Type ling_imm_num_sum = ling_imm_num.sum();
    REPORT(ling_imm_num_sum);

    double parp = 1 + 2^2;
    REPORT(parp);

    return 0;
}

/*
  library(TMB)
  TMB::compile("moo.cpp", "-Wno-ignored-attributes")
  dyn.load(dynlib("moo"))
  f <- MakeADFun(
      data=list(run_years = 10, run_steps = 1),
      parameters=list(growth_rate = 0))
  f$fn(list(growth_rate = 0.1)) ; f$report()
*/
