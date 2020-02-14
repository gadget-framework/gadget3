#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
    PARAMETER(growth_rate);
    DATA_INTEGER(run_years);
    DATA_INTEGER(run_steps);

    vector<Type> ling_imm_status(10);
    ling_imm_status = rnorm(10, 0, 10).cast<Type>();

    for (int year = 0; year < run_years; year++) {
        for (int step = 0; step < run_steps; step++) {
            // Reassign growth to next length bin
            vector<Type> growth = ling_imm_status * growth_rate;
            ling_imm_status -= growth;
            growth << 0, growth.segment(0, growth.size() - 1);
            ling_imm_status += growth;
        }
    }

    REPORT(ling_imm_status);
    return ling_imm_status.sum();
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
