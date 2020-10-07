\name{run_r}
\alias{g3_to_r}

\title{Gadget3 actions into R code}
\description{
  Convert g3 actions into an R function that can then be executed
}

\usage{
g3_to_r(actions, trace = FALSE)

}

\arguments{
  \item{actions}{
    A list of actions (i.e. list of formula objects), as produced by \var{g3a_}* functions.
  }
  \item{trace}{
    If TRUE, turn all comments into print statements.
  }
}

\value{
  A function that takes a \var{params} variable, that defines all \var{g3_param}s required by the model.
  Returns \var{nll} produced by the model, You can also use
  \code{environment(fn)$model_report} to get any report variables from the model
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)

initialconditions_action <- g3a_initialconditions_normalparam(
    ling_imm,
    # NB: area & age factor together (gadget2 just multiplied them)
    factor_f = ~g3_param("lingimm.init.scalar") *
        exp(-1 * (g3_param_vector("lingimm.M")[[age - 3 + 1]] + g3_param("ling.init.F")) * age) *
        g3_param_vector("lingimm.init")[[age - 3 + 1]],
    mean_f = ~g3_param("ling.Linf") * (1 - exp(-1 *
        (0.001 * g3_param("ling.k")) *
        (age - (1 + log(1 - g3_param("ling.recl") / g3_param("ling.Linf")) /
            (0.001 * g3_param("ling.k")))))),
    stddev_f = ~g3_param_vector("ling.init.sd")[[age]],
    alpha_f = ~g3_param("lingimm.walpha"),
    beta_f = ~g3_param("lingimm.wbeta"))

# Timekeeping action
time_action <- g3a_time(
    start_year = 2000,
    end_year = 2004,
    c(3, 3, 3, 3))

# Generate a model from the above 2 actions
# NB: Obviously in reality we'd need more actions
fn <- g3_to_r(list(initialconditions_action, time_action))

\dontrun{# Edit the resulting function
fn <- edit(fn)
}

# Run the model for provided parameters
nll <- fn(list(
    ling.init.F = 0.4,
    ling.Linf = 160,
    ling.k = 90,
    ling.recl = 12,
    ling.init.sd = rep(50.527220, 21),
    lingimm.init = rep(1, 15 - 5 + 1),
    lingimm.init.scalar = 200,
    lingimm.walpha = 2.27567436711055e-06,
    lingimm.wbeta = 3.20200445996187,
    lingimm.M = rep(0.15,3,10)))

# Get the report from the last model run
report <- environment(fn)$model_report
}
