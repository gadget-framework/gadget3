\name{run_r}
\alias{g3_to_r}
\alias{print.g3_r}

\title{Gadget3 actions into R code}
\description{
  Convert g3 actions into an R function that can then be executed
}

\usage{
g3_to_r(actions, trace = FALSE, strict = FALSE)

\method{print}{g3_r}(x, ..., with_environment = FALSE, with_template = FALSE)

}

\arguments{
  \item{actions}{
    A list of actions (i.e. list of formula objects), as produced by \var{g3a_}* functions.
  }
  \item{trace}{
    If TRUE, turn all comments into print statements.
  }
  \item{strict}{
    If TRUE, enable extra sanity checking in actions. Any invalid conditions
    (e.g. more/less fish after growth) will result in a warning.
  }
  \item{x}{
    The \code{g3_to_r}-generated function to print
  }
  \item{with_environment}{
    If TRUE, list data stored in function environment when printing
  }
  \item{with_template}{
    If TRUE, show parameter template when printing
  }
  \item{...}{Other arguments}
}

\value{
  A function that takes a \var{params} variable, that defines all \var{g3_param}s required by the model.
  The following attributes will be set:
  \describe{
    \item{actions}{The original \var{actions} list given to the function}
    \item{parameter_template}{A list of all parameters expected by the model, to fill in}
  }
  Use e.g. \code{attr(fn, 'parameter_template')} to retrieve them.

  Invariant model data will be stored as a closure, i.e. in \code{environment(fn)}.
  This can be fetched with \code{environment(fn)$cdist_sumofsquares_ldist_gil_obs__num}.

  The function will return \var{nll} produced by the model.
  You can also use \code{attributes(nll)} to get any report variables from the model.
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock(c(species = 'ling', 'imm'), seq(20, 156, 4)) \%>\% g3s_age(3, 10)

initialconditions_action <- g3a_initialconditions_normalparam(
    ling_imm,
    factor_f = g3a_renewal_initabund(by_stock_f = 'species'),
    by_stock = 'species',
    by_age = TRUE)

# Timekeeping action
time_action <- g3a_time(
    start_year = 2000,
    end_year = 2004,
    c(3, 3, 3, 3))

# Generate a model from the above 2 actions
# NB: Obviously in reality we'd need more actions
fn <- g3_to_r(list(initialconditions_action, time_action))

if (interactive()) {
  # Edit the resulting function
  fn <- edit(fn)
}

param <- attr(fn, 'parameter_template')
param$project_years <- 0
param$ling.init.F <- 0.4
param$ling.Linf <- 160
param$ling.K <- 90
param$ling.recl <- 12
param$recage <- g3_stock_def(ling_imm, 'minage')
param[grepl('^ling.init.sd.', names(param))] <- 50.527220
param[grepl('^ling_imm.init.\\\\d+', names(param))] <- 1
param$ling_imm.init.scalar <- 200
param$ling_imm.walpha <- 2.27567436711055e-06
param$ling_imm.wbeta <- 3.20200445996187
param$ling_imm.M <- 0.15

# Run the model with the provided parameters
nll <- fn(param)

# Get the report from the last model run
report <- attributes(nll)

# Fetch a value from the model data
environment(fn)$ling_imm__midlen
\dontshow{stopifnot(dim(environment(fn)$ling_imm__midlen) == 35)}
}
