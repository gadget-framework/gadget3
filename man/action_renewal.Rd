\name{action_renewal}
\alias{g3a_initialconditions}
\alias{g3a_initialconditions_normalparam}
\alias{g3a_renewal}
\alias{g3a_renewal_normalparam}

\title{Gadget3 renewal actions}
\description{
  Add renewal / initialconditions to a g3 model
}

\usage{
g3a_initialconditions(stock, num_f, wgt_f, run_f = ~cur_time == 0L, run_at = 0)

g3a_initialconditions_normalparam(stock, factor_f, mean_f, stddev_f, alpha_f, beta_f, 
    run_f = ~cur_time == 0L, run_at = 0)

g3a_renewal(stock, num_f, wgt_f, run_f = ~TRUE, run_at = 8)

g3a_renewal_normalparam(stock, factor_f, mean_f, stddev_f, alpha_f, beta_f, 
    run_f = ~TRUE, run_at = 8)

}

\arguments{
  \item{stock}{
    The \code{\link{g3_stock}} to apply to
  }
  \item{num_f}{
    \link{formula} that produces a lengthgroup vector of number of individuals for the current age/area/... length group.
  }
  \item{wgt_f}{
    \link{formula} that produces a lenghgroup vector of mean weight for the current age/area/... length group.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
  }
  \item{factor_f}{
    \link{formula} substituted into normalparam calcuations, see below.
  }
  \item{mean_f}{
    \link{formula} substituted into normalparam calcuations, see below.
  }
  \item{stddev_f}{
    \link{formula} substituted into normalparam calcuations, see below.
  }
  \item{alpha_f}{
    \link{formula} substituted into normalparam calcuations, see below.
  }
  \item{beta_f}{
    \link{formula} substituted into normalparam calcuations, see below.
  }
  \item{run_f}{
    \link{formula} specifying a condition for running this action,
    default always runs for renewal, first timestep for initialcondtions.
  }
}

\details{
  A model can have any number of \code{g3a_renewal_*} actions, so long as the
  calling arguments are different. For instance, \code{run_f = ~age == 5} and
  \code{run_f = ~age == 7}.

  The \code{g3a_renewal_*} actions will define the following stock instance variables for \var{stock}:
  \describe{
    \item{\var{stock__renewalnum}}{Extra individuals added to the stock}
    \item{\var{stock__renewalwgt}}{Mean weight of added individuals}
  }
}

\value{
  \subsection{g3a_initialconditions / g3a_renewal}{
  An action (i.e. list of formula objects) that will, for the given \var{stock}, iterate over each
  area/age/etc. combination, and generate a lengthgroup vector of new individuals and weights using
  \var{num_f} and \var{wgt_f}.

  renewal will add fish to the existing collection, whereas initialconditions will assume the stock is currently empty.
  }
  \subsection{g3a_initialconditions_normalparam / g3a_renewal_normalparam}{
    As \link{g3a_initialconditions} / \link{g3a_renewal}, but the following formulae are used to calculate num/wgt:

    \deqn{n = e^{-(\frac{L - \mu}{\sigma})^2 / 2}}
    \deqn{N = F 10000 \frac{n}{\sum n}}
    \deqn{W = \alpha L^{\beta}}
    \describe{
      \item{\eqn{L}}{Midlength of length groups for current area/age/...}
      \item{\eqn{F}}{Substituted for \var{factor_f}}
      \item{\eqn{\mu}}{Substituted for \var{mean_f}}
      \item{\eqn{\sigma}}{Substituted for \var{stddev_f}}
      \item{\eqn{\alpha}}{Substituted for \var{alpha_f}}
      \item{\eqn{\beta}}{Substituted for \var{beta_f}}
    }
  }
}

\seealso{
  \url{https://hafro.github.io/gadget2/userguide/chap-stock.html#sec:stockinitial},
  \url{https://hafro.github.io/gadget2/userguide/chap-stock.html#sec:stockrenew},
  \code{\link{g3_stock}}
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
renewal_action <- g3a_renewal_normalparam(
    ling_imm,
    factor_f = ~g3_param("ling.rec.scalar") *
        g3_param_vector("ling.rec")[[cur_year - start_year + 1]],
    mean_f = ~g3_param("ling.Linf") * (1 - exp(-1 *
        (0.001 * g3_param("ling.k")) *
        (age - (1 + log(1 - g3_param("ling.recl") / g3_param("ling.Linf")) /
            (0.001 * g3_param("ling.k")))))),
    stddev_f = ~g3_param_vector('ling.init.sd')[[age]],
    alpha_f = ~g3_param("lingimm.walpha"),
    beta_f = ~g3_param("lingimm.wbeta"),
    run_f = ~cur_step == 1 && age == 3)
}
