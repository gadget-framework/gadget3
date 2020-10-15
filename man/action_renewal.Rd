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
    Order of age actions within model, see TODO:
  }
  \item{factor_f}{
    TODO:
  }
  \item{mean_f}{
    TODO:
  }
  \item{stddev_f}{
    TODO:
  }
  \item{alpha_f}{
    TODO:
  }
  \item{beta_f}{
    TODO:
  }
  \item{run_f}{
    \link{formula} specifying a condition for running this action,
    default always runs for renewal, first timestep for initialcondtions.
  }
}

\value{
  \subsection{g3a_initialconditions}{TODO:}
  \subsection{g3a_initialconditions_normalparam}{TODO:}
  \subsection{g3a_renewal}{TODO:}
  \subsection{g3a_renewal_normalparam}{TODO:}
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
