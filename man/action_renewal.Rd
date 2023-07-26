\name{action_renewal}
\alias{g3a_renewal_vonb}
\alias{g3a_renewal_initabund}
\alias{g3a_initialconditions}
\alias{g3a_initialconditions_normalparam}
\alias{g3a_renewal}
\alias{g3a_renewal_normalparam}

\title{Gadget3 renewal actions}
\description{
  Add renewal / initialconditions to a g3 model
}

\usage{
g3a_renewal_vonb(
        Linf = g3_parameterized('Linf', by_stock = by_stock),
        K = g3_parameterized('K', by_stock = by_stock, scale = 0.001),
        recl = g3_parameterized('recl', by_stock = by_stock),
        recage = g3_parameterized('recage', by_stock = FALSE, optimise = FALSE),
        by_stock = TRUE)

g3a_renewal_initabund(
    scalar = g3_parameterized('init.scalar', by_stock = by_stock),
    init = g3_parameterized('init', by_stock = by_stock, by_age = TRUE),
    M = g3_parameterized('M', by_stock = by_stock),
    init_F = g3_parameterized('init.F', by_stock = by_stock_f),
    recage = g3_parameterized('recage', by_stock = FALSE, optimise = FALSE),
    proportion_f = ~1,
    by_stock = TRUE,
    by_stock_f = FALSE)

g3a_initialconditions(stock, num_f, wgt_f, run_f = ~cur_time == 0L, run_at = 0)

g3a_initialconditions_normalparam(
        stock,
        factor_f = g3a_renewal_initabund(by_stock = by_stock),
        mean_f = g3a_renewal_vonb(by_stock = by_stock),
        stddev_f = g3_parameterized('init.sd', by_stock = by_stock, by_age = by_age),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = ~cur_time == 0L,
        run_at = 0)

g3a_renewal(stock, num_f, wgt_f, run_f = ~TRUE, run_at = 8)

g3a_renewal_normalparam(
        stock,
        factor_f,
        mean_f = g3a_renewal_vonb(by_stock = by_stock),
        stddev_f = g3_parameterized('rec.sd', by_stock = by_stock, by_age = by_age),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = ~TRUE,
        run_at = 8)

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
  \item{Linf}{\link{formula} substituted into vonb calcuations, see below.}
  \item{K}{\link{formula} substituted into vonb calcuations, see below.}
  \item{recl}{\link{formula} substituted into vonb calcuations, see below.}
  \item{recage}{\link{formula} substituted into initial abundance and vonb calcuations, see below.}
  \item{proportion_f}{\link{formula} substituted into initial abundance calcuations, see below.}
  \item{scalar}{\link{formula} substituted into initial abundance calcuations, see below.}
  \item{init}{\link{formula} substituted into initial abundance calcuations, see below.}
  \item{M}{\link{formula} substituted into initial abundance calcuations, see below.}
  \item{init_F}{\link{formula} substituted into initial abundance calcuations, see below.}
  \item{by_stock}{Controls how parameters are grouped, see \code{\link{g3_parameterized}}}
  \item{wgt_by_stock}{Controls how parameters are grouped, see \code{\link{g3_parameterized}}}
  \item{by_stock_f}{Controls how parameters are grouped, see \code{\link{g3_parameterized}}}
  \item{by_age}{Controls how parameters are grouped, see \code{\link{g3_parameterized}}}
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
  \subsection{g3a_renewal_vonb}{A \link{formula} object representing
    \deqn{ L_{\infty} * {1 - e^{-1 * \kappa * (a - (a_{0} + \frac{\log(1 - L_{0}/L_{\infty})}{\kappa}  ))}} }
    \describe{
      \item{\eqn{L_{0}}}{Substituted for \var{recl}}
      \item{\eqn{L_{\infty}}}{Substituted for \var{Linf}}
      \item{\eqn{\kappa}}{Substituted for \var{K}}
      \item{\eqn{a_{0}}}{Substituted for \var{recage}}
    }

  }
  \subsection{g3a_renewal_initabund}{A \link{formula} object representing
    \deqn{ scalar * init * e^{-1 * (M + F_{0}) * (a - a_{0}) } * proportion }
    \describe{
      \item{scalar}{Substituted for \var{scalar}}
      \item{init}{Substituted for \var{init}}
      \item{\eqn{M}}{Substituted for \var{M}}
      \item{\eqn{F_{0}}}{Substituted for \var{init_F}}
      \item{\eqn{a_{0}}}{Substituted for \var{recage}}
      \item{proportion}{Substituted for \var{proportion}}
    }
  }
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
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-stock.html#sec:stockinitial},
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-stock.html#sec:stockrenew},
  \code{\link{g3_stock}}
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)

initialconditions_action <- g3a_initialconditions_normalparam(
    ling_imm,
    by_age = TRUE)  # per-age init.sd parameters
renewal_action <- g3a_renewal_normalparam(
    ling_imm,
    factor_f = g3_parameterized(
        'rec',
        by_stock = ling_imm,
        by_year = TRUE,
        ifmissing = NaN),
    mean_f = g3a_renewal_vonb(),
    run_f = ~cur_step == 1 && age == stock__minage && !cur_year_projection)
}
