\name{likelihood_understocking}
\alias{g3l_understocking}

\title{Gadget3 likelihood understocking action}
\description{
  Add rates of understocking in a g3 model to nll
}

\usage{
g3l_understocking(prey_stocks, power_f = ~2, nll_breakdown = FALSE, weight = 1,
    run_at = 10)

}

\arguments{
  \item{prey_stocks}{
    A list of \code{\link{g3_stock}} objects to collect catch data for
  }
  \item{power_f}{
    A \link{formula} representing power coefficient \eqn{p} to use.
  }
  \item{nll_breakdown}{
    Should the nll report be broken down by time? \code{TRUE} / \code{FALSE}
  }
  \item{weight}{
    Weighting applied to this likelihood component.
  }
  \item{run_at}{
    Order of age actions within model, see TODO:
  }
}

\details{
  The model report will contain nll_understocking__wgt, the results of the formula below.
  If \var{nll_breakdown} is \code{TRUE}, this will be an array with one entry per timestep.
}

\value{
  \subsection{g3l_catchdistribution_understocking}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{Sum the total biomass adjustment due to overstocking for each prey according to the formula
    \deqn{ \ell = \sum_{\it time}\sum_{\it areas} \Big(\sum_{\it prey\_stocks} U_{trs} \Big)^p }

    Where \eqn{ p } is the power coefficient from \var{power_f},
    \eqn{ U_{trs} } is the total biomass adjustment to predator consumtion due to overconsumtion.
  }}}
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)
ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) \%>\% g3s_age(5, 15)
lln <- g3_fleet('lln')

likelihood_actions <- list(
  g3l_understocking(list(ling_imm, ling_mat)))
}
