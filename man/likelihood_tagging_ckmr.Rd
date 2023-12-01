\name{likelihood_tagging_ckmr}
\alias{g3l_tagging_ckmr}

\title{Gadget3 CKMR likelihood}
\description{
  *Experimental* CKMR tagging likelihood
}

\usage{
g3l_tagging_ckmr(
        nll_name,
        obs_data,
        fleets,
        parent_stocks,
        offspring_stocks,
        weight = substitute(
            g3_param(n, optimise = FALSE, value = 1),
            list(n = paste0(nll_name, "_weight"))),
        run_at = g3_action_order$likelihood)
}

\arguments{
  \item{nll_name}{
    Character string, used to define the variable name for \var{obsstock} and \var{modelstock}.
  }
  \item{obs_data}{
    Data.frame of observed mother-offspring pairs with columns year / parent_age / offspring_age / mo_pairs
  }
  \item{fleets}{
    A list of \code{\link{g3_stock}} objects to collect catch data for.
  }
  \item{parent_stocks}{
    A list of \code{\link{g3_stock}} objects that are parents in a \link{g3a_spawn} action
  }
  \item{offspring_stocks}{
    A list of \code{\link{g3_stock}} objects that are \code{output_stocks} in a \link{g3a_spawn} action
  }
  \item{weight}{
    Weighting applied to this likelihood component. Default is a \code{g3_param}
    that defaults to 1, allowing weights to be altered without recompiling.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
  }
}

\details{
  Implementation of CKMR based on \cite{Bravington, M.V., Skaug, H.J., & Anderson, E.C. (2016). Close-Kin Mark-Recapture. Statistical Science, 31, 259-274.}

  Only one kinship probability is implemented, mother-offspring with lethal
  sampling, i.e. (3.2) in the paper. This is then used as a pseudo-likelihood
  as per (4.1).

  \subsection{obs_data}{
    The \var{obs_data} data.frame provides observed pairs. Unlike other likelihood
    mehthods, it has a fixed structure:

    \describe{
      \item{year}{
        Year of observation for the data point.
      }
      \item{parent_age}{
        Age of the parent in an observed parent-offspring pair.
      }
      \item{offspring_age}{
        Age of the offspring in an observed parent-offspring pair.
      }
      \item{mo_pairs}{
        Number of pairs observed with these ages.
      }
    }
  }
}

\seealso{
  \cite{Bravington, M.V., Skaug, H.J., & Anderson, E.C. (2016). Close-Kin Mark-Recapture. Statistical Science, 31, 259-274.}
  \code{\link{g3_stock}}
}

\value{
  \subsection{g3l_tagging_ckmr}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{For all \var{parent_stocks} and \var{offspring_stocks}, collect spawing rate into
    \var{modelhist}__spawning and \var{modelhist}__spawned, total number of parents and total number of spawned offspring respectively}
    \item{For all \var{fleets}, collect catch data into \var{modelhist}__catch}
    \item{For any observed pairs that year, include the probability of that event happening into \code{nll}}
  }}
}
