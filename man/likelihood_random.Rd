\name{likelihood_random}
\alias{g3l_random_dnorm}
\alias{g3l_random_walk}

\title{Gadget3 random effects likelihood actions}
\description{
  Add likelihood components for random effects
}

\usage{
g3l_random_dnorm(
        nll_name,
        param_f,
        mean_f = 0,
        sigma_f = 1,
        log_f = TRUE,
        weight = if (log_f) -1.0 else 1.0,
        run_at = 10)

g3l_random_walk(
        nll_name,
        param_f,
        sigma_f = 1,
        log_f = TRUE,
        period = 'year',
        nll_breakdown = FALSE,
        weight = if (log_f) -1.0 else 1.0,
        run_at = 10)
}

\arguments{
  \item{param_f}{
    A \link{formula} representing the value to apply \link{dnorm} to.
    Invariably a \link{g3_param} for \link{g3l_random_dnorm},
    a \link{g3_param_table} with \var{cur_year} for \link{g3l_random_walk}.
  }
  \item{mean_f}{
    A \link{formula} representing \var{mean} in \link{dnorm}.
  }
  \item{sigma_f}{
    A \link{formula} representing \var{sigma} in \link{dnorm}.
  }
  \item{log_f}{
    A \link{formula} representing \var{log} in \link{dnorm}.
  }
  \item{period}{
    When \link{dnorm} should be recalculated. Once per \code{year} or every \code{step}.
  }
  \item{nll_name}{
    Character string, used to define the variable name for \var{dnorm} output.
  }
  \item{nll_breakdown}{
    Should the nll report be broken down by time? \code{TRUE} / \code{FALSE}
  }
  \item{weight}{
    Weighting applied to this likelihood component.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
  }
}

\details{
  The model report will contain \code{nll_random_dnorm_dnorm_lin__dnorm}, the results of applying dnorm.
  If \var{nll_breakdown} is \code{TRUE}, this will be an array with one entry per timestep.
}

\value{
  \subsection{g3l_random_dnorm}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{On the final model step, calculate \code{dnorm(param_f, mean_f, sigma_f)} & add to nll}
  }}
  \subsection{g3l_random_walk}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{Calculate \code{dnorm(param_f, previous param_f, sigma_f)} (at final year if period = year)}
    \item{Add to nll.}
  }}
}

\examples{
\dontshow{library(magrittr)}

likelihood_actions <- list(
    # Calculate dnorm() for the dnorm_log parameter
    g3l_random_dnorm('dnorm_log',
        g3_parameterized('dnorm_log', value = 0, random = TRUE),
        mean_f = 0),

    # Treat the walk_year.xxxx parameters as a random walk
    g3l_random_walk('walk_year',
        g3_parameterized('walk_year', by_year = TRUE, value = 0, random = TRUE))
)}
