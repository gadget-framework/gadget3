\name{action_naturalmortality}
\alias{g3a_naturalmortality_exp}
\alias{g3a_naturalmortality}

\title{Gadget3 natural mortality action}
\description{
  Add natural mortality to a g3 model
}

\usage{
g3a_naturalmortality_exp(
        param_f = g3_parameterized('M', by_stock = by_stock, by_age = by_age),
        by_stock = TRUE,
        by_age = FALSE,
        action_step_size_f = ~cur_step_size)

g3a_naturalmortality(stock, mortality_f, run_f = TRUE, run_at = 4)

}

\arguments{
  \item{param_f}{
    A \link{formula} to substitute for \eqn{m}.
  }
  \item{action_step_size_f}{
    How much model time passes in between runs of action? defaults to \code{~cur_step_size}, i.e. every step.
    Use \code{action_step_size_f = 1} if action only runs yearly.
  }
  \item{by_stock}{
    Change the default parameterisation (e.g. to be by 'species'), see \code{\link{g3_parameterized}}.
  }
  \item{by_age}{
    Change the default parameterisation to be per stock age, see \code{\link{g3_parameterized}}.
  }
  \item{stock}{
    \code{\link{g3_stock}} mortality applies to.
  }
  \item{mortality_f}{
    A mortality \link{formula}, as defined by \code{\link{g3a_naturalmortality_exp}}.
  }
  \item{run_f}{
    \link{formula} specifying a condition for running this action, default always runs.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
  }
}

\details{
  A model can have any number of \code{g3a_naturalmortality} actions, so long as the
  calling arguments are different. For instance, \code{run_f = ~age == 5} and
  \code{run_f = ~age == 7}.
}

\value{
  \subsection{g3a_naturalmortality_exp}{A \link{formula} object with the following equation
  
    \deqn{ e^{-m \Delta t} }
  
    \describe{
      \item{\eqn{\Delta t}}{length of current timestep}
    }
  }
  \subsection{g3a_naturalmortality}{An action (i.e. list of formula objects) that will, for the given \var{stock}...\enumerate{
    \item{Remove a proportion of each stock group as calculated by the mortality formula \code{mortality_f}}
  }}
}

\seealso{
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-stock.html#sec:stocknatmort},
  \code{\link{g3a_growmature}},
  \code{\link{g3_stock}}
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)

# Natural mortality for immature ling
naturalmortality_action <- g3a_naturalmortality(ling_imm,
    g3a_naturalmortality_exp(~g3_param_vector("lingimm.M")[[age - 3 + 1]]))
}
