\name{likelihood_ckmr_po}
\alias{g3l_ckmr_po}
\concept{G3 action}

\title{Gadget3 CKMR likelihood}
\description{
  *Experimental* CKMR tagging likelihood
}

\usage{
g3l_ckmr_po(
        nll_name,
        obs_data,
        parent_stocks,
        offspring_stocks,
        weight = g3_parameterized(paste0(nll_name, "_weight"),
            optimise = FALSE, value = 1),
        run_at = g3_action_order$likelihood)
}

\arguments{
  \item{nll_name}{
    Character string, used as a prefix for model history storage (\var{nll_name}model) and observed data variable names.
  }
  \item{obs_data}{
    Data.frame of observed mother-offspring pairs with columns year / parent_age / offspring_age / mo_pairs / n_comparisons
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
    Integer order that actions will be run within model, see \code{\link{g3_action_order}}.
  }
}

\details{
  Implementation of CKMR based on \cite{Bravington, M.V., Skaug, H.J., & Anderson, E.C. (2016). Close-Kin Mark-Recapture. Statistical Science, 31, 259-274.}

  Only one kinship probability is implemented, mother-offspring with lethal
  sampling, i.e. (3.2) in the paper. This is then used as a pseudo-likelihood
  as per (4.1).

  \subsection{obs_data}{
    The \var{obs_data} data.frame provides observed pairs. Unlike other likelihood
    methods, it has a fixed structure:

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
        Number of mother-offspring pairs observed with these ages.
      }
      \item{n_comparisons}{
        Total number of comparisons made to find \var{mo_pairs}.
      }
    }
  }
}

\seealso{
  \cite{Bravington, M.V., Skaug, H.J., & Anderson, E.C. (2016). Close-Kin Mark-Recapture. Statistical Science, 31, 259-274.}
  \code{\link{g3_stock}}
}

\value{
  \subsection{g3l_ckmr_po}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{During each spawning step, for all \var{parent_stocks}, accumulate total parent numbers into
    \var{modelhist}__num and total spawned offspring into \var{modelhist}__spawned by year and age}
    \item{For any observed pairs in that year, compute the Poisson pseudo-likelihood of the observed
    mother-offspring pairs and add to \code{nll}}
  }}
}

\examples{
stocks <- list(
  mat = g3_stock(c("cod", "mat"), seq(20, 100, 10)) |> g3s_age(3, 10),
  imm = g3_stock(c("cod", "imm"), seq(5, 40, 5)) |> g3s_age(0, 2)
)

# Observed mother-offspring pairs: year / parent_age / offspring_age / mo_pairs / n_comparisons
obs_data <- data.frame(
    year          = c(2000L, 2001L),
    parent_age    = c(   6L,    7L),
    offspring_age = c(   1L,    1L),
    mo_pairs      = c(   3L,    1L),
    n_comparisons = c( 500L,  500L))

actions <- list(
    g3a_time(2000, 2002, c(6, 6), project_years = 0),
    g3a_initialconditions_normalcv(stocks$mat),
    g3a_initialconditions_normalcv(stocks$imm),
    g3a_age(stocks$mat),
    g3a_age(stocks$imm),
    g3a_spawn(
        stocks$mat,
        recruitment_f = g3a_spawn_recruitment_fecundity(
            p0 = 1, p1 = 0, p2 = 0, p3 = 1, p4 = 0),
        output_stocks = list(stocks$imm),
        run_step = 1),
    g3l_ckmr_po("ckmr", obs_data,
        parent_stocks = list(stocks$mat),
        offspring_stocks = list(stocks$imm)))
}
