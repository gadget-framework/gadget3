\name{action_migrate}
\alias{g3a_migrate_normalize}
\alias{g3a_migrate}

\title{Gadget3 migration action}
\description{
  Add migration to a g3 model
}

\usage{
g3a_migrate_normalize(row_total = 1)

g3a_migrate(stock, migrate_f, normalize_f = g3a_migrate_normalize(),
            run_f = TRUE,
            run_at = g3_action_order$migrate)
}

\arguments{
  \item{row_total}{
    When calculating the proportion of individuals that will stay in place, use this total
    for what rows are expected to sum to.
  }
  \item{stock}{
    The \code{\link{g3_stock}} that will migrate in this action.
  }
  \item{migrate_f}{
    A formula describing the migration in terms of (source) \code{area} and \code{dest_area}.
  }
  \item{normalize_f}{
    Function to normalize a vector of possible destinations, to make sure fish
    aren't added or destroyed.
  }
  \item{run_f}{
    \link{formula} specifying a condition for running this action, default always runs.
  }
  \item{run_at}{
    Integer order that spawning actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
  }
}

\details{
  To restrict movement to a particular step in a year, or a particular area, use \var{run_f}.
  For example:
  \describe{
    \item{\code{cur_step == 1}}{Migration will happen on first step of every year}
    \item{\code{cur_step == 1 && cur_year >= 1990}}{Migration will happen on first step of every year after 1990}
    \item{\code{cur_step == 2 && area = 1}}{Migration will happen on second step of every year, in the first area}
  }

  Multiple migration actions can be added, for a separate spring and autumn migration, for instance.

  The action will define the following stock instance variables for each given \var{stock}:
  \describe{
      \item{\var{stock__migratematrix}}{\eqn{a \times a} array, containing proportion of (stock) moved from one area to another. If NaN, no movement has occurred}
  }
}

\value{
  \subsection{g3a_migrate_normalize}{
    A formula transforming \code{stock__migratematrix[,stock__area_idx]} (i.e.
    all possible destinations from a given area) by:

    \enumerate{
      \item{Squaring so values are all positive}
      \item{Altering the proportion of static individuals so a row sums to \var{row_total}}
      \item{Dividing by row_total so a row sums to 1}
    }
  }
  \subsection{g3a_migrate}{An action (i.e. list of formula objects) that will, for the given \var{stock}...\enumerate{
    \item{Fill in \var{stock__migratematrix} using \var{migrate_f} and \var{normalize_f}}
    \item{Apply movement to \var{stock}}
  }}
}

\seealso{
  \code{\link{g3_stock}}
}

\examples{
\dontshow{library(magrittr)}
areas <- list(a=1, b=2, c=3, d=4)

# NB: stock doesn't live in b, so won't figure in stock_acd__migratematrix
stock_acd <- (g3_stock('stock_acd', seq(10, 40, 10))
    \%>\% g3s_livesonareas(areas[c('a', 'c', 'd')]))

movement_action <- list(
    g3a_migrate(
        stock_acd,
        # In spring, individuals in area 'a' will migrate to 'd'.
        ~if (area == area_a && dest_area == area_d) 0.8 else 0,
        run_f = ~cur_step == 2),
    g3a_migrate(
        stock_acd,
        # In autumn, individuals in all areas will migrate to 'a'
        ~if (dest_area == area_a) 0.8 else 0,
        run_f = ~cur_step == 4),
    list())
}
