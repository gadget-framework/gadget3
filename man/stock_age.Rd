\name{stock_age}
\alias{g3s_age}
\alias{g3s_agegroup}
\concept{G3 stock}

\title{Gadget3 stock age dimensions}
\description{
  Add age dimensions to g3_stock classes
}

\usage{
g3s_age(inner_stock, minage, maxage)

g3s_agegroup(inner_stock, agegroups)

}

\arguments{
  \item{inner_stock}{
    A \code{\link{g3_stock}} that we extend with an age dimension
  }
  \item{minage}{
    Minimum age to store, integer.
  }
  \item{maxage}{
    Maximum age to store, integer.
  }
  \item{agegroups}{
    (optionally named) list of vectors of ages, grouping them together.
  }
}

\value{
  \subsection{g3s_age}{
      A \code{\link{g3_stock}} with an additional 'age' dimension.

      When iterating over the stock, iterate over each age in turn,
      \var{age} will be set to the current integer age.

      When intersecting with another stock, only do anything if
      \var{age} is betweem \var{minage} and \var{maxage}.

      If an age dimension already exists, it is redefined with new
      parameters.
  }
  \subsection{g3s_agegroup}{
      A \code{\link{g3_stock}} with an additional 'age' dimension.

      When iterating over the stock, iterate over each agegroup in turn,
      \var{age} will be set to the first age in the group.

      When intersecting with another stock, only do anything if
      \var{age} is part of one of the groups.
  }
}

\examples{
\dontshow{library(magrittr)}

# Define a stock with 3 lengthgroups and 3 ages
stock <- g3_stock('name', c(1, 10, 100)) \%>\%
    g3s_age(5, 10)

# Use stock_instance to see what the array would look like
g3_stock_instance(stock)

# Define a stock that groups age into "young" and "old"
stock <- g3_stock('name', c(1, 10, 100)) \%>\%
    g3s_agegroup(list(
        young = 5:7,
        old = 8:10))

# Use stock_instance to see what the array would look like
g3_stock_instance(stock)
}
