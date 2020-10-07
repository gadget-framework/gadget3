\name{stock_areas}
\alias{g3s_livesonareas}
\alias{g3s_areagroup}

\title{Gadget3 stock area dimensions}
\description{
  Add area dimensions to g3_stock classes
}

\usage{
g3s_livesonareas(inner_stock, areas)

g3s_areagroup(inner_stock, areagroups)

}

\arguments{
  \item{inner_stock}{
    A \code{\link{g3_stock}} that we extend with an area dimension
  }
  \item{areas}{
    A vector of numeric areas that the stock is part of
  }
  \item{areagroups}{
    A list mapping names to vectors of numeric areas the stock is part of
  }
}

\value{
  \subsection{g3s_livesonareas}{
      A \code{\link{g3_stock}} with an additional 'area' dimension.

      When iterating over the stock, iterate over each area in turn,
      \var{area} will be set to the current integer area.

      When intersecting with another stock, only do anything if
      \var{area} is also part of our list of areas.
  }
  \subsection{g3s_areagroup}{
      A \code{\link{g3_stock}} with an additional 'area' dimension.

      When iterating over the stock, iterate over each areagroup in turn,
      \var{area} will be set to the first area in the group.

      When intersecting with another stock, only do anything if
      \var{area} is part of one of the groups.
  }
}

\examples{
\dontshow{library(magrittr)}

# Make a lookup so we can refer to areas by name
areas <- c(a=1, b=2, c=3, d=4, e=5)

# Define a stock with 3 lengthgroups and 3 areas
stock <- g3_stock('name', c(1, 10, 100)) \%>\%
    g3s_livesonareas(areas[c('a', 'b', 'c')])

# Use stock_instance to see what the array would look like
gadget3:::stock_instance(stock)

# Define a stock that groups areas into "north" and "south"
stock <- g3_stock('name', c(1, 10, 100)) \%>\%
    g3s_areagroup(list(
        north = areas[c('a', 'b', 'c')],
        south = areas[c('d', 'e')]))

# Use stock_instance to see what the array would look like
gadget3:::stock_instance(stock)
}
