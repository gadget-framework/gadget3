\name{stock_areas}
\alias{g3_areas}
\alias{g3s_livesonareas}
\alias{g3s_areagroup}
\concept{G3 stock}

\title{Gadget3 stock area dimensions}
\description{
  Add area dimensions to g3_stock classes
}

\usage{
g3_areas(area_names)

g3s_livesonareas(inner_stock, areas)

g3s_areagroup(inner_stock, areagroups)

}

\arguments{
  \item{area_names}{
    A character vector of area names to use in the model
  }
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

\details{
  \code{g3s_livesonareas} breaks up a stock by area.
  Within a model, areas are only referred to by integer, however if these are named
  then that name will be used for report output.

  Each area will be defined as a variable in your model as \code{area_x},
  allowing you to use names in formulas, e.g. \code{run_f = quote( area == area_x )}.

  \code{g3_areas} is a helper to map a set of names to an integer

  Inside a model each area will only be referred to by integer.

  \code{g3s_areagroup} allows areas to be combined, this is mostly used internally
  by \code{\link{g3l_catchdistribution}}.
}

\value{
  \subsection{g3_areas}{
      A named integer vector, assigning each of \var{area_names} a number.
  }
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
area_names <- g3_areas(c('a', 'b', 'c', 'd', 'e'))
stopifnot(area_names == c(a=1, b=2, c=3, d=4, e=5))

# Define a stock with 3 lengthgroups and 3 areas
stock <- g3_stock('name', c(1, 10, 100)) \%>\%
    g3s_livesonareas(area_names[c('a', 'b', 'c')])

# Area variables will be defined, so you can refer to them in formulas:
g3a_migrate(stock, g3_parameterized("migrate_spring"),
    run_f = ~area == area_b && cur_step == 2)

# Use stock_instance to see what the array would look like
g3_stock_instance(stock)

# Define a stock that groups areas into "north" and "south"
stock <- g3_stock('name', c(1, 10, 100)) \%>\%
    g3s_areagroup(list(
        north = area_names[c('a', 'b', 'c')],
        south = area_names[c('d', 'e')]))

# Use stock_instance to see what the array would look like
g3_stock_instance(stock)
}
