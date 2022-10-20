\name{params}
\alias{g3_parameterized}

\title{Gadget3 parameter helpers}
\description{
  Shortcuts to parameterise a model with g3_param
}

\usage{
g3_parameterized(
        name,
        by_stock = FALSE,
        by_year = FALSE,
        by_age = FALSE,
        exponentiate = FALSE,
        avoid_zero = FALSE,
        scale = 1,
        offset = 0,
        ...)
}

\arguments{
  \item{name}{
    Suffix for parameter name.
  }
  \item{by_stock}{
    Should there be individual parameters per-stock?
    \describe{
      \item{FALSE}{No}
      \item{TRUE}{Produce a \code{"stock_name.name"} parameter}
      \item{character vector}{Select the stock name_part(s) to use, e.g. to produce \code{"stock_species.name"} parameter with "species"}
      \item{List of \code{\link{g3_stock}} objects}{Produce a parameter that applies to all given stocks}
    }
  }
  \item{by_year}{
    Should there be individual parameters per model year?
    \describe{
      \item{FALSE}{No}
      \item{TRUE}{Produce a \code{"name.1998"} parameter for each year the model runs}
    }
  }
  \item{by_age}{
    Should there be individual parameters per stock age?
    \describe{
      \item{FALSE}{No}
      \item{TRUE}{Produce a \code{"name.4"} parameter for each age of the stock(s) in \var{by_stock}}
    }
  }
  \item{exponentiate}{Use \code{exp(value)} instead of the raw parameter value. Will add "_exp" to the parameter name.}
  \item{avoid_zero}{If TRUE, wrap parameter with \code{avoid_zero}}
  \item{scale}{
    Use \code{scale * value} instead of the raw parameter value.
    Either a numeric constant or character.
    If character, add another parameter for scale, using the same \var{by_stock} value.
  }
  \item{offset}{
    Use \code{value + offset} instead of the raw parameter value
    Either a numeric constant or character.
    If character, add another parameter for offset, using the same \var{by_stock} value.
  }
  \item{...}{Additional parameters passed through to \code{\link{g3_param}}, e.g. \var{optimise}, \var{random}, ...}
}

\details{
  The function provides shortcuts to common formulas used when parameterising a model.
}

\value{A \link{formula} object defining the given parameters}
  
\seealso{
  \code{\link{g3_param}},
  \code{\link{g3_param_table}},
  \code{\link{stock_param}},
  \code{\link{stock_param_table}}
}

\examples{
\dontshow{library(magrittr)}
stock_a <- g3_stock(c(species = 'stock', 'aaa'), seq(10, 35, 5)) \%>\% g3s_age(1, 10)
stock_b <- g3_stock(c(species = 'stock', 'bbb'), seq(10, 35, 5)) \%>\% g3s_age(1, 10)

# Not by anything, so just a regular parameter
g3_parameterized('K')

# by_stock, so will use stock_param() to rename variables
g3_parameterized('K', by_stock = TRUE)

# Adding by_year or by_age turns it into a table
g3_parameterized('K', by_stock = TRUE, by_year = TRUE, by_age = TRUE)

# Can specify the name parts you want
g3_parameterized('K', by_stock = 'species', by_year = TRUE)

# Can give a list of stocks, in which case it works out name parts for you
g3_parameterized('K', by_stock = list(stock_a, stock_b))
g3_parameterized('K', by_stock = list(stock_a, stock_b), by_age = TRUE)
}
