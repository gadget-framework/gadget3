\name{stock}
\alias{g3_stock_def}
\alias{g3_stock_instance}
\alias{g3_stock}
\alias{g3_fleet}
\alias{g3s_clone}
\alias{g3_is_stock}
\concept{G3 stock}

\title{Gadget3 stock storage}
\description{
    Define multi-dimensional storage for use in models, mostly to contain state
    about stocks.
}

\usage{
g3_stock(var_name, lengthgroups, open_ended = TRUE)

g3_stock_instance(stock, init_value = NA, desc = "")

g3_fleet(var_name)

g3_stock_def(stock, name)

g3s_clone(inner_stock, var_name)

g3_is_stock(stock)

}

\arguments{
  \item{var_name}{
    Prefix used for all instance variables of this stock. Can have multiple
    parts that will be concatentated together, see example.
  }
  \item{lengthgroups}{
    Vector defining length groups, each entry defining the minimum value.
  }
  \item{open_ended}{
    If TRUE, final \var{lengthgroups} value defines a group \code{x:Inf}.
    If FALSE, final \var{lengthgroups} value is the upper bound for the previous group.
  }
  \item{inner_stock}{
    A \code{\link{g3_stock}} or \code{\link{g3_fleet}} object to clone.
  }
  \item{stock}{
    A \code{\link{g3_stock}} or \code{\link{g3_fleet}}.
  }
  \item{init_value}{
    Intially the array will be filled with this constant, e.g. \code{1}, \code{0} or \code{NaN}
  }
  \item{desc}{
    Description of the array that will be included in models
  }
  \item{name}{
    Name of definition to extract, e.g. \code{"minlen"}.
  }
}

\value{
  \subsection{g3_stock}{
    A \code{\link{g3_stock}} with length groups
  }
  \subsection{g3_stock_instance}{
    An array with dimensions matching the stock.
  }
  \subsection{g3_fleet}{
    A \code{\link{g3_stock}} without length groups
  }
  \subsection{g3s_clone}{
    A \code{\link{g3_stock}} with identical dimensions to \var{inner_stock} but with a new name.
  }
  \subsection{g3_is_stock}{
    \code{TRUE} iff \var{stock} is a \code{\link{g3_stock}} object.
  }
}

\examples{
\dontshow{library(magrittr)}

# Define a stock with 3 lengthgroups
stock <- g3_stock('name', c(1, 10, 100))

# Define a stock with a multi-part name. We can then dig out species name
stock <- g3_stock(c(species = 'ling', 'imm'), c(1, 10, 100))
stopifnot( stock$name == 'ling_imm' )
stopifnot( stock$name_parts[['species']] == 'ling' )

# Use stock_instance define storage for mean weight of stock,
# has dimensions matching what was defined above.
g3_stock_instance(stock, 1, "Mean weight")

# Retrieve the upperlen for the stock
g3_stock_def(stock, 'upperlen')

# Define a stock, not-open-ended. Now only 2 groups long
stock <- g3_stock('name', c(1, 10, 100), open_ended = FALSE)

# Use stock_instance to see what the array would look like
g3_stock_instance(stock)

# Fleets don't have lengthgroups
stock <- g3_fleet('name') \%>\% g3s_livesonareas(1)

# Use stock_instance to see what the array would look like
g3_stock_instance(stock)
}
