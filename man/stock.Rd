\name{stock}
\alias{g3_stock}
\alias{g3_fleet}
\alias{g3s_clone}

\title{Gadget3 stock storage}
\description{
    Define multi-dimensional storage for use in models, mostly to contain state
    about stocks.
}

\usage{
g3_stock(var_name, lengthgroups, open_ended = TRUE)

g3_fleet(var_name)

g3s_clone(inner_stock, var_name)

}

\arguments{
  \item{var_name}{
    Prefix used for all instance variables of this stock.
  }
  \item{lengthgroups}{
    Vector defining length groups, each entry defining the minimum value.
  }
  \item{open_ended}{
    If TRUE, final \var{lengthgroups} value defines a group \code{x:Inf}.
    If FALSE, final \var{lengthgroups} value is the upper bound for the previous group.
  }
  \item{inner_stock}{
    A \code{\link{g3_stock}} object to clone.
  }
}

\value{
  \subsection{g3_stock}{
    A \code{\link{g3_stock}} with length groups
  }
  \subsection{g3_fleet}{
    A \code{\link{g3_stock}} without length groups
  }
  \subsection{g3s_clone}{
    A \code{\link{g3_stock}} with identical dimensions to \var{inner_stock} but with a new name.
  }
}

\examples{
\dontshow{library(magrittr)}

# Define a stock with 3 lengthgroups
stock <- g3_stock('name', c(1, 10, 100))

# Use stock_instance to see what the array would look like
gadget3:::stock_instance(stock)

# Define a stock, not-open-ended. Now only 2 groups long
stock <- g3_stock('name', c(1, 10, 100), open_ended = FALSE)

# Use stock_instance to see what the array would look like
gadget3:::stock_instance(stock)

# Fleets don't have lengthgroups
stock <- g3_fleet('name') \%>\% g3s_livesonareas(1)

# Use stock_instance to see what the array would look like
gadget3:::stock_instance(stock)
}
