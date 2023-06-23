\name{stock_time}
\alias{g3s_time_convert}
\alias{g3s_time}

\title{Gadget3 stock time dimensions}
\description{
  Add time dimensions to g3_stock classes
}

\usage{
g3s_time_convert(year, step = NULL)

g3s_time(inner_stock, time_data, year = NULL, step = NULL)

}

\arguments{
  \item{year}{
    Vector of years, used to generate \var{times} if provided.
  }
  \item{step}{
    Vector of steps, used to generate \var{times} if provided.
  }
  \item{inner_stock}{
    A \code{\link{g3_stock}} that we extend with a time dimension
  }
  \item{time_data}{
    A dataframe that contains a 'year' (required), 'step' (optional), and 'time' (optional) columns
  }
}

\value{
  \subsection{g3s_time_convert}{
      A single integer vector representing \var{year} and \var{step}.

      If \var{step} is \code{NULL}, returns \var{year}, otherwise \code{\var{year} * 1000 + \var{step}}.
  }
  \subsection{g3s_time}{
      A \code{\link{g3_stock}} with an additional 'time' dimension.

      If \var{year}/\var{step} provided, time is defined by those, otherwise time is calculated from \var{time_data}.

      The \code{\link{g3_stock}} will not support iterating,
      only intersecting.

      When intersecting with another stock, only do anything if
      \var{cur_year} and \var{cur_step} matches a time stored in the vector
  }
}

\examples{
\dontshow{library(magrittr)}

# Define a stock with 3 lengthgroups and 3 years, not continuous
# When used, all steps within a year will be aggregated, year 2002 will be ignored.
stock <- g3_stock('name', c(1, 10, 100)) \%>\%
    g3s_time(year = c(2000, 2001, 2003))

# Use stock_instance to see what the array would look like
g3_stock_instance(stock)

# Define a stock with 3 lengthgroups and 3 years, 2 steps
# The dimension will have 6 entries, 2000.1, 2000.2, 2001.1, 2001.2, 2002.1, 2002.2
stock <- g3_stock('name', c(1, 10, 100)) \%>\%
    g3s_time(year = c(2000, 2001, 2002), step = 1:2)

# Use stock_instance to see what the array would look like
g3_stock_instance(stock)

# g3s_time_convert is best used with a data.frame
data <- read.table(header = TRUE, text = '
year step
2001 1
2001 2
# NB: No "2002 1"
2002 2
')
data$time <- g3s_time_convert(data$year, data$step)
stock <- g3_stock('name', c(1, 10, 100)) \%>\%
    g3s_time(time_data = data)

# Use stock_instance to see what the array would look like
g3_stock_instance(stock)
}
