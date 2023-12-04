\name{run_desc}
\alias{g3_to_desc}
\concept{G3 compilation}

\title{Gadget3 actions into R code}
\description{
  Convert g3 actions into a character vector describing the model
}

\usage{
g3_to_desc(actions, minor_steps = FALSE)

}

\arguments{
  \item{actions}{
    A list of actions (i.e. list of formula objects), as produced by \var{g3a_}* functions.
  }
  \item{minor_steps}{
    Include minor steps (e.g. zeroing cumulative arrays)?  \code{TRUE} / \code{FALSE}
  }
}

\value{
  Character vector describing each step in the model. An action in a model may have generated
  multiple steps (e.g. select each prey stock, scale total amount, apply overstocking), and there
  will be a line in here for each.
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)

initialconditions_action <- g3a_initialconditions_normalparam(
    ling_imm,
    by_age = TRUE)

# Timekeeping action
time_action <- g3a_time(
    start_year = 2000,
    end_year = 2004,
    c(3, 3, 3, 3))

# Generate a list outlining the steps the model uses
as.list(g3_to_desc(list(initialconditions_action, time_action)))
}
