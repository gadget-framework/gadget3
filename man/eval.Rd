\name{eval}
\alias{g3_eval}

\title{Evaluate G3 forumulas}
\description{
  Evaluate G3 formulas / code outside a model
}

\usage{
g3_eval(f, ...)

}

\arguments{
  \item{f}{
    A \link{formula} object or \link{quote}d code to be evaluated
  }
  \item{...}{
    Named items to add to the formula's environment,
    or a single list / environment to use.
  }
}

\details{
  Allows snippets of gadget3 code to be run outside a model. This could
  be done with regular \code{\link{eval}}, however, \code{g3_eval} does a number of things first:

  \enumerate{
    \item The global \code{\link{g3_env}} is in the environment, so functions such as \code{\link{avoid_zero}} can be used
    \item If substituting a \code{\link{g3_stock}}, all definitions such as \code{stock__minlen} will also be substituted
  }
}

\value{
  Result of evaluating \var{f}.
}

\examples{
# Evaluate suitiability function for given stocks
g3_eval(
    g3_suitability_andersen(0,1,2,3,4),
    pred_stock = g3_stock('pred', 11:20),
    stock = g3_stock('prey', 1:10))

# Graph gadget3's built-in logspace_add()
if (interactive()) {
  curve(g3_eval(quote( logspace_add(a, 10) ), a = x), 0, 50)
}

}
