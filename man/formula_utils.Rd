\name{formula_utils}
\alias{g3_formula}

\title{Gadget3 formula helpers}
\description{
  Tools to create R formulas
}

\usage{
g3_formula(code, ...)

}

\arguments{
  \item{code}{
    Unevaluated code to be turned into a formula
  }
  \item{...}{
    Items to add to the formula's environment
  }
}

\details{
  When using \code{~}, the local environment is attached to the code.
  This can leak unwanted variables into a model. This allows you to avoid
  the problem without resorting to \link{local}.
}

\value{
  A \link{formula} object, with environment created from \var{...}.
  Can then be used anywhere in gadget3 that accepts a \link{formula}.
}

\examples{

# g3_formula is identical to defining a formula within local():
stopifnot(all.equal(
    g3_formula(x + 1, z = 44),
    local({ z = 44; ~x + 1 })
    ))

# ... we can use these in g3a_grow_lengthvbsimple(), e.g.
g3a_grow_lengthvbsimple(
    linf_f = g3_formula( g3_param("ling.Linf") ),
    kappa_f = g3_formula( g3_param("ling.k") * k, k = 0.001))

}
