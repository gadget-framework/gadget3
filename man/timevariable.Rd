\name{timevariable}
\alias{g3_timevariable}

\title{Gadget3 time-based formulas}
\description{
  Switch formula based on current time step
}

\usage{
g3_timevariable(lookup_name, fs)

}

\arguments{
  \item{lookup_name}{
    A unique name for this lookup, e.g. \code{"igfs_landings"}.
  }
  \item{fs}{
    A list of \link{formula} objects, named with either "init", "(year)" or
    "(year)-(step)".
    When the matching time step is reached, the value of the lookup will be changed.
  }
}

\details{
  This is mostly for backwards compatibility with gadget2, before using this,
  consider other simpler options, e.g. \code{\link{g3_timeareadata}} or the
  \var{by_year} option in \code{\link{g3_parameterized}}.
}

\value{
  A \link{formula} object that will switch values at the given time points.
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)

naturalmortality_action <- g3a_naturalmortality(ling_imm,
    g3a_naturalmortality_exp( g3_timevariable("lingimm.M", list(
        # Start off using lingimm.M.early
        "init" = g3_parameterized("lingimm.M.early"),
        # At 2005 step 2, switch to lingimm.M.mid
        "2005-02" = g3_parameterized("lingimm.M.mid"),
        # At 2010 step 1, switch to lingimm.M.late
        "2010" = g3_parameterized("lingimm.M.late")))))
}
