\name{deprecated}
\alias{g3a_renewal_vonb}

\title{Deprecated gadget3 functions}
\description{
  These functions are deprecated, and will be removed in a future version.
}

\usage{
g3a_renewal_vonb(
        Linf = g3_parameterized('Linf', value = 1, by_stock = by_stock),
        K = g3_parameterized('K', value = 1, by_stock = by_stock),
        recl = g3_parameterized('recl', by_stock = by_stock),
        recage = g3_parameterized('recage', by_stock = FALSE, optimise = FALSE),
        by_stock = TRUE)
}

\arguments{
  \item{Linf,K,recl,recage,by_stock}{See \code{\link{g3a_renewal_vonb_recl}}}
}

\details{
  \describe{
    \item{\code{g3a_renewal_vonb}}{Use \code{\link{g3a_renewal_vonb_recl}} instead.}
  }
}
