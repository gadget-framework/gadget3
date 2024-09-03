\name{env_dif}
\alias{dif_pmax}
\alias{dif_pmin}
\alias{dif_pminmax}
\concept{G3 internals}

\title{g3 env: differentiable functions}
\description{
  Differentiable helper functions available to any gadget3 model
}

\details{
  These functions are part of \code{g3_env} is the top-level \link{environment} that any gadget3 model uses.
}

\section{dif_pmax}{
  \preformatted{dif_pmax(a, b, scale)}

  Returns the maximum of \var{a} & \var{b}.
  If \var{a} is a vector/array, then all members of \var{a} are compared against \var{b}.
  If \var{b} is also a vector, then all members of \var{a} are compared against the matching member of \var{b} (repeating \var{b} if necessary).

  \var{scale} influences the sharpness of inflection points,
  should be about 1e5, depending on ranges of input values.
}

\section{dif_pmin}{
  \preformatted{dif_pmin(a, b, scale)}

  Returns the minimum of \var{a} & \var{b}, otherwise works like \code{dif_pmax}.

  \var{scale} influences the sharpness of inflection points,
  should be about 1e5, depending on ranges of input values.
}

\section{dif_pminmax}{
  \preformatted{dif_pminmax(a, lower, upper, scale)}

  Returns values of \var{a} bounded between \var{lower} & \var{upper}.

  \var{scale} influences the sharpness of inflection points at \var{lower} & \var{upper},
  should be about 1e5, depending on ranges of input values.
}

\examples{
## dif_pmax
g3_eval(quote( dif_pmax(1:10, 5, 1e5) ))
g3_eval(quote( dif_pmax(1:10, c(4, 7), 1e5) ))
g3_eval(quote( dif_pmax(array(1:9, dim = c(3,3)), c(3,6,8), 1e5) ))

## dif_pmin
g3_eval(quote( dif_pmin(1:10, 5, 1e5) ))
g3_eval(quote( dif_pmin(1:10, c(4, 7), 1e5) ))

## dif_pminmax
g3_eval(quote( dif_pminmax(1:10, 3, 6, 1e5) ))

}
