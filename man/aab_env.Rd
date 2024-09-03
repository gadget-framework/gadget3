\name{aab_env}
\alias{g3_env}
\alias{ADREPORT}
\alias{as_integer}
\alias{as.numeric}
\alias{assert_msg}
\alias{avoid_zero}
\alias{avoid_zero_vec}
\alias{bounded}
\alias{bounded_vec}
\alias{g3_matrix_vec}
\alias{lgamma_vec}
\alias{logspace_add}
\alias{normalize_vec}
\alias{nvl}
\alias{print_array}
\alias{ratio_add_vec}
\alias{REPORT}
\alias{REprintf}
\alias{Rprintf}
\alias{nonconform_mult}
\alias{nonconform_add}
\alias{nonconform_div}
\alias{nonconform_divavz}
\concept{G3 internals}

\title{Gadget3 global environment}
\description{
  Functions available to any gadget3 model
}

\details{
  \code{g3_env} is the top-level \link{environment} that any gadget3 model uses,
  populated with utility functions.

  NB: Several functions have \code{_vec} variants.
  Due to TMB limitations these should be used when you have a vector not scalar input.
}

\section{ADREPORT}{TMB's \code{ADREPORT} function. See \link[TMB:sdreport]{sdreport} documentation}

\section{as_integer}{C++ compatible equivalent to \code{\link{as.integer}}}

\section{as.numeric}{R \code{\link{as.numeric}} or TMB \code{asDouble}}

\section{assert_msg}{
  C++/R function that ensures expression is true, or stops model.

  \preformatted{assert_msg(x > 0, "x must be positive")}
}

\section{avoid_zero}{
  Adds small value to input to ensure output is never zero
  \code{avoid_zero_vec} is identical to \code{avoid_zero}, and is only present for backward compatibility.
}

\section{bounded / bounded_vec}{
  Ensures \var{x} is within limits \var{b} & \var{a}.

  If \var{x} positive, return \var{a}.
  If \var{x} negative, \var{b}.
  If \var{x} \code{-10..10}, smoothly transition from \var{b} to \var{a}

  \preformatted{bounded_vec(x, 200, 100)}
}

\section{g3_matrix_vec}{
  Apply matrix transformation \var{tf} to vector \var{vec}, return resultant vector.

  \preformatted{g3_matrix_vec(tf, vec)}
}

\section{lgamma_vec}{Vector equivalent of \code{\link{lgamma}}}

\section{logspace_add}{
  TMB's \code{logspace_add}, essentially a differentiable version of \code{\link{pmax}}.
}

\section{normalize_vec}{
  Divide vector \var{a} by it's sum, i.e. so it now sums to 1
}

\section{nvl}{
  Return first non-null argument.
  NB: No C++ implementation.
}

\section{print_array}{
  Utility to pretty-print array \var{ar}
}

\section{ratio_add_vec}{
  Sum \var{orig_vec} & \var{new_vec} according to ratio of \var{orig_amount} & \var{new_amount}
}

\section{nonconform_add / nonconform_mult / nonconform_div / nonconform_div_avz}{
  Scalar sum/multiply/divide 2 non-conforming arrays,
  by treating the latter as a vector and repeating as many times as required.
  Results will be structured identically to the first array.

  \code{nonconform_div_avz(x, y)} is equivalent to \code{nonconform_div(x, avoid_zero_vec(y))}
}

\section{REPORT}{
  TMB's \code{REPORT} function.
}

\section{REprintf}{
  Equivalent of RCpp \href{https://teuder.github.io/rcpp4everyone_en/060_printing_massages.html#rprintf-reprintf}{REprintf}
}

\section{Rprintf}{
  Equivalent of RCpp \href{https://teuder.github.io/rcpp4everyone_en/060_printing_massages.html#rprintf-reprintf}{Rprintf}
}

\examples{
## avoid_zero
g3_eval(quote( c( avoid_zero(0), avoid_zero(10) ) ))
g3_eval(quote( avoid_zero(0:5) ))

## bounded / bounded_vec
curve(g3_eval(quote( bounded(x, 200, 100) ), x = x), -100, 100)

## logspace_add
curve(g3_eval(quote( logspace_add(x, 10) ), x = x), 0, 40)

## normalize_vec
g3_eval(quote( normalize_vec(c( 4, 4, 8, 2 )) ))

## nonconform_mult
g3_eval(quote( nonconform_mult(
    array(seq(0, 4*5*6), dim = c(4,5,6)),
    c(1e1, 1e2, 1e3, 1e4)) ))

## nonconform_div_avz
g3_eval(quote( nonconform_div_avz(
    array(seq(0, 4*5*6), dim = c(4,5,6)),
    c(1e1, 1e2, 0, 1e4)) ))
g3_eval(quote( nonconform_div(
    array(seq(0, 4*5*6), dim = c(4,5,6)),
    avoid_zero(c(1e1, 1e2, 0, 1e4))) ))
}
