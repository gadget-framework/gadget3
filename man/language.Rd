\name{language}
\alias{g3_idx}
\alias{g3_param}
\alias{g3_param_vector}
\alias{g3_report}

\title{G3 language extensions to R}
\description{
  Additional meta-functions available for use in G3 formula.
}

\details{
  Whilst used as functions, these functions alter the code output of the model,
  rather than appearing directly.
}

\section{g3_idx}{
  Adds a \code{- 1} to a given expression C++, which has 0-based indexes.

  \emph{Note:} This is generally for internal use, as \code{[[} will do this
  automatically for you.

  For example, \code{g3_idx(a)} will be replaced with \code{a} in R output
  and \code{a - 1} in C++ output.
}

\section{g3_param}{
  Reference a scalar parameter by name.

  For example, \code{g3_param("ling.Linf")} will register a scalar parameter
  called \var{ling.Linf}, available in the model parameter template, and be
  replaced by a reference to that parameter.

  \code{g3_param("ling.Linf")} can be used multiple times, to refer to the same
  value.  
}

\section{g3_param_vector}{
  Reference a vector parameter by name.

  Same as \code{g3_param}, but the parameter will be expected to be a vector.
  You can then dereference with \code{[[}.

  For example, \code{g3_param_vector("lingimm.M")[[age - 3 + 1]]}.
}

\section{g3_report}{
  Add a variable to the report output of the model.

  \emph{Note:} This is generally for internal use, there are higher-level
  mechanisms such as \code{\link{g3a_report_stock}}.

  For example, \code{g3_report(ling_imm__num)} will ensure that the
  \code{ling_imm__num} variable is available in the report output of the model.
}
