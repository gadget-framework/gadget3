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
  Adds a \code{- 1} to the supplied expression, but only in C++ (which has 0-based
  indexes). Under R the expression is passed through unchanged.

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

\section{g3_param_table}{
  Reference a lookup-table of parameters.

  This is similar to providing a vector, but can use values in the model to
  provide bounds-checking.

  The function takes 2 arguments, a prefix for the generated parameters, and a
  data.frame of variables to possible values. \code{\link{expand.grid}} can be
  used to produce a cross product of all provided variables.

  \emph{Note:} The variables referenced will need to be integer variables, most
  likely iteration variables such as \code{cur_year}, \code{age},
  \code{area}...

  For example, the following: \code{g3_param_table('lingimm.M', expand.grid(age = seq(ling_imm__minage,
  ling_imm__maxage)))} will generate parameters
  \var{lingimm.M.3}..\var{lingimm.M.10}, assuming that ling_imm has ages 3..10.

  The call to \code{g3_param_table} will be replaced with
  \code{param[[paste("lingimm.M", age, sep = ".")]]}, or equivalent code
  in C++.
}

\section{g3_report}{
  Add a variable to the report output of the model.

  \emph{Note:} This is generally for internal use, there are higher-level
  mechanisms such as \code{\link{g3a_report_stock}}.

  For example, \code{g3_report(ling_imm__num)} will produce
  ``REPORT(ling_imm__num)`` under TMB, and ``attr(nll, 'ling_imm__num') <-
  ling_imm__num`` under R.

  Under R reported values can be got after running the function with
  \code{attributes(nll)}, where \code{nll} is the return value of the model
  function.
}
