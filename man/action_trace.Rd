\name{action_trace}
\alias{g3a_trace_var}
\concept{G3 utilities}

\title{Trace value of variables in a G3 model}
\description{
  Trace value of variables in a G3 model, warning if conditions are met
}

\usage{
g3a_trace_var(
        actions,
        check_finite = TRUE,
        check_positive = FALSE,
        check_strictly_positive = FALSE,
        on_error = c("continue", "browser", "stop"),
        print_var = FALSE,
        var_re = c("__num$", "__wgt$"))
}

\arguments{
  \item{actions}{
    A list of model actions to add tracing to
  }
  \item{check_finite}{Boolean, notify if variable is not finite (i.e. Inf, NA, NaN)}
  \item{check_positive}{Boolean, notify if variable is < 0}
  \item{check_strictly_positive}{Boolean, notify if variable is <= 0}
  \item{on_error}{
    What to do when a variable fails one of the checks?
    NB: "browser" will not work in a TMB-compiled model.
  }
  \item{print_var}{
    Boolean, if true print the value of the variable at the point the test fails.
    NB: This will not work in a TMB-compiled model.
  }
  \item{var_re}{
    Regular expression(s), variable whose name matches will be traced.
  }
}

\details{
  The main reason to use \code{g3a_trace_var} is to find out why a model is producing NaN in reports / likelihood.
  Adding this to your model will help pinpoint the action this originally occurs in, so you can inspect closer for incorrect settings and/or bugs.
  
  \subsection{Suggested \var{var_re} settings}{
    The \var{var_re} parameter chooses which variables are traced, and should be tweaked to further pinpoint the problem.
    Generally, once an error has been found, dig into the code (e.g. by doing \code{edit(g3_to_r(actions))}),
    and see what other variables are available for tracing.
    Some pre-canned suggestions follow:

    \describe{
      \item{\code{c("__num$", "__wgt$") (i.e. default)}}{
        This will trace abundance/weight for all stocks, and a good starting point.
      }
      \item{\code{^[stock_name]__(num|wgt|cons|suit|totalpredate|consratio|feedinglevel)$}}{
        This will, once \code{[stock_name]} is replaced with the name of your stock,
        dig deeper into the predation mechanisms.
      }
    }
  }
}

\seealso{
  \code{\link{g3a_predate_catchability_project}}
}

\value{
  \subsection{g3_trace_var}{
    A list of actions that will report when variables stop being finite (e.g.)
  }
}

\examples{
stocks <- list(
    st = g3_stock("st", 1:10 * 10) |> g3s_age(1, 5) )

actions <- list(
    g3a_time(1990, 1995, c(3,3,3,3)),
    g3a_initialconditions_normalcv(stocks$st),
    g3a_growmature(stocks$st, impl_f = gadget3::g3a_grow_impl_bbinom(
        maxlengthgroupgrowth = 2L) ),

    NULL )
model_fn <- g3_to_r(c(actions,
    list(g3a_trace_var(actions)),
    g3a_report_detail(actions) ))

# Configure set of working parameters
attr(model_fn, "parameter_template") |>
    g3_init_val("*.K", 0.3) |>
    g3_init_val("*.t0", 0.2) |>
    g3_init_val("*.Linf", 80) |>
    g3_init_val("*.lencv", 0.1) |>
    g3_init_val("*.walpha", 0.01) |>
    g3_init_val("*.wbeta", 3) |>
    g3_init_val("*.M.#", 0.01) |>
    identity() -> params.in
nll <- model_fn(params.in) ; r <- attributes(nll) ; nll <- as.vector(nll)

# Try setting parameters to NaN and see what fails:
r <- model_fn(params.in |> g3_init_val("*.t0", NaN))
r <- model_fn(params.in |> g3_init_val("*.bbin", NaN))
}
