\name{action_report}
\alias{g3a_report_stock}
\alias{g3a_report_history}

\title{Gadget3 report actions}
\description{
  Add report to a g3 model
}

\usage{
g3a_report_stock(report_stock, input_stock, report_f, run_f = TRUE, 
    run_at = 11)

g3a_report_history(actions, var_re = "__num$|__wgt$", run_at = 11)
}

\arguments{
  \item{report_stock}{
    The \code{\link{g3_stock}} to aggregate into
  }
  \item{input_stock}{
    The \code{\link{g3_stock}} that will be aggregated
  }
  \item{report_f}{
    \link{formula} specifying what to collect, for instance \code{~stock_ss(input_stock__num)} or \code{~stock_ss(input_stock__wgt)}.
  }
  \item{actions}{
    List of actions that model will consist of.
  }
  \item{var_re}{
    Regular expression specifying variables to log history for.
  }
  \item{run_f}{
    \link{formula} specifying a condition for running this action, default always runs.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
  }
}

\details{
  The actions will define the following variables in your model:
  \describe{
    \item{\var{report_stock}__\var{instance_name}}{
        Results of collating \var{input_stock}__\var{instance_name}, where
        \var{instance_name} is defined by the first instance variable in \var{report_f}.
        For example, if \var{report_f} is \code{~input_stock__num}, then we will report \code{report_stock__num}.
    }
  }

  A model can have any number of \code{g3a_report_*} actions, so long as the
  calling arguments are different. For instance, \code{run_f = ~age == 5} and
  \code{run_f = ~age == 7}.
}

\value{
  \subsection{g3a_report_stock}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{Iterate over \var{input_stock}, collecting data into \var{report_stock}}
    \item{Add the contents of \var{report_stock}__\var{instance_name} to the model report}
  }}
  \subsection{g3a_report_history}{
    An action (i.e. list of formula objects) that will store the current state of each variable found matching \var{var_re}.
  }
}

\seealso{
  \code{\link{g3_stock}}
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)

# Report that aggregates ages together
agg_report <- g3_stock('agg_report', c(1)) \%>\%
    g3s_agegroup(list(young = 1:3, old = 4:5)) \%>\%
    g3s_time(year = 2000:2002)
# Generate dissaggregated report by cloning the source stock, adding time
raw_report <- g3s_clone(ling_imm, 'raw_report') \%>\%
    g3s_time(year = 2000:2002)

actions <- list(
    g3a_report_stock(agg_report, ling_imm, ~stock_ss(ling_imm__num)),
    g3a_report_stock(raw_report, ling_imm, ~stock_ss(ling_imm__num)))
# "raw_report__num" and "agg_report__num" will be available in the model report

# Report history of all "__num" and "__wgt" variables
actions <- c(actions, list(g3a_report_history(actions)))

# Report history of just "ling_imm__num"
actions <- c(actions, list(g3a_report_history(actions, "^ling_imm__num$")))
}
