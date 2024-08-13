\name{action_weightloss}
\alias{g3a_weightloss}
\concept{G3 action}

\title{Gadget3 weightloss action}
\description{
  Add weight loss events to a g3 model
}

\usage{
g3a_weightloss(
        stock,
        rel_loss = NULL,
        abs_loss = NULL,
        min_weight = 1e-7,
        apply_to_pop = quote( stock__num ),
        run_f = TRUE,
        run_step = NULL,
        run_at = g3_action_order$naturalmortality )
}
  
\arguments{
  \item{stock}{
     The \code{\link{g3_stock}} that will lose weight in this action.
  }
  \item{rel_loss}{
    Fractional weight loss, \code{0.1} will result in the stock having 90% of the weight it did before.
    \code{NULL} means no fractional weight loss will be applied.
  }
  \item{abs_loss}{
    Absolute weight loss, applied after \var{rel_loss}.
    \code{NULL} means no absolute weight loss will be applied.
  }
  \item{min_weight}{
    Minimum weight below which weight cannot fall further.
    Should be more than zero to avoid models returning NaN.
  }
  \item{apply_to_pop}{
    Stock instance weightloss applies to, by default applies to whole stock.
    Used by \code{\link{g3a_spawn}} to apply to subset that spawned.
  }
  \item{run_step}{
    Which step to perform renewal in, or \code{NULL} for continuous spawning.
    Adds \code{cur_step == (run_step)} into default \var{run_f}.
  }
  \item{run_f}{
    \link{formula} specifying a condition for running this action, default always runs.
  }
  \item{run_at}{
    Integer order that spawning actions will be run within model, see \code{\link{g3_action_order}}.
  }
}

\value{
  \subsection{g3a_weightloss}{An action (i.e. list of formula objects) that will, for the given \var{stock}...\enumerate{
    \item{Apply \var{rel_loss} and \var{abs_loss} to the parent \var{stock}}
  }}
}

\seealso{
  \code{\link{g3a_naturalmortality}},
  \code{\link{g3a_spawn}},
  \code{\link{g3_stock}}
}

\examples{
  st <- g3_stock('st', 10:20) |> g3s_age(3, 5)

  actions <- list(
    g3a_time(2000, 2005, step_lengths = c(3, 3, 3, 3)),
    g3a_initialconditions(st,
        # Set initial abundance & weight based on age
        ~1e5 + 0 * st__midlen,
        ~1000 * age + 0 * st__midlen ),
    g3a_age(st),

    g3a_weightloss(st,
        # 20% of body weight should be shed in autumn
        rel_loss = 0.2,
        run_step = 4 ),
    NULL)

  model_fn <- g3_to_r(c(actions, g3a_report_detail(actions)))
  r <- model_fn(attr(model_fn, 'parameter_template'))
  attr(r, "detail_st__wgt")[1,,]

  ## See g3a_spawn for an example of weightloss in spawning
}
