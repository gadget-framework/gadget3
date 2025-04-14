\name{quota}
\alias{g3_quota_hockeyfleet}
\alias{g3_quota_assess}
\alias{g3_quota}
\concept{G3 projections}

\title{Gadget3 projected quotas}
\description{
  Add projected fishing quotas / MSE
}

\usage{
g3_quota_hockeyfleet(
        predstocks,
        preystocks,
        preyprop_fs = 1,
        btrigger = g3_parameterized("hf.btrigger", by_stock = predstocks),
        harvest_rate = g3_parameterized("hf.harvest_rate", by_stock = predstocks),
        stddev = g3_parameterized("hf.stddev", by_stock = predstocks, value = 0) )

g3_quota_assess(
        predstocks,
        preystocks,
        assess_f,
        unit = c("biomass-year", "biomass", "harvest-rate", "harvest-rate-year",
                 "individuals", "individuals-year") )

g3_quota(
        function_f,
        quota_name = attr(function_f, 'quota_name'),
        year_length = 1L,
        start_step = 1L,
        init_val = 0.0,
        run_revstep = -1,
        run_f = TRUE,
        run_at = g3_action_order$quota )
}

\arguments{
  \item{predstocks, preystocks}{
    Lists of \code{\link{g3_stock}} objects to represent the predators/fleets and prey when forming the quota.
  }
  \item{preyprop_fs}{
    A \code{\link{formula}} or list of formulas representing the proportion of that prey that makes up the Spawning Stock Biomass (SSB).
    The proportions do not need to sum to 1, for example you may use \code{preyprop_fs = 0.4} to assume that 40% of a single prey stock is mature.

    Using a suitability function is also supported, e.g. \code{\link{g3_suitability_exponentiall50}}.
  }
  \item{btrigger}{
    Trigger biomass, see formula
  }
  \item{harvest_rate}{
    Harvest rate, see formula
  }
  \item{stddev}{
    If > 0, then apply log-normal noise to the output quota.
  }
  \item{assess_f}{
    A formula that runs an assessment model & returns a quota. See vignette TODO:
  }
  \item{unit}{
    A single string representing the unit of the value that \var{assess_f} returns.
  }
  \item{function_f}{
    Output of one of the \code{g3_quota_*} functions, responsible for choosing the next quota
  }
  \item{quota_name}{
    A name used to refer to the quota internally, by default a combination of the quota function and the stocks used.
  }
  \item{year_length}{
    The length of the \emph{fishing year}, in years, see details.
  }
  \item{start_step}{
    The initial step of the \emph{fishing year}, in model steps, see details.
    This can be used to offset the fishing year from the calendar year for, if your fishing year should run autumn--autum.
    It can also offset from the start of the model, if your model starts at 1998 and your fishing year should run 2000--2005.
  }
  \item{run_revstep}{
    A negative integer, defining which step in the \emph{fishing year} an assessment for next year is performed.
    If \code{NULL}, run every step.
  }
  \item{run_f}{
    When the quota should be recalculated, in addition to any condition defined by \var{run_revstep}.
  }
  \item{init_val}{
    Numeric scalar that the quota array is initalised to.
    Will be used if the quota used before it is calculated, e.g. at model start.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see \code{\link{g3_action_order}}.
  }
}

\details{
  \subsection{Variables defined in your model}{
    Once added the following variables can be reported:

    \describe{
      \item{quota_\var{quota_name}__var}{A vector of quota values, one per \emph{fishing year} in your model, see \code{quota_hockeyfleet_fl__var} in example below}
    }
  }

  \subsection{Fishing year}{
    Instead of generating a quota per calendar year or step, as we do with other projections, quotas are per fishing year.

    The schedule of the fishing calendar is defined with:

    \describe{
      \item{year_length}{The length of the fishing year, in years. If > 1, the same yearly quota will be used for all years}
      \item{start_step}{Offset of the initial fishing year, in model steps. So you can both start your fishing year in autumn, and have a fishing year that is offset against the model start year}
      \item{run_revstep}{The step in the fishing year that the quota for the next year should be calculated}
    }

    In addition, \code{\link{g3a_predate_catchability_project}} will allow you to assign proportions of a quota to model steps.

    Examples:

    \describe{
      \item{\code{year_length = 2, start_step = 3, run_revstep = -2}}{
        A 2 year fishing calendar, i.e. a quota will be calculated every other year and the same value used for the next 2 years.
        Assuming 4 model steps, the quota will be calculated in winter for the next fishing year in summer.
      }
      \item{\code{year_length = 5, start_step = 4 * 2}}{
        A 5 year fishing calendar, the quota will be recalulated every 5 years in the autumn (the final step before the next fishing year starts).
        If our model starts at 1998 and have 4 steps, \code{4 * 2} means our first full fishing year is 2000--2005.
      }
    }

  }
}

\seealso{
  \code{\link{g3a_predate_catchability_project}}
}

\value{
  \subsection{g3_quota_hockeyfleet}{
    A formula for use in \code{\link{g3_quota}}

    \deqn{
      H (\sum_{predators}{\sum_{preys}{S_{p,p} P_p}}) {minmax}(\sum_{preys}\frac{B_p}{T}, 0, 1)
    }
    \describe{
      \item{\eqn{H}}{Harvest rate, provided by \var{harvest_rate} argument, by default the \code{hf.harvest_rate} parameter}
      \item{\eqn{T}}{Trigger biomass, provided by \var{btrigger} argument, by default the \code{hf.btrigger} parameter}
      \item{\eqn{P_p}}{Prey proportion, provided by \var{preyprop_fs} (i.e. the proportion of the prey stock that can be considered part of the spawning stock)}
      \item{\eqn{S_{p, p}}}{Total suitable biomass for predator/prey combination}
      \item{\eqn{B_p}}{Total abundance of prey in biomass}
    }
  }
  \subsection{g3_quota_assess}{A formula for use in \code{\link{g3_quota}} }
  \subsection{g3_quota}{A formula for use in \code{\link{g3a_predate_catchability_project}} }
}

\examples{
st <- g3_stock("st", c(10))
fl <- g3_fleet('fl')

# Define quota for the fleet, with an assessment in spring, application in autumn
fl_quota <- g3_quota(
    g3_quota_hockeyfleet(list(fl), list(st), preyprop_fs = 1),
    start_step = 4L,
    run_revstep = -2L )

actions <- list(
    g3a_time(1990, 1995, c(3,3,3,3)),
    # Define st with steadily collapsing stock
    g3a_otherfood(st, num_f = g3_timeareadata('st_abund', data.frame(
        year = 1990:2050,
        abund = 1e6 - 1e4 * seq(0, 2050-1990)), "abund"), wgt_f = 10),
    # Fleet predates stock
    g3a_predate(
        fl,
        list(st),
        suitabilities = 0.8,
        catchability_f = g3a_predate_catchability_project(
            # Use the quota when projecting, otherwise use landings parameters
            fl_quota,
            g3_parameterized("landings", value = 0, by_year = TRUE, by_predator = TRUE) )),
    NULL )
model_fn <- g3_to_r(c(actions,
    g3a_report_detail(actions),
    g3a_report_history(actions, "__num$|__wgt$", out_prefix="dend_"),  # NB: Late reporting
    g3a_report_history(actions, "quota_", out_prefix = NULL) ))

attr(model_fn, "parameter_template") |>
    # Project for 30 years
    g3_init_val("project_years", 30) |>
    # Fishing generally occurs in spring/summer, none in winter
    g3_init_val("fl.quota.step.#", c(0.0, 0.5, 0.4, 0.1)) |>
    # Initial landings fixed
    g3_init_val("fl.landings.#", 1e6) |>
    # Hockefleet: harvest rate & trigger biomass
    g3_init_val("fl.hf.harvest_rate", 0.2) |>
    g3_init_val("fl.hf.btrigger", 7.2e6) |>
    identity() -> params.in
r <- attributes(model_fn(params.in))

## Total biomass at assessment step, with btrigger marked
barplot(g3_array_agg(r$dend_st__num * r$dend_st__wgt, "year", step = 2), las = 2)
abline(h=7.2e6) ; abline(v=26.5)

## Quota values, inflection once total biomass falls below btrigger
par(mar = c(6, 5, 1, 0)) ; barplot(r$quota_hockeyfleet_fl__var, las = 2) ; abline(v=27.7)

## Consumption by fleet, demonstrating
##   (a) fixed landings before projections (fl.landings.#)
##   (b) inflection of hitting btrigger
##   (c) Uneven spread of fishing effort throughout year (fl.quota.step.#)
barplot(g3_array_agg(r$detail_st_fl__cons, "time"), las = 2)

}
