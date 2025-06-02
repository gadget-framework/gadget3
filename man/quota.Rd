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
        interim_value = NULL,
        run_revstep = -1,
        run_f = TRUE,
        run_at = g3_action_order$quota )
}

\arguments{
  \item{predstocks}{
    A list of \code{\link{g3_stock}} objects for all predators/fleets that will use the quota.
    In \code{\link{g3_quota_hockeyfleet}}, these will be used to name the quota/parameters.
    In \code{\link{g3_quota_assess}}, these will define helper variables for use in \var{function_f}.
  }
  \item{preystocks}{
    A list of \code{\link{g3_stock}} objects for all prey that the quota will apply to.
    In \code{\link{g3_quota_hockeyfleet}}, these will be used to define the spawning stock biomass.
    In \code{\link{g3_quota_assess}}, these will define helper variables for use in \var{function_f}.
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
  \item{interim_value}{
    A formula that provides the interim value to bridge the gap between historical landings & projected quota values, see examples.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see \code{\link{g3_action_order}}.
  }
}

\details{
  \subsection{Variables defined in your model}{
    Once added the following variables can be reported:

    \describe{
      \item{quota_\var{quota_name}__var}{A vector of quota values, one per \emph{fishing year} in your model, see \code{quota_hockeyfleet_trawl__var} in example below}
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

    \deqn{SSB = \sum^{preys}_{p}{S_p N_p W_p}}
    \deqn{H {min}(\frac{SSB}{B_{trig}}, 1)}
    \describe{
      \item{\eqn{H}}{Harvest rate, provided by \var{harvest_rate} argument, by default the \code{hf.harvest_rate} parameter}
      \item{\eqn{B_{trig}}}{Trigger biomass, provided by \var{btrigger} argument, by default the \code{hf.btrigger} parameter}
      \item{\eqn{SSB}}{Spawning Stock Biomass (SSB)}
      \item{\eqn{S_{p}}}{Suitable proportion of prey \eqn{p}, as decided by \var{preyprop_fs}}
      \item{\eqn{N_p}}{Total abundance of prey \eqn{p}}
      \item{\eqn{N_p}}{Mean weight of prey \eqn{p}}
    }
  }
  \subsection{g3_quota_assess}{A formula for use in \code{\link{g3_quota}} }
  \subsection{g3_quota}{A formula for use in \code{\link{g3a_predate_catchability_project}} }
}

\examples{
st <- g3_stock("st", c(10))
fleets <- list(
    # NB: We break down our fleet names into parts, g3_quota_hockeyfleet() will
    # use the common name part (read: trawl) when naming parameters.
    nor = g3_fleet(c(type = "trawl", country = "nor")),
    oth = g3_fleet(c(type = "trawl", country = "oth")) )

# Define quota for both fleets, with an assessment in spring, application in autumn
fl_quota <- g3_quota(
    g3_quota_hockeyfleet(fleets, list(st), preyprop_fs = 1),
    start_step = 4L,
    run_revstep = -2L )

# Invent some historical landings tables for the sake of example
landings_trawl_nor <- expand.grid(year = 1990:1995, step = 2)
landings_trawl_nor$total_weight <- 1e6
landings_trawl_oth <- expand.grid(year = 1990:1995, step = 2)
landings_trawl_oth$total_weight <- 1e8

actions <- list(
    g3a_time(1990, 1995, c(3,3,3,3)),
    # Define st with steadily collapsing stock
    g3a_otherfood(st, num_f = g3_timeareadata('st_abund', data.frame(
        year = 1990:2050,
        abund = 1e6 - 1e4 * seq(0, 2050-1990)), "abund"), wgt_f = 10),
    # Fleet predation, both sharing the same quota
    g3a_predate(
        fleets$nor,
        list(st),
        suitabilities = 0.8,
        catchability_f = g3a_predate_catchability_project(
            # Use the (shared) quota when projecting, otherwise use historical landings
            quota_f = fl_quota,
            landings_f = g3_timeareadata("landings_trawl_nor", landings_trawl_nor) )),
    g3a_predate(
        fleets$oth,
        list(st),
        suitabilities = 0.8,
        catchability_f = g3a_predate_catchability_project(
            # Use the (shared) quota when projecting, otherwise use historical landings
            quota_f = fl_quota,
            landings_f = g3_timeareadata("landings_trawl_oth", landings_trawl_oth) )),
    NULL )
model_fn <- g3_to_r(c(actions,
    g3a_report_detail(actions),
    g3a_report_history(actions, "__num$|__wgt$", out_prefix="dend_"),  # NB: Late reporting
    g3a_report_history(actions, "quota_", out_prefix = NULL) ))

attr(model_fn, "parameter_template") |>
    # Project for 30 years
    g3_init_val("project_years", 30) |>
    # Quota is yearly, so specify cons.step parameters to divide up into steps
    # Fishing predominantly occurs in spring/summer, none in winter
    g3_init_val("trawl_*.cons.step.#", c(0.0, 0.5, 0.4, 0.1)) |>
    # 3/4 of the quota goes to nor, the rest to oth
    g3_init_val("trawl_nor.quota.prop", 0.75) |>
    g3_init_val("trawl_oth.quota.prop", 0.25) |>
    # Hockefleet: harvest rate & trigger biomass (shared across trawl_nor & trawl_oth)
    g3_init_val("trawl.hf.harvest_rate", 0.2) |>
    g3_init_val("trawl.hf.btrigger", 7.2e6) |>
    identity() -> params.in
r <- attributes(model_fn(params.in))

## Quota values, inflection once total biomass falls below btrigger
par(mar = c(6, 5, 1, 0)) ; barplot(r$quota_hockeyfleet_trawl__var, las = 2) ; abline(v=27.7)

## Consumption by fleet, demonstrating
##   (a) fixed landings before projections (landings_trawl_nor)
##   (b) inflection of hitting btrigger
##   (c) Uneven spread of fishing effort throughout year (fl.quota.step.#)
barplot(g3_array_agg(r$detail_st_trawl_nor__cons, "time"), las = 2)

## Timing of calculations for fishing year
fl_quota <- g3_quota(
    # Our quota values are year/step at the assessment time step
    quote( cur_year * 10 + cur_step ),
    year_length = 1L,
    start_step = 4L,
    interim_value = g3_parameterized("interim_value", value = 99, optimize = FALSE),
    run_revstep = -3L )
yr <- as.integer(format(Sys.Date(), "\%Y"))
actions <- list(
    g3a_time(yr - 6, yr - 1, project_years = 10, step_lengths = rep(3L, 4)),
    fl_quota,
    # At each step in the model, print the current year/step, and the quota value that will get used
    # NB: before projection, g3a_predate_catchability_project() will use landings data not the quota
    g3_step(g3_formula(
        writeLines(paste(cur_year, cur_step, if (cur_year_projection) q else "landings")),
        q = fl_quota )),
    NULL)
model_fn <- g3_to_r(c(actions,
    g3a_report_history(actions, "quota_", out_prefix = NULL) ))
attr(model_fn(), "quota__var")

}
