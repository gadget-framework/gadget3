\name{param_project}
\alias{g3_param_project_dlnorm}
\alias{g3_param_project_dnorm}
\alias{g3_param_project_rwalk}
\alias{g3_param_project}
\concept{G3 projections}

\title{Gadget3 projected parameters}
\description{
  Add time-based random deviates / projections
}

\usage{
g3_param_project_dlnorm(
        lmean_f = 0,
        lstddev_f = 1e5 )

g3_param_project_dnorm(
        mean_f = 0,
        stddev_f = 1 )

g3_param_project_rwalk(
        mean_f = 0,
        stddev_f = 1 )

g3_param_project(
        param_name,
        project_fs = g3_param_project_rwalk(),
        by_step = TRUE,
        weight = g3_parameterized(
            paste("proj", project_fs$name, by_stock, param_name, "weight", sep = "_"),
            optimise = FALSE, value = 1),
        random = TRUE )
}

\arguments{
  \item{mean_f, stddev_f, lmean_f, lstddev_f}{
    mean / stddev in normal / logspace used for both the likelihood of deviates & to project future values.
  }
  \item{param_name}{
    Character string used to name the parameters.
  }
  \item{project_fs}{
    Results of either \code{\link{g3_param_project_dnorm}}, \code{\link{g3_param_project_rwalk}}.
  }
  \item{by_step}{
    Boolean, generate per-step or per-year values.
  }
  \item{weight}{
    A weighting to give to the likelhood when generating total nll.
  }
  \item{random}{
    Boolean, tell TMB to treat the deviates as random variables by default. Can be changed in the parameter template.
  }
}

\details{
  The actions will define the following variables in your model, which could be reported with \code{\link{g3a_report_history}}:
  \describe{
    \item{proj_(dnorm|rwalk)_(param_name)__var}{Vector of all values, both parameters & projected, by time}
    \item{proj_(dnorm|rwalk)_(param_name)__nll}{Likelihood of each value}
  }
}

\seealso{
  \code{\link{g3a_time}}
  \code{\link{g3_parameterized}}
}

\value{
  \subsection{g3_param_project_dnorm}{
    Returns a "nll" & "project" \link{formula} objects for use as \var{project_fs}.

    The parameter will describe deviates around a mean.

    \describe{
      \item{nll}{Compare values against \code{dnorm(x, mean_f, stddev_f)}}
      \item{proj}{Generate new values with \code{rnorm(mean_f, stddev_f)}}
    }
  }

  \subsection{g3_param_project_rwalk}{
    Returns a "nll" & "project" \link{formula} objects for use as \var{project_fs}.

    The parameter will describe a random walk.

    \describe{
      \item{nll}{Compare difference between values \code{dnorm(x, mean_f, stddev_f)}}
      \item{proj}{Generate new values with a delta of \code{rnorm(mean_f, stddev_f)}}
    }
  }
  
  \subsection{g3_param_project}{
    Returns a \link{formula} to choose the current value from the \code{__var} vector.

    An extra G3 action will:
    \enumerate{
      \item{Populate the array with random deviates from parameters (see examples)}
      \item{Project for any projection years (see \code{\link{g3a_time}})}
      \item{Add likelihood comparing random deviates to expected values}
    }
  }

  \subsection{g3l_sparsesample_sumsquares}{
    Returns a \link{formula} for use as \var{function_f}:

    \deqn{
      \sum_{\it i}^{rows} w (\frac{\nu_{i}}{P_{i}} - N_{i})^2
    }
    \describe{
      \item{\eqn{N_{i}}}{"mean" column from \var{obs_df}}
      \item{\eqn{\nu_{i}}}{Total predicted values, i.e. \var{nll_spabund_name__model_sum}}
      \item{\eqn{P_{i}}}{Number of data points, i.e. \var{nll_spabund_name__model_n}}
      \item{\eqn{w}}{\var{weighting} parameter, either:\enumerate{
        \item{\eqn{1 / \sigma^2}, using stddev of model predicted values if \code{weighting = "model_stddev"}}
        \item{\eqn{1 / \sigma^2}, using stddev column from \var{obs_df} if \code{weighting = "obs_stddev"}}
        \item{A custom forumla provided for \var{weighting}}
      }}
    }
  }
}

\examples{
st <- g3_stock("fish", c(10, 20, 30))

actions <- list(
    g3a_time(1990, 1994, c(6,6)),
    g3a_initialconditions(st,
        quote( 100 + stock__minlen ),
        quote( 1e4 + 0 * stock__minlen ) ),

    # Natural mortality with per-year random walk
    g3a_naturalmortality(st, g3a_naturalmortality_exp(
        g3_param_project("Mrw", g3_param_project_rwalk(
          mean_f = 0,
          stddev_f = 0.001 ), by_step = FALSE ))),

    # Natural mortality with per-step deviates
    g3a_naturalmortality(st, g3a_naturalmortality_exp(
        g3_param_project("Mdn", g3_param_project_dnorm(
          mean_f = g3_parameterized("Mdn_mean", value = 0.1, optimise = FALSE),
          stddev_f = g3_parameterized("Mdn_sd", value = 0.001, optimise = FALSE) )))),
    NULL )

model_fn <- g3_to_r(c(actions, list(
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    NULL )))

# Mdn has a parameter for each year/step, as well as mean/sd (added above) & likelihood weighting
grep("^Mdn", names(attr(model_fn, 'parameter_template')), value = TRUE)

# Mrw has parameters for each year
grep("^Mrw", names(attr(model_fn, 'parameter_template')), value = TRUE)

attr(model_fn, 'parameter_template') |>
    # Fill in initial values for each
    g3_init_val("Mdn.#.#", 0.5, lower = 0.1, upper = 0.9, random = TRUE) |>
    g3_init_val("Mrw.#", 0.5, lower = 0.1, upper = 0.9, random = TRUE) |>
    # Project forwards 20 years
    g3_init_val("project_years", 20) |>

    # Don't include projections in nll calculations:
    # allows a stddev to be supplied for projections, but estimated freely
    g3_init_val("proj_Mrw_weight", 0) |>
    g3_init_val("proj_Mdn_weight", 0) |>

    identity() -> params
r <- attributes(model_fn(params))

# Values used for dnorm
plot(r$proj_dnorm_Mdn__var)

# Values used for random walk
plot(r$proj_rwalk_Mrw__var)

}
