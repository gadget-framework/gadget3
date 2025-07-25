\name{param_project}
\alias{g3_param_project_dlnorm}
\alias{g3_param_project_dnorm}
\alias{g3_param_project_rwalk}
\alias{g3_param_project_ar1}
\alias{g3_param_project_logar1}
\alias{g3_param_project}
\concept{G3 projections}

\title{Gadget3 projected parameters}
\description{
  Add time-based random deviates / projections
}

\usage{
g3_param_project_dlnorm(
        lmean_f = g3_parameterized("proj.dlnorm.lmean",
            value = 0, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        lstddev_f = g3_parameterized("proj.dlnorm.lstddev",
            value = 1e5, optimise = FALSE,
            prepend_extra = quote(param_name) ))

g3_param_project_dnorm(
        mean_f = g3_parameterized("proj.dnorm.mean",
            value = 0, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        stddev_f = g3_parameterized("proj.dnorm.stddev",
            value = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ))

g3_param_project_rwalk(
        mean_f = g3_parameterized("proj.rwalk.mean",
            value = 0, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        stddev_f = g3_parameterized("proj.rwalk.stddev",
            value = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ))

g3_param_project_ar1(
        phi_f = g3_parameterized(
            "proj.ar1.phi",
            value = 0.8, lower = 0, upper = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        stddev_f = g3_parameterized(
            "proj.ar1.stddev",
            value = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        level_f = g3_parameterized(
            "proj.ar1.level",
            value = 0,
            prepend_extra = quote(param_name) ),
        lastx_f = 0L)

g3_param_project_logar1(
        logphi_f = g3_parameterized(
            "proj.logar1.logphi",
            value = 0.8, lower = 0, upper = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        lstddev_f = g3_parameterized(
            "proj.logar1.lstddev",
            value = 1, optimise = FALSE,
            prepend_extra = quote(param_name) ),
        loglevel_f = g3_parameterized(
            "proj.logar1.loglevel",
            value = 0,
            prepend_extra = quote(param_name) ),
        lastx_f = 0L)

g3_param_project(
        param_name,
        project_fs = g3_param_project_rwalk(),
        by_step = TRUE,
        by_stock = FALSE,
        weight = g3_parameterized(
            paste("proj", project_fs$name, param_name, "weight", sep = "_"),
            optimise = FALSE, value = 1),
        scale = 1,
        offset = 0,
        random = TRUE )
}

\arguments{
  \item{mean_f, stddev_f, phi_f, lmean_f, lstddev_f, logphi_f}{
    mean / stddev in normal / logspace used for both the likelihood of deviates & to project future values.
    Defaults to parameters with names \code{(by_stock).(param_name).proj.(mean|stddev)}
  }
  \item{level_f, loglevel_f}{
    (logspace) level (or offset) applied on top of ar1/logar1 regression.
    Defaults to parameter with name \code{(by_stock).(param_name).proj.(level|loglevel)},
  }
  \item{lastx_f}{
    If \code{> 0}, the setting of \var{level_f} / \var{loglevel_f} will be ignored,
    and the mean of the last (x) non-projection values are used as (log)level.
    Defaults to 0L, i.e. disabled.
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
  \item{by_stock}{
    Prepend stock name to the projection variable, i.e. \var{param_name}.
    Unlike \code{\link{g3_parameterized}}, can only be \code{FALSE} or \code{\link{g3_stock}} objects,
    \code{TRUE} or \code{"species"} isn't supported.
  }
  \item{weight}{
    A weighting to give to the likelhood when generating total nll.
  }
  \item{scale, offset}{
    Number, formula or string. Scale / offset to add to parameter values.
    If string, then the scale/offset will also be a parameter, equivalent to setting
    \code{scale = g3_parameterized(c(param_name, "proj", "scale"))}.
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
  \subsection{g3_param_project_dlnorm}{
    Returns a "nll" & "project" \link{formula} objects for use as \var{project_fs}.

    The functions compare / generate normally-distributed deviates around a mean, i.e:
    \deqn{ V_t = \epsilon_{M - \frac{e^{2*\Sigma}}{2},\Sigma} }
    \deqn{ v = exp(V) }
    \describe{
      \item{\eqn{M}}{\var{lmean_f} / \code{(by_stock).(param_name).proj.lmean} parameter}
      \item{\eqn{\Sigma}}{\var{lstddev_f} / \code{(by_stock).(param_name).proj.lstddev} parameter}
      \item{\eqn{\epsilon_{\mu,\sigma}}}{Normally distributed noise generated using \code{\link{rnorm}}}
      \item{\eqn{v}}{Output time series}
    }

    \describe{
      \item{nll}{Compare values against \code{dnorm(x, mean_f, stddev_f)}}
      \item{proj}{Generate new values with \code{rnorm(mean_f, stddev_f)}}
    }
  }

  \subsection{g3_param_project_dnorm}{
    Returns a "nll" & "project" \link{formula} objects for use as \var{project_fs}.

    The functions compare / generate log-normal deviates around a mean, i.e:
    \deqn{ v_t = \epsilon_{\mu,\sigma} }
    \describe{
      \item{\eqn{\mu}}{\var{mean_f} / \code{(by_stock).(param_name).proj.mean} parameter}
      \item{\eqn{\sigma}}{\var{stddev_f} / \code{(by_stock).(param_name).proj.stddev} parameter}
      \item{\eqn{\epsilon_{\mu,\sigma}}}{Normally distributed noise generated using \code{\link{rnorm}}}
      \item{\eqn{v}}{Output time series}
    }

    \describe{
      \item{nll}{Compare values against \code{dnorm(x, mean_f, stddev_f)}}
      \item{proj}{Generate new values with \code{rnorm(mean_f, stddev_f)}}
    }
  }

  \subsection{g3_param_project_rwalk}{
    Returns a "nll" & "project" \link{formula} objects for use as \var{project_fs}.

    The functions compare / generate to a random walk, i.e:
    \deqn{ v_t = v_{t-1} + \epsilon_{\mu,\sigma} }
    \describe{
      \item{\eqn{\mu}}{\var{mean_f} / \code{(by_stock).(param_name).proj.mean} parameter}
      \item{\eqn{\sigma}}{\var{stddev_f} / \code{(by_stock).(param_name).proj.stddev} parameter}
      \item{\eqn{\epsilon_{\mu,\sigma}}}{Normally distributed noise generated using \code{\link{rnorm}}}
      \item{\eqn{v}}{Output time series}
    }

    \describe{
      \item{nll}{Compare difference between values \code{dnorm(x, mean_f, stddev_f)}}
      \item{proj}{Generate new values with a delta of \code{rnorm(mean_f, stddev_f)}}
    }
  }
  
  \subsection{g3_param_project_ar1}{
    Returns a "nll" & "project" \link{formula} objects for use as \var{project_fs}.

    The functions compare / generate a AR1 process projecting from any existing values, i.e:
    \deqn{ v_t = \phi v_{t-1} + (1 - \phi) \theta + \epsilon_{0,\sigma} }
    \describe{
      \item{\eqn{\phi}}{\var{phi_f} / \code{(by_stock).(param_name).proj.phi} parameter}
      \item{\eqn{\theta}}{\var{level_f} / \code{(by_stock).(param_name).proj.level} parameter}
      \item{\eqn{\sigma}}{\var{stddev_f} / \code{(by_stock).(param_name).proj.stddev} parameter, if 0 1e-7 is used, so we don't return Inf}
      \item{\eqn{\epsilon_{\mu,\sigma}}}{Normally distributed noise generated using \code{\link{rnorm}}}
      \item{\eqn{v}}{Output time series}
    }
  }

  \subsection{g3_param_project_logar1}{
    Returns a "nll" & "project" \link{formula} objects for use as \var{project_fs}.

    The functions compare / generate a log-AR1 process projecting from any existing values, i.e:
    \deqn{ V_t = \Phi V_{t-1} + (1 - \Phi) \Theta + \epsilon_{0 - \frac{e^{2*\Sigma}}{2},\Sigma} }
    \deqn{ v = exp(V) }
    \describe{
      \item{\eqn{\Phi}}{\var{logphi_f} / \code{(by_stock).(param_name).proj.logphi} parameter}
      \item{\eqn{\Theta}}{\var{loglevel_f} / \code{(by_stock).(param_name).proj.loglevel} parameter}
      \item{\eqn{\Sigma}}{\var{lstddev_f} / \code{(by_stock).(param_name).proj.lstddev} parameter, if 0 1e-7 is used, so we don't return Inf}
      \item{\eqn{\epsilon_{\mu,\sigma}}}{Normally distributed noise generated using \code{\link{rnorm}}}
      \item{\eqn{v}}{Output time series}
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
st <- list(
    imm = g3_stock(c("fish", maturity = "imm"), c(10, 20, 30)),
    mat = g3_stock(c("fish", maturity = "mat"), c(10, 20, 30)) )
st2 <- g3_stock("other", c(10, 20, 30))

# Set up a projected parameter to share over both stocks
st_Mdn <- g3_param_project(
   "Mdn",
   g3_param_project_dnorm(),
   # Append common part of stock names to parameter name
   by_stock = st )

actions <- list(
    g3a_time(1990, 1994, c(6,6)),
    g3a_initialconditions(st$imm,
        quote( 100 + stock__minlen ),
        quote( 1e4 + 0 * stock__minlen ) ),
    g3a_initialconditions(st$mat,
        quote( 100 + stock__minlen ),
        quote( 1e4 + 0 * stock__minlen ) ),
    g3a_initialconditions(st2,
        quote( 100 + stock__minlen ),
        quote( 1e4 + 0 * stock__minlen ) ),

    # Natural mortality with per-step deviates
    g3a_naturalmortality(st$imm, g3a_naturalmortality_exp(st_Mdn)),
    g3a_naturalmortality(st$mat, g3a_naturalmortality_exp(st_Mdn)),

    # Natural mortality with per-year random walk
    g3a_naturalmortality(st2, g3a_naturalmortality_exp(
        g3_param_project(
          "Mrw",
          g3_param_project_rwalk(),
          # The same value will be used for each step
          by_step = FALSE,
          # by_stock means the stock name will be included in parameter names
          by_stock = st2 ))),
    NULL )

model_fn <- g3_to_r(c(actions, list(
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    NULL )))

# Mdn has a parameter for each year/step, as well as mean/sd (added above) & likelihood weighting
grep("^fish.Mdn", names(attr(model_fn, 'parameter_template')), value = TRUE)

# Mrw has parameters for each year
grep("^other.Mrw", names(attr(model_fn, 'parameter_template')), value = TRUE)

attr(model_fn, 'parameter_template') |>
    g3_init_val("stst.Mdn.#.#", 0.5, lower = 0.1, upper = 0.9, random = TRUE) |>
    g3_init_val("stst.Mdn.proj.dnorm.lmean", 0.1) |>
    g3_init_val("stst.Mdn.proj.dnorm.lstddev", 0.001) |>

    g3_init_val("other.Mrw.proj.rwalk.mean", 0) |>
    g3_init_val("other.Mrw.proj.rwalk.stddev", 0.001) |>
    g3_init_val("other.Mrw.#", 0.5, lower = 0.1, upper = 0.9, random = TRUE) |>

    # Project forwards 20 years
    g3_init_val("project_years", 20) |>

    # Don't include projections in nll calculations:
    # allows a stddev to be supplied for projections, but estimated freely
    g3_init_val("proj_rwalk_fish_Mrw_weight", 0) |>
    g3_init_val("proj_dnorm_fish_Mdn_weight", 0) |>

    identity() -> params
r <- attributes(model_fn(params))

# Values used for dnorm
plot(r$proj_dnorm_fish_Mdn__var)

# Values used for random walk
plot(r$proj_rwalk_other_Mrw__var)

### Plot values for an individual projection function

actions <- list( g3a_time(1990, 1991), g3_param_project("M", g3_param_project_dlnorm()) )
model_fn <- g3_to_r(c(actions, list(
    g3a_report_history(actions, 'proj_.*', out_prefix = NULL),
    NULL )))

attr(model_fn, 'parameter_template') |>
    g3_init_val("M.proj.dlnorm.lmean", log(20)) |>
    g3_init_val("M.proj.dlnorm.lstddev", log(1e-6)) |>
    g3_init_val("M.#.#", 20) |>

    g3_init_val("project_years", 100) |>

    identity() -> params

par(mfrow=c(3, 1))
plot(attr(model_fn(params |>
    g3_init_val("M.proj.dlnorm.lstddev", log(1.001)) ), "proj_dlnorm_M__var"), ylim = c(15, 25))
plot(attr(model_fn(params |>
    g3_init_val("M.proj.dlnorm.lstddev", log(1e-1)) ), "proj_dlnorm_M__var"), ylim = c(15, 25))
plot(attr(model_fn(params |>
    g3_init_val("M.proj.dlnorm.lstddev", log(1e-2)) ), "proj_dlnorm_M__var"), ylim = c(15, 25))
}
