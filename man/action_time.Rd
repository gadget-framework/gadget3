\name{action_time}
\alias{g3a_time}

\title{Gadget3 timekeeping actions}
\description{
  Add timekeeping to a g3 model
}

\usage{
g3a_time(start_year, end_year, step_lengths = c(12),
     final_year_steps = quote( length(step_lengths) ),
     project_years = ~g3_param('project_years', default = 0, optimise = FALSE),
     retro_years = ~g3_param('retro_years', default = 0, optimise = FALSE),
     run_at = 0)

}

\arguments{
  \item{start_year}{
    Year model run will start.
  }
  \item{end_year}{
    After this year, model run will stop.
  }
  \item{step_lengths}{
    Either an MFDB time grouping, e.g. \code{mfdb::mfdb_timestep_quarterly}, or a
    vector of step lengths which should should sum to 12,
    for example, \code{c(3,3,3,3)} for quarterly steps within a year.
  }
  \item{final_year_steps}{
    Number of steps of final year to include. Either as an integer or quoted
    code, in which case it will be calcuated when the model runs.
    For example:
    \describe{
      \item{\code{0}}{Model stops before the start of \var{end_year} (it is exclusive)}
      \item{\code{length(step_lengths)}}{Model stops at the end of \var{end_year} (it is inclusive)}
      \item{\code{2}}{Model stops at the second step of \var{end_year}, mid-year if \var{step_lengths} is quarterly}
    }
  }
  \item{project_years}{
    Number of years to continue running after the "end" of the model. Must be \code{>= 0}

    Defaults to an unoptimized \code{project_years} parameter, set to 0 (i.e. no projection).
    Generally, you would change this parameter in the parameter template, rather than changing here.
  }
  \item{retro_years}{
    Adjust \var{end_year} to finish model early. Must be \code{>= 0}
    Can be used in conjunction with \var{project_years} to project instead.

    The true end year of the model will be \code{end_year - retro_years + project_years}.

    Defaults to an unoptimized \code{retro_years} parameter, set to 0.
    Generally, you would change this parameter in the parameter template, rather than changing here.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
  }
}

\details{
  The actions will define the following variables in your model:
  \describe{
    \item{\var{cur_time}}{Current iteration of model, starts at 0 and increments until finished}
    \item{\var{cur_step}}{Current step within individual year}
    \item{\var{cur_step_size}}{Proportion of year this step contains, e.g. quarterly = 3/12}
    \item{\var{cur_year}}{Current year}
    \item{\var{cur_step_final}}{TRUE iff this is the final step of the year}
    \item{\var{cur_year_projection}}{TRUE iff we are currently projecting past \var{end_year}}
    \item{\var{total_steps}}{Total # of iterations (including projection) before model stops}
    \item{\var{total_years}}{Total # of years (including projection) before model stops}
  }
}

\value{
  \subsection{g3a_time}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{Define \var{cur_*} variables listed above}
    \item{If we've reached the end of the model, return \var{nll}}
  }}
}

\examples{
# Run model 2000..2004, in quarterly steps
time_action <- g3a_time(
    start_year = 2000,
    end_year = 2004,
    c(3, 3, 3, 3))
}
