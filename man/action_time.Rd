\name{action_time}
\alias{g3a_time}

\title{Gadget3 timekeeping actions}
\description{
  Add timekeeping to a g3 model
}

\usage{
g3a_time(start_year, end_year, steps = as.array(c(12)),
     project_years = ~g3_param('project_years', default = 0, optimize = FALSE),
     run_at = 0)

}

\arguments{
  \item{start_year}{
    Year model run will start.
  }
  \item{end_year}{
    After this year, model run will stop.
  }
  \item{steps}{
    Either an MFDB time grouping, e.g. \code{mfdb::mfdb_timestep_quarterly}, or a
    vector of step lengths which should should sum to 12,
    for example, \code{c(3,3,3,3)} for quarterly steps within a year.
  }
  \item{project_years}{
    Number of years to continue running after the "end" of the model, or if negative
    stop early before the end of the model.

    Defaults to an unoptimized \code{project_years} parameter, set to 0 (i.e. no projection).
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
