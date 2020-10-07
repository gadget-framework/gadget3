\name{action_time}
\alias{g3a_time}

\title{Gadget3 timekeeping actions}
\description{
  Add timekeeping to a g3 model
}

\usage{
g3a_time(start_year, end_year, steps = as.array(c(12)), run_at = 0)

}

\arguments{
  \item{start_year}{
    Year model run will start.
  }
  \item{end_year}{
    After this year, model run will stop.
  }
  \item{steps}{
    Vector of step lengths, should sum to 12,
    for example, \code{c(3,3,3,3)} for quarterly steps within a year.
  }
  \item{run_at}{
    Order of age actions within model, see TODO:
  }
}

\details{
  The actions will define the following variables in your model:
  \describe{
    \item{\var{cur_time}}{Current iteration of model, starts at 0 and increments until finished}
    \item{\var{cur_step}}{Current step within individual year}
    \item{\var{cur_step_len}}{How many months long this step is}
    \item{\var{cur_year}}{Current year}
    \item{\var{cur_step_final}}{TRUE iff this is the final step of the year}
    \item{\var{total_steps}}{Total # of iterations before model stops}
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
