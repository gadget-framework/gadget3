\name{likelihood_catchdistribution}
\alias{g3l_catchdistribution_sumofsquares}
\alias{g3l_catchdistribution_multinomial}
\alias{g3l_catchdistribution_multivariate}
\alias{g3l_catchdistribution_surveyindices_log}
\alias{g3l_catchdistribution_surveyindices_linear}
\alias{g3l_catchdistribution}

\title{Gadget3 likelihood actions}
\description{
  Gather nll in a g3 model
}

\usage{
g3l_catchdistribution_sumofsquares(over = c("area"))

g3l_catchdistribution_multinomial(epsilon = 10)

g3l_catchdistribution_multivariate(rho_f, sigma_f, over = c("area"))

g3l_catchdistribution_surveyindices_log(alpha = 0, beta = 1)

g3l_catchdistribution_surveyindices_linear(alpha = 0, beta = 1)

g3l_catchdistribution(nll_name, obs_data, fleets, stocks, function_f, missing_val = 0, 
    area_group = NULL, nll_breakdown = FALSE, weight = 1, run_at = 10)

}

\arguments{
  \item{over}{
    When comparing proportions of lengthgroups, specifies the dimensions that define the total.
    For example the default "area" means the proprtion of the current lengthgroup to all individuals
    in that area.
  }
  \item{rho_f}{
    TODO:
  }
  \item{sigma_f}{
    TODO:
  }
  \item{epsilon}{
    Value to be used whenever the calculated probability is very unlikely. Default 10.
  }
  \item{alpha}{
    TODO:
  }
  \item{beta}{
    TODO:
  }
  \item{nll_name}{
    Character string, used to define the variable name for \var{obsstock} and \var{modelstock}.
  }
  \item{obs_data}{
    Data.frame of observation data, for example the results of \link[mfdb:mfdb_sample_count]{mfdb_sample_count}
    Should have year column, optional step/area/age columns, and a length or weight column.
    If step/area/age are missing, then the model data will be summed over these dimensions first.
    The length or weight column decides what to compare using \var{function_f}
  }
  \item{fleets}{
    A list of \code{\link{g3_stock}} objects to collect catch data for
  }
  \item{stocks}{
    A list of \code{\link{g3_stock}} objects to collect catch data for
  }
  \item{function_f}{
    A \link{formula} to compare \var{obsstock__x} to \var{modelstock__x} and generate nll,
    defined by one of the \var{g3l_catchdistribution_}* functions.

    This will be adapted to compare either number (\var{modelstock__num}) or weight (\var{modelstock__wgt})
    depending on what columns \var{obs_data} has.
  }
  \item{missing_val}{
    Where there are missing values in the incoming data, value to replace them with.
  }
  \item{area_group}{
    TODO:
  }
  \item{nll_breakdown}{
    Should the nll report be broken down by time? \code{TRUE} / \code{FALSE}
  }
  \item{weight}{
    Weighting applied to this likelihood component.
  }
  \item{run_at}{
    Order of age actions within model, see TODO:
  }
}

\details{
  The actions will define the following variables in your model:
  \describe{
    \item{\var{obsstock}__num/wgt}{A \code{\link{g3_stock}} instance that contains all observations in an array}
    \item{\var{modelstock}__num/wgt}{A \code{\link{g3_stock}} instance that groups in an identical fashion to \var{obsstock},
        that will be filled with the model's predicted values}
  }

  The model report will contain nll_cdist_\var{nll_name}__num and/or nll_cdist_\var{nll_name}__wgt, depending on
  the columns in \var{obs_data} (a number column will compare by individuals, and produce a corresponding num report).
  If \var{nll_breakdown} is \code{TRUE}, this will be an array with one entry per timestep.
}

\seealso{
  \url{https://hafro.github.io/gadget2/userguide/chap-like.html},
  \code{\link{g3_stock}}
}

\value{
  \subsection{g3l_catchdistribution_sumofsquares}{TODO:}
  \subsection{g3l_catchdistribution_multinomial}{TODO:}
  \subsection{g3l_catchdistribution_multivariate}{TODO:}
  \subsection{g3l_catchdistribution_surveyindices_log}{TODO:}
  \subsection{g3l_catchdistribution_surveyindices_linear}{TODO:}
  \subsection{g3l_catchdistribution}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{For all \var{fleets} and \var{stocks} combinations, collect catch data into \var{modelstock}__num or \var{modelstock}__wgt, depending on the columns provided in \var{obs_data}}
    \item{Compare \var{modelstock}__num/wgt with \var{obsstock}__num/wgt, using \var{function_f}}
  }}
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)
ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) \%>\% g3s_age(5, 15)
lln <- g3_fleet('lln')

\dontrun{# Fetch length distribution via. MFDB
ldist.lln <- mfdb_sample_count(mdb, c('age', 'length'), c(list(
    sampling_type = 'SEA',
    data_source = 'iceland-ldist',
    gear = c('LLN','HLN'),
    age = mfdb_interval("all",
        c(minage, maxage),
        open_ended = c("upper","lower")),
    length = mfdb_interval("len",
        seq(minlength, maxlength, by = dl),
        open_ended = c("upper","lower"))), defaults))
}
\dontshow{# Invent a ldist.lln table for our tests
ldist.lln <- list(expand.grid(
    year = 1999:2000,
    step = c(1, 2),
    age = c(5,6,7,8)))
ldist.lln[[1]]$number <- floor(runif(nrow(ldist.lln[[1]]), min=100, max=999))
}

likelihood_actions <- list(
  g3l_catchdistribution(
    'ldist_lln',
    ldist.lln[[1]],
    fleets = list(lln),
    stocks = list(ling_imm, ling_mat),
    g3l_catchdistribution_sumofsquares()))
}
