\name{likelihood_catchdistribution}
\alias{g3l_distribution_sumofsquares}
\alias{g3l_distribution_multinomial}
\alias{g3l_distribution_multivariate}
\alias{g3l_distribution_surveyindices_log}
\alias{g3l_distribution_surveyindices_linear}
\alias{g3l_distribution_sumofsquaredlogratios}
\alias{g3l_abundancedistribution}
\alias{g3l_catchdistribution}

\title{Gadget3 likelihood actions}
\description{
  Gather nll in a g3 model
}

\usage{
g3l_distribution_sumofsquares(over = c("area"))

g3l_distribution_multinomial(epsilon = 10)

g3l_distribution_multivariate(rho_f, sigma_f, over = c("area"))

g3l_distribution_surveyindices_log(alpha = NULL, beta = NULL)

g3l_distribution_surveyindices_linear(alpha = NULL, beta = NULL)

g3l_abundancedistribution(nll_name, obs_data, fleets, stocks, function_f,
    missing_val = 0, area_group = NULL, report = FALSE, nll_breakdown = FALSE,
    weight = substitute(g3_param(n, optimise = FALSE, value = 1),
        list(n = paste0(nll_name, "_weight"))),
    run_at = 10)

g3l_catchdistribution(nll_name, obs_data, fleets, stocks, function_f, missing_val = 0, 
    area_group = NULL, report = FALSE, nll_breakdown = FALSE,
    weight = substitute(g3_param(n, optimise = FALSE, value = 1),
        list(n = paste0(nll_name, "_weight"))),
    run_at = 10)

g3l_distribution_sumofsquaredlogratios(epsilon = 10)

}

\arguments{
  \item{over}{
    When comparing proportions of lengthgroups, specifies the dimensions that define the total.
    For example the default "area" means the proprtion of the current lengthgroup to all individuals
    in that area.
  }
  \item{rho_f}{
    \link{formula} substituted into multivariate calcuations, see below.
  }
  \item{sigma_f}{
    \link{formula} substituted into multivariate calcuations, see below.
  }
  \item{epsilon}{
    Value to be used whenever the calculated probability is very unlikely. Default 10.
  }
  \item{alpha}{
    \link{formula} substituted into surveyindices calcuations to fix intercept of linear regression,
    or NULL if not fixed. See below.
  }
  \item{beta}{
    \link{formula} substituted into surveyindices calcuations to fix slope of linear regression,
    or NULL if not fixed. See below.
  }
  \item{nll_name}{
    Character string, used to define the variable name for \var{obsstock} and \var{modelstock}.
  }
  \item{obs_data}{
    Data.frame of observation data, for example the results of
    \link[mfdb:mfdb_sample_count]{mfdb_sample_count}.

    Should at least have a year column, and a length or weight column.
    For more information, see "obs_data and data aggregation" below.
  }
  \item{fleets}{
    A list of \code{\link{g3_stock}} objects to collect catch data for.
    If empty, will collect abundance data for \var{stocks} instead.
  }
  \item{stocks}{
    A list of \code{\link{g3_stock}} objects to collect catch or abundance data for,
    depending if \var{stocks} were provided.
  }
  \item{function_f}{
    A \link{formula} to compare \var{obsstock__x} to \var{modelstock__x} and generate nll,
    defined by one of the \var{g3l_distribution_}* functions.

    This will be adapted to compare either number (\var{modelstock__num}) or weight (\var{modelstock__wgt})
    depending on what columns \var{obs_data} has.
  }
  \item{missing_val}{
    Where there are missing values in the incoming data, value to replace them with.
  }
  \item{area_group}{
    \link[mfdb:mfdb_group]{mfdb_group} or list mapping area names used in
    \var{obs_data} to integer model areas, see "obs_data and data aggregation"
    below.
  }
  \item{report}{
    If TRUE, add model and observation arrays to the model report, called
    \code{cdist_\var{nll_name}_model__num/wgt} and \code{cdist_\var{nll_name}_obs__num/wgt}
    respectively
  }
  \item{nll_breakdown}{
    Should the nll report be broken down by time? \code{TRUE} / \code{FALSE}
  }
  \item{weight}{
    Weighting applied to this likelihood component. Default is a \code{g3_param}
    that defaults to 1, allowing weights to be altered without recompiling.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
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

  \var{g3l_abundancedistribution} compares abundance of stocks, \var{g3l_catchdistribution} compares fleet catch.
  Thus providing fleets is mandatory for \var{g3l_catchdistribution}, and an error for \var{g3l_abundancedistribution}.

  \subsection{obs_data and data aggregation}{
    The \var{obs_data} data.frame, as well as providing the observation data to
    compare the model data against, controls the grouping of model data to
    compare to the observation data, by inspecting the MFDB column attributes
    produced by e.g. \link[mfdb:mfdb_sample_count]{mfdb_sample_count}.

    Metadata columns describe the observation datapoint in that row. The
    columns should be from this list:
    \describe{
      \item{year}{
        Required.
        Year for the data point. Gaps in years will result in no comparison for that year
      }
      \item{step}{
        Optional.
        If there is no step column, then the data is assumed to be yearly, and
        the model data for all timesteps will be summed before comparing.

        Model timestep for the data point. Gaps in steps will result in no comparison for that year/step.
      }
      \item{length}{
        Optional.
        If missing all lengthgroups in the model will be summed to compare to
        the data.

        If \code{open_ended = c('lower', 'upper')} was used when querying MFDB
        for the data, then the bottom/top length groups will be modified to
        start from zero or be infinite respectively.

        Any missing lengthgroups (when there is otherwise data for that
        year/step) will be compared to zero.
      }
      \item{age}{
        Optional.
        If missing all age-groups (if any) in the model will be summed to compare to
        the data.

        Model ages will be grouped by the same groupings as MFDB used, thus if
        the data was formed with a query \code{age = mfdb_group(young = 1:3,
        old = 4:5)}, then the model data will similarly have 2 groups in it.

        Any missing ages (when there is otherwise data for that
        year/step) will be compared to zero.
      }
      \item{stock}{
        Optional.
        If this and stock_re are missing all stocks in \var{stocks} will be
        summed to compare to the data.

        The values in the stocks column should match the names of the stocks
        given in the \var{stocks} parameter. This column can be factor or
        character.

        Any missing stocks (when there is otherwise data for that
        year/step) will be compared to zero.
      }
      \item{stock_re}{
        Optional.
        If this and stock are missing all stocks in \var{stocks} will be summed
        to compare to the data.

        The values in the stocks column will be used as regular expressions to
        match the names of the stocks given in the \var{stocks} parameter. For
        example, '_mat_' will match both 'ghd_mat_f' and 'ghd_mat_m' and will
        be compared against the sum of the 2 stocks.

        Any missing stocks (when there is otherwise data for that
        year/step) will be compared to zero.
      }
      \item{fleet}{
        Optional.
        If this and fleet_re are missing all fleets in \var{fleets} will be
        summed to compare to the data.

        The values in the fleets column should match the names of the fleets
        given in the \var{fleets} parameter. This column can be factor or
        character.

        Any missing fleets (when there is otherwise data for that
        year/step) will be compared to zero.
      }
      \item{fleet_re}{
        Optional.
        If this and fleet are missing all fleets in \var{fleets} will be summed
        to compare to the data.

        The values in the fleets column will be used as regular expressions to
        match the names of the fleets given in the \var{fleets} parameter. For
        example, '_trawl_' will match both 'fleet_trawl_is' and 'fleet_trawl_no' and will
        be compared against the sum of the 2 fleets.

        Any missing fleets (when there is otherwise data for that
        year/step) will be compared to zero.
      }
      \item{area}{
        Optional.
        If missing all areas in the model will be summed to compare to
        the data.

        Unlike other columns, the MFDB grouping here is ignored (the areas it
        is grouping over aren't integer model areas). Instead, the
        \var{area_group} parameter should describe how to map from the area
        names used in the table to integer model areas.

        For example, if \code{area_group = list(north=1:2, south=3:5)}, then
        the area column of \var{obs_data} should contain either "north" or
        "south", and corresponding model data will be summed from integer model
        areas 1,2 and 3,4,5 respectively.

        If \var{area_group} is not supplied, then we assume that \var{obs_data}
        area column will contain model area integers.

        Any missing areas (when there is otherwise data for that
        year/step) will be compared to zero.
      }
    }

    Data columns contain the observation data to compare. There should be at
    least one of:

    \describe{
      \item{number}{
        If a number column appears in \var{obs_data}, then the stock abundance
        by individuals will be aggregated and compared to the \var{obs_data}
        number column.
      }
      \item{weight}{
        If a weight column appears in \var{obs_data}, then the total biomass of
        the stock will be aggregated and compared to the \var{obs_data} number
        column.
      }
    }
  }
}

\seealso{
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-like.html},
  \code{\link{g3_stock}}
}

\value{
  \subsection{g3l_distribution_sumofsquares}{
    Returns a \link{formula} for use as \var{function_f}:

    \deqn{ \sum_{\it lengths} \Big( \frac{N_{tral}}{N_{tr}} - \frac{\nu_{tral}}{\nu_{tr}}  \Big) ^2 }
    \describe{
      \item{\eqn{N_{tral}}}{Observation sample size for current time/area/age/length combination}
      \item{\eqn{\nu_{tral}}}{Model sample size for current time/area/age/length combination}
      \item{\eqn{N_{tr}}}{Total observation sample size for current time/area (or dimensions set in \var{over})}
      \item{\eqn{\nu_{tr}}}{Total model sample size for current time/area (or dimensions set in \var{over})}
    }
  }

  \subsection{g3l_distribution_multinomial}{
    Returns a \link{formula} for use as \var{function_f}:

    \deqn{
      2 (
        \sum_{\it lengths} \log N_{tral}! -
        \log (\sum_{\it lengths} N_{tral})! -
        \sum_{\it lengths} ( N_{tral} \log min(\frac{\nu_{tral}}{\sum_{\it lengths} \nu_{tral}}, \frac{1}{l \epsilon}) )
      )
    }
    
    \describe{
      \item{\eqn{N_{tral}}}{Observation sample size for current time/area/age/length combination}
      \item{\eqn{\nu_{tral}}}{Model sample size for current time/area/age/length combination}
      \item{\eqn{l}}{Number of lengthgroups in sample}
      \item{\eqn{\epsilon}}{\var{epsilon} parameter}
    }
  }
  \subsection{g3l_distribution_multivariate}{
    Returns a \link{formula} for use as \var{function_f}, which calls TMB's \code{SCALE(AR1(rho), sigma)(x)}, where
    \var{rho} and \var{sigma} are parameters, and \code{x} is defined as:

    \deqn{ \frac{N_{tral}}{N_{tr}} - \frac{\nu_{tral}}{\nu_{tr}} }
    \describe{
      \item{\eqn{N_{tral}}}{Observation sample size for current time/area/age/length combination}
      \item{\eqn{\nu_{tral}}}{Model sample size for current time/area/age/length combination}
      \item{\eqn{N_{tr}}}{Total observation sample size for current time/area (or dimensions set in \var{over})}
      \item{\eqn{\nu_{tr}}}{Total model sample size for current time/area (or dimensions set in \var{over})}
    }
    For more information, see \href{Autoregressive processes}{http://kaskr.github.io/adcomp/_book/Densities.html#autoregressive-processes}
    in the TMB book.
  }
  \subsection{g3l_distribution_surveyindices_log}{
    Returns a \link{formula} for use as \var{function_f}:

    \deqn{
      \sum_{\it time} (\alpha + \beta \log N_{tral} - \log \nu_{tral})^2
    }
    \describe{
      \item{\eqn{N_{tral}}}{Observation sample size for current time/area/age/length combination}
      \item{\eqn{\nu_{tral}}}{Model sample size for current time/area/age/length combination}
      \item{\eqn{\alpha}}{\var{alpha} parameter}
      \item{\eqn{\beta}}{\var{beta} parameter}
    }

    If \var{alpha} or \var{beta} is not provided, then linear regression is
    performed on \eqn{N}, \eqn{\nu} over time for each area/age/length combination.
    The used values will be stored in a \code{cdist_\var{nll_name}_model__param} array and
    reported after model run, whether calculated or hard-coded.
  }
  \subsection{g3l_distribution_surveyindices_linear}{
    Returns a \link{formula} for use as \var{function_f}:

    \deqn{
      \sum_{\it lengths} (\alpha + \beta N_{tral} - \nu_{tral})^2
    }
    \describe{
      \item{\eqn{N_{tral}}}{Observation sample size for current time/area/age/length combination}
      \item{\eqn{\nu_{tral}}}{Model sample size for current time/area/age/length combination}
      \item{\eqn{\alpha}}{\var{alpha} parameter}
      \item{\eqn{\beta}}{\var{beta} parameter}
    }

    If \var{alpha} or \var{beta} is not provided, then linear regression is
    performed on \eqn{N}, \eqn{\nu} over time for each area/age/length combination.
    The used values will be stored in a \code{cdist_\var{nll_name}_model__param} array and
    reported after model run, whether calculated or hard-coded.
  }
  \subsection{g3l_*distribution}{Assuming \var{fleets} is empty, an action (i.e. list of formula objects) that will...\enumerate{
    \item{For all \var{stocks}, collect catch data into \var{modelstock}__num or \var{modelstock}__wgt, depending on the columns provided in \var{obs_data}}
    \item{Compare \var{modelstock}__num/wgt with \var{obsstock}__num/wgt, using \var{function_f}}
  }
  The output of \var{function_f} is summed over all stock dimensions (age/area) and time and added to \code{nll}.
  }
  \subsection{g3l_catchdistribution}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{For all \var{fleets} and \var{stocks} combinations, collect catch data into \var{modelstock}__num or \var{modelstock}__wgt, depending on the columns provided in \var{obs_data}}
    \item{Compare \var{modelstock}__num/wgt with \var{obsstock}__num/wgt, using \var{function_f}}
  }
  The output of \var{function_f} is summed over all stock dimensions (age/area) and time and added to \code{nll}.
  }
  \subsection{g3l_distribution_sumofsquaredlogratios}{
    The equivalent of gadget2's \code{catchinkilos}.

    Returns a \link{formula} for use as \var{function_f}:

    \deqn{
      \sum_{\it lengths} (log(N_{tral} + \epsilon) - log(\nu_{tral} + \epsilon))^2
    }
    \describe{
      \item{\eqn{N_{tral}}}{Observation sample size for current time/area/age/length combination}
      \item{\eqn{\nu_{tral}}}{Model sample size for current time/area/age/length combination}
      \item{\eqn{\epsilon}}{\var{epsilon} parameter}
    }
  }
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
    g3l_distribution_sumofsquares()))
}
