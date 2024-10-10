\name{params}
\alias{g3_parameterized}
\concept{G3 utilities}

\title{Gadget3 parameter helpers}
\description{
  Shortcuts to parameterise a model with g3_param
}

\usage{
g3_parameterized(
        name,
        by_stock = FALSE,
        by_predator = FALSE,
        by_year = FALSE,
        by_step = FALSE,
        by_age = FALSE,
        by_area = FALSE,
        exponentiate = FALSE,
        avoid_zero = FALSE,
        scale = 1,
        offset = 0,
        ifmissing = NULL,
        ...)
}

\arguments{
  \item{name}{
    Suffix for parameter name.
  }
  \item{by_stock}{
    Should there be individual parameters per-stock?
    \describe{
      \item{FALSE}{No}
      \item{TRUE}{Produce a \code{"stock_name.name"} parameter}
      \item{character vector}{Select the stock name_part(s) to use, e.g. to produce \code{"stock_species.name"} parameter with "species"}
      \item{List of \code{\link{g3_stock}} objects}{Produce a parameter that applies to all given stocks}
    }
  }
  \item{by_predator}{
    Should there be individual parameters per-predator (read: per-fleet) stock?
    \describe{
      \item{FALSE}{No}
      \item{TRUE}{Produce a \code{"fleet_stock_name.name"} parameter}
      \item{character vector}{Select the stock name_part(s) to use, e.g. to produce \code{"fleet_country.name"} parameter with "country"}
      \item{List of \code{\link{g3_stock}} objects}{Produce a parameter that applies to all given stocks}
    }
  }
  \item{by_year}{
    Should there be individual parameters per model year?
    \describe{
      \item{FALSE}{No}
      \item{TRUE}{Produce a \code{"name.1998"} parameter for each year the model runs}
      \item{1998:2099}{Override the year range, so when projecting there will be sufficient parameters available.}
    }
  }
  \item{by_step}{
    Should there be individual parameters per step within years?
    \describe{
      \item{FALSE}{No}
      \item{TRUE}{Produce a \code{"name.1"} seasonal parameter for each step, or \code{"name.1998.1"} for every timestep in the model if combined with \var{by_year}.}
    }
  }
  \item{by_age}{
    Should there be individual parameters per stock age?
    \describe{
      \item{FALSE}{No}
      \item{TRUE}{Produce a \code{"name.4"} parameter for each age of the stock(s) in \var{by_stock}}
    }
  }
  \item{by_area}{
    Should there be individual parameters per stock area?
    \describe{
      \item{FALSE}{No}
      \item{TRUE}{Produce a \code{"name.area"} parameter for each area of the stock(s) in \var{by_stock}}
    }
  }
  \item{ifmissing}{
    Value to use for when there is no valid parameter (read: year when by_year = TRUE)
    Either a numeric constant or character.
    If character, add another parameter for ifmissing, using the same \var{by_stock} value.
  }
  \item{exponentiate}{Use \code{exp(value)} instead of the raw parameter value. Will add "_exp" to the parameter name.}
  \item{avoid_zero}{If TRUE, wrap parameter with \code{avoid_zero}}
  \item{scale}{
    Use \code{scale * value} instead of the raw parameter value.
    Either a numeric constant or character.
    If character, add another parameter for scale, using the same \var{by_stock} value.
  }
  \item{offset}{
    Use \code{value + offset} instead of the raw parameter value
    Either a numeric constant or character.
    If character, add another parameter for offset, using the same \var{by_stock} value.
  }
  \item{...}{Additional parameters passed through to \code{\link{g3_param}}, e.g. \var{optimise}, \var{random}, ...}
}

\details{
  The function provides shortcuts to common formulas used when parameterising a model.
}

\value{A \link{formula} object defining the given parameters}
  
\seealso{
  \code{\link{g3_param}},
  \code{\link{g3_param_table}},
  \code{\link{stock_prepend}}
}

\examples{
\dontshow{library(magrittr)}
stock_imm <- g3_stock(c(species = 'stock', 'imm'), seq(10, 35, 5)) \%>\% g3s_age(1, 4)
stock_mat <- g3_stock(c(species = 'stock', 'mat'), seq(10, 35, 5)) \%>\% g3s_age(3, 6)

# Helper function that shows the parameter template for the given parameter
param_template_for <- function (g3_param) {
  model_code <- g3_to_tmb(list(
      # g3a_naturalmortality() isn't important, it is a place to add our parameter
      g3a_naturalmortality(stock_imm, g3_param),
      # We also need stock_mat in the model at least once
      g3a_naturalmortality(stock_mat, 0),

      # Set a year range to use for parameters where relevant
      g3a_time(1990, 1994) ))

  # Extract template, throw away default parameters from g3a_time()
  params <- attr(model_code, "parameter_template")
  params <- params[!(rownames(params) \%in\% c('retro_years', 'project_years')),]
  return(params)
}

# Not 'by' anything, so we add a single parameter value
param_template_for( g3_parameterized('K') )

# Can set defaults for the parameter template when defining a parameter
param_template_for( g3_parameterized('K', value = 5, lower = 2,
    upper = 8, optimise = FALSE) )

# by_stock, so the parameters will have the stock name prepended
param_template_for( g3_parameterized('K', by_stock = TRUE) )

# Similarly, we can prepend year & age
param_template_for( g3_parameterized('K', by_stock = TRUE, by_year = TRUE, by_age = TRUE) )

# You can specify the name part to use,
# e.g. if a parameter should be shared between mature and immature:
param_template_for( g3_parameterized('K', by_stock = 'species', by_year = TRUE) )

# Can give a list of stocks, in which case it works out name parts for you
param_template_for( g3_parameterized('K', by_stock = list(stock_imm, stock_mat)) )
param_template_for( g3_parameterized('K', by_stock = list(stock_imm, stock_mat), by_age = TRUE) )

# If there are no shared name parts, then all names will be added
param_template_for( g3_parameterized(
    'btrigger',
    by_stock = list(g3_fleet("surv"), g3_fleet("comm"))) )

# You can set fixed scale/offset for the parameter
g3_parameterized('K', scale = 5, offset = 9)

# ...or give names and they will also be parameters, sharing the by_stock setting
param_template_for( g3_parameterized('K', by_stock = TRUE, scale = "sc", offset = "offs") )

}
