\name{timedata}
\alias{g3_timeareadata}

\title{Gadget3 time-based data}
\description{
  Convert time-based data into a formula to lookup values
}

\usage{
g3_timeareadata(lookup_name, df, value_field = "total_weight", areas = NULL)

}

\arguments{
  \item{lookup_name}{
    A unique name for this lookup, e.g. \code{"igfs_landings"}.
  }
  \item{df}{
    A \link{data.frame} with any of columns out of age, area, year and step, finally \var{value_field}.
  }
  \item{value_field}{
    Column name that contains output value.
  }
  \item{areas}{
    Named integer vector of area names to integer values. See \code{\link{g3s_livesonareas}}.
  }
}

\value{
  A \link{formula} object that looks up \var{value_field} for the current
  values of \code{age}, \code{area}, \code{cur_year} and \code{cur_step},
  depending on the columns in \var{df}. If there's no match, return 0.
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock(c(species = 'ling', 'imm'), seq(20, 156, 4)) \%>\% g3s_age(3, 10)
ling_mat <- g3_stock(c(species = 'ling', 'mat'), seq(20, 156, 4)) \%>\% g3s_age(5, 15)
igfs <- g3_fleet('igfs')

igfs_landings <-
  structure(expand.grid(year=1990:1994, step=2, area=1, total_weight=1),
            area_group = list(`1` = 1))

# Generate a fleet predation action, use g3_timeareadata to supply landings
# NB: Since igfs_landings only contains values for step=2, there will be no
#     predation on other steps (since g3_timeareadata will return 0).
predate_action <- g3a_predate_fleet(
    igfs,
    list(ling_imm, ling_mat),
    suitabilities = list(
        ling_imm = g3_suitability_exponentiall50(
            g3_parameterized('lln.alpha', by_stock = 'species'),
            g3_parameterized('lln.l50', by_stock = 'species')),
        ling_mat = g3_suitability_exponentiall50(
            g3_parameterized('lln.alpha', by_stock = 'species'),
            g3_parameterized('lln.l50', by_stock = 'species'))),
    catchability = g3a_predate_catchability_totalfleet(
        g3_timeareadata('igfs_landings', igfs_landings)))
}
