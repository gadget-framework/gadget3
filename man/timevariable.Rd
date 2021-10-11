\name{timevariable}
\alias{g3_timeareadata}

\title{Gadget3 time-based data}
\description{
  Convert time-based data into a formula to lookup values
}

\usage{
g3_timeareadata(lookup_name, df, value_field = "total_weight")

}

\arguments{
  \item{lookup_name}{
    A unique name for this lookup, e.g. \code{"igfs_landings"}.
  }
  \item{df}{
    A \link{data.frame} with area (optional), year, step and \var{value_field} columns.
  }
  \item{value_field}{
    Column name that contains output value.
  }
}

\value{
  A \link{formula} object that looks up a value for the current values of
  \code{area} (if part of \var{df}), \code{cur_year} and \code{cur_step}.
  If none exists, return 0.
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)
ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) \%>\% g3s_age(5, 15)
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
            ~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50')),
        ling_mat = g3_suitability_exponentiall50(
            ~g3_param('ling.igfs.alpha'), ~g3_param('ling.igfs.l50'))),
    catchability = g3a_predate_catchability_totalfleet(
        g3_timeareadata('igfs_landings', igfs_landings)))
}
