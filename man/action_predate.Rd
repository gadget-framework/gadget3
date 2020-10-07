\name{action_predate}
\alias{g3a_predate_totalfleet}

\title{Gadget3 predation actions}
\description{
  Add predation to a g3 model
}

\usage{
g3a_predate_totalfleet(fleet_stock, prey_stocks, suitabilities, amount_f, run_at = 3)

}

\arguments{
  \item{fleet_stock}{
    \code{\link{g3_stock}} that describes the harvesting fleet.
  }
  \item{prey_stocks}{
    List of \code{\link{g3_stock}}s that maturing \var{stock} should move into.
  }
  \item{suitabilities}{
    List of \link{formula} objects defining suitability of a given stock group,
    as defined by \code{\link{g3_suitability_exponentiall50}}.
  }
  \item{amount_f}{
    A \link{formula} that returns the total biomass a stock can harvest in the current time/area,
    generally defined by a \code{\link{g3_timeareadata}} table.
  }
  \item{run_at}{
    Order of age actions within model, see TODO:
  }
}

\details{
  The actions will define the following stock instance variables for each given \var{fleet_stock} and \var{prey_stock}:
  \describe{
    \item{\var{prey_stock__fleet_stock}}{Total biomass selected for that fleet/prey combination}
    \item{\var{fleet_stock__catch}}{Total biomass caught by that fleet}
    \item{\var{prey_stock__totalpredate}}{Total biomass removed from that stock}
    \item{\var{prey_stock__overconsumption}}{Factor to scale consumption down to at most 95\% of available stock}
  }
}

\value{
  \subsection{g3a_predate_totalfleet}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{Zero fleet and prey catch counters}
    \item{For each prey, collect maximum desired by fleet for all prey, into a \var{prey_stock__fleet_stock} variable}
    \item{After all fleet consumption is done, scale consumption using \var{amount_f}, sum into \var{prey_stock__totalpredate}}
    \item{After all other consumption is done, work out an overconsumption co-efficient for each prey}
    \item{For \var{prey_stock__fleet_stock}, \var{fleet_stock__catch}, scale by prey's overconsumption co-efficient}
  }}
}

\seealso{
  \url{https://hafro.github.io/gadget2/userguide/chap-stock.html#sec:stockpredator},
  \code{\link{g3_stock}}
}

\examples{
\dontshow{library(magrittr)}
areas <- c(a = 1, b = 2)
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_livesonareas(areas[c('a')])
ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) \%>\% g3s_livesonareas(areas[c('b')])
lln <- g3_fleet('lln') \%>\% g3s_livesonareas(areas[c('a', 'b')])

\dontrun{# Fetch landings data via. MFDB
lln_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('HLN','LLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))
}
\dontshow{# Invent a lln_landings table
lln_landings <- list(expand.grid(
    year = 1999:2000,
    step = c(1, 2),
    area = areas[c('a', 'b')]))
lln_landings[[1]]$total_weight <- floor(runif(nrow(lln_landings[[1]]), min=100, max=999))
}

predate_action <- g3a_predate_totalfleet(
    lln,
    list(ling_imm, ling_mat),
    suitabilities = list(
        ling_imm = g3_suitability_exponentiall50(
            ~g3_param('ling.lln.alpha'),
            ~g3_param('ling.lln.l50')),
        ling_mat = g3_suitability_exponentiall50(
            ~g3_param('ling.lln.alpha'),
            ~g3_param('ling.lln.l50'))),
    amount_f = g3_timeareadata('lln_landings', lln_landings[[1]]))
}
