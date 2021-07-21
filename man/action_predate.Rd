\name{action_predate}
\alias{g3a_predate_totalfleet}

\title{Gadget3 predation actions}
\description{
  Add predation to a g3 model
}

\usage{
g3a_predate_totalfleet(fleet_stock, prey_stocks, suitabilities, amount_f,
    overconsumption_f = ~0.96 - logspace_add_vec((0.96 - prey_stock__consratio) * 100
    , 0.96)/100, run_at = 3)

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
  \item{overconsumption_f}{
    Overconsumption rule, a formula that should cap all values in \var{prey_stock__consratio} to <= 95%.
    You can emulate gadget2 more closely than the default with the following
    \code{overconsumption_f = quote(pmin(prey_stock__consratio, 0.95)))} (NB: Not differentiable!)
  }
  \item{run_at}{
    Integer order that actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
  }
}

\details{
  The actions will define the following stock instance variables for each given \var{fleet_stock} and \var{prey_stock}:
  \describe{
    \item{\var{prey_stock__suit_fleet_stock}}{Suitability of (prey_stock) for (fleet_stock), in a prey array.
        i.e. the result of calculating the formula in \var{suitabilities} for the current state of the prey}
    \item{\var{prey_stock__fleet_stock}}{Biomass of (prey_stock) caught by (fleet_stock), in a prey array}
    \item{\var{fleet_stock__catch}}{Biomass of all prey caught by (fleet stock), in a fleet array}
    \item{\var{prey_stock__totalpredate}}{Biomass of total consumed (prey_stock), in a prey array}
    \item{\var{prey_stock__consratio}}{Ratio of prey_stock__totalpredate / (current biomass), capped by \var{overconsumption_f}}
  }

  A model can have any number of \code{g3a_predate_*} actions, so long as the
  calling arguments are different. For instance, \code{run_f = ~age == 5} and
  \code{run_f = ~age == 7}.
}

\value{
  \subsection{g3a_predate_totalfleet}{An action (i.e. list of formula objects) that will...\enumerate{
    \item{Zero fleet and prey catch counters}
    \item{For each prey, collect maximum desired by fleet for all prey, into a \var{prey_stock__fleet_stock} variable}
    \item{After all fleet consumption is done, scale consumption using \var{amount_f}, sum into \var{prey_stock__totalpredate}}
    \item{After all consumption is done, temporarily convert \var{prey_stock__fleet_stock} to a proprotion of \var{prey_stock__totalpredate}}
    \item{Calculate \var{prey_stock__consratio} (ratio of consumed to available), capping using \var{overconsumption_f}. Update \var{prey_stock__num}}
    \item{Recalculate \var{prey_stock__fleet_stock}, \var{fleet_stock__catch}, post-overconsumption}
  }}
}

\seealso{
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-stock.html#sec:stockpredator},
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
