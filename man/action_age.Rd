\name{action_age}
\alias{g3a_age}

\title{Gadget3 age action}
\description{
  Add ageing actions to a g3 model
}

\usage{
g3a_age(stock, output_stocks = list(), output_ratios = rep(1/length(output_stocks), 
    times = length(output_stocks)), run_f = ~cur_step_final, 
    run_at = 12, transition_at = 12)

}

\arguments{
  \item{stock}{
    \code{\link{g3_stock}} to age.
  }
  \item{output_stocks}{
    List of \code{\link{g3_stock}}s that oldest specimens in \var{stock} should move into.
  }
  \item{output_ratios}{
    Vector of proportions for how to distribute into \var{output_stocks}, default evenly spread.
  }
  \item{run_f}{
    \link{formula} specifying a condition for running this action, default is end of model year.
  }
  \item{run_at}{
    Order of age actions within model, see TODO:
  }
  \item{transition_at}{
    Order of transition actios within model, see TODO:
  }
}

\value{An action (i.e. list of formula objects) that will, for the given \var{stock}...\enumerate{
  \item{Move the final age group into temporary storage, \code{stock__transitioning_num} / \code{stock__transitioning_wgt}}
  \item{Move the contents of all other age groups into the age group above}
  \item{Move the contents of the temporary storage into \var{output_stocks}}
}}

\seealso{
  \url{https://hafro.github.io/gadget2/userguide/chap-stock.html#sec:stockmature},
  \code{\link{g3_stock}}
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)
ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) \%>\% g3s_age(5, 15)

# Ageing for immature ling
age_action <- g3a_age(ling_imm,
    output_stocks = list(ling_mat))
}
