\name{action_grow}
\alias{g3a_grow_lengthvbsimple}
\alias{g3a_grow_impl_bbinom}
\alias{g3a_growmature}

\title{Gadget3 growth action}
\description{
  Add growth/maturity actions to a g3 model
}

\usage{
g3a_grow_lengthvbsimple(linf_f, kappa_f, alpha_f, beta_f)

g3a_grow_impl_bbinom(beta_f, maxlengthgroupgrowth)

g3a_growmature(stock, growth_f, impl_f, maturity_f = ~0, output_stocks = list(), 
    output_ratios = rep(1/length(output_stocks), times = length(output_stocks)), 
    transition_f = ~cur_step_final, run_f = ~TRUE, run_at = 5, 
    transition_at = 7)

}

\arguments{
  \item{linf_f}{
    A \link{formula} to substitute for \eqn{L_\infty}.
  }
  \item{kappa_f}{
    A \link{formula} to substitute for \eqn{\kappa}.
  }
  \item{alpha_f}{
    A \link{formula} to substitute for \eqn{\alpha}.
  }
  \item{beta_f}{
    A \link{formula} to substitute for \eqn{\beta}.
  }
  \item{maxlengthgroupgrowth}{
    TODO: This currently does nothing.
  }
  \item{stock}{
    \code{\link{g3_stock}} to grow.
  }
  \item{growth_f}{
    A list of 2 \link{formula} objects that calculate mean growth,
    \code{delta_len_f} for \eqn{\Delta L}, \code{delta_wgt_f} for \eqn{\Delta W},
    as defined by \code{\link{g3a_grow_lengthvbsimple}}
  }
  \item{impl_f}{
    A growth implementation \link{formula}, as defined by \code{\link{g3a_grow_impl_bbinom}}.
  }
  \item{maturity_f}{
    A maturity \link{formula}, as defined by \code{\link{g3a_mature_constant}}.
  }
  \item{output_stocks}{
    List of \code{\link{g3_stock}}s that maturing \var{stock} should move into.
  }
  \item{output_ratios}{
    Vector of proportions for how to distribute into \var{output_stocks}, summing to 1, default evenly spread.
  }
  \item{transition_f}{
    \link{formula} specifying a contition for running maturation steps as well as growth, default final step of year.
  }
  \item{run_f}{
    \link{formula} specifying a condition for running this action, default always runs.
  }
  \item{run_at}{
    Order of age actions within model, see TODO:
  }
  \item{transition_at}{
    Order of transition actios within model, see TODO:
  }
}

\value{
  \subsection{g3a_grow_lengthvbsimple}{Returns pair of \link{formula} objects 

  \deqn{ {{\Delta}L}_i = ( L_\infty - L_i )(1 - e^{-\kappa{\Delta}t}) }

  \deqn{ {{\Delta}W}_i = \alpha ( (L_i + {{\Delta}L}_i)^\beta - {L_i}^\beta ) }

  Where \eqn{ \Delta{t} } is the length of the current timestep.}

  \subsection{g3a_grow_impl_bbinom}{\link{formula} object converting mean growths using beta-binomia distribution. See \url{https://hafro.github.io/gadget2/userguide/chap-stock.html#beta-binomial}}

  \subsection{g3a_growmature}{An action (i.e. list of formula objects) that will, for the given \var{stock}...\enumerate{
    \item{Move any maturing individuals into temporary storage, \code{stock__transitioning_num} / \code{stock__transitioning_wgt}}
    \item{Calculate increase in length/weight using \var{growth_f} and \var{impl_f}}
    \item{Move the contents of the temporary storage into \var{output_stocks}}
  }}
}

\seealso{
  \url{https://hafro.github.io/gadget2/userguide/chap-stock.html#sec:stockgrowth},
  \code{\link{g3_stock}}
}

\examples{
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4))
ling_mat <- g3_stock('ling_mat', seq(20, 156, 4))

# Growth / maturity for immature ling
growth_action <- g3a_growmature(ling_imm,
    growth_f = g3a_grow_lengthvbsimple(
        linf_f = ~g3_param("ling.Linf"),
        kappa_f = ~g3_param("ling.k") * 0.001,
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    impl_f = g3a_grow_impl_bbinom(
        beta_f = ~g3_param("ling.bbin") * 10,
        maxlengthgroupgrowth = 15),
    maturity_f = g3a_mature_constant(
        alpha = ~0.001 * g3_param("ling.mat1"),
        l50 = ~g3_param("ling.mat2")),
        output_stocks = list(ling_mat))
}
