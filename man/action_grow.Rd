\name{action_grow}
\alias{g3a_grow_lengthvbsimple}
\alias{g3a_grow_weightsimple}
\alias{g3a_grow_impl_bbinom}
\alias{g3a_growmature}
\concept{G3 action}

\title{Gadget3 growth action}
\description{
  Add growth/maturity actions to a g3 model
}

\usage{
g3a_grow_lengthvbsimple(
        linf_f = g3_parameterized('Linf', by_stock = by_stock),
        kappa_f = g3_parameterized('K', by_stock = by_stock),
        by_stock = TRUE)

g3a_grow_weightsimple(
        alpha_f = g3_parameterized('walpha', by_stock = by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = by_stock),
        by_stock = TRUE)

g3a_grow_impl_bbinom(
        delta_len_f = g3a_grow_lengthvbsimple(by_stock = by_stock),
        delta_wgt_f = g3a_grow_weightsimple(by_stock = by_stock),
        beta_f = g3_parameterized('bbin', by_stock = by_stock),
        maxlengthgroupgrowth,
        by_stock = TRUE)

g3a_growmature(stock, impl_f, maturity_f = ~0, output_stocks = list(), 
    output_ratios = rep(1/length(output_stocks), times = length(output_stocks)), 
    transition_f = ~cur_step_final, run_f = ~TRUE,
    run_at = g3_action_order$grow,
    transition_at = g3_action_order$mature)

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
    An integer with the maximum length groups an individual can jump in one step.
  }
  \item{stock}{
    \code{\link{g3_stock}} to grow.
  }
  \item{delta_len_f}{
    A \link{formula} defining a non-negative vector for mean increase in length for \code{stock}
    for each lengthgroup, as defined by \code{\link{g3a_grow_lengthvbsimple}}.
  }
  \item{delta_wgt_f}{
    A \link{formula} defining the corresponding weight increase as a matrix of
    lengthgroup to lengthgroup delta for \code{stock}, as defined by \code{\link{g3a_grow_weightsimple}}.
  }
  \item{by_stock}{
    Change the default parameterisation (e.g. to be by 'species'), see \code{\link{g3_parameterized}}.
  }
  \item{impl_f}{
    A pair of \link{formula} objects, as defined by \code{\link{g3a_grow_impl_bbinom}}.
    Both define a matrix of length groups \code{i} to length group deltas \code{j} (0..\var{maxlengthgroupgrowth}),
    the values in the first indicate the proportion of individuals moving from \code{i} to \code{i + j},
    the values in the second indicate the corresponding weight increase of individuals moving from \code{i} to \code{i + j}.
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
    Integer order that actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
  }
  \item{transition_at}{
    Integer order that transition actions will be run within model, see "Ordering of actions" in \code{vignette("model_structure")}.
  }
}

\details{
  A model can have any number of \code{g3a_growmature} actions, so long as the
  calling arguments are different. For instance, \code{run_f = ~age == 5} and
  \code{run_f = ~age == 7}.

  \var{impl_f}'s dependent variables are analysed to see what will affect growth.
  If nothing but \code{cur_step_size} will affect growth, then growth will only
  be recalculated when the step size changes.
}

\value{
  \subsection{g3a_grow_lengthvbsimple}{Returns a \link{formula} for use as \var{delta_len_f}:

  \deqn{ {{\Delta}L}_i = ( L_\infty - L_i )(1 - e^{-\kappa{\Delta}t}) }

  Where \eqn{ \Delta{t} } is the length of the current timestep.
  }
  \subsection{g3a_grow_weightsimple}{Returns a \link{formula} for use as \var{delta_wgt_f}:

  \deqn{ {{\Delta}W}_{i,j} = \alpha ( (L_i + {{\Delta}L}_j)^\beta - {L_i}^\beta ) }

  Where \eqn{ \Delta{t} } is the length of the current timestep,
  \eqn{ {{\Delta}L} } is all possible length group increases i.e \code{0..maxlengthgroupgrowth}.
  }
  \subsection{g3a_grow_impl_bbinom}{\link{formula} object converting mean growths using beta-binomia distribution. See \url{https://gadget-framework.github.io/gadget2/userguide/chap-stock.html#beta-binomial}}

  \subsection{g3a_growmature}{An action (i.e. list of formula objects) that will, for the given \var{stock}...\enumerate{
    \item{Move any maturing individuals into temporary storage, \code{stock__transitioning_num} / \code{stock__transitioning_wgt}}
    \item{Calculate increase in length/weight using \var{growth_f} and \var{impl_f}}
    \item{Move the contents of the temporary storage into \var{output_stocks}}
  }}
}

\seealso{
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-stock.html#sec:stockgrowth},
  \code{\link{g3_stock}}
}

\examples{
ling_imm <- g3_stock(c(species = 'ling', 'imm'), seq(20, 156, 4))
ling_mat <- g3_stock(c(species = 'ling', 'mat'), seq(20, 156, 4))

# Growth / maturity for immature ling
growth_action <- g3a_growmature(ling_imm,
    impl_f = g3a_grow_impl_bbinom(
        # Parameters will be ling.Linf, ling.K
        g3a_grow_lengthvbsimple(by_stock = 'species'),
        # Parameters will be ling_imm.walpha, ling_imm.wbeta
        g3a_grow_weightsimple(),
        maxlengthgroupgrowth = 15),
    maturity_f = g3a_mature_constant(
        alpha = g3_parameterized('ling.mat1', scale = 0.001),
        l50 = g3_parameterized('ling.mat2')),
        output_stocks = list(ling_mat))
}
