\name{action_grow}
\alias{g3a_grow_lengthvbsimple}
\alias{g3a_grow_weightsimple}
\alias{g3a_grow_length_multspec}
\alias{g3a_grow_weight_multspec}
\alias{g3a_grow_length_weightjones}
\alias{g3a_grow_weight_weightjones}
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

g3a_grow_length_multspec(
        p0 = g3_parameterized('multispec.p0', value = 1, by_stock = by_stock),
        p1 = g3_parameterized('multispec.p1', value = 1, by_stock = by_stock),
        p2 = g3_parameterized('multispec.p2', value = 1, by_stock = by_stock),
        p3 = g3_parameterized('multispec.p3', value = 0, by_stock = by_stock),
        temperature = 0,
        by_stock = TRUE)

g3a_grow_weight_multspec(
        p4 = g3_parameterized('multispec.p4', value = 1, by_stock = by_stock),
        p5 = g3_parameterized('multispec.p5', value = 1, by_stock = by_stock),
        p6 = g3_parameterized('multispec.p6', value = 0, by_stock = by_stock),
        p7 = g3_parameterized('multispec.p7', value = 1, by_stock = by_stock),
        p8 = g3_parameterized('multispec.p8', value = 0, by_stock = by_stock),
        temperature = 0,
        by_stock = TRUE)

g3a_grow_length_weightjones(
        p0 = g3_parameterized('weightjones.p0', value = 0, by_stock = by_stock),
        p1 = g3_parameterized('weightjones.p1', value = 0, by_stock = by_stock),
        p2 = g3_parameterized('weightjones.p2', value = 1, by_stock = by_stock),
        p3 = g3_parameterized('weightjones.p3', value = 0, by_stock = by_stock),
        p4 = g3_parameterized('weightjones.p4', value = 1, by_stock = by_stock),
        p5 = g3_parameterized('weightjones.p5', value = 100, by_stock = by_stock),
        p6 = g3_parameterized('weightjones.p6', value = 1, by_stock = by_stock),
        p7 = g3_parameterized('weightjones.p7', value = 1, by_stock = by_stock),
        reference_weight = 0,
        temperature = 0,
        by_stock = TRUE)

g3a_grow_weight_weightjones(
        q0 = g3_parameterized('weightjones.q0', value = 1, by_stock = by_stock),
        q1 = g3_parameterized('weightjones.q1', value = 1, by_stock = by_stock),
        q2 = g3_parameterized('weightjones.q2', value = 1, by_stock = by_stock),
        q3 = g3_parameterized('weightjones.q3', value = 1, by_stock = by_stock),
        q4 = g3_parameterized('weightjones.q4', value = 1, by_stock = by_stock),
        q5 = g3_parameterized('weightjones.q5', value = 0, by_stock = by_stock),
        max_consumption = g3a_predate_maxconsumption(temperature = temperature),
        temperature = 0,
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
  \item{p0, p1, p2, p3, p4, p5, p6, p7, p8, q0, q1, q2, q3, q4, q5}{
    A \link{formula} to substitute for the equivalent value.
  }
  \item{max_consumption}{
    Maximum predator consumption, see \code{\link{g3a_predate_maxconsumption}}.
  }
  \item{temperature}{
    A \link{formula} providing values for the current temperature,
    likely implemented with \code{\link{g3_timeareadata}}.
  }
  \item{maxlengthgroupgrowth}{
    An integer with the maximum length groups an individual can jump in one step.
  }
  \item{reference_weight}{
    Reference weight. see formula for \code{\link{g3a_grow_length_weightjones}}.
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
    Integer order that actions will be run within model, see \code{\link{g3_action_order}}.
  }
  \item{transition_at}{
    Integer order that transition actions will be run within model, see \code{\link{g3_action_order}}.
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

    \deqn{ {{\Delta}L}_i = ( L_\infty - L_i )(1 - e^{-\kappa {\Delta t}}) }
    \describe{
      \item{\eqn{\Delta t}}{Length of current step as a proportion of the year, e.g. 0.25. See \code{cur_step_size} in \code{\link{g3a_time}}}
    }
  }
  \subsection{g3a_grow_weightsimple}{Returns a \link{formula} for use as \var{delta_wgt_f}:

    \deqn{ {{\Delta}W}_{i,j} = \alpha ( (L_i + {{\Delta}L}_j)^\beta - {L_i}^\beta ) }
    \describe{
      \item{\eqn{{\Delta}L}}{Vector of all possible length group increases i.e \code{0..maxlengthgroupgrowth}}
    }
  }

  \subsection{g3a_grow_length_multspec}{Returns a \link{formula} for use as \var{delta_len_f}:
    \deqn{ {{\Delta}L}_i = {\Delta t} p_0 L_i^{p_1} \psi_i (p_2 T + p_3) }
    \describe{
      \item{\eqn{p_x}}{Supplied parameters}
      \item{\eqn{\Delta t}}{Length of current step as a proportion of the year, e.g. 0.25. See \code{cur_step_size} in \code{\link{g3a_time}}}
      \item{\eqn{L_i}}{Current length}
      \item{\eqn{\psi_i}}{Feeding level of stock. See \code{\link{g3a_predate_catchability_predator}}}
      \item{\eqn{T}}{Temperature of current region}
    }
  }
  \subsection{g3a_grow_weight_multspec}{Returns a \link{formula} for use as \var{delta_wgt_f}:
    \deqn{ {{\Delta}W}_{i,j} = {\Delta t} p_4 {W_i}^p_5 (\psi_i - p_6) (p_7 T + p_8) }
    \describe{
      \item{\eqn{p_x}}{Supplied parameters}
      \item{\eqn{\Delta t}}{Length of current step as a proportion of the year, e.g. 0.25. See \code{cur_step_size} in \code{\link{g3a_time}}}
      \item{\eqn{W_i}}{Current mean weight}
      \item{\eqn{\psi_i}}{Feeding level of stock. See \code{\link{g3a_predate_catchability_predator}}}
      \item{\eqn{T}}{Temperature of current region}
    }
    Note that the equation is not dependent on the change in length, the value will be the same for each \eqn{j}.
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

# Multspec growth - define a data frame with temperature
temperature <- g3_timeareadata(
    'temp',
    data.frame(year = 2000, step=c(1,2), temp=c(10, 14)),
    value_field = "temp" )

ms_growth_actions <- list(
  g3a_growmature(ling_imm, g3a_grow_impl_bbinom(
    g3a_grow_length_multspec(temperature = temperature),
    g3a_grow_weight_multspec(temperature = temperature),
    maxlengthgroupgrowth = 8 )),
  NULL)
}
