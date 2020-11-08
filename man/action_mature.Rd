\name{action_mature}
\alias{g3a_mature_constant}
\alias{g3a_mature}

\title{Gadget3 maturity action}
\description{
  Add maturity actions to a g3 model
}

\usage{
g3a_mature_constant(alpha = NULL, l50 = NA, beta = NULL, a50 = NA, gamma = NULL, 
    k50 = NA)

g3a_mature(stock, maturity_f, output_stocks, output_ratios = rep(1/length(output_stocks), 
    times = length(output_stocks)), run_f = ~TRUE, run_at = 5, 
    transition_at = 7)

}

\arguments{
  \item{alpha}{
    A \link{formula} to substitute for \eqn{\alpha}.
  }
  \item{l50}{
    A \link{formula} to substitute for \eqn{l_{50}}. Must be defined if \var{alpha} is defined.
  }
  \item{beta}{
    A \link{formula} to substitute for \eqn{\beta}.
  }
  \item{a50}{
    A \link{formula} to substitute for \eqn{a_{50}}. Must be defined if \var{beta} is defined.
  }
  \item{gamma}{
    A \link{formula} to substitute for \eqn{\gamma}.
  }
  \item{k50}{
    A \link{formula} to substitute for \eqn{k_{50}}. Must be defined if \var{gamma} is defined.
  }
  \item{stock}{
    \code{\link{g3_stock}} to mature.
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

\details{
  Generally you would use \code{\link{g3a_growmature}}, which does both growth
  and maturity at the same time.

  A model can have any number of \code{g3a_mature} actions, so long as the
  calling arguments are different. For instance, \code{run_f = ~age == 5} and
  \code{run_f = ~age == 7}.
}

\value{
  \subsection{g3a_mature_constant}{A \link{formula} object with the following equation

    \deqn{ \frac{1}{ 1 + e^{-\alpha(l - l_{50}) -\beta(a - a_{50}) -\gamma(k - k_{50})}} }

    \describe{
      \item{\eqn{l}}{length of stock}
      \item{\eqn{l_{50}}}{length of stock when 50\% are mature}
      \item{\eqn{a}}{age of stock}
      \item{\eqn{a_{50}}}{age of stock when 50\% are mature}
      \item{\eqn{k}}{weight of stock}
      \item{\eqn{k_{50}}}{weight of stock when 50\% are mature}
    }
  }
  \subsection{g3a_mature}{An action (i.e. list of formula objects) that will, for the given \var{stock}...\enumerate{
    \item{Move any maturing individuals into temporary storage, \code{stock__transitioning_num} / \code{stock__transitioning_wgt}}
    \item{Move the contents of the temporary storage into \var{output_stocks}}
  }}
}

\seealso{
  \url{https://hafro.github.io/gadget2/userguide/chap-stock.html#sec:stockmature},
  \code{\link{g3a_growmature}},
  \code{\link{g3_stock}}
}

\examples{
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4))
ling_mat <- g3_stock('ling_mat', seq(20, 156, 4))

# Maturity for immature ling
maturity_action <- g3a_mature(ling_imm,
    maturity_f = g3a_mature_constant(
        alpha = ~0.001 * g3_param("ling.mat1"),
        l50 = ~g3_param("ling.mat2")),
        output_stocks = list(ling_mat))
}
