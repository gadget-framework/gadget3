\name{suitability}
\alias{g3_suitability_exponentiall50}
\alias{g3_suitability_andersen}
\alias{g3_suitability_gamma}
\alias{g3_suitability_exponential}
\alias{g3_suitability_straightline}
\alias{g3_suitability_constant}
\alias{g3_suitability_richards}

\title{Gadget3 suitability formulae}
\description{
  Formula-returning functions describing predator/prey suitability relationships.
}

\usage{
g3_suitability_exponentiall50(alpha, l50)

g3_suitability_andersen(p0, p1, p2, p3 = p4, p4, p5 = ~pred_stock__midlen)

g3_suitability_gamma(alpha, beta, gamma)

g3_suitability_exponential(alpha, beta, gamma, delta)

g3_suitability_straightline(alpha, beta)

g3_suitability_constant(alpha)

g3_suitability_richards(p0, p1, p2, p3, p4)

}

\arguments{
  \item{alpha}{
    \link{formula} substituted into calcuations, see below.
  }
  \item{l50}{
    \link{formula} substituted into calcuations, see below.
  }
  \item{p0}{
    \link{formula} substituted into calcuations, see below.
  }
  \item{p1}{
    \link{formula} substituted into calcuations, see below.
  }
  \item{p2}{
    \link{formula} substituted into calcuations, see below.
  }
  \item{p3}{
    \link{formula} substituted into calcuations, see below.
  }
  \item{p4}{
    \link{formula} substituted into calcuations, see below.
  }
  \item{p5}{
    \link{formula} substituted into calcuations, see below.
  }
  \item{beta}{
    \link{formula} substituted into calcuations, see below.
  }
  \item{gamma}{
    \link{formula} substituted into calcuations, see below.
  }
  \item{delta}{
    \link{formula} substituted into calcuations, see below.
  }
}

\value{
  \subsection{g3_suitability_exponentiall50}{
    Returns a \link{formula} for use in predation function's \var{suitabilities} argument:

    \deqn{
      \frac{1}{ 1 + e^{-\alpha (l - l_{50})} }
    }
    \describe{
      \item{\eqn{l}}{Vector of prey midlength for each lengthgroup}
      \item{\eqn{l_{50}}}{Length of the prey with a 50\% probability of predation, from parameter \var{l50}}
    }
  }

  \subsection{g3_suitability_andersen}{
    Returns a \link{formula} for use in predation function's \var{suitabilities} argument:

    \deqn{
      p_0 + p_2 e^{-\frac{(\log\frac{p_5}{l} - p_1)^2}{p_4}}
    }
    \describe{
      \item{\eqn{L}}{Vector of predator midlength for each lengthgroup}
      \item{\eqn{l}}{Vector of prey midlength for each lengthgroup}
      \item{\eqn{p_0} .. \eqn{p_2}}{Function parameter \var{p0} .. \var{p2}}
      \item{\eqn{p_3}}{Currently unsupported. Should be used in place of \eqn{p_3} if \eqn{\log\frac{L}{l} <= p_1}}
      \item{\eqn{p_4}}{Function parameter \var{p4}}
      \item{\eqn{p_5}}{Function parameter \var{p5}, if unspecified uses \eqn{L}, Vector of predator midlength for each lengthgroup.}
    }

    NB: Specifying \eqn{p_5} is equivalent to using the \code{andersenfleet} function in gadget2.
  }

  \subsection{g3_suitability_gamma}{
    Returns a \link{formula} for use in predation function's \var{suitabilities} argument:

    \deqn{
      (\frac{l}{(\alpha - 1)\beta\gamma}) ^ {(\alpha - 1) e^{\alpha - 1 - \frac{l}{\beta\gamma}}}
    }
    \describe{
      \item{\eqn{l}}{Vector of prey midlength for each lengthgroup}
      \item{\eqn{\alpha}}{Function parameter \var{alpha}}
      \item{\eqn{\beta}}{Function parameter \var{beta}}
      \item{\eqn{\gamma}}{Function parameter \var{gamma}}
    }

    This is a suitability function that is more suitable for use when
    considering the predation by a fleet, where the parameter \eqn{\gamma}
    would represent the size of the mesh used by the fleet (specified in
    centimetres).
  }

  \subsection{g3_suitability_exponential}{
    Returns a \link{formula} for use in predation function's \var{suitabilities} argument:

    \deqn{
      \frac{\delta}{1 + e^{-\alpha - \beta l - \gamma L}}
    }
    \describe{
      \item{\eqn{L}}{Vector of predator midlength for each lengthgroup}
      \item{\eqn{l}}{Vector of prey midlength for each lengthgroup}
      \item{\eqn{\alpha}}{Function parameter \var{alpha}}
      \item{\eqn{\beta}}{Function parameter \var{beta}}
      \item{\eqn{\gamma}}{Function parameter \var{gamma}}
      \item{\eqn{\delta}}{Function parameter \var{delta}}
    }
  }

  \subsection{g3_suitability_straightline}{
    Returns a \link{formula} for use in predation function's \var{suitabilities} argument:

    \deqn{
      \alpha + \beta l
    }
    \describe{
      \item{\eqn{l}}{Vector of prey midlength for each lengthgroup}
      \item{\eqn{\alpha}}{Function parameter \var{alpha}}
      \item{\eqn{\beta}}{Function parameter \var{beta}}
    }
  }

  \subsection{g3_suitability_constant}{
    Returns a \link{formula} for use in predation function's \var{suitabilities} argument:

    \deqn{
      \alpha
    }
    \describe{
      \item{\eqn{\alpha}}{Function parameter \var{alpha}}
    }
  }

  \subsection{g3_suitability_richards}{
    Returns a \link{formula} for use in predation function's \var{suitabilities} argument:

    \deqn{
      {\big( \frac{p_3}{1 + e^{-p_0 - p_1 l - p_2 L}} \big)}^{\frac{1}{p_4}}
    }
    \describe{
      \item{\eqn{L}}{Vector of predator midlength for each lengthgroup}
      \item{\eqn{l}}{Vector of prey midlength for each lengthgroup}
      \item{\eqn{p_0} .. \eqn{p_4}}{Function parameter \var{p0} .. \var{p4}}
    }
    This is an extension to \link{g3_suitability_exponential}.
  }
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)
ling_mat <- g3_stock('ling_mat', seq(20, 156, 4)) \%>\% g3s_age(5, 15)
igfs <- g3_fleet('igfs')

igfs_landings <-
  structure(expand.grid(year=1990:1994, step=2, area=1, total_weight=1),
            area_group = list(`1` = 1))

# Generate a fleet predation action using g3_suitability_exponentiall50
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
