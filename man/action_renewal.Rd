\name{action_renewal}
\alias{g3a_renewal_vonb_recl}
\alias{g3a_renewal_vonb_t0}
\alias{g3a_renewal_vonb}
\alias{g3a_renewal_initabund}
\alias{g3a_initialconditions_normalparam}
\alias{g3a_initialconditions_normalcv}
\alias{g3a_renewal_normalparam}
\alias{g3a_renewal_normalcv}
\alias{g3a_otherfood}
\alias{g3a_otherfood_normalparam}
\alias{g3a_otherfood_normalcv}
\concept{G3 action}

\title{Gadget3 renewal actions}
\description{
  Add renewal / initialconditions to a g3 model
}

\usage{
g3a_renewal_vonb_recl(
        Linf = g3_parameterized('Linf', value = 1, by_stock = by_stock),
        K = g3_parameterized('K', value = 1, by_stock = by_stock),
        recl = g3_parameterized('recl', by_stock = by_stock),
        recage = g3_parameterized('recage', by_stock = FALSE, optimise = FALSE),
        by_stock = TRUE)

g3a_renewal_vonb_t0(
        Linf = g3_parameterized('Linf', value = 1, by_stock = by_stock),
        K = g3_parameterized('K', value = 1, by_stock = by_stock),
        t0 = g3_parameterized('t0', by_stock = by_stock),
        by_stock = TRUE)

g3a_renewal_initabund(
    scalar = g3_parameterized('init.scalar', value = 1, by_stock = by_stock),
    init = g3_parameterized('init', value = 1, by_stock = by_stock, by_age = TRUE),
    M = g3_parameterized('M', by_stock = by_stock, by_age = TRUE),
    init_F = g3_parameterized('init.F', by_stock = by_stock_f),
    recage = g3_parameterized('recage', by_stock = FALSE, optimise = FALSE),
    proportion_f = ~1,
    by_stock = TRUE,
    by_stock_f = FALSE)

############################# g3a_initialconditions

g3a_initialconditions_normalparam(
        stock,
        factor_f = g3a_renewal_initabund(by_stock = by_stock),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        stddev_f = g3_parameterized('init.sd', value = 10,
            by_stock = by_stock, by_age = by_age),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        age_offset = quote( cur_step_size ),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = ~cur_time == 0L,
        run_at = g3_action_order$initial)

g3a_initialconditions_normalcv(
        stock,
        factor_f = g3a_renewal_initabund(by_stock = by_stock),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        cv_f = g3_parameterized('lencv', by_stock = by_stock, value = 0.1,
            optimise = FALSE),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        age_offset = quote( cur_step_size ),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = ~cur_time == 0L,
        run_at = g3_action_order$initial)

############################# g3a_renewal

g3a_renewal_normalparam(
        stock,
        factor_f = g3_parameterized('rec',
            by_stock = by_stock,
            by_year = TRUE,
            scale = g3_parameterized(
                name = 'rec.scalar',
                by_stock = by_stock),
            ifmissing = g3_parameterized(
                name = 'rec.proj',
                optimise = FALSE,
                by_stock = by_stock )),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        stddev_f = g3_parameterized('rec.sd', value = 10, by_stock = by_stock),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        by_stock = TRUE,
        wgt_by_stock = TRUE,
        run_age = quote(stock__minage),
        run_projection = TRUE,
        run_step = 1,
        run_f = NULL,
        run_at = g3_action_order$renewal)

g3a_renewal_normalcv(
        stock,
        factor_f = g3_parameterized('rec',
            by_stock = by_stock,
            by_year = TRUE,
            scale = g3_parameterized(
                name = 'rec.scalar',
                by_stock = by_stock),
            ifmissing = g3_parameterized(
                name = 'rec.proj',
                optimise = FALSE,
                by_stock = by_stock )),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        cv_f = g3_parameterized('lencv', by_stock = by_stock, value = 0.1,
            optimise = FALSE),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        by_stock = TRUE,
        wgt_by_stock = TRUE,
        run_age = quote(stock__minage),
        run_projection = TRUE,
        run_step = 1,
        run_f = NULL,
        run_at = g3_action_order$renewal)

############################# g3a_otherfood

g3a_otherfood(
        stock,
        num_f = g3_parameterized('of_abund', by_year = TRUE, by_stock = by_stock,
            scale = g3_parameterized(
                'of_abund.step', value = 1, by_step = TRUE, by_stock = by_stock),
            ifmissing = "of_abund.proj" ),
        wgt_f = g3_parameterized('of_meanwgt', by_stock = by_stock),
        by_stock = TRUE,
        force_lengthvector = !any(grepl("__midlen$", all.vars(num_f))),
        run_f = quote( cur_time <= total_steps ),
        run_at = g3_action_order$initial)

g3a_otherfood_normalparam(
        stock,
        factor_f = g3_parameterized(
            'of_abund', by_year = TRUE, by_stock = by_stock,
            scale = g3_parameterized(
                'of_abund.step', value = 1, by_step = TRUE, by_stock = by_stock),
            ifmissing = "of_abund.proj" ),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        stddev_f = g3_parameterized('init.sd', value = 10,
            by_stock = by_stock, by_age = by_age),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = quote( cur_time <= total_steps ),
        run_at = g3_action_order$initial)

g3a_otherfood_normalcv(
        stock,
        factor_f = g3_parameterized(
            'of_abund', by_year = TRUE, by_stock = by_stock,
            scale = g3_parameterized(
                'of_abund.step', value = 1, by_step = TRUE, by_stock = by_stock),
            ifmissing = "of_abund.proj" ),
        mean_f = g3a_renewal_vonb_t0(by_stock = by_stock),
        cv_f = g3_parameterized('lencv', by_stock = by_stock, value = 0.1,
            optimise = FALSE),
        alpha_f = g3_parameterized('walpha', by_stock = wgt_by_stock),
        beta_f = g3_parameterized('wbeta', by_stock = wgt_by_stock),
        by_stock = TRUE,
        by_age = FALSE,
        wgt_by_stock = TRUE,
        run_f = quote( cur_time <= total_steps ),
        run_at = g3_action_order$initial)

}

\arguments{
  \item{stock}{
    The \code{\link{g3_stock}} to apply to
  }
  \item{num_f}{
    \link{formula} that produces a lengthgroup vector of number of individuals for the current age/area/... length group.
  }
  \item{wgt_f}{
    \link{formula} that produces a lenghgroup vector of mean weight for the current age/area/... length group.
  }
  \item{run_at}{
    Integer order that actions will be run within model, see \code{\link{g3_action_order}}.
  }
  \item{factor_f,mean_f,stddev_f,alpha_f,beta_f}{
    \link{formula} substituted into normalparam calcuations, see below.
  }
  \item{cv_f}{
    \link{formula} substituted into normalcv calcuations, basically \code{stddev_f = mean_f * cv_f}, see below.
  }
  \item{age_offset}{
    Replace \code{age} with \code{age - age_offset} in \var{mean_f}. Used to simulate initialconditions at time "-1".
  }
  \item{force_lengthvector}{
    Should we assume that \code{num_f} is a constant, and needs repeating enough times to turn into a length vector?
  }
  \item{run_age}{Age to run renewals for, used as \code{age == (run_age)} into default \var{run_f}}
  \item{run_projection}{Boolean. Run renewal in projection years? If false adds \code{!cur_year_projection} into default \var{run_f}}
  \item{run_step}{Which step to perform renewal in, or \code{NULL} for continuous renewal. Adds \code{cur_step == (run_step)} into default \var{run_f}}
  \item{run_f}{
    \link{formula} specifying a condition for running this action,
    For initialconditions defaults to first timestep.
    For renewal, the default is a combination of \var{run_age}, \var{run_step} & \var{run_projection}.
    For otherfood, the default is to always run, apart from when the model is ending.
  }
  \item{Linf,K,t0,recl}{\link{formula} substituted into vonb calcuations, see below.}
  \item{recage}{\link{formula} substituted into initial abundance and vonb calcuations, see below.}
  \item{proportion_f,scalar,init,M,init_F}{\link{formula} substituted into initial abundance calcuations, see below.}
  \item{by_stock,wgt_by_stock,by_stock_f,by_age}{Controls how parameters are grouped, see \code{\link{g3_parameterized}}}
}

\details{
  All of the following actions will renew stock in a model. The differences are when and what they apply to by default:

  \describe{
    \item{\code{g3a_initialconditions_*}}{Will run at the start of the model, building an inital state of all ages}
    \item{\code{g3a_renewal_*}}{Will run at every step but only for the minimal age, adding new recruits as an alternative to \code{\link{g3a_spawn}()}}
    \item{\code{g3a_otherfood_*}}{Will run at every step, replacing the previous state, creating a non-dynamic stock for predators to consume}
  }

  Specifying the quantities and mean-weights in each case works identically.

  A model can have any number of \code{g3a_renewal_*} actions, so long as the
  calling arguments are different. For instance, \code{run_f = ~age == 5} and
  \code{run_f = ~age == 7}.

  The \code{g3a_renewal_*} actions will define the following stock instance variables for \var{stock}:
  \describe{
    \item{\var{stock__renewalnum}}{Extra individuals added to the stock}
    \item{\var{stock__renewalwgt}}{Mean weight of added individuals}
  }
}

\value{
  \subsection{g3a_renewal_vonb_recl}{A \link{formula} object representing
    \deqn{ L_{\infty} (1 - e^{-\kappa (a - a_{0} + \frac{\log(1 - L_{0}/L_{\infty})}{\kappa} )}) }
    \describe{
      \item{\eqn{L_{\infty}}}{\var{Linf} argument, by default model parameter named \code{(stock).Linf}}
      \item{\eqn{\kappa}}{\var{K} argument, by default model parameter named \code{(stock).K}}
      \item{\eqn{L_{0}}}{\var{recl} argument, by default model parameter named \code{(stock).recl}}
      \item{\eqn{a_{0}}}{\var{recage} argument, by default model parameter named \code{recage}}
    }
    \strong{NB:} \code{\link{g3a_initialconditions_normalparam}} will replace \eqn{a} with \eqn{a - \Delta{t}}, see \var{age_offset}
  }
  \subsection{g3a_renewal_vonb_t0}{A \link{formula} object representing
    \deqn{ L_{\infty} (1 - e^{-\kappa (a - t_{0}) }) }
    \describe{
      \item{\eqn{L_{\infty}}}{\var{Linf} argument, by default model parameter named \code{(stock).Linf}}
      \item{\eqn{\kappa}}{\var{K} argument, by default model parameter named \code{(stock).K}}
      \item{\eqn{t_{0}}}{\var{t0} argument, by default model parameter named \code{(stock).t0}}
    }
    \strong{NB:} \code{\link{g3a_initialconditions_normalparam}} will replace \eqn{a} with \eqn{a - \Delta{t}}, see \var{age_offset}
  }
  \subsection{g3a_renewal_vonb}{An alias for \code{g3a_renewal_vonb_recl}()}
  \subsection{g3a_renewal_initabund}{A \link{formula} object representing
    \deqn{ p s_{0} s_{a} e^{-1 (M + F_{0}) (a - a_{0})} }
    \describe{
      \item{\eqn{s_{0}}}{\var{scalar} argument, by default model parameter named \code{(stock).init.scalar}}
      \item{\eqn{s_{a}}}{\var{init} argument, by default model parameter named \code{(stock).init.(age)}}
      \item{\eqn{M}}{\var{M} argument, by default model parameter named \code{(stock).M.(age)}}
      \item{\eqn{F_{0}}}{\var{init_F} argument, by default model parameter named \code{init.F}}
      \item{\eqn{a_{0}}}{\var{recage} argument, by default model parameter named \code{recage}}
      \item{p}{\var{proportion} argument, by default \code{1}}
    }
  }
  \subsection{g3a_otherfood}{
  An action (i.e. list of formula objects) that will, for the given \var{stock}, iterate over each
  area/age/etc. combination, and generate a lengthgroup vector of new individuals and weights using
  \var{num_f} and \var{wgt_f}.

  renewal will add fish to the existing stock,
  whereas initialconditions & otherfood will replace any previous values.
  }
  \subsection{g3a_initialconditions_normalparam / g3a_renewal_normalparam / g3a_otherfood_normalparam}{
    An action (i.e. list of formula objects) that will, for the given \var{stock}, iterate over each
    area/age/etc. combination, and generate new individuals.

    The following formulas are used to calculate abundance (\eqn{N}) and mean weight (\eqn{W}):

    \deqn{n = {dnorm}(L, \mu, \sigma)}
    \deqn{N = F 10000 \frac{n}{\sum n}}
    \deqn{W = \alpha L^{\beta}}
    \describe{
      \item{\eqn{L}}{Midlength of all length groups for current area/age/...}
      \item{\eqn{F}}{\var{factor_f} argument, by default output of \code{\link{g3a_renewal_initabund}}}
      \item{\eqn{\mu}}{\var{mean_f} argument, by default output of \code{\link{g3a_renewal_vonb_t0}}}
      \item{\eqn{\sigma}}{\var{stddev_f} argument, by default model parameter named \code{(stock).(init|rec).sd}}
      \item{\eqn{\alpha}}{\var{alpha_f} argument, by default model parameter named \code{(stock).walpha}}
      \item{\eqn{\beta}}{\var{beta_f} argument, by default model parameter named \code{(stock).wbeta}}
    }
  }
  \subsection{g3a_initialconditions_normalcv / g3a_renewal_normalcv / g3a_otherfood_normalcv}{
    An action (i.e. list of formula objects) that will, for the given \var{stock}, iterate over each
    area/age/etc. combination, and generate new individuals.

    The following formulas are used to calculate abundance (\eqn{N}) and mean weight (\eqn{W}):

    \deqn{n = {dnorm}(L, \mu, \mu * {CV})}
    \deqn{N = F 10000 \frac{n}{\sum n}}
    \deqn{W = \alpha L^{\beta}}
    \describe{
      \item{\eqn{L}}{Midlength of all length groups for current area/age/...}
      \item{\eqn{F}}{\var{factor_f} argument, by default output of \code{\link{g3a_renewal_initabund}}}
      \item{\eqn{\mu}}{\var{mean_f} argument, by default output of \code{\link{g3a_renewal_vonb_t0}}}
      \item{\eqn{CV}}{\var{cv_f} argument, by default model parameter named \code{(stock).lencv}}
      \item{\eqn{\alpha}}{\var{alpha_f} argument, by default model parameter named \code{(stock).walpha}}
      \item{\eqn{\beta}}{\var{beta_f} argument, by default model parameter named \code{(stock).wbeta}}
    }
  }
}

\seealso{
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-stock.html#sec:stockinitial},
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-stock.html#sec:stockrenew},
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-other.html#chap-other},
  \code{\link{g3_stock}}
}

\examples{
\dontshow{library(magrittr)}
stocks <- list(
    imm = g3_stock(c('ling', maturity = "imm"), seq(10, 100, 10)) \%>\% g3s_age(3, 7),
    mat = g3_stock(c('ling', maturity = "mat"), seq(10, 100, 10)) \%>\% g3s_age(5, 10) )

actions <- list(
    g3a_time(2000, 2000),
    g3a_initialconditions_normalcv(stocks$imm),
    g3a_initialconditions_normalcv(stocks$mat),
    NULL)
model_fn <- g3_to_r(c(actions, list(
    g3a_report_detail(actions) )))
attr(model_fn, 'parameter_template') |>
    g3_init_val("init.F", 0.4) |>
    g3_init_val("ling_imm.Linf", 80) |>
    g3_init_val("ling_mat.Linf", 160) |>
    g3_init_val("ling_*.K", 90) |>
    g3_init_val("ling_*.t0", 0) |>
    g3_init_val("ling_*.lencv", 0.1) |>
    g3_init_val("ling_imm.init.#", 3:7 * 100) |>
    g3_init_val("ling_mat.init.#", 5:10 * 200) |>
    g3_init_val("ling_*.init.scalar", 200) |>
    g3_init_val("ling_*.walpha", 2.275e-06) |>
    g3_init_val("ling_*.wbeta", 3.2020) |>
    g3_init_val("ling_*.M.#", 0.15) |>
    identity() -> params.in
r <- model_fn(params.in)

g3_array_plot(attr(r, "dstart_ling_imm__num")[,,time=1])
g3_array_plot(attr(r, "dstart_ling_mat__num")[,,time=1])

## Plots
par(mar = c(4,2,2,1), cex.main = 1)
curve(g3_eval(g3a_renewal_vonb_t0(Linf = 20, K = 0.8, t0 = 0), age = x),
    0, 10, col = 2, xlab = "age", main = "g3a_renewal_vonb_t0(Linf = 20, K = 0.8..1.4, t0 = 0)")
curve(g3_eval(g3a_renewal_vonb_t0(Linf = 20, K = 1.0, t0 = 0), age = x),
    0, 10, col = 1, add = TRUE)
curve(g3_eval(g3a_renewal_vonb_t0(Linf = 20, K = 1.2, t0 = 0), age = x),
    0, 10, col = 3, add = TRUE)
curve(g3_eval(g3a_renewal_vonb_t0(Linf = 20, K = 1.4, t0 = 0), age = x),
    0, 10, col = 4, add = TRUE)

## Otherfood
# "Otherfood" stocks are defined in a similar manner to any other stock
# Note that _normalparam & _normalcv need both length & age dimensions
other_wgt <- g3_stock('other_wgt', 0)
other_cv <- g3_stock('other_cv', seq(50, 100, by = 10)) \%>\% g3s_age(5,10)

actions <- list(
    g3a_time(2000, 2010),
    # Will get other_wgt.of_abund.1998.1, other_wgt.of_meanwgt parameters
    g3a_otherfood(other_wgt),
    # Use standard vonB parameters (Linf/K/t0) to define abundance
    g3a_otherfood_normalcv(other_cv),
    NULL)
model_fn <- g3_to_r(c(actions, list(
    g3a_report_detail(actions) )))
attr(model_fn, 'parameter_template') |>
    g3_init_val("other_cv.Linf", 80) |>
    g3_init_val("other_cv.K", 90) |>
    g3_init_val("other_cv.t0", 0) |>
    g3_init_val("other_cv.of_abund.#", 100:110) |>
    g3_init_val("other_wgt.of_abund.#", 100:110) |>
    g3_init_val("other_wgt.of_abund.step.#", 1) |>
    g3_init_val("other_cv.of_abund.proj", 80) |>
    g3_init_val("other_wgt.of_abund.proj", 70) |>
    g3_init_val("project_years", 5) |>
    identity() -> params.in
r <- model_fn(params.in)

g3_array_plot(t(attr(r, "dstart_other_wgt__num")))
g3_array_plot(t(attr(r, "dstart_other_cv__num")[,age="age7",]))
}
