\name{run_tmb}
\alias{g3_to_tmb}
\alias{g3_tmb_adfun}
\alias{g3_tmb_par}
\alias{g3_tmb_lower}
\alias{g3_tmb_upper}
\alias{g3_tmb_parscale}
\alias{g3_tmb_relist}
\concept{G3 compilation}

\title{Gadget3 actions into TMB code}
\description{
  Turn g3 actions into CPP code that can be compiled using TMB
}

\usage{
g3_to_tmb(actions, trace = FALSE, strict = FALSE)

g3_tmb_adfun(
        cpp_code,
        parameters = attr(cpp_code, 'parameter_template'),
        compile_flags = getOption('gadget3.tmb.compile_flags', default =
            if (.Platform$OS.type == "windows") c("-O1", "-march=native")
            else c("-O3", "-flto=auto", "-march=native") ),
        work_dir = getOption('gadget3.tmb.work_dir', default = tempdir()),
        output_script = FALSE,
        compile_args = list(
            framework = getOption("gadget3.tmb.framework", default = "TMBad") ),
        ...)

g3_tmb_par(parameters, include_random = TRUE)

g3_tmb_lower(parameters)

g3_tmb_upper(parameters)

g3_tmb_parscale(parameters)

g3_tmb_relist(parameters, par)

}

\arguments{
  \item{actions}{
    A list of actions (i.e. list of formula objects), as produced by \var{g3a_}* functions.
  }
  \item{trace}{
    If TRUE, turn all comments into print statements.
  }
  \item{strict}{
    If TRUE, enable extra sanity checking in actions. Any invalid conditions
    (e.g. more/less fish after growth) will result in a warning.
  }
  \item{cpp_code}{
    cpp_code as produced by \var{g3_to_tmb}.
  }
  \item{parameters}{
    Parameter table as produced by \code{attr(g3_to_tmb(...), 'parameter_template')},
    modified to provide initial conditions, etc.
  }
  \item{par}{
    Parameter vector, as produced by one of \enumerate{
      \item{\code{nlminb(...)$par}}
      \item{\code{obj.fun$env$last.par}}
      \item{\code{g3_tmb_par()}}
    }
    The first will not include random parameters by default, the others will.
  }
  \item{include_random}{
    Should random parameters assumed to be part of par? Should be \code{TRUE}
    if using \code{obj.fun$fn}, \code{obj.fun$report} directly, e.g.
    \code{obj.fun$fn(g3_tmb_par(param_tbl))}. In other cases, \code{FALSE}.
  }
  \item{compile_flags}{
    List of extra flags to compile with, use e.g. "-g" to enable debugging output.
    Can be set with an option, e.g. \code{options(gadget3.tmb.compile_flags = c('-O0', '-g'))}
  }
  \item{compile_args}{
    Any other arguments to pass to \link[TMB:compile]{TMB::compile}
  }
  \item{work_dir}{
    Directory to write and compile .cpp files in. Defaults to R's current temporary directory
    Set this to preserve compiled output and re-use between R sessions if possible.
    Can be set with an option, e.g. \code{options(gadget3.tmb.work_dir = fs::path_abs('tmb-workdir'))}
  }
  \item{output_script}{
    If \code{TRUE}, create a temporary R script that runs \link[TMB:MakeADFun]{MakeADFun}, and return the location.
    This can then be directly used with \link[TMB:gdbsource]{gdbsource} or \code{callr::rscript}.
  }
  \item{...}{
    Any other options handed directly to \link[TMB:MakeADFun]{MakeADFun}
  }
}

\details{
  \subsection{g3_tmb_adfun}{
    \code{\link{g3_tmb_adfun}} will do both the \link[TMB:compile]{compile} and \link[TMB:MakeADFun]{MakeADFun}
    steps of making a model. If the code is identical to an already-loaded model then it
    won't be recompiled, so repeated calls to \link{g3_tmb_adfun} to change \var{parameters} are fast.

    If \link[TMB:MakeADFun]{MakeADFun} is crashing your R session, then you can use \var{output_script} to run
    in a separate R session. Use this with \link[TMB:gdbsource]{gdbsource} to debug your model.
  }
}

\value{
  \subsection{g3_to_tmb}{
    A string of C++ code that can be used as an input to \var{g3_tmb_adfun}, with the following attributes:
    \describe{
      \item{actions}{The original \var{actions} list given to the function}
      \item{model_data}{An environment containing data attached to the model}
      \item{parameter_template}{A data.frame to be filled in and used as \var{parameters} in the other \code{g3_tmb_*} functions}
    }
    Use e.g. \code{attr(cpp_code, 'parameter_template')} to retrieve them.
  }
  \subsection{g3_tmb_adfun}{An ADFun as produced by TMB's \link[TMB:MakeADFun]{MakeADFun}, or location of temporary script if \var{output_script} is TRUE}
  \subsection{g3_tmb_par}{Values extracted from \var{parameters} table converted into a vector of values for \code{obj$fn(par)} or \code{nlminb}}
  \subsection{g3_tmb_lower}{Lower bounds extracted from \var{parameters} table converted into a vector of values for \code{nlminb}. Random parameters are always excluded}
  \subsection{g3_tmb_upper}{Lower bounds extracted from \var{parameters} table converted into a vector of values for \code{nlminb}. Random parameters are always excluded}
  \subsection{g3_tmb_parscale}{Parscale extracted from \var{parameters} table, converted into a vector of values for \code{nlminb}. Random parameters are always excluded}
  \subsection{g3_tmb_relist}{
    The \var{parameters} table value column, but with optimised
    values replaced with contents of \var{par} vector.
    i.e. the inverse operation to \link{g3_tmb_par}.
    \var{par} can either include or discount random variables.
  }
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock(c(species = 'ling', 'imm'), seq(20, 156, 4)) \%>\% g3s_age(3, 10)

initialconditions_action <- g3a_initialconditions_normalparam(
    ling_imm,
    factor_f = g3a_renewal_initabund(by_stock_f = 'species'),
    by_stock = 'species',
    by_age = TRUE)

abundance_action <- g3l_abundancedistribution(
    'abundance',
    data.frame(year = 2000:2004, number = 100),
    stocks = list(ling_imm),
    function_f = g3l_distribution_sumofsquares())

# Timekeeping action
time_action <- g3a_time(
    start_year = 2000,
    end_year = 2004,
    c(3, 3, 3, 3))

# Generate a model from the above 2 actions
# NB: Obviously in reality we'd need more actions
cpp <- g3_to_tmb(list(initialconditions_action, abundance_action, time_action))

if (interactive()) {
  # Edit the resulting code
  cpp <- edit(cpp)
}

# Set initial conditions for parameters
attr(cpp, 'parameter_template') |>
    g3_init_val("project_years", 0) |>
    g3_init_val("ling.init.F", 0.4) |>
    g3_init_val("ling.Linf", 160) |>
    g3_init_val("ling.K", 90) |>
    g3_init_val("ling.t0", 0) |>
    g3_init_val("ling.init.sd.#", 50.527220) |>
    g3_init_val("ling_imm.init.#", 1, lower = 0, upper = 1000) |>
    g3_init_val("ling_imm.init.scalar", 200) |>
    g3_init_val("ling_imm.walpha", 2.275e-06) |>
    g3_init_val("ling_imm.wbeta", 3.2020) |>
    g3_init_val("ling_*.M.#", 0.15) |>
    identity() -> tmb_param

\donttest{\dontshow{comment("NB: Making / optimising a TMB function is slow")}

\dontshow{comment("NB: Github windows CI can't compile a model")}
if (!( nzchar(Sys.getenv('GITHUB_CI')) && .Platform$OS.type == "windows" )) {
  # Compile to a TMB ADFun
  tmb <- g3_tmb_adfun(cpp, tmb_param)
}

# NB: TMB::gdbsource() requires both "R" and "gdb" to be available
# NB: gdbsource hangs on windows - https://github.com/kaskr/adcomp/issues/385
if (all(nzchar(Sys.which(c('gdb', 'R')))) && .Platform$OS.type !="windows") {

  cpp_broken <- g3_to_tmb(list(
    initialconditions_action,
    abundance_action,
    g3_formula(quote( stop("This model is broken") )),
    time_action))

  # Build the model in an isolated R session w/debugger
  writeLines(TMB::gdbsource(g3_tmb_adfun(
      cpp_broken,
      compile_flags = "-g",
      output_script = TRUE)))

}

if (!( nzchar(Sys.getenv('GITHUB_CI')) && .Platform$OS.type == "windows" )) {
  # Perform a single run, using values in table
  result <- tmb$fn(g3_tmb_par(tmb_param))
}

if (!( nzchar(Sys.getenv('GITHUB_CI')) && .Platform$OS.type == "windows" )) {
  # perform optimisation using upper/lower/parscale from table
  fit <- optim(tmb$par, tmb$fn, tmb$gr,
      method = "L-BFGS-B",
      upper = g3_tmb_upper(tmb_param),
      lower = g3_tmb_lower(tmb_param),
      control = list(maxit=10, parscale=g3_tmb_parscale(tmb_param)))
}

if (!( nzchar(Sys.getenv('GITHUB_CI')) && .Platform$OS.type == "windows" )) {
  # perform optimisation without bounds
  fit <- optim(tmb$par, tmb$fn, tmb$gr)
}

if (!( nzchar(Sys.getenv('GITHUB_CI')) && .Platform$OS.type == "windows" )) {
  # Go back to a list of parameters, suitable for the R version
  # NB: This will not set the values for random parameters
  param_list <- g3_tmb_relist(tmb_param, fit$par)
}

if (!( nzchar(Sys.getenv('GITHUB_CI')) && .Platform$OS.type == "windows" )) {
  # Update parameters with values from last run, *including* random parameters.
  param_list <- g3_tmb_relist(tmb_param, tmb$env$last.par)
}

if (!( nzchar(Sys.getenv('GITHUB_CI')) && .Platform$OS.type == "windows" )) {
  # Rebuild, only including "Fun" (i.e. without auto-differentiation)
  # Result will only work for tmb$report
  tmb <- g3_tmb_adfun(cpp, tmb_param, type = "Fun")
  result <- tmb$report(g3_tmb_par(tmb_param))
}
}

}
