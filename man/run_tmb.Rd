\name{run_tmb}
\alias{g3_to_tmb}
\alias{g3_tmb_adfun}
\alias{g3_tmb_par}
\alias{g3_tmb_lower}
\alias{g3_tmb_upper}
\alias{g3_tmb_relist}

\title{Gadget3 actions into TMB code}
\description{
  Turn g3 actions into CPP code that can be compiled using TMB
}

\usage{
g3_to_tmb(actions, trace = FALSE, strict = FALSE)

g3_tmb_adfun(cpp_code, parameters = attr(cpp_code, "parameter_template"),
    compile_flags = if (.Platform$OS.type == "windows")
                    c("-O3", "-march=native", "-fno-asynchronous-unwind-tables")
                    else c("-O3", "-flto", "-march=native"),
    work_dir = tempdir(),
    output_script = FALSE, ...)

g3_tmb_par(parameters)

g3_tmb_lower(parameters)

g3_tmb_upper(parameters)

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
    If TRUE, enable extra sanity checking in actions.
  }
  \item{cpp_code}{
    cpp_code as produced by \var{g3_to_tmb}.
  }
  \item{parameters}{
    Parameter table as produced by \code{attr(g3_to_tmb(...), 'parameter_template')},
    modified to provide initial conditions, etc.
  }
  \item{par}{
    Parameter vector, as produced by \code{g3_tmb_par()} or \code{nlminb(...)$par}.
  }
  \item{compile_flags}{
    List of extra flags to compile with, use e.g. "-g" to enable debugging output.
  }
  \item{work_dir}{
    Directory to write and compile .cpp files in. Defaults to R's current temporary directory
  }
  \item{output_script}{
    If \code{TRUE}, create a temporary R script that runs \link[TMB:MakeADFun]{MakeADFun}, and return the location.
    This can then be directly used with \link[TMB:gdbsource]{gdbsource} or \code{callr::rscript}.
  }
  \item{...}{
    Any other options handed directly to \link{MakeADFun}
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
  \subsection{g3_to_tmb}{A string of C++ code that can be used as an input to \var{g3_tmb_adfun}.}
  \subsection{g3_tmb_adfun}{An ADFun as produced by TMB's \link{MakeADFun}, or location of temporary script if \var{output_script} is TRUE}
  \subsection{g3_tmb_par}{Values extracted from \var{parameters} table converted into a vector of values for \code{obj$fn(par)} or \code{nlminb}}
  \subsection{g3_tmb_lower}{Lower bounds extracted from \var{parameters} table converted into a vector of values for \code{nlminb}}
  \subsection{g3_tmb_upper}{Lower bounds extracted from \var{parameters} table converted into a vector of values for \code{nlminb}}
  \subsection{g3_tmb_relist}{A value list for the \var{parameters} table, filled in with values from parameter vector}
}

\examples{
\dontshow{library(magrittr)}
ling_imm <- g3_stock('ling_imm', seq(20, 156, 4)) \%>\% g3s_age(3, 10)

initialconditions_action <- g3a_initialconditions_normalparam(
    ling_imm,
    # NB: area & age factor together (gadget2 just multiplied them)
    factor_f = ~g3_param("lingimm.init.scalar") *
        exp(-1 * (g3_param_vector("lingimm.M")[[age - 3 + 1]] + g3_param("ling.init.F")) * age) *
        g3_param_vector("lingimm.init")[[age - 3 + 1]],
    mean_f = ~g3_param("ling.Linf") * (1 - exp(-1 *
        (0.001 * g3_param("ling.k")) *
        (age - (1 + log(1 - g3_param("ling.recl") / g3_param("ling.Linf")) /
            (0.001 * g3_param("ling.k")))))),
    stddev_f = ~g3_param_vector("ling.init.sd")[[age]],
    alpha_f = ~g3_param("lingimm.walpha"),
    beta_f = ~g3_param("lingimm.wbeta"))

# Timekeeping action
time_action <- g3a_time(
    start_year = 2000,
    end_year = 2004,
    c(3, 3, 3, 3))

# Add our own custom action that will be added to model code
# (This is mostly to avoid problems in TMB)
custom_action <- list(~{
    nll <- g3_param('ling.k')
})

# Generate a model from the above 2 actions
# NB: Obviously in reality we'd need more actions
cpp <- g3_to_tmb(list(initialconditions_action, custom_action, time_action))

\dontrun{# Edit the resulting code
cpp <- edit(cpp)
}

# Set initial conditions for parameters
tmb_param <- attr(cpp, 'parameter_template')
tmb_param$value <- I(list(
    ling.init.F = 0.4,
    ling.Linf = 160,
    ling.k = 90,
    ling.recl = 12,
    ling.init.sd = rep(50.527220, 21),
    lingimm.init = rep(1, 15 - 5 + 1),
    lingimm.init.scalar = 200,
    lingimm.walpha = 2.27567436711055e-06,
    lingimm.wbeta = 3.20200445996187,
    lingimm.M = rep(0.15,3,10))[rownames(tmb_param)])

# Compile to a TMB ADFun
tmb <- g3_tmb_adfun(cpp, tmb_param)

\dontrun{# Build the model in an isolated R session w/debugger
 writeLines(TMB::gdbsource(g3_tmb_adfun(cpp, compile_flags = "-g", output_script = TRUE)))
}

# Perform a single run, using values in table
result <- tmb$fn(g3_tmb_par(tmb_param))

# perform optimisation based on parameter table
fit <- nlminb(tmb$par, tmb$fn, tmb$gr,
    upper = g3_tmb_upper(tmb_param),
    lower = g3_tmb_lower(tmb_param),
    control = list(iter.max=1))

# Go back to a list of parameters, suitable for the R version
param_list <- g3_tmb_relist(tmb_param, fit$par)

}
