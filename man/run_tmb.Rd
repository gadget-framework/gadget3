\name{run_tmb}
\alias{g3_to_tmb}
\alias{g3_tmb_adfun}

\title{Gadget3 actions into TMB code}
\description{
  Turn g3 actions into CPP code that can be compiled using TMB
}

\usage{
g3_to_tmb(actions, trace = FALSE)

g3_tmb_adfun(cpp_code, parameters = list(), cpp_path = tempfile(fileext = ".cpp"), 
    ...)

}

\arguments{
  \item{actions}{
    A list of actions (i.e. list of formula objects), as produced by \var{g3a_}* functions.
  }
  \item{trace}{
    If TRUE, turn all comments into print statements.
  }
  \item{cpp_code}{
    cpp_code as produced by \var{g3_to_tmb}.
  }
  \item{parameters}{
    Default parameters for the ADFun to use
  }
  \item{cpp_path}{
    Output path to write .cpp file to before compiling. Defaults to a tempfile
  }
  \item{...}{
    Any other options handed directly to \link{MakeADFun}
  }
}

\value{
  \subsection{g3_to_tmb}{A string of C++ code that can be used as an input to \var{g3_tmb_adfun}.}
  \subsection{g3_tmb_adfun}{An ADFun as produced by TMB's \link{MakeADFun}.}
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

# Compile to a TMB ADFun
tmb <- g3_tmb_adfun(cpp, list(
    ling.init.F = 0.4,
    ling.Linf = 160,
    ling.k = 90,
    ling.recl = 12,
    ling.init.sd = rep(50.527220, 21),
    lingimm.init = rep(1, 15 - 5 + 1),
    lingimm.init.scalar = 200,
    lingimm.walpha = 2.27567436711055e-06,
    lingimm.wbeta = 3.20200445996187,
    lingimm.M = rep(0.15,3,10)))
}
