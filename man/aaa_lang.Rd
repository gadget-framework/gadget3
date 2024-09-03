\name{aaa_lang}
\alias{g3_native}
\alias{g3_global_formula}
\concept{G3 internals}

\title{Gadget3 language utilities}
\description{
  Produce objects with special meaning to gadget3
}

\usage{
g3_native(r, cpp, depends = c())
g3_global_formula(f = quote(noop), init_val = NULL)
}

\arguments{
  \item{r}{
    An R \link{function} to decorate with a 'C++' equivalent
  }
  \item{cpp}{Either: \enumerate{
    \item{A character string containing the 'C++' equivalent as a Lambda function}
    \item{A character string containing 'C++' function template definition, calling the function \code{ __fn__}}
    \item{A \link{list} of type-casts to use when calling an equivalently named native function}
  }}
  \item{depends}{
    A \link{list} of string names of dependent functions.
    The content of this and the initial \code{[]} for any Lambda function should match.
  }
  \item{f}{
    An optional \link{formula} to modify the content of a globablly-defined variable
  }
  \item{init_val}{
    An optiona \link{formula} to set the initial value of a globally-defined variable
  }
}

\details{
  These functions are generally for gadget3 development, but made available so actions can be produced outside the package.
}

\value{
  \subsection{g3_native}{
    Returns a function that can be used in formulas for both R and TMB-based models.
  }

  \subsection{g3_global_formula}{
    Returns a \link{formula} that will be defined globally, and this can preserve state across timesteps.
  }
}

\examples{
# The definition of g3_env$ratio_add_vec looks like:
eg_ratio_add_vec <- g3_native(r = function(orig_vec, orig_amount,
                                        new_vec, new_amount) {
    ((orig_vec * orig_amount + new_vec * new_amount)
      /
    avoid_zero_vec(orig_amount + new_amount))
}, cpp = '[](vector<Type> orig_vec, vector<Type> orig_amount,
                            vector<Type> new_vec, vector<Type> new_amount)
                            -> vector<Type> {
    return (orig_vec * orig_amount + new_vec * new_amount)
             /
           avoid_zero_vec(orig_amount + new_amount);
}', depends = c('avoid_zero_vec'))
# eg_ratio_add_vec() can then be used in formulas, both in R & TMB.

# Define a random walk action, using g3_global_formula to keep track of
# previous value. NB: my_randomwalk_prevrec must be unique in a model
random_walk_action <- g3_formula(quote({
    if (cur_time > 0) nll <- nll + dnorm(x, stock__prevrec, 1, 1)
    my_randomwalk_prevrec <- x
}), x = 'TODO', my_randomwalk_prevrec = g3_global_formula(init_val = 0.0))

}
