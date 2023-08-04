\name{step}
\alias{g3_step}
\alias{debug_label}
\alias{debug_trace}
\alias{stock_assert}
\alias{stock_ss}
\alias{stock_ssinv}
\alias{stock_switch}
\alias{stock_with}
\alias{stock_iterate}
\alias{stock_intersect}
\alias{stock_interact}
\alias{stock_prepend}

\title{G3 stock_* transformation functions}
\description{
  Additional meta-functions to help manage writing stock-handling actions.
}

\usage{
g3_step(step_f, recursing = FALSE, orig_env = environment(step_f))
}

\arguments{
  \item{step_f}{Input \link{formula} containing references to functions below}
  \item{recursing}{Only use default value}
  \item{orig_env}{Only use default value}
}

\details{
  All action producing functions will run their output through \code{g3_step}.
  This means that the functions described here will be available in any gadget3
  code.

  They handle translation of stock instance naming, so code can refer to e.g.
  \code{stock__num} without having to translate naming to the final stock name,
  and iterating over stock dimensions.
}

\section{debug_label}{
  Add a comment to the code to act as a label for that step, when producing an
  outline of the model. There shouldn't be more than one \code{debug_label}
  call in a step.

  Models compiled with \code{trace = TRUE} will print the resultant string to stdout.

  \subsection{Arguments}{
    Any number of character strings, or \link{g3_stock} variables. The latter
    will be replaced with the final name.
  }
}
\section{debug_trace}{
  Identical to \link{debug_label}, but not considered a "label", just a code
  comment, so any number of calls can be added.
}

\section{stock_assert}{
  \code{stock_assert(expression, message, message/stock-var, ...)}

  Assert that \var{expression} is true, if not abort with a message.
}

\section{stock_reshape}{
  \code{stock_reshape(dest_stock, expression)}

  Output \var{expression} with it's length structure reshaped to match
  \var{dest_stock}. The source stock is considered to be the first one
  found in \var{expression}

  How this is achieved depends on the difference. If the source and
  destination match then this is a no-op. Otherwise a transformation
  matrix is generated and included into the model.
}

\section{stock_ss}{
  \code{stock_ss(stock_var, [ dimname = override, dimname = override, ... ])}

  Subsets \var{stock_var} to get a length vector for the current iteration
  of \link{stock_iterate}(). If \var{dimname}s are supplied, then these
  dimensions overwritten with the suppled override, which could be a missing
  value, to preserve a dimension. See final example.

  Length is a missing value by default, i.e. we return length groups. To avoid
  this happening, set \code{length = default} to iterate over length too.
}

\section{stock_ssinv}{
  \code{stock_ssinv(stock_var, [ dimname, dimname, ... ])}

  like \link{stock_ss}(), but subset only the mentioned \var{dimname}s.
}

\section{stock_switch}{
  \code{stock_ss(stock, stock_name1 = expr, stock_name2 = expr, ... [ default ])}

  Switch based on name of \var{stock}, returning the relevant \var{expr} or
  \var{default}. If no default supplied, then an unknown stock is an error.

  \var{expr} is implicitly wrapped with \code{\link{stock_with}(stock, ...)},
  so any references to the stock variable will work. If only default is provided,
  then this is identical to calling \code{\link{stock_with}}.
}

\section{stock_with}{
  \code{stock_with(stock, expr)}

  Replaced with \var{expr} but with all stock variables of \var{stock} renamed
  with their final name. This is generally needed when not iterating over a
  stock, but e.g. zeroing or summing the whole thing.
}

\section{stock_iterate}{
  \code{stock_iterate(stock, expr)}

  Wrap \var{expr} with the code to iterate over vector dimensions in
  \var{stock}, accessed using \code{stock_ss(stock)}.

  Which dimensions are iterated over is decided based on the call to
  \code{stock_ss(stock)}. By default, stock_ss leaves length blank so will
  iterate over a length vector for each dimension.

  You can iterate over each value individually with the following:
  \code{stock_iterate(stock, stock_ss(stock, length = default) )}

  Current values for each dimension will be available as variables,
  e.g. \code{area}, \code{age}, and can be used in formulae.

}

\section{stock_intersect}{
  \code{stock_intersect(stock, expr)}

  Wrap \var{expr} with the code to intersect all dimensions with
  the dimensions of an outer \link{stock_iterate}().
}

\section{stock_interact}{
  \code{stock_interact(stock, expr, prefix = prefix)}

  Wrap \var{expr} with the code to interact with the dimensions of an outer
  \link{stock_iterate}(). Interact means to intersect over area, but try the
  combinatoral explosion of all other dimensions, i.e. what would make most
  sense when 2 stocks interact in a predator-prey relationship.

  Additional variables will be prefixed with \var{prefix}.

}

\section{stock_prepend}{
  \code{stock_prepend(stock, param_call, name_part = NULL)}

  Converts a \link{g3_param} or \link{g3_param_table} call, prefixing the
  parameter name with the stock name, and renaming any references to stock variables.
  If \var{name_part} given, will only add given part(s) of the stock name.

  Returns \var{param_call} with the additions made.
}

\value{
  \subsection{g3_step}{
    A \link{formula} object with references to above functions replaced.
  }
}

\examples{
\dontshow{library(magrittr)}
### debug_label
stock <- g3_stock('halibut', 1:10) \%>\% g3s_age(1,10)
prey_stock <- g3_stock('herring', 1:3) \%>\% g3s_age(1,3)
g3_step(~debug_trace("Zero ", stock, "-", prey_stock, " biomass-consuming counter"))

### stock_assert
stock <- g3_stock('halibut', 1:10) \%>\% g3s_age(1,10)
g3_step(~stock_assert(stock_with(stock, all(is.finite(stock__num))), stock, "__num became NaN/Inf"))

### stock_reshape
imm_stock <- g3_stock('halibut.imm', 1:5) \%>\% g3s_age(1,3)
g3_step(~stock_reshape(stock, stock_ss(imm_stock__transitioning_num)))

### stock_ss
stock <- g3_stock('halibut', 1:10) \%>\% g3s_age(1,10) \%>\% g3s_livesonareas(1)
stock__num <- g3_stock_instance(stock)
g3_step(~stock_iterate(stock, { x <- x + stock_ss(stock__num) }))
g3_step(~stock_ss(stock__num, area = 5))
g3_step(~stock_ss(stock__num, age = i + 1))
g3_step(~stock_ss(stock__num, area = , age = j))

### stock_ssinv
stock <- g3_stock('halibut', 1:10) \%>\% g3s_age(1,10) \%>\% g3s_livesonareas(1)
g3_step(~g3_step(~stock_ssinv(stock, 'age')))
g3_step(~g3_step(~stock_ssinv(stock, 'area')))

### stock_switch
stock <- g3_stock('halibut', 1:10) ; fleet_stock <- g3_fleet('igfs')
g3_step(~stock_switch(stock, halibut = 2, herring = 3, -1))
g3_step(~stock_switch(fleet_stock, halibut = 2, herring = 3, -1))
g3_step(~stock_switch(stock, halibut = stock__midlen, -1))

### stock_with
stock <- g3_stock('halibut', 1:10)
g3_step(~stock_with(stock, sum(stock__num)))

### stock_iterate
stock <- g3_stock('halibut', 1:10) \%>\% g3s_age(1,10)
g3_step(~stock_iterate(stock, x <- x + stock_ss(stock__num)))

### stock_intersect
stock <- g3_stock('halibut', 1:10) \%>\% g3s_age(1,10)
prey_stock <- g3_stock('herring', 1:3) \%>\% g3s_age(1,3)
g3_step(~stock_iterate(stock, stock_intersect(prey_stock, {
  x <- x + stock_ss(stock__num) + stock_ss(prey_stock__num)
})))

### stock_interact
stock <- g3_stock('halibut', 1:10) \%>\% g3s_age(1,10)
prey_stock <- g3_stock('herring', 1:3) \%>\% g3s_age(1,3)
g3_step(~stock_iterate(stock, stock_interact(prey_stock, {
  x <- x + stock_ss(stock__num) + stock_ss(prey_stock__num)
}, prefix = "prey" )))

### stock_param
stock <- g3_stock(c(species = 'halibut', sex = 'm', 'imm'), 1:10) \%>\% g3s_age(1,10)
g3_step(~stock_param(stock, 'K'))
g3_step(~stock_param(stock, 'K', name_part = 'species'))
g3_step(~stock_param(stock, 'K', name_part = c('species', 'sex')))

### stock_param_table
stock <- g3_stock(c(species = 'halibut', 'imm'), 1:10) \%>\% g3s_age(1,10)
g3_step(~stock_param_table(stock, 'K', expand.grid(year = 2000:2004)))
g3_step(~stock_param_table(stock, 'K', expand.grid(age = seq(stock__minage, stock__maxage))))
}
