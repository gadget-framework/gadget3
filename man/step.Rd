\name{step}
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
\alias{stock_param}
\alias{stock_param_table}

\title{G3 stock_* transformation functions}
\description{
  Additional meta-functions to help manage writing stock-handling actions.
}

\details{
  Whilst used as functions, these functions alter the code output of the model,
  rather than appearing directly. They make it easier to write code that can
  handle stocks generically without having to worry about renaming and stock
  dimensions.

  The transformation itself is managed by \code{g3_step}(), which all actions use.
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

  \subsection{Examples}{

\preformatted{> stock <- g3_stock('halibut', 1:10) |> g3s_age(1,10)
> prey_stock <- g3_stock('herring', 1:3) |> g3s_age(1,3)
>  gadget3:::g3_step(~debug_trace("Zero ", stock, "-", prey_stock, " biomass-consuming counter"))
~debug_trace("Zero halibut-herring biomass-consuming counter")}

  }
}

\section{debug_trace}{
  Identical to \link{debug_label}, but not considered a "label", just a code
  comment, so any number of calls can be added.
}

\section{stock_assert}{
  \code{stock_assert(expression, message, message/stock-var, ...)}

  Assert that \var{expression} is true, if not abort with a message.

  \subsection{Examples}{

\preformatted{> stock <- g3_stock('halibut', 1:10) |> g3s_age(1,10)
> gadget3:::g3_step(~stock_assert(stock_with(stock, all(is.finite(stock__num))), stock, "__num became NaN/Inf"))
~assert_msg(~all(is.finite(halibut__num)), "halibut__num became NaN/Inf")}

  }
}

\section{stock_reshape}{
  \code{stock_reshape(dest_stock, expression)}

  Output \var{expression} with it's length structure reshaped to match
  \var{dest_stock}. The source stock is considered to be the first one
  found in \var{expression}

  How this is achieved depends on the difference. If the source and
  destination match then this is a no-op. Otherwise a transformation
  matrix is generated and included into the model.

  \subsection{Examples}{
    \code{stock_reshape(output_stock, stock_ss(input_stock__transitioning_num))}
  }
}

\section{stock_ss}{
  \code{stock_ss(stock_var, [ dimname = override, dimname = override, ... ])}

  Subsets \var{stock_var} to get a length vector for the current iteration
  of \link{stock_iterate}(). If \var{dimname}s are supplied, then these
  dimensions overwritten with the suppled override, which could be a missing
  value, to preserve a dimension. See final example.

  \subsection{Examples}{

\preformatted{> stock <- g3_stock('halibut', 1:10) |> g3s_age(1,10)
> stock__num <- gadget3:::stock_instance(stock)
> gadget3:::g3_step(~stock_iterate(stock, { x <- x + stock_ss(stock__num) }))
~for (age in seq(halibut__minage, halibut__maxage, by = 1)) g3_with(
  halibut__age_idx := g3_idx(age - halibut__minage + 1L), {
    x <- x + stock__num[, halibut__age_idx]
  })}

\preformatted{> stock <- g3_stock('halibut', 1:10) |> g3s_age(1,10) |> g3s_livesonareas(1)
> stock__num <- gadget3:::stock_instance(stock)
> gadget3:::g3_step(~stock_ss(stock__num, area = 5)),
~stock__num[, stock__age_idx, 5]
> gadget3:::g3_step(~stock_ss(stock__num, age = i + 1))
~stock__num[, i + 1, stock__area_idx]
> gadget3:::g3_step(~stock_ss(stock__num, area = , age = j))
~stock__num[, j, ]}

  }
}

\section{stock_inv}{
  \code{stock_ss(stock_var, [ dimname, dimname, ... ])}

  like \link{stock_ss}(), but subset only the mentioned \var{dimname}s.

  \subsection{Examples}{

\preformatted{> stock <- g3_stock('halibut', 1:10) |> g3s_age(1,10) |> g3s_livesonareas(1)
> gadget3:::g3_step(~gadget3:::g3_step(~stock_ssinv(stock, 'age')))
~gadget3:::g3_step(~stock[, stock__age_idx, ])
> gadget3:::g3_step(~gadget3:::g3_step(~stock_ssinv(stock, 'area')))
~gadget3:::g3_step(~stock[, , stock__area_idx])}

  }
}

\section{stock_switch}{
  \code{stock_ss(stock, stock_name1 = expr, stock_name2 = expr, ... [ default ])}

  Switch based on name of \var{stock}, returning the relevant \var{expr} or
  \var{default}. If no default supplied, then an unknown stock is an error.

  \var{expr} is implicitly wrapped with \code{\link{stock_with}(stock, ...)},
  so any references to the stock variable will work. If only default is provided,
  then this is identical to calling \code{\link{stock_with}}.

  \subsection{Examples}{

\preformatted{> stock <- g3_stock('halibut', 1:10) ; fleet_stock <- g3_fleet('igfs')
> gadget3:::g3_step(~stock_switch(stock, halibut = 2, herring = 3, -1))
~2
> gadget3:::g3_step(~stock_switch(fleet_stock, halibut = 2, herring = 3, -1))
~-1
> gadget3:::g3_step(~stock_switch(stock, halibut = stock__midlen, -1))
~halibut__midlen}

  }
}

\section{stock_with}{
  \code{stock_with(stock, expr)}

  Replaced with \var{expr} but with all stock variables of \var{stock} renamed
  with their final name. This is generally needed when not iterating over a
  stock, but e.g. zeroing or summing the whole thing.

  \subsection{Examples}{

\preformatted{> stock <- g3_stock('halibut', 1:10)
> gadget3:::g3_step(~stock_with(stock, sum(stock__num)))
~sum(halibut__num)}

  }
}

\section{stock_iterate}{
  \code{stock_iterate(stock, expr)}

  Wrap \var{expr} with the code to iterate over each length vector in
  \var{stock}, accessed using \code{stock_ss(stock)}

  Current values for each dimension will be available as variables,
  e.g. \code{area}, \code{age}, and can be used in formulae.

  \subsection{Examples}{

\preformatted{> stock <- g3_stock('halibut', 1:10) |> g3s_age(1,10)
> gadget3:::g3_step(~stock_iterate(stock, x <- x + stock_ss(stock__num)))
~for (age in seq(halibut__minage, halibut__maxage, by = 1)) g3_with(`:=`(halibut__age_idx,
    g3_idx(age - halibut__minage + 1L)), x <- x + halibut__num[, halibut__age_idx])}

  }
}

\section{stock_intersect}{
  \code{stock_intersect(stock, expr)}

  Wrap \var{expr} with the code to intersect all dimensions with
  the dimensions of an outer \link{stock_iterate}().

  \subsection{Examples}{

\preformatted{> stock <- g3_stock('halibut', 1:10) |> g3s_age(1,10)
> prey_stock <- g3_stock('herring', 1:3) |> g3s_age(1,3)
> gadget3:::g3_step(~stock_iterate(stock, stock_intersect(prey_stock, x <- x + stock_ss(stock__num) + stock_ss(prey_stock__num) )))
~for (age in seq(halibut__minage, halibut__maxage, by = 1)) if (age >=
    herring__minage && age <= herring__maxage) g3_with(
      halibut__age_idx := g3_idx(age - halibut__minage + 1L),
      herring__age_idx := g3_idx(age - herring__minage + 1L), {
        x <- x + halibut__num[, halibut__age_idx] + herring__num[, herring__age_idx]
      })}

  }
}

\section{stock_interact}{
  \code{stock_interact(stock, expr, prefix = prefix)}

  Wrap \var{expr} with the code to interact with the dimensions of an outer
  \link{stock_iterate}(). Interact means to intersect over area, but try the
  combinatoral explosion of all other dimensions, i.e. what would make most
  sense when 2 stocks interact in a predator-prey relationship.

  Additional variables will be prefixed with \var{prefix}.

  \subsection{Examples}{

\preformatted{> stock <- g3_stock('halibut', 1:10) |> g3s_age(1,10)
> prey_stock <- g3_stock('herring', 1:3) |> g3s_age(1,3)
> gadget3:::g3_step(~stock_iterate(stock, stock_interact(prey_stock, {
    x <- x + stock_ss(stock__num) + stock_ss(prey_stock__num)}, prefix = "prey" )))
~for (age in seq(halibut__minage, halibut__maxage, by = 1)) g3_with(
  halibut__age_idx := g3_idx(age - halibut__minage + 1L), {
    for (prey_age in seq(herring__minage, herring__maxage, by = 1)) g3_with(
      herring__age_idx := g3_idx(prey_age - herring__minage + 1L), {
        x <- x + halibut__num[, halibut__age_idx] + herring__num[, herring__age_idx]
      }
  }))}

  }
}

\section{stock_param}{
  \code{stock_param(stock, name, name_part = NULL, ...)}

  Convert to a call to \link{g3_param}, prefixing the \var{name} with the stock name.
  If \var{name_part} given, only use given part(s) of the stock name.

  All other args passed through to \link{g3_param}.

  \subsection{Examples}{

\preformatted{> stock <- g3_stock(c(species = 'halibut', sex = 'm', 'imm'), 1:10) |> g3s_age(1,10)
> gadget3:::g3_step(~stock_param(stock, 'K'))
~g3_param("halibut_m_imm.K")
> gadget3:::g3_step(~stock_param(stock, 'K', name_part = 'species'))
~g3_param("halibut.K")
> gadget3:::g3_step(~stock_param(stock, 'K', name_part = c('species', 'sex')))
~g3_param("halibut_m.K")}
  }
}

\section{stock_param_table}{
  \code{stock_param_table(stock, name, name_part = NULL, table_defn, ...)}

  Convert to a call to \link{g3_param_table}, prefixing the \var{name} with the stock name.
  If \var{name_part} given, only use given part(s) of the stock name.

  The \var{table_defn} will have any instances of \var{stock} replaced using \link{stock_with}().

  All other args passed through to \link{g3_param_table}.

  \subsection{Examples}{

\preformatted{> stock <- g3_stock(c(species = 'halibut', 'imm'), 1:10) |> g3s_age(1,10)
> gadget3:::g3_step(~stock_param_table(stock, 'K', expand.grid(year = 2000:2004)))
~g3_param_table("halibut_imm.K", expand.grid(year = 2000:2004))
> gadget3:::g3_step(~stock_param_table(stock, 'K', expand.grid(age = seq(stock__minage, stock__maxage))))
~g3_param_table("halibut_imm.K", expand.grid(age = seq(halibut_imm__minage,
    halibut_imm__maxage)))}
  }
}
