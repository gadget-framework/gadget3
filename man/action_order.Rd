\name{action_order}
\alias{g3_action_order}
\concept{G3 action}

\title{Standard gadget3 order of actions}
\description{
  Constant defining standard order of actions
}

\usage{
g3_action_order

}

\details{
  All gadget3 actions have a \var{run_at} parameter. This decides the point in
  the model that the action will happen relative to others.

  The defaults for these are set via \code{g3_action_order}.
}

\value{
  A named integer list
}

\seealso{
  \url{https://gadget-framework.github.io/gadget2/userguide/chap-order.html}
}

\examples{
# The default action order
unlist(g3_action_order)

# View single value
g3_action_order$age

}
