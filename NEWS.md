# gadget3 0.8-4-999:

## New features

* ``g3_eval()``, to evaluate snippets of a gadget3 model.

## Breaking changes

* ``g3l_distribution_*(transform_fs = ...)`` now happens before aggregation, not after.
  Any matrix used now has to be expressed in terms of the stock, not aggregated age.

* ``g3_param_table()`` now returns NaN (and warns) on a missing value, instead of aborting.

* ``optim(g3_tmb_par(...))`` is now an error. When optimising, always use ``obj.fun$par``.

## New features

* Support ``as.vector(array)`` in TMB formulas, so arrays can be used with TMB vectorized functions, e.g. ``pnorm(as.vector(ar[,1]))``.

# gadget3 0.8-0:

## New features

* Initial CRAN release
