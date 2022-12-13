# gadget3 0.8-4-999:

## Breaking changes

* ``g3l_distribution_*(transform_fs = ...)`` now happens before aggregation, not after.
  Any matrix used now has to be expressed in terms of the stock, not aggregated age.

## New features

* Support ``as.vector(array)`` in TMB formulas, so arrays can be used with TMB vectorized functions, e.g. ``pnorm(as.vector(ar[,1]))``.

# gadget3 0.8-0:

## New features

* Initial CRAN release
