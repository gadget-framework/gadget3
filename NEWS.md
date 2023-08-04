# gadget3 0.9-0-999:

## New features

* Add ``g3_parameterized('x', by_step = TRUE)`` for seasonal parameters #115
* Support R factor levels as well as MFDB-style attributes for likelihood observation data.
  This means dplyr ``group_by(length = cut(...))`` can be used as well as MFDB for groupings #112
* Parameterised defaults for ``g3l_understocking``(), ``g3a_naturalmortality``(), ``g3a_renewal_normalparam``()

## Bug fixes

* g3s_modeltime() (used for history) now works when final_year_steps set
* Stop assuming 1-dimensional R arrays should be TMB vectors.
* Code for ``g3_parameterized(by_age = TRUE)`` no longer falls outside the age loop.
  Specifically, ``g3a_renewal_normalparam(factor_f = g3_parameterized(by_age = TRUE))`` would generate nonsense code
  as ``factor`` would be defined outside the definition of the age loop variable.

## Breaking changes

* ``stock_param``() & ``stock_param_table``() replaced with ``stock_prepend``()

# gadget3 0.9-0:

## Bug fixes

* ``g3_suitability_andersen()`` now produces sensible values - https://github.com/gadget-framework/gadget3/issues/108
* ``g3a_age()`` now supports stocks with a single age (i.e. minage == maxage)

## New features

* Add ``recage`` parameter to ``g3a_renewal_vonb`` / ``g3a_renewal_initabund``
* Add ``g3_suitability_andersenfleet()``, a fleet-specialised andsersen suitability function.
* Make ``g3_is_stock()`` public
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
