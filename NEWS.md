# gadget3 0.12-0-999:

# gadget3 0.12-0:

## Bug fixes
* ``g3a_spawn()`` splits offspring into multiple stocks correctly

# gadget3 0.11-0:

## Bug fixes
* g3_to_r() output can be debugged in R-Studio
* Unknown stock names in likelihood data now an error
* ``g3l_distribution_surveyindices_*()`` can now be broken down by age without error

## New features
* Add ``g3_parameterized('x', by_year = 1998:2099)`` to override year range for parameters
* Add ``g3a_initialconditions_normalcv`` for initialconditions driven by a ``lencv`` parameter
* Add ``g3a_renewal_normalcv`` for initialconditions driven by a ``lencv`` parameter
* ``by_predator`` parameter for ``g3_parameterized``
* Defaults for ``g3_suitability_exponentiall50``
* Support cut() formatted character columns as well as factor columns in likelihood observation data.
* Add ``g3_init_val()`` helper, to replace ``gadgetutils::g3_init_guess()``
* Add ``g3_areas()`` helper to create numbered area vectors
* Add ``g3_distribution_preview()`` helper to see how observatation data will be converted to array
* ``period`` now optional for ``g3l_random_walk()``, ``g3l_random_dnorm()``
* ``g3l_bounds_penalty()`` can now generate bounds from actions which update automatically

## Breaking changes

* ``g3l_distribution_surveyindices_*`` now default to ``beta = 1``, set ``beta = NULL`` to restore previous behaviour.
* ``stock_param``() & ``stock_param_table``() replaced with ``stock_prepend``()
* Remove unused ``g3_to_tmb(adreport_re)`` option #60
* Naturalmortality ``M`` now by-age by default #113
* ``g3_suitability_andersenfleet()`` now uses ``by_predator = TRUE`` by default #133

# gadget3 0.10-0:

## New features

* Add ``g3_parameterized('x', by_step = TRUE)`` for seasonal parameters #115
* Support R factor levels as well as MFDB-style attributes for likelihood observation data.
  This means dplyr ``group_by(length = cut(...))`` can be used as well as MFDB for groupings #112
* Parameterised defaults for ``g3l_understocking``(), ``g3a_naturalmortality``(), ``g3a_renewal_normalparam``()
* Add ``g3a_renewal_vonb_t0``() & ``g3a_renewal_vonb_recl``(), with ``g3a_renewal_vonb``() being an alias to the latter

## Bug fixes

* g3s_modeltime() (used for history) now works when final_year_steps set
* Stop assuming 1-dimensional R arrays should be TMB vectors.

## Breaking changes

* Remove ``scale = 0.001`` from default ``K`` parameters in g3a_grow, g3a_renew_*
* ``g3a_renewal_vonb_t0``() is now the default mean_f for ``*_normalparam``()
* ``g3a_initialconditions_normalparam``() now offsets any ``age`` in ``mean_f`` (i.e. the VonB formula) by ``cur_step_size``,
  in effect running at step -1.

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
