# Gadget3

To run, do:

    > source("run.R", chdir = TRUE)

## Development

By default, tests are only run against the R backend for speed.
To run against the TMB backend, set the ``G3_TEST_TMB`` environment variable, with:

1. ``Sys.setenv(G3_TEST_TMB = 1) ; source('tests/test-action_grow.R')``
2. ``make test``, which turns it on by default
