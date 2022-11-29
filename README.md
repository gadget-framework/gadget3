# Gadget3: Globally applicable Area Disaggregated General Ecosystem Toolbox v3

[![R-CMD-check](https://github.com/gadget-framework/gadget3/workflows/R-CMD-check/badge.svg)](https://github.com/gadget-framework/gadget3/actions)

Gadget3 is a framework for producing marine ecosystem models. Gadget3 creates a
R or [TMB](http://kaskr.github.io/adcomp/_book/Introduction.html) objective
function for you, that can then be optimised with standard tools such as
``nlminb()``.

Gadget3 is primarily designed to produce models matching the behaviour of
[gadget2](https://gadget-framework.github.io/gadget2/), although implementing new modelling
systems would be possible

This package is part of suite of tools, including:

* [gadgetutils](https://github.com/gadget-framework/gadgetutils): Simplify common steps when setting up a gadget3 model
* [gadgetplots](https://github.com/gadget-framework/gadgetplots): Plot output from a gadget3 model
* [gadget2to3](https://github.com/gadget-framework/gadget2to3): Convert gadget2 model configuration to gadget3
* [mfdb](https://github.com/gadget-framework/mfdb): Marine ecosystem data management tool

## Installation

Install latest version from github:

    > remotes::install_github('gadget-framework/gadget3')

## Documentation

For a quick start, look at the [demo-ling] example in this repository.

To run, do:

    > source('demo-ling/setup.R', echo = TRUE)

For more information, read the [structure of a gadget3 model](https://gadget-framework.github.io/gadget3/articles/model_structure.html)
vignette.

## Development of gadget3

Tests can be run with ``R CMD check``.
By default, tests are only run against the R backend for speed.
To run against the TMB backend, set the ``G3_TEST_TMB`` environment variable, with:

1. ``Sys.setenv(G3_TEST_TMB = 1) ; source('tests/test-action_grow.R')``
2. ``make``, which runs both test & integration tests with G3_TEST_TMB

Releases are made with:

    make release NEW_VERSION=0.9-0
    git push --tags && git push

On success, upload the tarball to https://cran.r-project.org/submit.html
