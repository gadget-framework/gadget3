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

## Publications

* [Mini-CAPAM 2025 presentation on length-based processes](https://gadget-framework.github.io/publications/2025-12-11-miniCAPAM.html)
* [WGSAM 2025 presentation introducing gadget3](https://presentations.shuttlethread.com/2025-10-09-gadget3-mice)
* [ICES 2024 poster](https://gadget-framework.github.io/publications/2024-09-ices-asc-gadget3-evolution-of-gadget-modelling-framework.pdf)

## Installation

gadget3 is available on CRAN:

    > install.packages('gadget3')

You can also install the latest development version from github:

    > remotes::install_github('gadget-framework/gadget3')

## Documentation

The gadget3 documentation is [published online](https://gadget-framework.github.io/gadget3/).
To get started, read the [introductory vignette](https://gadget-framework.github.io/gadget3/articles/introduction-single-stock.html),
which walks through the process of building a model.

For some real-life examples of gadget3 models,
look in the [gadget-models](https://github.com/gadget-framework/gadget-models/) repository:

* [ling](https://github.com/gadget-framework/gadget-models/tree/master/06-ling/gadget3/00-setup)
* [Blue Ling](https://github.com/gadget-framework/gadget-models/tree/master/07-bling/gadget3/00-setup)

## Development of gadget3

The documentation for the latest development version is [published online](https://gadget-framework.github.io/gadget3/master/).

You can install it with:

```r
remotes::install_github("gadget-framework/gadget3")
```

Tests can be run with ``R CMD check``.
By default, tests are only run against the R backend for speed.
To run against the TMB backend, set the ``G3_TEST_TMB`` environment variable, with one of:

1. ``Sys.setenv(G3_TEST_TMB = 1) ; source('tests/test-action_grow.R')``
2. ``make``, which runs both test & integration tests with G3_TEST_TMB
3. ``make test G3_TEST_TMB="2"``, which runs all TMB tests

Releases are made with:

    make release NEW_VERSION=0.9-0
    git push --tags && git push

On success, upload the tarball to https://cran.r-project.org/submit.html

## Acknowledgements

Gadget3 has received funding from:

* Innviðasjóður Icelandic infrastructure grant #191774-0031
* Under One Cod #206740-051
* Fishing into the Future, Rannís grant of excellence #206967-051
* NORSUSTAIN project funded by the joint Danish, Greenlandic, and Faroese Presidency of the Nordic Council of Ministers
* Formas Swedish Research Council for Sustainable Development #2021-00826
