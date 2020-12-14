# Gadget3: Globally applicable Area Disaggregated General Ecosystem Toolbox v3

Gadget3 is a framework for producing marine ecosystem models. Gadget3 creates a
R or [TMB](http://kaskr.github.io/adcomp/_book/Introduction.html) objective
function for you, that can then be optimised with standard tools such as
``nlminb()``.

Gadget3 is primarily designed to produce models matching the behaviour of
[gadget2](https://hafro.github.io/gadget2), although implementing new modelling
systems would be possible

*NB:* Currently under active development.

## Installation

Install latest version from github:

    > remotes::install_github('lentinj/gadget3')

## Documentation

For a quick start, look at the [demo-ling] example in this repository.

To run, do:

    > source('demo-ling/setup.R', echo = TRUE)

For more information, read the [structure of a gadget3 model](https://lentinj.github.io/gadget3/articles/model_structure.html)
vignette.

## Development of gadget3

By default, tests are only run against the R backend for speed.
To run against the TMB backend, set the ``G3_TEST_TMB`` environment variable, with:

1. ``Sys.setenv(G3_TEST_TMB = 1) ; source('tests/test-action_grow.R')``
2. ``make test``, which turns it on by default
