
# tracer

<!-- badges: start -->
[![Build Status](https://travis-ci.org/WetRobot/tracer.png?branch=master)](https://travis-ci.org/WetRobot/tracer) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/WetRobot/tracer?branch=master&svg=true)](https://ci.appveyor.com/project/WetRobot/tracer) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tracer)](https://cran.r-project.org/package=tracer)
<!-- badges: end -->

tracer enables easy tracing of an iterative process such as Gibbs sampling.

## Installation

``` r
devtools::install_github("WetRobot/tracer")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tracer)

## beta-binomial Gibbs sampling, multi-core

g <- tracer(
  fixed = list(n = 20L),
  param_init = lapply(list(x = 10, theta = 0.5), as.array),
  step_funs = list(
    x = function(arg_list) {
      rbinom(
        n = 1,
        size = arg_list[["fixed"]][["n"]],
        prob = arg_list[["param_list"]][["theta"]]
      )
    },
    theta = function(arg_list) {
      rbeta(
        n = 1,
        shape1 = 2 + arg_list[["param_list"]][["x"]],
        shape2 = 4 + arg_list[["fixed"]][["n"]] - arg_list[["param_list"]][["x"]]
      )
    }
  ),
  n_chains = 1L,
  n_iter = 300,
  n_burn = 50L,
  n_skip = 4L,
  n_parallel = 1L
)
```
