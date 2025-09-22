
# liu.lab3.algorithms

<!-- badges: start -->
[![R-CMD-check](https://github.com/siyxu544/liu.lab3.algorithms/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/siyxu544/liu.lab3.algorithms/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of liu.lab3.algorithms is to provide R implementations of the Euclidean algorithm for finding the greatest common divisor and Dijkstra's algorithm for finding the shortest path in a directed graph. This package was created for Lab 3 in the course 732A94 Advanced R Programming at Linköping University.

## Installation

You can install the development version of liu.lab3.algorithms from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("siyxu544/liu.lab3.algorithms")
```

## Example

These are two basic examples:

``` r
library(liu.lab3.algorithms)
euclidean(100, 1000)
#> [1] 100

# The wiki_graph dataset is included in the package
dijkstra(wiki_graph, 1)
#> [1]  0  7  9 20 20 11
```

