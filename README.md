
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iDiagrams

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Installation

Install dev version with:

``` r
remotes::install.github("kismet303/iDiagrams")
```

## Create win-loss diagrams

Uses the DiagrammeR package to create win loss diagram.

``` r
library(iDiagrams)

data <- data.frame(endpoint = c(paste0("endpoint", 1:5)),
                   wins = c(5,5,5,5,5),
                   ties = c(50,40,30,20,10),
                   losses = c(5,5,5,5,5))

diagram <- wl_diagram(data, topline="N=10*6 = 60")
```

<img src="man/figures/ex_wl_plot.png" title="Example" alt="Example" width="50%" />
