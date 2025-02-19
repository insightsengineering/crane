
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crane

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/insightsengineering/crane/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/crane)
<!-- badges: end -->

The {crane} package provides supplementary functions to the {gtsummary}
specifically for trial reporting in the pharmaceutical industry.

## Installation

You can install the development version of crane from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("insightsengineering/crane")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(crane)
#> ── Attaching packages ──────────────────────────────────────────────────────────
#> ✔ crane     0.0.0.9010     ✔ gtsummary 2.1.0     
#> 
#> ▶ Setting Roche gtsummary theme. See ?theme_gtsummary_roche() (`?crane::theme_gtsummary_roche()`).

tbl <- trial |>
  tbl_demographics(by = trt, include = c(age, grade)) |>
  add_blank_row()
```

<img src="man/figures/README-tbl_print_simple-1.png" width="40%" />
