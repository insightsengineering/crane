
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crane

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/insightsengineering/crane/graph/badge.svg)](https://app.codecov.io/gh/insightsengineering/crane)
<!-- badges: end -->

The {crane} package provides supplementary functions to the {gtsummary}
specifically for trial reporting in the pharmaceutical industry.

## Installation

You can install {crane} with the following code.

``` r
install.packages("crane")
```

Install the development version with
`pak::pak("insightsengineering/crane")`

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(crane)
#> ── Attaching packages ──────────────────────────────────────────────────────────
#> ✔ crane     0.1.0     ✔ gtsummary 2.1.0
#> 
#> ▶ Setting Roche gtsummary theme. See ?theme_gtsummary_roche() (`?crane::theme_gtsummary_roche()`).
theme_gtsummary_roche()
#> Setting theme "Roche"

tbl <- trial |>
  tbl_demographics(by = trt, include = c(age, grade)) |>
  add_blank_row()
```

<img src="man/figures/README-tbl_print_simple-1.png" width="40%" />
