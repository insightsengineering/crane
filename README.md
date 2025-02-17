
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
#> Loading required package: gtsummary
theme_gtsummary_roche()
#> Setting theme "Roche"

tbl <- trial |>
  tbl_demographics(by = trt, include = c(age, grade)) |>
  add_blank_row()
```

<img src="man/figures/README-tbl_print_simple-1.png" width="55%" />

Each gtsummary table also contains the Analysis Results Dataset (ARD)

``` r
# extract ARD from table
gather_ard(tbl)
#> $tbl_summary
#> {cards} data frame: 66 x 12
#>    group1 group1_level variable variable_level stat_name stat_label  stat
#> 1     trt       Drug A    grade              I         n          n    35
#> 2     trt       Drug A    grade              I         N          N    98
#> 3     trt       Drug A    grade              I         p          % 0.357
#> 4     trt       Drug A    grade             II         n          n    32
#> 5     trt       Drug A    grade             II         N          N    98
#> 6     trt       Drug A    grade             II         p          % 0.327
#> 7     trt       Drug A    grade            III         n          n    31
#> 8     trt       Drug A    grade            III         N          N    98
#> 9     trt       Drug A    grade            III         p          % 0.316
#> 10    trt       Drug B    grade              I         n          n    33
#> ℹ 56 more rows
#> ℹ Use `print(n = ...)` to see more rows
#> ℹ 5 more variables: context, fmt_fn, warning, error, gts_column
```
