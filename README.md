
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crane <a href="https://insightsengineering.github.io/crane/"><img src='man/figures/logo.png' align="right" height="120" /></a>

<!-- start badges -->

[![Check
🛠](https://github.com/insightsengineering/crane/actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering/crane/actions/workflows/check.yaml)
[![Docs
📚](https://github.com/insightsengineering/crane/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/crane/)
[![Code Coverage
📔](https://raw.githubusercontent.com/insightsengineering/crane/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/crane/_xml_coverage_reports/data/main/coverage.xml)

[![CRAN
Version](https://www.r-pkg.org/badges/version/crane)](https://CRAN.R-project.org/package=crane)
[![Current
Version](https://img.shields.io/github/r-package/v/insightsengineering/crane/main?color=purple&label=Development%20Version)](https://github.com/insightsengineering/crane/tree/main)
<!-- end badges -->

The {crane} package provides supplementary functions to the {gtsummary}
specifically for trial reporting in the pharmaceutical industry.

## Installation

You can install {crane} with the following code.

``` r
install.packages("crane")
```

Install the development version with
`pak::pak("insightsengineering/crane")`

## Examples

The package exports a gtsummary theme for reporting at Roche. The theme
sets function defaults to meet reporting requirements at Roche, making
it simpler programmers to achieve the desired results without setting
too many arguments.

The package also exports a wrapper for the `gtsummary::tbl_summary()`
function with defaults more suited to reporting at Roche.

``` r
library(crane)
#> Loading required package: gtsummary
theme_gtsummary_roche()
#> Setting theme "Roche"

tbl <- trial |>
  tbl_roche_summary(by = trt, include = c(age, grade), nonmissing = "always")
```

<img src="man/figures/README-tbl_print_simple-1.png" width="40%" />

The package also exports functions for Roche adverse event reporting,
functions for shift tables, and others.

### Export gtsummary tables to reporter RTF/TXT

`gtsummary_to_reporter_clinical()` converts a `gtsummary` table (or plain
data frame) into clinical-style `reporter` outputs. It supports RTF/TXT
export, optional ARD/data RDS outputs, custom column labels/widths,
spanning headers, and pagination.

``` r
if (requireNamespace("reporter", quietly = TRUE)) {
  library(crane)
  library(gtsummary)

  tbl_demo <- trial |>
    dplyr::select(trt, age, grade) |>
    tbl_summary(by = trt) |>
    add_p()

  out_files <- gtsummary_to_reporter_clinical(
    gts_obj = tbl_demo,
    file_path = tempfile("clinical_demo_", fileext = ".rtf"),
    output_types = c("RTF", "TXT"),
    column_widths = "8|4|4",
    save_rds = FALSE
  )

  out_files
}
```
