#' Regression Table for MMRM Models
#'
#' Creates a formatted regression table for Mixed Models for Repeated Measures
#' (MMRM) using [gtsummary::tbl_regression()]. This function provides a
#' convenient wrapper specifically for `mmrm` class objects.
#'
#' @param x (`mmrm`)\cr
#'   A fitted model object from the {mmrm} package.
#' @param ... Additional arguments passed to [gtsummary::tbl_regression()].
#'   Common arguments include `exponentiate`, `include`, `exclude`,
#'   `show_single_row`, `conf.level`, etc.
#'
#' @return A 'gtsummary' table object
#'
#' @details
#' This function is a wrapper around [gtsummary::tbl_regression()] specifically
#' designed for MMRM models. It relies on the tidy methods for `mmrm` objects
#' provided by the {mmrm} package or {broom.mixed} package.
#'
#' The {mmrm} package must be installed to use this function.
#'
#' @export
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("mmrm", quietly = TRUE)
#' library(mmrm)
#' 
#' # Fit an MMRM model using the FEV data
#' fit_mmrm <- mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = mmrm::fev_data
#' )
#'
#' # Create a regression table
#' tbl_mmrm(fit_mmrm)
#'
#' # With exponentiation (for log-transformed outcomes)
#' # tbl_mmrm(fit_mmrm, exponentiate = TRUE)
tbl_mmrm <- function(x, ...) {
  set_cli_abort_call()
  
  # Check that mmrm package is installed
  broom.helpers::.assert_package("mmrm", fn = "tbl_mmrm()")
  
  # Check that x is an mmrm object
  if (!inherits(x, "mmrm")) {
    cli::cli_abort(
      c(
        "The {.arg x} argument must be an object of class {.cls mmrm}.",
        "i" = "The provided object has class: {.cls {class(x)}}."
      ),
      call = get_cli_abort_call()
    )
  }
  
  # Check that tidy method exists for mmrm
  # Try to get the method to ensure it exists and is properly registered
  tidy_exists <- tryCatch(
    {
      getS3method("tidy", "mmrm", optional = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )
  
  if (!tidy_exists) {
    cli::cli_abort(
      c(
        "No {.fun tidy} method found for {.cls mmrm} objects.",
        "i" = "The {.pkg mmrm} package should provide tidy methods.",
        "i" = "Try updating the {.pkg mmrm} package to the latest version."
      ),
      call = get_cli_abort_call()
    )
  }
  
  # Use tbl_regression to create the table
  gtsummary::tbl_regression(x, ...)
}
