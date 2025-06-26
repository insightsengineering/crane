#' Deprecated functions
#'
#' `r lifecycle::badge('deprecated')`\cr
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

# v0.2.0 -----------------------------------------------------------------------
#' @rdname deprecated
#' @export
tbl_demographics <- function(..., nonmissing = "always") {
  lifecycle::deprecate_soft(
    "0.2.0",
    "crane::tbl_demographics()",
    "tbl_roche_summary()"
  )

  tbl_roche_summary(..., nonmissing = nonmissing)
}
