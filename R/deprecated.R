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

#' @rdname deprecated
#' @export
g_lineplot <- function(...) {
  lifecycle::deprecate_soft(
    "0.3.1.9017",
    "crane::g_lineplot()",
    "crane::gg_lineplot"
  )
}

#' @rdname deprecated
#' @export
g_lineplot_table <- function(...) {
  lifecycle::deprecate_soft(
    "0.3.1.9017",
    "crane::g_lineplot_table()",
    "crane::annotate_gg()"
  )
}

#' @rdname deprecated
#' @export
preprocess_lineplot_data <- function(...) {
  lifecycle::deprecate_soft(
    "0.3.1.9017",
    "crane::preprocess_lineplot_data()",
    "crane::annotate_gg()"
  )
}
