#' Deprecated functions
#'
#' `r lifecycle::badge('deprecated')`\cr
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @returns Warnings
#' @examples
#' NULL
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
    "0.3.2",
    "crane::g_lineplot()",
    "crane::gg_lineplot"
  )
}

#' @rdname deprecated
#' @export
g_lineplot_table <- function(...) {
  lifecycle::deprecate_soft(
    "0.3.2",
    "crane::g_lineplot_table()",
    "crane::annotate_gg()"
  )
}

#' @rdname deprecated
#' @export
preprocess_lineplot_data <- function(...) {
  lifecycle::deprecate_soft(
    "0.3.2",
    "crane::preprocess_lineplot_data()",
    "crane::annotate_gg()"
  )
}

#' @rdname deprecated
#' @export
add_overall.tbl_survfit_times <- function(x, ...) {
  lifecycle::deprecate_stop(
    when = "0.3.4", # Replace with the package version where this became defunct
    what = "add_overall()",
    details = paste(
      "Since we decoupled the architecture, a user wanting an overall column",
      "alongside stratified columns should simply fit an unstratified model,",
      "extract the dataframe, and use standard `dplyr::bind_rows()` with",
      "their stratified dataframe before pushing the combined output into",
      "`tbl_survfit_times()`."
    )
  )
}

#' @rdname deprecated
#' @export
add_difference_row.tbl_survfit_times <- function(x, ...) {
  lifecycle::deprecate_stop(
    when = "0.3.4", # Replace with the package version where this became defunct
    what = "add_difference_row()",
    with = "get_surv_diff_df()",
    details = paste(
      "`add_difference_row()` is defunct for `tbl_survfit_times`.",
      "Since the architecture was decoupled, the table object no longer",
      "contains the raw data required to calculate survival differences.",
      "Please use the new `get_surv_diff_df()` function to extract difference",
      "statistics, bind those rows to your survival dataframe using",
      "`dplyr::bind_rows()`, and then render the final `tbl_survfit_times()`."
    )
  )
}
