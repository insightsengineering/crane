#' Creates null report
#'
#' This function creates a null report for tables or listings without any statistics.
#'
#' @param label (`string`)\cr label to display in the header of the null report. It defaults to
#'   "Null Report: no observations met the reporting criteria for inclusion in this output."
#'
#' @examples
#' tbl_null_report(label = "No data available for the selected criteria.")
#'
#' @export
#' @rdname tbl_null_report
tbl_null_report <- function(
  label =
    "Null Report: no observations met the reporting criteria for inclusion in this output."
) {
  set_cli_abort_call()

  # Check input label ----------------------------------------------------------
  check_string(label)

  # Create empty gtsummary object ----------------------------------------------
  x <- gtsummary::as_gtsummary(data.frame(label = character())) |>
    gtsummary::modify_header(label = label)

  # add class and attributes ---------------------------------------------------
  x <- structure(
    x,
    class = c("tbl_null_report", "gtsummary")
  )

  x
}
