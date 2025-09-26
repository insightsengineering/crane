#' Creates null report
#'
#' This function creates a null report for tables or listings without any statistics.
#'
#' @examples
#' tbl_null_report()
#'
#' @export
#' @rdname tbl_null_report
tbl_null_report <- function() {
  set_cli_abort_call()

  # Create empty gtsummary object
  x <- gtsummary::as_gtsummary(data.frame(blank = character())) |>
    gtsummary::modify_header(
      blank = "Null Report: no observations met the reporting criteria for inclusion in this output."
    )

  # add class and attributes ---------------------------------------------------
  x <- structure(
    x,
    class = c("tbl_null_report", "gtsummary")
  )

  # add inputs as attribute
  attr(x, "inputs") <- as.list(environment())

  x
}
