#' Wrap Text in a gtsummary Table Column
#'
#' @description
#' Wraps text within a specific column of a `gtsummary` table to prevent layout
#' engine issues during document generation (e.g., squished final columns).
#'
#' @param x (`gtsummary`)\cr
#'   A `gtsummary` object.
#' @param width (`integer`)\cr
#'   The maximum number of characters before a line break is inserted. Defaults
#'   to 50.
#' @param column (`character`)\cr
#'   The name of the column in `x$table_body` to apply the text wrapping.
#'   Defaults to `"label"`, which is typically the first column.
#'
#' @return A modified `gtsummary` object with text wrapping applied.
#'
#' @examples
#' library(gtsummary)
#'
#' # Create a table with intentionally long labels
#' tbl <- trial |>
#'   dplyr::mutate(
#'     trt = factor(
#'       trt,
#'       labels = c("Drug A: A very long string that will wrap",
#'                  "Drug B: Another extremely long string to test")
#'     )
#'   ) |>
#'   tbl_summary(by = trt, include = age)
#'
#' # Wrap the label column at 20 characters
#' tbl_wrapped <- wrap_column_text(tbl, width = 20)
#'
#' @importFrom stringr str_wrap
#' @export
wrap_column_text <- function(x, width = 50, column = "label") {
  # Halt execution early if the input isn't a gtsummary object,
  # preventing confusing downstream errors when modifying list elements.
  if (!inherits(x, "gtsummary")) {
    cli::cli_abort("Argument {.arg x} must be a {.cls gtsummary} object.")
  }

  # Verify the column exists in the internal table_body to ensure we are
  # targeting valid data structure before attempting to apply stringr functions.
  if (!column %in% colnames(x$table_body)) {
    cli::cli_abort("Column {.val {column}} not found in {.code x$table_body}.")
  }

  # Physically insert newlines to limit text width. This proactively fixes
  # rendering engines (like Word) trying to stretch the first column too wide.
  x$table_body[[column]] <- stringr::str_wrap(
    x$table_body[[column]],
    width = width
  )

  x
}