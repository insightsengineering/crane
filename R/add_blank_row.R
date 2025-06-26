#' Add Blank Row
#'
#' @description
#' Add a blank row below each variable group.
#' A blank row will not be added to the bottom of the table.
#'
#' *NOTE*: For HTML flextable output (which includes the RStudio IDE Viewer),
#'         the blank rows do not render. But they will appear when the
#'         table is rendered to Word.
#'
#' @param x (`gtsummary`)\cr
#'   a 'gtsummary' table. The table must include a column named `'variable'`
#'   in `x$table_body`.
#'
#' @returns updated 'gtsummary' table
#' @export
#'
#' @examples
#' trial |>
#'   tbl_roche_summary(
#'     by = trt,
#'     include = c(age, marker, grade),
#'     nonmissing = "always"
#'   ) |>
#'   add_blank_row()
add_blank_row <- function(x) {
  set_cli_abort_call()
  # check inputs ---------------------------------------------------------------
  check_class(x, "gtsummary")
  if (!"variable" %in% names(x$table_body)) {
    cli::cli_abort(
      "The {.cls gtsummary} table passed in argument {.arg x} must have a {.val variable} column.",
      call = get_cli_abort_call()
    )
  }
  updated_call_list <- append(x$call_list, list(add_blank_row = match.call()))

  # add blank row --------------------------------------------------------------
  # get row indices where to add blank row
  row_indices <-
    x$table_body["variable"] |>
    dplyr::mutate(row_number = dplyr::row_number()) |>
    dplyr::filter(.by = "variable", dplyr::row_number() == dplyr::n()) |>
    deframe() |>
    rev() %>%
    `[`(-1) # don't add a blank row to the bottom of the table

  # cycle through the row indices and add a blank row after each
  for (i in seq_along(row_indices)) {
    x <- x |>
      gtsummary::modify_table_body(
        ~ .x |>
          dplyr::add_row(
            dplyr::tibble(variable = names(row_indices)[i]),
            .after = row_indices[i]
          )
      )
  }

  # return table ---------------------------------------------------------------
  x$call_list <- updated_call_list
  x
}
