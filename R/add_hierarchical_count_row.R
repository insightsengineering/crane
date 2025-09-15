#' Add row with counts
#'
#' Typically used to add a row with overall AE counts to a table that primarily
#' displays AE rates.
#'
#' @param x (`gtsummary`)\cr
#'  a gtsummary table
#' @param label (`string`)\cr
#'  label for the new row
#' @param .before,.after (`integer`)\cr
#'  Row index where to add the new row. Default is after last row.
#' @param data_preprocess (`function` or `formula`)\cr
#'  a function that is applied to `x$inputs$data` before the total row counts
#'  are tabulated. Default is `identity`. Tidyverse formula shortcut notation
#'  for the function is accepted. See `rlang::as_function()` for details.
#'
#' @returns gtsummary table
#' @export
#'
#' @examples
#' # Example 1 ----------------------------------
#' cards::ADAE |>
#'   # subset the data for a shorter example table
#'   dplyr::slice(1:10) |>
#'   tbl_hierarchical(
#'     by = "TRTA",
#'     variables = AEDECOD,
#'     denominator = cards::ADSL,
#'     id = "USUBJID",
#'     overall_row = TRUE
#'   ) |>
#'   add_hierarchical_count_row(.after = 1L)
add_hierarchical_count_row <- function(x,
                                       label = "Overall total number of events",
                                       .before = NULL,
                                       .after = NULL,
                                       data_preprocess = identity) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, "gtsummary")
  check_string(label)
  check_scalar_integerish(.before, allow_empty = TRUE)
  check_scalar_integerish(.after, allow_empty = TRUE)

  # function to pre-process data frame
  data_preprocess <- as_function(data_preprocess, call = get_cli_abort_call())

  # create one row summary table -----------------------------------------------
  tbl_count_one <-
    x$inputs$data |>
    data_preprocess() |>
    dplyr::mutate(...row_count... = TRUE) |>
    gtsummary::tbl_summary(
      by = x$inputs$by,
      include = "...row_count...",
      type = list(...row_count... = "continuous"),
      statistic = list(...row_count... = "{sum}"),
      digits = list(...row_count... = label_roche_number()),
      label = list(...row_count... = label)
    )

  # add overall ----------------------------------------------------------------
  if ("add_overall" %in% names(x$call_list)) {
    tbl_count_one <- add_overall(tbl_count_one)
  }

  # add row to primary table ---------------------------------------------------
  x$table_body <-
    x$table_body |>
    dplyr::add_row(
      tbl_count_one$table_body[intersect(names(x$table_body), names(tbl_count_one$table_body))],
      .before = .before,
      .after = .after
    )

  # add count row ard and call -------------------------------------------------
  x$cards$add_hierarchical_count_row <-
    gtsummary::gather_ard(tbl_count_one) |>
    dplyr::bind_rows() |>
    dplyr::filter(.data$stat_name == "sum")
  x$call_list$add_hierarchical_count_row <- match.call()

  # return table ---------------------------------------------------------------
  x
}
