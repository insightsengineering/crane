#' Nested Stacking
#'
#' @inheritParams gtsummary::tbl_stack
#' @param group_header (`list`)\cr
#'   list of character vectors, where the character vectors are the nested headers,
#'   e.g.`list(c("Table 1", "Sub-table a"), c("Table 2", "Sub-table b"))` would
#'   result in the first tbl being nested under headers `c("Table 1", "Sub-table a")`,
#'   and the second tbl nested under `c("Table 2", "Sub-table b")`.
#'
#' @returns a gtsummary table
#' @export
#'
#' @examples
#' tbl <- tbl_summary(trial, include = age)
#'
#' tbl_nested_stack(
#'   list(tbl, tbl),
#'   group_header = list(c("table 1", "table a"),
#'                       c("table 2", "table b"))
#' )
tbl_nested_stack <- function(tbls, group_header, quiet = TRUE) {
  browser()
  # TODO: check inputs

  # first non-hidden column
  first_non_hidden_col <- "label" # TODO: Generalize this, also the column must be character (which it will be in all the default tables)
  depth <- length(group_header[[1]]) # TODO: check consistency of header depths for all tbls

  # TODO: add check that `tbl_indent_id` column doesn't already exist OR generalize the code so that these functions can be run more than once
  # add headers with their associated `tbl_indent_id`
  for (i in seq_along(tbls)) {
    # define indent ID for tbls
    tbls[[i]]$table_body$tbl_indent_id <- depth + 1L

    # indent the innermost table
    tbls[[i]]$table_styling$indent$n_spaces <- tbls[[i]]$table_styling$indent$n_spaces + depth * 4L

    # add nesting header rows
    tbls[[i]]$table_body <-
      dplyr::bind_rows(
        dplyr::tibble(
          tbl_indent_id = seq_len(depth),
          "{first_non_hidden_col}" := group_header[[i]]
        ),
        tbls[[i]]$table_body
      )
  }

  # stack the tbls
  tbl <- gtsummary::tbl_stack(tbls = tbls, quiet = quiet)

  # cycle over the depth and indenting nesting headers
  for (d in seq_len(depth)) {
    tbl <- tbl |>
      gtsummary::modify_column_indent(
        columns = all_of(first_non_hidden_col),
        rows = !!rlang::expr(.data$tbl_indent_id == !!d),
        indent = (d - 1L) * 4L
        )
  }

  tbl
}
