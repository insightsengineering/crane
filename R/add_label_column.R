#' Add a Label Column to a Table
#'
#' @description
#' Add a new column, `label0`, to the left of a `{gtsummary}` table's `label`
#' column, and move part of the row content into it. This splits a single label
#' column into two: `label0` (the new label column) and `label`.
#'
#' The contents of `label0` are set by the `value` expression, which is
#' evaluated against the table body. Any per-row content can be placed there,
#' for example a stratum label on the first row only, a folded category, or a
#' constant such as `"n"`.
#'
#' This is the layout [tbl_shift()] builds internally with
#' `strata_location = "new_column"`, provided here as a standalone step so other
#' count and shift tables can reuse it instead of re-writing the
#' `modify_table_body()` / alignment / indentation sequence.
#'
#' @param x (`gtsummary`)\cr
#'   a gtsummary object.
#' @param value (`expression`)\cr
#'   expression evaluated with [dplyr::mutate()] against `x$table_body` to fill
#'   the new `label0` column. May reference any column in `x$table_body` (e.g.
#'   `label`, `row_type`) and [dplyr::row_number()]. `NA` rows render blank.
#' @param header (`string`)\cr
#'   header for the new `label0` column. Default is `"Value"`.
#' @param label_header (`string`)\cr
#'   header for the existing `label` column. Default is `NULL`, which leaves the
#'   current `label` header unchanged.
#' @param .after (`string`)\cr
#'   name of the column after which `label0` is placed. Default is `NULL`, which
#'   places `label0` directly before `label` (the [tbl_shift()] layout, value
#'   column on the left). Use `.after = "label"` to place it on the right.
#' @param indent (`integer`)\cr
#'   indentation applied to the `label` column via [gtsummary::modify_indent()].
#'   Default is `0L`, matching [tbl_shift()], so value rows sit flush left once
#'   the stratum label moves to `label0`. Use `NULL` to leave the current
#'   `label` indentation unchanged (e.g. to keep a nested table's indentation).
#'
#' @returns a gtsummary object with a `label0` column added next to `label`.
#'
#' @seealso [tbl_shift()]
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' tbl <- gtsummary::trial |>
#'   select(trt, grade) |>
#'   gtsummary::tbl_summary(by = trt, include = grade) |>
#'   gtsummary::remove_row_type(type = "header")
#'
#' # place a stratum label on the first row, blank elsewhere
#' add_label_column(
#'   tbl,
#'   value = ifelse(dplyr::row_number() == 1L, "Grade", NA_character_),
#'   header = "Group"
#' )
#'
#' @export
add_label_column <- function(x,
                             value,
                             header = "Value",
                             label_header = NULL,
                             .after = NULL,
                             indent = 0L) {
  set_cli_abort_call()

  # checks ---------------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(value)
  check_class(x, "gtsummary")
  check_string(header)
  check_string(label_header, allow_empty = TRUE)
  check_string(.after, allow_empty = TRUE)
  check_scalar_integerish(indent, allow_empty = TRUE)

  value <- rlang::enquo(value)

  # build value column ---------------------------------------------------------
  # `label0` is computed from `value` in the table body and placed next to
  # `label` (before it by default, matching tbl_shift). Both label columns are
  # left-aligned; the `label` indent is optionally reset so value rows sit flush.
  x <- x |>
    gtsummary::modify_table_body(
      function(.x) {
        if (is.null(.after)) {
          rlang::inject(dplyr::mutate(.x, .before = "label", label0 = !!value))
        } else {
          rlang::inject(dplyr::mutate(.x, .after = all_of(.after), label0 = !!value))
        }
      }
    ) |>
    gtsummary::modify_column_alignment(columns = c("label", "label0"), align = "left") |>
    gtsummary::modify_header(label0 = header)

  # indent = NULL leaves any existing (e.g. nested) label indentation in place
  if (!is.null(indent)) {
    x <- gtsummary::modify_indent(x, columns = "label", indent = indent)
  }

  if (!is.null(label_header)) {
    x <- gtsummary::modify_header(x, label = label_header)
  }

  x
}
