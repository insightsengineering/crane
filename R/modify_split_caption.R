#' Label and Trim Split Tables
#'
#' @description
#' Post-process a table that was paginated with
#' [gtsummary::tbl_split_by_rows()] (via its `variable_level` argument). For each
#' page, a subtitle is built from the split level using a [glue()]
#' `pattern`, and the now-redundant split column is hidden.
#'
#' This is useful for any split `{gtsummary}` table, such as listings
#' ([tbl_listing()]) split by treatment, or baseline/change tables
#' ([tbl_baseline_chg()]) and shift tables ([tbl_shift()]) split by `PARAM`.
#'
#' The label is written to the native `variable_level` attribute rather than a
#' `{gtsummary}` caption, so decorators that read that attribute (e.g.
#' `citril::decorate_tlg()`) render it as a page subtitle. Writing a caption as
#' well would duplicate the text after decoration (once as the subtitle, once as
#' a separate centered caption).
#'
#' @param x (`tbl_split`, `list`, or `gtsummary`)\cr
#'   a table split with [gtsummary::tbl_split_by_rows()], the list of split
#'   tables, or a single split page.
#' @param spl_col (`string`)\cr
#'   name of the column the table was split by (the `variable_level` passed to
#'   [gtsummary::tbl_split_by_rows()]). Used to hide that column on each page
#'   when `hide_spl_col = TRUE`.
#' @param pattern (`string`)\cr
#'   a [glue()] pattern used to build each page's subtitle from the split level.
#'   The `{spl_level}` token is replaced by the split value. Defaults to
#'   `"Parameter: {spl_level}"`, matching parameter-split tables such as
#'   laboratory tables split by `PARAM`.
#' @param hide_spl_col (`flag`)\cr
#'   whether to hide `spl_col` on each split page, since it is redundant after a
#'   single-level split. Defaults to `TRUE`. Silently does nothing when the
#'   column is absent or already hidden.
#'
#' @returns an object of the same class as `x`, with the split subtitle set on
#'   the `variable_level` attribute (and the split column hidden) on each page.
#'
#' @seealso [tbl_listing()], [tbl_baseline_chg()], [tbl_shift()],
#'   [gtsummary::tbl_split_by_rows()]
#'
#' @examples
#' lst <- gtsummary::trial |>
#'   dplyr::select(trt, age, grade) |>
#'   dplyr::arrange(trt) |>
#'   tbl_listing(split_by_rows = list(variable_level = "trt"))
#'
#' # Split a listing by treatment, then subtitle each page and drop the column
#' modify_split_caption(lst, spl_col = "trt", pattern = "Treatment: {spl_level}")[[1]]
#'
#' # Keep the split column and use the default "Parameter:" subtitle
#' modify_split_caption(lst, spl_col = "trt", hide_spl_col = FALSE)[[1]]
#'
#' @export
modify_split_caption <- function(x,
                                 spl_col,
                                 pattern = "Parameter: {spl_level}",
                                 hide_spl_col = TRUE) {
  set_cli_abort_call()

  # checks ---------------------------------------------------------------------
  # argument checks run before the map so a list of split tables is validated
  # once, up front, rather than on each recursive call.
  check_not_missing(x)
  check_not_missing(spl_col)
  check_string(spl_col)
  check_string(pattern)
  check_scalar_logical(hide_spl_col)

  # map over a list of split tables --------------------------------------------
  if (is.list(x) && inherits(x[[1]], "gtsummary")) {
    return(
      map(
        x,
        modify_split_caption,
        spl_col = spl_col,
        pattern = pattern,
        hide_spl_col = hide_spl_col
      ) |>
        structure(class = class(x))
    )
  }

  check_class(x, "gtsummary")

  # build the split subtitle from the split level ------------------------------
  # `variable_level` is the gtsummary-native attribute set by
  # tbl_split_by_rows(variable_level = ); row-number splits do not set it, so
  # those pages are skipped silently.
  spl_level <- attr(x, "variable_level")
  if (!is_empty(spl_level)) {
    subtitle <- as.character(glue::glue(pattern))
    # store the formatted label back on `variable_level`; citril's
    # decorate_tlg() reads it and renders it as a subtitle row. We deliberately
    # do not set a gtsummary caption here: as_flex_table() would render it as a
    # separate (centered) caption and duplicate the subtitle after decoration.
    attr(x, "variable_level") <- subtitle
  }

  # hide the split column, if present and not already hidden --------------------
  if (isTRUE(hide_spl_col) && all(spl_col %in% x$table_styling$header$column)) {
    x <- gtsummary::modify_column_hide(x, columns = all_of(spl_col))
  }

  x
}
