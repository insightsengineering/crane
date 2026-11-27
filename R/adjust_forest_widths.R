#' Fit the Forest Plot to the Table's Column Widths
#'
#' @description
#' Sets new column widths, if given, and redraws the forest plot of
#' [add_forest()] for the table as it is now, e.g. after
#' `citril::decorate_tlg()` added titles and footnotes and changed the fonts,
#' or after `citril::set_column_widths()`. Only the forest column is redrawn:
#' the decoration, other edits and every attribute of `x` are kept. The
#' pictures are redrawn only when they would change, or when their files are
#' gone (e.g. a table read back with [readRDS()] in a new session).
#'
#' @param x (`tbl_forest`)\cr
#'   A table made by [add_forest()].
#' @param widths (`numeric`)\cr
#'   Column widths as shares of `total_width`, one per column. Default is `NULL`,
#'   the current widths.
#' @param total_width (`numeric`)\cr
#'   Width of the whole table, in inches. Default is `NULL`, the current width.
#' @param page_height (`numeric`)\cr
#'   Text height of the page, in inches. Default is `NULL`, the height of the page
#'   size the table is made or decorated for.
#'
#' @details
#' With `widths` or `total_width`, the table gets a fixed layout, as with
#' `citril::set_column_widths()`. A table wider than its page warns.
#'
#' The body rows must be the ones the plots were drawn for: when rows were
#' added, removed or reordered after [add_forest()], it stops. Title and
#' footnote rows can change freely. Text in the body cells of the forest column
#' is dropped with a warning; a text label or footnote mark in its header is
#' kept.
#'
#' `attr(x, "forest")$width` gives the forest column's minimum and natural
#' width, `c(min = 1.5, natural = 2.5)` inches. When the header labels do not
#' fit the column, a warning gives the width they need.
#'
#' In HTML, each forest table gets its own CSS class, added to citril, so the
#' pictures keep their drawn size and several forest tables can share a page.
#'
#' @return `x`, with the widths set and the forest plot drawn for them.
#'
#' @seealso [add_forest()]
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("broom.helpers", quietly = TRUE)
#' tbl <- trial |>
#'   tbl_roche_subgroups(
#'     rsp = "response",
#'     by = "trt",
#'     subgroups = "grade",
#'     ~ glm(response ~ trt, data = .x) |>
#'       gtsummary::tbl_regression(show_single_row = trt, exponentiate = TRUE)
#'   ) |>
#'   add_forest(pagesize = "L8")
#'
#' # redraw after changing the widths
#' tbl |>
#'   flextable::width(j = "ggplot", width = 2) |>
#'   adjust_forest_widths()
#'
#' # or set them here: shares of 8in, the forest column twice as wide
#' adjust_forest_widths(tbl, widths = c(rep(1, length(tbl$col_keys) - 1), 2), total_width = 8)
#' @export
adjust_forest_widths <- function(x, widths = NULL, total_width = NULL, page_height = NULL) {
  set_cli_abort_call()
  # drawing and measuring open and close devices, which makes another one active
  device <- grDevices::dev.cur()
  on.exit(.forest_reactivate(device), add = TRUE)
  check_not_missing(x)
  .forest_check_table(x)
  .forest_check_widths(widths, n_col = length(x$col_keys))
  .forest_check_inches(total_width, allow_null = TRUE)
  .forest_check_inches(page_height, allow_null = TRUE)
  .forest_check_rows(x)

  if (!is.null(widths) || !is.null(total_width)) {
    shares <- rep_len(widths %||% x$body$colwidths, length(x$col_keys))
    x <- flextable::width(x, width = unname(shares / sum(shares) * (total_width %||% sum(x$body$colwidths))))
    # as citril::set_column_widths() does: Word and HTML keep the widths of a fixed layout only
    x <- .forest_table_properties(x, layout = "fixed")
  }
  # one device measures the text and the axis for the whole call
  .forest_measuring({
    pagesize <- .forest_pagesize(x)
    if (identical(x$properties$layout, "fixed")) {
      .forest_warn_wider(sum(x$body$colwidths), pagesize, given = !is.null(total_width))
    } else if (!is.null(pagesize)) {
      # Word gives an autofit table the widths its text needs
      key <- attr(x, "forest")$col_key
      need <- sum(.forest_min_widths(x, setdiff(x$col_keys, key), slack_in = 0)) + x$body$colwidths[[key]]
      .forest_warn_wider(need, pagesize, given = FALSE, autofit = TRUE)
    }
    .forest_refresh(x, page_height)
  })
}
