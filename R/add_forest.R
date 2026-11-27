#' Add a Forest Plot Column to a gtsummary Table
#'
#' This function adds a forest plot column to a gtsummary table, typically produced
#' by [tbl_roche_subgroups()]. The forest plot visualizes estimates and confidence intervals
#' for each subgroup in the table. The table is rendered with `flextable`.
#'
#' @param x (`gtsummary`)\cr
#'   A gtsummary table with estimates and confidence intervals in the table body. Usually produced
#'   by [tbl_roche_subgroups()].
#' @param estimate ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Estimate column name.
#' @param conf_low ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Confidence interval lower bound column name.
#' @param conf_high ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'  Confidence interval upper bound column name.
#' @param pvalue ([`tidy-select`][dplyr::dplyr_tidy_select], optional)\cr
#'  P-value column name. Point sizes in the forest plot will be scaled
#'  according to p-value (smaller p-values = smaller points). `NULL` to turn this off.
#' @param after ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'  Column name after which the forest plot column will be added. Default is after
#'  the p-value column.
#' @param header_spaces `r lifecycle::badge("deprecated")`\cr Previously the number
#'   of spacer characters used to separate the two treatment labels in the forest
#'   plot header. The header is now drawn on the same scale as the plots, so the
#'   labels are positioned geometrically and this argument has no effect.
#' @param table_engine `r lifecycle::badge("deprecated")`\cr crane renders tables
#'   with flextable only, so this argument has no effect. The `"gt"` engine was
#'   removed in crane 0.4.1 and gives an error.
#' @param row_height (`numeric`)\cr
#'  Height of a one-line body row, in inches. Rows whose text wraps get one
#'  extra line height per extra line.
#' @param pagesize (`string`, optional)\cr
#'  The citril page size the table is made for: `"L6"` to `"L10"` (A4
#'  landscape) or `"P6"` to `"P10"` (A4 portrait), the number being the font
#'  size. The table gets that font size and fills the page's text width.
#' @param total_width (`numeric`, optional)\cr
#'  Table width in inches, instead of the page's text width. It must hold the
#'  forest column's minimum width (1.5in) plus the cell padding of the other
#'  columns. With neither `pagesize` nor `total_width`, the columns keep their
#'  widths (autofit).
#'
#' @details
#' Each body row gets an exact height, and the forest plot is drawn at those
#' heights as one picture in a merged cell, so each CI lines up with its row
#' and the reference lines are continuous. If the whole table (titles, column
#' labels, rows and footnotes) does not fit the page (A4 landscape without
#' `pagesize`), each row gets its own picture, as a merged cell cannot break
#' across pages. On a page too narrow for the table, the text columns wrap;
#' words break, with a warning, only when not even the longest ones fit.
#'
#' The header picture shows the two treatment labels either side of the line
#' at 1. When the forest column is too narrow for them, a warning gives the
#' width they need.
#'
#' The pictures are drawn for the table's widths and fonts: after changing
#' them, e.g. with `citril::decorate_tlg()` or `citril::set_column_widths()`,
#' call [adjust_forest_widths()]. Add, remove or reorder body rows in the
#' gtsummary table, before `add_forest()`.
#'
#' The picture paragraphs use the "header" paragraph style of the Word
#' template, which the default document of `flextable::save_as_docx()` lacks:
#' export with `citril::export_tlg()`, or add the style with
#' `officer::docx_set_paragraph_style()`.
#'
#' @return a `tbl_forest`/`flextable` object with an added forest plot column. The plots are
#'   embedded as images, so drawing the table for PDF, PNG, or SVG export (e.g.
#'   with `flextable::gen_grob()` or `flextable::save_as_image()`) requires the
#'   `{magick}` package; without it the forest plot column is left blank and
#'   `add_forest()` warns once per session.
#'
#' @note
#' Output from `{tlg.templates}` errors when exported to PDF; export it to
#' docx, PNG, or SVG instead.
#'
#' @seealso [adjust_forest_widths()] to fit the forest plot to new column widths.
#'
#' @examples
#' # Simple example ------------------------------------------------------------
#' trial |>
#'   select(age, marker, grade, response) |>
#'   tbl_uvregression(
#'     y = response,
#'     method = glm,
#'     method.args = list(family = binomial),
#'     exponentiate = TRUE,
#'     hide_n = TRUE
#'   ) |>
#'   modify_column_merge(
#'     pattern = "{estimate} (95% CI {ci}; {p.value})",
#'     rows = !is.na(estimate)
#'   ) |>
#'   modify_header(estimate = "**Odds Ratio**") |>
#'   add_forest()
#'
#' # Realistic example ---------------------------------------------------------
#' \donttest{
#' if (requireNamespace("broom.helpers", quietly = TRUE)) {
#'   trial |>
#'     tbl_roche_subgroups(
#'       rsp = "response",
#'       by = "trt",
#'       subgroups = c("grade"),
#'       ~ glm(response ~ trt, data = .x) |>
#'         gtsummary::tbl_regression(
#'           show_single_row = trt,
#'           exponentiate = TRUE # , tidy_fun = broom.helpers::tidy_parameters
#'         )
#'     ) |>
#'     add_forest(pvalue = starts_with("p.value"), pagesize = "L8")
#' }
#' }
#' @export
add_forest <- function(x,
                       estimate = starts_with("estimate"),
                       conf_low = starts_with("conf.low"), conf_high = starts_with("conf.high"),
                       pvalue = starts_with("p.value"),
                       after = starts_with("p.value"),
                       header_spaces = lifecycle::deprecated(),
                       table_engine = lifecycle::deprecated(),
                       row_height = 0.24,
                       pagesize = NULL,
                       total_width = NULL) {
  set_cli_abort_call()
  # drawing and measuring open and close devices, which makes another one active
  device <- grDevices::dev.cur()
  on.exit(.forest_reactivate(device), add = TRUE)
  if (lifecycle::is_present(header_spaces)) {
    lifecycle::deprecate_warn(
      "0.4.1",
      "crane::add_forest(header_spaces)",
      details = paste(
        "The forest plot header is now drawn on the same scale as the plots,",
        "so the treatment labels are positioned geometrically."
      )
    )
  }
  check_not_missing(x)
  check_not_missing(estimate)
  check_not_missing(conf_low)
  cards::process_selectors(
    x$table_body,
    estimate = {{ estimate }}, conf_low = {{ conf_low }}, conf_high = {{ conf_high }},
    pvalue = {{ pvalue }}, after = {{ after }}
  )
  check_scalar(estimate)
  check_scalar(conf_low)
  check_scalar(conf_high)
  check_scalar(pvalue, allow_empty = TRUE)
  check_scalar(after)
  .forest_check_inches(row_height)
  .check_pagesize(pagesize)
  .forest_check_inches(total_width, allow_null = TRUE)

  # 1. SETUP DEFAULTS ----------------------------------------------------------
  # crane renders with flextable only: "gt" errors, other values than "flextable" too (a typo is not ignored)
  if (lifecycle::is_present(table_engine)) {
    if (identical(table_engine, "gt")) {
      lifecycle::deprecate_stop(
        "0.4.1",
        "crane::add_forest(table_engine = 'no longer accepts \"gt\"')",
        details = "crane renders tables with flextable only. Drop the argument to use the default."
      )
    }
    rlang::arg_match(table_engine, values = "flextable", error_call = get_cli_abort_call())
    lifecycle::deprecate_warn(
      "0.4.1",
      "crane::add_forest(table_engine)",
      details = "crane renders tables with flextable only, so the argument has no effect."
    )
  }

  .warn_if_no_magick()
  page <- .forest_page(pagesize)
  if (!is.null(total_width)) .forest_warn_wider(total_width, pagesize)

  # one device measures the text and the axis for the whole call
  .forest_measuring({
    # 2. PLOT DATA ---------------------------------------------------------------
    forest <- .forest_inputs(x, estimate, conf_low, conf_high, pvalue, row_height, pagesize)

    # 3. BUILD TABLE -------------------------------------------------------------
    out <- x |>
      gtsummary::modify_table_body(~ .x |>
        dplyr::add_row() |>
        dplyr::mutate(ggplot = NA, .after = dplyr::all_of(after))) |>
      gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
      gtsummary::modify_header(ggplot = " ") |>
      gtsummary::as_flex_table() |>
      flextable::line_spacing(space = 0.8, part = "body") |>
      .forest_picture_par(part = "body") |>
      flextable::valign(valign = "center", part = "body") |>
      flextable::align(j = "ggplot", align = "center", part = "header") |>
      flextable::padding(padding.top = 0, part = "body") |>
      flextable::padding(padding.bottom = 0, part = "body") |>
      # default 5pt L/R padding would push the fixed-width image out of its cell
      flextable::padding(j = "ggplot", padding.left = 0, padding.right = 0, part = "all") |>
      flextable::width(j = "ggplot", width = forest$width[["natural"]]) |>
      # picture top = row top; the text stays centered on the plot's CI line
      flextable::valign(j = "ggplot", valign = "top", part = "body") |>
      # gen_grob() drops a body row whose cells are all empty: keep the axis row
      # "\u00a0" is non-ascii accepted symbol for non-breaking space
      flextable::mk_par(i = nrow(x$table_body) + 1, j = 1, value = flextable::as_paragraph("\u00a0"), part = "body")
    label_row <- flextable::nrow_part(out, "header")
    if (!is.null(forest$header)) out <- .forest_picture_par(out, i = label_row, part = "header")
    # all column labels (and the forest header) end on the same line, above the header rule
    out <- flextable::valign(out, i = label_row, valign = "bottom", part = "header")
    # the body rows the plots are drawn for, to detect later row edits
    forest$rows <- .forest_row_values(out$body$dataset, setdiff(names(out$body$dataset), forest$col_key))

    # the page's font size, as decorate_tlg(pagesize) sets it, then widths that fill the page
    if (!is.null(page)) {
      out <- out |>
        flextable::fontsize(size = page$font_size, part = "body") |>
        flextable::fontsize(size = page$font_size, part = "header")
    }
    if (!is.null(total_width)) .forest_check_room(out, total_width, forest)
    table_width <- total_width %||% page$width
    if (!is.null(table_width)) out <- .forest_fit_widths(out, table_width, forest, label_row)

    # 4. DRAW FOREST PLOT ---------------------------------------------------------
    # exact row heights, each plot drawn at its row's height; adjust_forest_widths() runs this again
    attr(out, "forest") <- forest
    class(out) <- c("tbl_forest", class(out))
    .forest_refresh(out, header_row = if (!is.null(forest$header)) label_row)
  })
}

.is_na_or_chr <- function(x, i, estimate, conf_low, conf_high) {
  any(c(
    is.na(x$table_body[[estimate]][i]),
    is.na(x$table_body[[conf_low]][i]),
    is.na(x$table_body[[conf_high]][i]),
    is.character(x$table_body[[estimate]][i]),
    is.character(x$table_body[[conf_low]][i]),
    is.character(x$table_body[[conf_high]][i]),
    x$table_body[[estimate]][i] > 999.99,
    x$table_body[[conf_high]][i] > 999.99,
    x$table_body[[conf_low]][i] > 999.99 # Needed?
  ))
}
