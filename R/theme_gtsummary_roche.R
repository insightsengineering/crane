#' Roche Theme
#'
#' @description
#' A gtsummary theme for Roche tables
#' - flextable- and gt-printed tables are styled with reduced padding and font size.
#' - Uses `label_roche_pvalue()` as the default formatting function for all p-values.
#' - Uses `label_roche_percent()` as the default formatting function for all percent values.
#' - Font size defaults are 8 points for all the table by the footers that are 7 points.
#' - If flextable-printed, header text is always bold.
#' - Border defaults to `flextable::fp_border_default(width = 0.5)`.
#' - The `add_overall(col_label)` default value has been updated.
#' - The results from `gtsummary::tbl_hierarchical()` and `gtsummary::tbl_hierarchical_count()`
#'   are now post-processed with `gtsummary::remove_footnote_header()`,
#'   `crane::modify_zero_recode()`, and `crane::modify_header_rm_md()`.
#'
#'
#' @inheritParams gtsummary::theme_gtsummary_printer
#' @inheritParams gtsummary::theme_gtsummary_compact
#'
#' @return theme list
#' @export
#'
#' @examples
#' theme_gtsummary_roche()
#'
#' tbl_roche_summary(
#'   trial,
#'   by = trt,
#'   include = c(age, grade),
#'   nonmissing = "always"
#' )
#'
#' reset_gtsummary_theme()
theme_gtsummary_roche <- function(font_size = NULL,
                                  print_engine = c("flextable", "gt", "kable", "kable_extra", "huxtable", "tibble"),
                                  set_theme = TRUE) {
  print_engine <- arg_match(print_engine)
  check_scalar_logical(set_theme)

  # {crane} defaults
  border <- flextable::fp_border_default(width = 0.5)

  # Initialization with compact gt options -------------------------------------
  lst_theme <- list("pkgwide-str:theme_name" = "Roche")

  # updating with some pharma-specific bits ------------------------------------
  lst_theme <- lst_theme |>
    append(
      list(
        "tbl_summary-fn:percent_fun" = label_roche_percent(),
        "pkgwide-fn:pvalue_fun" = label_roche_pvalue(),
        "add_overall.tbl_summary-arg:col_label" = "All Participants  \n(N = {gtsummary::style_number(N)})",
        "pkgwide-str:print_engine" = print_engine,
        "tbl_hierarchical-fn:addnl-fn-to-run" =
          \(x) {
            gtsummary::remove_footnote_header(x) |>
              gtsummary::modify_header(
                gtsummary::all_stat_cols() ~ "{level}  \n(N = {gtsummary::style_number(n)})"
              ) |>
              modify_zero_recode() |>
              modify_header_rm_md()
          },
        "tbl_hierarchical_count-fn:addnl-fn-to-run" =
          \(x) {
            gtsummary::remove_footnote_header(x) |>
              modify_zero_recode() |>
              modify_header_rm_md()
          }
      )
    )

  # {flextable} options --------------------------------------------------------
  lst_theme$`as_flex_table-lst:addl_cmds` <-
    c(
      lst_theme$`as_flex_table-lst:addl_cmds`,
      list(
        fontsize = list(
          rlang::expr(flextable::fontsize(size = !!(font_size %||% 8), part = "all")),
          rlang::expr(flextable::fontsize(size = !!((font_size %||% 8) - 1), part = "footer"))
        ),
        border = list(
          rlang::expr(flextable::border_outer(part = "body", border = !!border)),
          rlang::expr(flextable::border_outer(part = "header", border = !!border))
        ),
        valign = list( # valign only because it will append to to last commands
          rlang::expr(flextable::bold(bold = TRUE, part = "header")),
          rlang::expr(flextable::valign(valign = "top", part = "all")),
          rlang::expr(flextable::font(fontname = "Arial", part = "all")),
          rlang::expr(flextable::padding(padding.top = 0, part = "all")),
          rlang::expr(flextable::padding(padding.bottom = 0, part = "all")),
          rlang::expr(flextable::line_spacing(space = 1, part = "all")),
          rlang::expr(flextable::set_table_properties(layout = "autofit"))
        )
      )
    )

  # add a monospace font for gt ------------------------------------------------
  lst_theme$`as_gt-lst:addl_cmds` <-
    c(
      lst_theme$`as_gt-lst:addl_cmds`,
      list(
        cols_hide =
          list(
            rlang::expr(gt::opt_table_font(font = "arial")),
            rlang::expr(
              gt::tab_options(
                table.font.size = !!(font_size %||% 13),
                data_row.padding = gt::px(1),
                summary_row.padding = gt::px(1),
                grand_summary_row.padding = gt::px(1),
                footnotes.padding = gt::px(1),
                source_notes.padding = gt::px(1),
                row_group.padding = gt::px(1)
              )
            )
          )
      )
    )

  # finishing up ---------------------------------------------------------------
  if (set_theme == TRUE) gtsummary::set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}
