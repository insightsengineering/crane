#' Roche Theme
#'
#' @description
#' A gtsummary theme for Roche tables
#' - flextable- and gt-printed tables are styled with reduced padding and font size.
#' - Uses `label_roche_pvalue()` as the default formatting function for all p-values.
#' - Font size defaults are 8 points for all the table by the footers that are 7 points.
#' - Border defaults to `flextable::fp_border_default(width = 0.5)`.
#' - The `add_overall(col_label)` default value has been updated.
#'
#' @inheritParams gtsummary::theme_gtsummary_compact
#'
#' @return theme list
#' @export
#'
#' @examples
#' theme_gtsummary_roche()
#'
#' tbl_demographics(trial, by = trt, include = c(age, grade))
#'
#' reset_gtsummary_theme()
theme_gtsummary_roche <- function(set_theme = TRUE) {
  # {crane} defaults
  font_size <- 8
  font_size_gt <- 13 # gt counts size points differently
  border <- flextable::fp_border_default(width = 0.5)

  # Initialization with compact gt options -------------------------------------
  lst_theme <-
    list(
      "pkgwide-str:theme_name" = "Roche",
      # compact gt tables
      "as_gt-lst:addl_cmds" = list(
        tab_spanner = rlang::expr(
          gt::tab_options(
            table.font.size = !!font_size_gt,
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

  # updating with some pharma-specific bits ------------------------------------
  lst_theme <- lst_theme |>
    append(
      list(
        "tbl_summary-fn:percent_fun" = label_roche_percent(),
        "pkgwide-fn:pvalue_fun" = label_roche_pvalue(),
        "add_overall.tbl_summary-arg:col_label" = "All Participants  \nN = {gtsummary::style_number(N)}",
        "pkgwide-str:print_engine" = "flextable"
      )
    )

  # {flextable} options --------------------------------------------------------
  lst_theme$`as_flex_table-lst:addl_cmds` <-
    c(
      lst_theme$`as_flex_table-lst:addl_cmds`,
      list(
        fontsize =
          list(
            rlang::expr(flextable::fontsize(size = !!font_size, part = "all")),
            rlang::expr(flextable::fontsize(size = !!(font_size - 1), part = "footer"))
          ),
        border = list(
          rlang::expr(flextable::border_outer(part = "body", border = !!border)),
          rlang::expr(flextable::border_outer(part = "header", border = !!border))
        ),
        valign = list( # valign only because it will append to to last commands
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
                table.font.size = 13,
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
