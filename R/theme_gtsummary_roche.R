#' Roche Theme
#'
#' @description
#' A gtsummary theme for Roche tables
#' - Calls the [`gtsummary::theme_gtsummary_compact()`] theme.
#' - Uses `label_roche_pvalue()` as the default formatting function for all p-values.
#' - Defaults to a mono-spaced font for gt tables.
#'
#' @inheritParams gtsummary::theme_gtsummary_compact
#' @param border (`fp_border`)\cr
#'   Border style. Defaults to `flextable::fp_border_default(width = 0.5)`.
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
theme_gtsummary_roche <- function(set_theme = TRUE, font_size = 8, border = flextable::fp_border_default(width = 0.5)) {
  # start with the compact theme -----------------------------------------------
  lst_theme <- gtsummary::theme_gtsummary_compact(set_theme = FALSE, font_size = font_size)
  lst_theme$`pkgwide-str:theme_name` <- "Roche"

  # updating with some pharma-specific bits ------------------------------------
  lst_theme <- lst_theme |>
    append(
      list(
        "tbl_summary-fn:percent_fun" = gtsummary::label_style_number(digits = 1, scale = 100),
        "pkgwide-fn:pvalue_fun" = label_roche_pvalue(),
        "pkgwide-fun:pre_conversion" = gtsummary::bold_labels(),
        "pkgwide-str:print_engine" = "flextable"
      )
    )


  # use arial font for flextables ----------------------------------------------
  lst_theme$`as_flex_table-lst:addl_cmds` <-
    c(
      lst_theme$`as_flex_table-lst:addl_cmds`,
      list(
        fontsize = rlang::expr(flextable::fontsize(size = !!font_size - 1, part = "footer")),
        border = rlang::expr(
          flextable::border_outer(part = "body", border = !!border)
        ),
        border = rlang::expr(
          flextable::border_outer(part = "header", border = !!border)
        ),
        valign = rlang::expr(flextable::valign(valign = "top", part = "all")),
        valign = rlang::expr(flextable::font(fontname = "Arial", part = "all")), # valign only bc last cmd
        valign = rlang::expr(flextable::line_spacing(space = 1, part = "all"))
      )
    )
  # todo:
  # * vertical padding different for label rows (3 + 0.2)
  # * horizontal padding has various different defaults
  # * section_properties (landscape e.g.): officer::body_set_default_section(doc, section_properties)
  # * metadata: do.call(officer::set_doc_properties, c(list("x" = doc), doc_metadata))
#   } else { # A4
# page_size <- officer::page_size(
#     orient = orientation,
#     width = 8.27, height = 11.69
#   )
# }
#
# # Final output
# officer::prop_section(
#   page_size = page_size,
#   type = "continuous",
#   page_margins = margins_potrait()
# )


  # add a monospace font for gt ------------------------------------------------
  lst_theme$`as_gt-lst:addl_cmds` <-
    c(
      lst_theme$`as_gt-lst:addl_cmds`,
      list(font_type = rlang::expr(gt::opt_table_font(stack = "monospace-code")))
    )


  # finishing up ---------------------------------------------------------------
  if (set_theme == TRUE) gtsummary::set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}
