#' Roche Theme
#'
#' @description
#' A gtsummary theme for Roche tables
#' - Calls the [`gtsummary::theme_gtsummary_compact()`] theme.
#' - Uses `gtsummary::label_style_pvalue(digits = 2)` as the default formatting function for all p-values.
#' - Defaults to a mono-spaced font for gt tables.
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
theme_gtsummary_roche <- function(set_theme = TRUE, font_size = NULL) {
  # start with the compact theme -----------------------------------------------
  lst_theme <- gtsummary::theme_gtsummary_compact(set_theme = FALSE, font_size = font_size)
  lst_theme$`pkgwide-str:theme_name` <- "Roche"

  # updating with some pharma-specific bits ------------------------------------
  lst_theme <- lst_theme |>
    append(
      list(
        "pkgwide-fn:pvalue_fun" = gtsummary::label_style_pvalue(digits = 2)
      )
    )

  # add a monospace font for gt ------------------------------------------------
  lst_theme$`as_gt-lst:addl_cmds` <-
    c(
      lst_theme$`as_gt-lst:addl_cmds`,
      list(tab_spanner = rlang::expr(gt::opt_table_font(stack = "monospace-code")))
    )

  # finishing up ---------------------------------------------------------------
  if (set_theme == TRUE) gtsummary::set_gtsummary_theme(lst_theme)
  return(invisible(lst_theme))
}
