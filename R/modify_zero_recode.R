#' Zero Count Recode
#'
#' @description
#' This function removes the percentage from cells with zero counts.
#' For example,
#'
#' ```r
#' 0 (0.0%)      -->  0
#' 0 (0%)        -->  0
#' 0 (NA%)       -->  0
#' 0 / nn (0%)   -->  0 / nn
#' 0/nn (0.0%)   -->  0/nn
#' 0 / 0 (NA%)   -->  0 / 0
#' ```
#'
#' @details
#' The function is a wrapper for `gtsummary::modify_post_fmt_fun()`.
#'
#'
#' ```r
#' gtsummary::modify_post_fmt_fun(
#'   x,
#'   fmt_fun = \(x) {
#'     dplyr::case_when(
#'       # convert "0 (0%)" OR "0 (0.0%)" OR 0 (NA%) to "0"
#'       str_detect(x, "^0\\s\\((?:0(?:\\.0)?|NA)%\\)$") ~ str_remove(x, pattern = "\\s\\((?:0(?:\\.0)?|NA)%\\)$"),
#'       # convert "0 / nn (0%)" OR "0/nn (0.0%)" OR 0/0 (NA%) to "0 / nn" OR "0/nn" OR "0/0"
#'       str_detect(x, pattern = "^(0 ?/) ?\\d+[^()]* \\((?:0(?:\\.0)?|NA)%\\)$") ~ str_remove(x, pattern = "\\s\\((?:0(?:\\.0)?|NA)%\\)$"),
#'       .default = x
#'     )
#'   },
#'   columns = gtsummary::all_stat_cols()
#' )
#' ```
#'
#' @param x (`gtsummary`)\cr
#'   a gtsummary table
#'
#' @returns a gtsummary table
#' @export
#'
#' @examples
#' trial |>
#'   dplyr::mutate(trt = factor(trt, levels = c("Drug A", "Drug B", "Drug C"))) |>
#'   tbl_summary(include = trt) |>
#'   modify_zero_recode()
modify_zero_recode <- function(x) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, "gtsummary")

  # recode zero and percent to "0" ---------------------------------------------
  # recode zero and percent to "0" ---------------------------------------------
  gtsummary::modify_post_fmt_fun(
    x,
    fmt_fun = \(col_vals) {
      if (is.numeric(col_vals)) {
        return(col_vals)
      }
      dplyr::case_when(
        # convert "0 (0%)" OR "0 (0.0%)" OR 0 (NA%) to "0"
        str_detect(col_vals, "^[ \u00A0]*0[ \u00A0]+\\((?:0(?:\\.0+)?|NA)%\\)$") ~
          str_remove(col_vals, pattern = "[ \u00A0]+\\((?:0(?:\\.0+)?|NA)%\\)$"),
        # convert "0 / nn (0%)" OR "0/nn (0.0%)" OR 0/0 (NA%) to "0 / nn" OR "0/nn" OR "0/0"
        str_detect(col_vals, pattern = "^[ \u00A0]*0[ \u00A0]*/[ \u00A0]*[0-9,.]+[ \u00A0]+\\((?:0(?:\\.0+)?|NA)%\\)$") ~
          str_remove(col_vals, pattern = "[ \u00A0]+\\((?:0(?:\\.0+)?|NA)%\\)$"),
        .default = as.character(col_vals)
      )
    },
    columns = c(gtsummary::all_stat_cols(), dplyr::starts_with("stat_"))
  )
}
