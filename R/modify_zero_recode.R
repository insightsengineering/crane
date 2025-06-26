#' Zero Count Recode
#'
#' @description
#' This function converts `"0 (0.0%)"` cells in a summary table to `"0"`.
#'
#' It's a simple wrapper for `gtsummary::modify_post_fmt_fun()`.
#'
#' ```r
#' gtsummary::modify_post_fmt_fun(
#'   x,
#'   fmt_fun = ~ ifelse(. %in% c("0 (0.0%)", "0 (NA%)"), "0", .),
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
  gtsummary::modify_post_fmt_fun(
    x,
    fmt_fun = ~ ifelse(. %in% c("0 (0.0%)", "0 (NA%)"), "0", .),
    columns = gtsummary::all_stat_cols()
  )
}
