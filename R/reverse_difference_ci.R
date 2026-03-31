#' Reverse Rate Difference
#'
#' @description
#' Negates numeric rate difference values while preserving any suffix (e.g., "%").
#' This is useful when `gtsummary::add_difference_row()` computes `reference - arm`
#' but you need `arm - reference`.
#'
#' @param x (`character`)\cr
#'   A character vector of rate difference values, possibly with suffixes like "%".
#'
#' @returns A character vector with negated numeric values.
#' @export
#'
#' @seealso Usually used together with [reverse_ci()] for reversing confidence intervals; see examples there
#'   for usage with `gtsummary::modify_table_body()`.
#'
#' @examples
#' # Basic usage with percentage suffix
#' reverse_rate_difference(c("5.0%", "-3.2%", "0.0%"))
#'
#' # Handles NA and empty strings
#' reverse_rate_difference(c("2.5%", NA, "", "-1.0%"))
#'
#' # Works with values without suffix
#' reverse_rate_difference(c("10.0", "-5.5"))
reverse_rate_difference <- function(x) {
  sapply(
    x,
    function(val) {
      if (is.na(val) || val == "") {
        return(NA_character_)
      }
      num <- as.numeric(gsub("[^0-9.-]", "", val))
      if (is.na(num)) {
        return(val)
      }
      suffix <- gsub("[0-9.-]", "", val)
      paste0(ifelse(num == 0, "0.0", format(-num, nsmall = 1)), suffix)
    },
    USE.NAMES = FALSE
  )
}

#' Reverse Confidence Interval
#'
#' @description
#' Negates and swaps confidence interval bounds. Takes a CI string in the format
#' "(lower%, upper%)" and returns "(-upper%, -lower%)".
#' This is useful when `gtsummary::add_difference_row()` computes `reference - arm`
#' but you need `arm - reference`.
#'
#' @param x (`character`)\cr
#'   A character vector of confidence interval strings in format "(lower%, upper%)".
#'
#' @returns A character vector with negated and swapped CI bounds.
#' @export
#'
#' @seealso [reverse_rate_difference()] for reversing rate difference values.
#'
#' @examples
#' # Basic usage - negates values and swaps order
#' reverse_ci(c("(2.5%, 10.0%)", "(-5.0%, 3.0%)"))
#'
#' # Handles NA and empty strings
#' reverse_ci(c("(1.0%, 5.0%)", NA, ""))
#'
#' # Handles negative bounds
#' reverse_ci("(-8.0%, -2.0%)")
#'
#' # Example: Reversing direction in a gtsummary table
#' # When add_difference_row() computes "reference - arm" but you need "arm - reference"
#' library(gtsummary)
#'
#' tbl <- trial |>
#'   tbl_summary(
#'     by = trt,
#'     include = response,
#'     missing = "no"
#'   ) |>
#'   add_difference_row(
#'     include = response,
#'     reference = "Drug A",
#'     statistic = response ~ c("{estimate}", "({conf.low}, {conf.high})"),
#'     estimate_fun = response ~ label_style_number(digits = 1, scale = 100, suffix = "%")
#'   )
#'
#' # Reverse the direction using modify_table_body
#' tbl |>
#'   modify_table_body(
#'     ~ .x |>
#'       dplyr::mutate(
#'         dplyr::across(
#'           dplyr::starts_with("stat_"),
#'           ~ ifelse(variable == "response-row_difference" & label == "Rate Difference",
#'                    reverse_rate_difference(.x), .x)
#'         )
#'       ) |>
#'       dplyr::mutate(
#'         dplyr::across(
#'           dplyr::starts_with("stat_"),
#'           ~ ifelse(variable == "response-row_difference" &
#'                      label == "(CI Lower Bound, CI Upper Bound)",
#'                    reverse_ci(.x), .x)
#'         )
#'       )
#'   )
reverse_ci <- function(x) {
  sapply(
    x,
    function(val) {
      if (is.na(val) || val == "") {
        return(NA_character_)
      }
      nums <- as.numeric(unlist(regmatches(val, gregexpr("-?[0-9.]+", val))))
      if (length(nums) != 2) {
        return(val)
      }
      sprintf(
        "(%s%%, %s%%)",
        format(-nums[2], nsmall = 1),
        format(-nums[1], nsmall = 1)
      )
    },
    USE.NAMES = FALSE
  )
}
