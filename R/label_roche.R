#' Formatting percent and p-values
#'
#' @description
#' - `label_roche_pvalue()` returns formatted p-values.
#' - `label_roche_percent()` returns formatted percent values. This function only formats percentages between 0 and 1.
#'
#' @return A character vector of rounded p-values
#' @inheritParams gtsummary::style_pvalue
#' @examples
#' # p-value formatting
#' x <- c(0.0000001, 0.123456)
#'
#' roche_pvalue(x)
#' label_roche_pvalue()(x)
#'
#' # percent formatting
#' x <- c(0.0008, 0.9998)
#'
#' roche_percent(x)
#' label_roche_percent()(x)
#' @name label_roche
NULL

#' @export
#' @rdname label_roche
roche_pvalue <- function(x,
                         big.mark = ifelse(decimal.mark == ",", " ", ","),
                         decimal.mark = getOption("OutDec"),
                         ...) {
  dplyr::case_when(
    # allowing some leeway for numeric storage errors
    x > 1 + 1e-15 ~ NA_character_,
    x < 0 - 1e-15 ~ NA_character_,
    x < 0.0001 ~
      paste0("<", gtsummary::style_number(0.0001, digits = 4, big.mark = big.mark, decimal.mark = decimal.mark, ...)),
    .default = gtsummary::style_number(x, digits = 4, big.mark = big.mark, decimal.mark = decimal.mark, ...)
  )
}

#' @export
#' @rdname label_roche
label_roche_pvalue <- function(big.mark = ifelse(decimal.mark == ",", " ", ","),
                               decimal.mark = getOption("OutDec"),
                               ...) {
  function(x) roche_pvalue(x, big.mark = big.mark, decimal.mark = decimal.mark, ...)
}

#' @inheritParams gtsummary::style_number
#' @export
#' @rdname label_roche
roche_percent <- function(x,
                          prefix = "",
                          suffix = "",
                          digits = 1,
                          scale = 100,
                          big.mark = ifelse(decimal.mark == ",", " ", ","),
                          decimal.mark = getOption("OutDec"),
                          ...) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    abs(x - 1) < sqrt(.Machine$double.eps) ~ gtsummary::style_number(1, scale = 100, digits = digits - 1, big.mark = big.mark, decimal.mark = decimal.mark, suffix = suffix, ...),
    x > 0.999 & x < 1 ~ gtsummary::style_number(0.999, scale = 100, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, prefix = ">", suffix = suffix, ...),
    x < 0.001 & x > 0 ~ gtsummary::style_number(0.001, scale = 100, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, prefix = "<", suffix = suffix, ...),
    abs(x - 0) < sqrt(.Machine$double.eps) ~ gtsummary::style_number(0.0, scale = 100, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, suffix = suffix, ...),
    x < 0 | x > 1 ~ NA_character_,
    TRUE ~ gtsummary::style_number(
      x,
      scale = 100,
      digits = digits,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      suffix = suffix,
      prefix = prefix,
      ...
    )
  )
}

#' @export
#' @rdname label_roche
label_roche_percent <- function(big.mark = ifelse(decimal.mark == ",", " ", ","),
                                suffix = "",
                                scale = 100,
                                digits = 1,
                                decimal.mark = getOption("OutDec"),
                                ...) {
  function(x) roche_percent(x, suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark, digits = digits, ...)
}
