#' Round p-values
#'
#' @inheritParams gtsummary::style_pvalue
#'
#' @return A character vector of rounded p-values
#' @name roche_pvalue
#'
#' @examples
#' x <- c(0.0008, 0.9998)
#'
#' roche_pvalue(x)
#' label_roche_pvalue()(x)
NULL

#' @export
#' @rdname roche_pvalue
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
#' @rdname roche_pvalue
label_roche_pvalue <- function(big.mark = ifelse(decimal.mark == ",", " ", ","),
                               decimal.mark = getOption("OutDec"),
                               ...) {
  function(x) roche_pvalue(x, big.mark = big.mark, decimal.mark = decimal.mark, ...)
}

#' Round percentages
#'
#' @inheritParams gtsummary::style_percent
#'
#' @return A character vector of rounded percent values
#' @name roche_percent
#'
#' @examples
#' x <- c(0.0000001, 0.123456)
#'
#' roche_percent(x)
#' label_roche_percent()(x)
NULL

#' @export
#' @rdname roche_percent
roche_percent <- function(x,
                          big.mark = ifelse(decimal.mark == ",", " ", ","),
                          decimal.mark = getOption("OutDec"),
                          ...) {
  percent <- x * 100

  dplyr::case_when(
    is.na(percent) ~ NA_character_,
    abs(percent - 100) < 1e-8 ~ "100%",
    percent > 99.9 & percent < 100 ~ ">99.9%",
    percent < 0.1 & percent > 0 ~ "<0.1%",
    percent < 0 | percent > 100 ~ NA_character_, # Out-of-bound check
    TRUE ~ paste0(gtsummary::style_number(
      percent,
      digits = 1,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      ...
    ), "%")
  )
}

#' @export
#' @rdname roche_percent
label_roche_percent <- function(big.mark = ifelse(decimal.mark == ",", " ", ","),
                                decimal.mark = getOption("OutDec"),
                                ...) {
  function(x) roche_percent(x, big.mark = big.mark, decimal.mark = decimal.mark, ...)
}
