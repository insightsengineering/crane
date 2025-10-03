#' Formatting percent and p-values
#'
#' @description
#' - `label_roche_pvalue()` returns formatted p-values.
#' - `label_roche_percent()` returns formatted percent values. This function only formats percentages between 0 and 1.
#' - `label_roche_ratio()` returns formatted ratios with values below and above a threshold being returned as `< 0.1` and `> 999.9`, for example, when `digits=1`.
#' - `label_roche_number()` returns formatted numbers.
#'
#' @return A character vector of rounded p-values
#' @inheritParams gtsummary::style_number
#' @inheritParams gtsummary::style_pvalue
#' @param inf (`string`)\cr
#'   Character to replace `Inf` values with. Default is "NE".
#' @param nan (`NA`/`string`)\cr
#'   Character to replace `NaN` values with. Default is `NA`.
#' @examples
#' # p-value formatting
#' x <- c(0.0000001, 0.123456)
#'
#' style_roche_pvalue(x)
#' label_roche_pvalue()(x)
#'
#' # percent formatting
#' x <- c(0.0008, 0.9998)
#'
#' style_roche_percent(x)
#' label_roche_percent()(x)
#'
#' # ratio formatting
#' x <- c(0.0008, 0.8234, 2.123, 1000)
#'
#' style_roche_ratio(x)
#' label_roche_ratio()(x)
#'
#' # number formatting
#' x <- c(0.0008, 0.8234, 2.123, 1000, NA, Inf, -Inf)
#'
#' style_roche_number(x)
#' label_roche_number()(x)
#' @name label_roche
NULL

#' @export
#' @rdname label_roche
style_roche_pvalue <- function(x,
                               big.mark = ifelse(decimal.mark == ",", " ", ","),
                               decimal.mark = getOption("OutDec"),
                               ...) {
  dplyr::case_when(
    # allowing some leeway for numeric storage errors
    x > 1 + 1e-15 ~ NA_character_,
    x < 0 - 1e-15 ~ NA_character_,
    x < 0.0001 ~
      paste0("<", style_roche_number(0.0001, digits = 4, big.mark = big.mark, decimal.mark = decimal.mark, ...)),
    .default = style_roche_number(x, digits = 4, big.mark = big.mark, decimal.mark = decimal.mark, ...)
  )
}

#' @export
#' @rdname label_roche
label_roche_pvalue <- function(big.mark = ifelse(decimal.mark == ",", " ", ","),
                               decimal.mark = getOption("OutDec"),
                               ...) {
  function(x) style_roche_pvalue(x, big.mark = big.mark, decimal.mark = decimal.mark, ...)
}

#' @export
#' @rdname label_roche
style_roche_percent <- function(x,
                                digits = 1,
                                prefix = "",
                                suffix = "",
                                scale = 100,
                                big.mark = ifelse(decimal.mark == ",", " ", ","),
                                decimal.mark = getOption("OutDec"),
                                ...) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    abs(x - 1) < sqrt(.Machine$double.eps) ~ style_roche_number(1, scale = 100, digits = digits - 1, big.mark = big.mark, decimal.mark = decimal.mark, suffix = suffix, ...),
    x > 0.999 & x < 1 ~ style_roche_number(0.999, scale = 100, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, prefix = ">", suffix = suffix, ...),
    x < 0.001 & x > 0 ~ style_roche_number(0.001, scale = 100, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, prefix = "<", suffix = suffix, ...),
    abs(x - 0) < sqrt(.Machine$double.eps) ~ style_roche_number(0.0, scale = 100, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, suffix = suffix, ...),
    x < 0 | x > 1 ~ NA_character_,
    TRUE ~ style_roche_number(
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
label_roche_percent <- function(digits = 1,
                                suffix = "",
                                scale = 100,
                                big.mark = ifelse(decimal.mark == ",", " ", ","),
                                decimal.mark = getOption("OutDec"),
                                ...) {
  function(x) style_roche_percent(x, suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark, digits = digits, ...)
}

#' @export
#' @rdname label_roche
style_roche_ratio <- function(x,
                              digits = 2,
                              prefix = "",
                              suffix = "",
                              scale = 1,
                              big.mark = ifelse(decimal.mark == ",", " ", ","),
                              decimal.mark = getOption("OutDec"),
                              ...) {
  dplyr::case_when(
    # Lower threshold
    x < 10^(-digits) ~
      gtsummary::style_number(
        x = 10^(-digits),
        digits = digits,
        prefix = paste0(prefix, "<"),
        suffix = suffix,
        scale = scale,
        big.mark = big.mark,
        decimal.mark = decimal.mark,
        ...
      ),
    # Upper threshold
    x > 1000 - 10^(-digits) ~
      gtsummary::style_number(
        x = 1000 - 10^(-digits),
        digits = digits,
        prefix = paste0(prefix, ">"),
        suffix = suffix,
        scale = scale,
        big.mark = big.mark,
        decimal.mark = decimal.mark,
        ...
      ),
    # All numbers not above or below threshold
    .default =
      gtsummary::style_number(
        x = x,
        digits = digits,
        prefix = prefix,
        suffix = suffix,
        scale = scale,
        big.mark = big.mark,
        decimal.mark = decimal.mark,
        ...
      )
  )
}

#' @export
#' @rdname label_roche
label_roche_ratio <- function(digits = 2,
                              prefix = "",
                              suffix = "",
                              scale = 1,
                              big.mark = ifelse(decimal.mark == ",", " ", ","),
                              decimal.mark = getOption("OutDec"),
                              ...) {
  function(x) style_roche_ratio(x, prefix = prefix, suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark, digits = digits, ...)
}

#' @export
#' @rdname label_roche
style_roche_number <- function(x,
                               digits = 0,
                               big.mark = ifelse(decimal.mark == ",", " ", ","),
                               decimal.mark = getOption("OutDec"),
                               scale = 1,
                               prefix = "",
                               suffix = "",
                               na = "NE",
                               inf = "NE",
                               nan = NA,
                               ...) {
  set_cli_abort_call()

  ret <- gtsummary::style_number(
    x = x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark,
    scale = scale, prefix = prefix, suffix = suffix, na = na, ...
  )

  ret[is.infinite(x)] <- inf
  ret[is.nan(x)] <- nan

  ret
}

#' @export
#' @rdname label_roche
label_roche_number <- function(digits = 0,
                               big.mark = ifelse(decimal.mark == ",", " ", ","),
                               decimal.mark = getOption("OutDec"),
                               scale = 1,
                               prefix = "",
                               suffix = "",
                               na = "NE",
                               inf = "NE",
                               nan = NA,
                               ...) {
  function(x) style_roche_number(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, scale = scale, prefix = prefix, suffix = suffix, na = na, inf = inf, nan = nan, ...)
}
