#' Coefficient of Variation
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (`numeric`)\cr numeric vector.
#'
#' @returns A single numeric value representing the coefficient of variation (%).
#'
#' @examples
#' cv(c(1, 2, 3, 4, 5))
#'
#' @export
cv <- function(x) {
  (100 * stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
}

#' Geometric Coefficient of Variation
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (`numeric`)\cr numeric vector of positive values.
#'
#' @returns A single numeric value representing the geometric coefficient of variation (%).
#'
#' @examples
#' geom_cv(c(1, 2, 3, 4, 5))
#'
#' @export
geom_cv <- function(x) {
  (sqrt(exp(stats::sd(log(x[x > 0]), na.rm = TRUE)^2) - 1)) * 100
}

#' Geometric Mean
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (`numeric`)\cr numeric vector. Non-positive values are ignored.
#' @param na.rm (`logical(1)`)\cr whether to remove `NA` values. Default is `TRUE`.
#'
#' @returns A single numeric value, or `NA_real_` if there are no usable values
#'   or if any non-`NA` value is <= 0.
#'
#' @examples
#' geom_mean(c(1, 2, 3, 4, 5))
#'
#' @export
geom_mean <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # no usable values (e.g. all NA) or any non-positive value -> undefined
  if (length(x) == 0 || any(x <= 0)) {
    return(NA_real_)
  }
  exp(mean(log(x)))
}

#' Format to 3 Significant Figures
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Formats numeric values to 3 significant figures, removing trailing decimals.
#'
#' @param x (`numeric`)\cr a numeric vector.
#'
#' @returns A `character` vector the same length as `x`, with `NA_character_`
#'   where `x` is `NA` or non-finite.
#'
#' @examples
#' fmt_3sig(0.001234)
#' fmt_3sig(123456)
#' fmt_3sig(c(0.001234, NA, 123456))
#'
#' @export
fmt_3sig <- function(x) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- gsub(
    "\\.$", "",
    formatC(signif(x[ok], 3), digits = 3, format = "fg", flag = "#")
  )
  out
}

#' Format to 1 Decimal Place
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Formats numeric values to a single decimal place.
#'
#' @param x (`numeric`)\cr a numeric vector.
#'
#' @returns A `character` vector the same length as `x`, with `NA_character_`
#'   where `x` is `NA` or non-finite.
#'
#' @examples
#' fmt_pct(45.678)
#' fmt_pct(c(45.678, NA))
#'
#' @export
fmt_pct <- function(x) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- sprintf("%.1f", x[ok])
  out
}

#' Apply BLQ Imputation Rules
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Applies BLQ (Below Limit of Quantification) display rules to a statistic
#' based on the proportion of BLQ observations. When too many observations are
#' BLQ, summary statistics that cannot be meaningfully reported are replaced by
#' `"ND"` (not determined), and a geometric mean that could not be computed is
#' replaced by `"NE"` (not estimable).
#'
#' @param stat_val (`character(1)`)\cr formatted statistic value.
#' @param label (`character(1)`)\cr label identifying the statistic type (e.g. `"Median"`, `"Max"`).
#' @param blq_ratio (`numeric(1)`)\cr proportion of BLQ observations (between 0 and 1).
#' @param rule (`character(1)`)\cr imputation rule to apply. One of `"1/3"` (default) or `"1/2"`.
#'
#' @returns A `character` string: the original `stat_val`, `"ND"` (not determined),
#'   or `"NE"` (not estimable).
#'
#' @details
#' This function only decides how a statistic is *displayed* given the BLQ ratio.
#' Any substitution of BLQ values in the data (e.g. based on dosing timing) must
#' be applied upstream, before the statistics are computed.
#'
#' @examples
#' imputation_rules("1.23", "Mean", blq_ratio = 0.5, rule = "1/3")
#'
#' @export
imputation_rules <- function(stat_val, label, blq_ratio, rule = "1/3") {
  if (is.null(rule) || is.na(blq_ratio)) {
    return(stat_val)
  }
  force(label)

  dplyr::case_when(
    label == "Geom_mean" & (is.na(stat_val) | stat_val == "NA") ~ "NE",

    # 1/2 rule
    rule == "1/2" & blq_ratio > 0.5 ~ dplyr::case_when(
      label %in% c("Max", "No. obs.", "Number of LTR/BLQ") ~ stat_val,
      TRUE ~ "ND"
    ),

    # 1/3 rule
    rule == "1/3" & blq_ratio > 1 / 3 ~ dplyr::case_when(
      label %in% c("Median", "Max", "Geom_mean", "No. obs.", "Number of LTR/BLQ") ~ stat_val,
      TRUE ~ "ND"
    ),
    TRUE ~ stat_val
  )
}
