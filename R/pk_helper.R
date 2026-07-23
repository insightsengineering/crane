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
  (100 * sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
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
  (sqrt(exp(sd(log(x[x > 0]), na.rm = TRUE)^2) - 1)) * 100
}

#' Geometric Mean
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @param x (`numeric`)\cr numeric vector. Non-positive values are ignored.
#' @param na.rm (`logical(1)`)\cr whether to remove `NA` values. Default is `TRUE`.
#'
#' @returns A single numeric value, or `NA_real_` if any non-`NA` value is <= 0.
#'
#' @examples
#' geom_mean(c(1, 2, 3, 4, 5))
#'
#' @export
geom_mean <- function(x, na.rm = TRUE) {
  if (any(x <= 0, na.rm = na.rm)) {
    return(NA_real_)
  }
  exp(mean(log(x[x > 0]), na.rm = na.rm))
}

#' Format to 3 Significant Figures
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Formats a numeric value to 3 significant figures, removing trailing decimals.
#'
#' @param x (`numeric(1)`)\cr a single numeric value.
#'
#' @returns A `character` string, or `NA_character_` if `x` is `NA` or non-finite.
#'
#' @examples
#' fmt_3sig(0.001234)
#' fmt_3sig(123456)
#'
#' @export
fmt_3sig <- function(x) {
  if (is.na(x) || !is.finite(x)) return(NA_character_)
  gsub("\\.$", "", formatC(signif(x, 3), digits = 3, format = "fg", flag = "#"))
}

#' Apply BLQ Imputation Rules
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Applies BLQ (Below Limit of Quantification) imputation rules to a statistic
#' based on the proportion of BLQ observations and dosing timing.
#'
#' @param stat_val (`character(1)`)\cr formatted statistic value.
#' @param label (`character(1)`)\cr label identifying the statistic type (e.g. `"Median"`, `"Max"`).
#' @param blq_ratio (`numeric(1)`)\cr proportion of BLQ observations (between 0 and 1).
#' @param postdose (`logical(1)`)\cr whether the timepoint is post-dose.
#' @param rule (`character(1)`)\cr imputation rule to apply. One of `"1/3"` (default) or `"1/2"`.
#'
#' @returns A `character` string: the original `stat_val`, `"ND"` (not determined),
#'   or `"NE"` (not estimable).
#'
#' @examples
#' imputation_rules("1.23", "Mean", blq_ratio = 0.5, postdose = TRUE, rule = "1/3")
#'
#' @export
imputation_rules <- function(stat_val, label, blq_ratio, postdose, rule = "1/3") {
  if (is.na(blq_ratio)) return(stat_val)

  dplyr::case_when(
    label == "Geom_mean" & (is.na(stat_val) | stat_val == "NA") ~ "NE",

    is.null(rule) ~ stat_val,

    # 1/2 rule
    rule == "1/2" & blq_ratio > 0.5 ~ dplyr::case_when(
      label %in% c("Max", "No. obs.", "Number of LTR/BLQ") ~ stat_val,
      TRUE ~ "ND"
    ),

    # 1/3 rule: pre-dose
    rule == "1/3" & !postdose & blq_ratio > 1 / 3 ~ dplyr::case_when(
      label %in% c("Median", "Max", "Geom_mean", "No. obs.", "Number of LTR/BLQ") ~ stat_val,
      TRUE ~ "ND"
    ),

    # 1/3 rule: post-dose
    rule == "1/3" & postdose & blq_ratio > 1 / 3 ~ dplyr::case_when(
      label %in% c("Median", "Max", "Geom_mean", "No. obs.", "Number of LTR/BLQ") ~ stat_val,
      TRUE ~ "ND"
    ),

    TRUE ~ stat_val
  )
}
