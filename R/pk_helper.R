#' PK Summary Statistic Helpers
#'
#' @description `r lifecycle::badge("stable")`
#'
#' **These helpers are intended primarily for pharmacokinetic (PK) concentration
#' summaries.** Together they cover the pieces needed to build PK concentration
#' tables:
#'
#' * `cv()` -- coefficient of variation (%).
#' * `geom_cv()` -- geometric coefficient of variation (%).
#' * `geom_mean()` -- geometric mean.
#' * `fmt_3sig()` -- format numbers to 3 significant figures.
#' * `fmt_pct()` -- format numbers to a single decimal place.
#' * `pk_imputation_rules()` -- apply BLQ display rules to a formatted statistic.
#'
#' @param x (`numeric`)\cr numeric vector.
#' @param na.rm (`logical(1)`)\cr whether to remove `NA` values. Default is `TRUE`.
#' @param stat_val (`character(1)`)\cr formatted statistic value.
#' @param label (`character(1)`)\cr label identifying the statistic type (e.g. `"Median"`, `"Max"`).
#' @param blq_ratio (`numeric(1)`)\cr proportion of BLQ observations (between 0 and 1).
#' @param postdose (`logical(1)`)\cr whether the timepoint is post-dose. The 1/3
#'   rule keeps a separate set of reportable statistics for pre-dose and
#'   post-dose timepoints.
#' @param rule (`character(1)`)\cr imputation rule to apply. One of `"1/3"` (default) or `"1/2"`.
#'
#' @returns
#' `cv()`, `geom_cv()`, and `geom_mean()` return a single numeric value.
#' `geom_mean()` returns `NA_real_` if there are no usable values or if any
#' non-`NA` value is <= 0.
#'
#' `fmt_3sig()` and `fmt_pct()` return a `character` vector the same length as
#' `x`, with `NA_character_` where `x` is `NA` or non-finite.
#'
#' `pk_imputation_rules()` returns a `character` string: the original
#' `stat_val`, `"ND"` (not determined), or `"NE"` (not estimable).
#'
#' @name pk_helpers
NULL

# An all-NA vector is typed `logical` by R (e.g. `c(NA, NA)`); treat it as an
# empty numeric so the numeric helpers accept it instead of erroring on type.
as_pk_numeric <- function(x) {
  if (is.logical(x) && all(is.na(x))) as.numeric(x) else x
}

#' @rdname pk_helpers
#' @examples
#' cv(c(1, 2, 3, 4, 5))
#'
#' @export
cv <- function(x) {
  check_not_missing(x)
  x <- as_pk_numeric(x)
  check_numeric(x)
  (100 * stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
}

#' @rdname pk_helpers
#' @examples
#' geom_cv(c(1, 2, 3, 4, 5))
#'
#' @export
geom_cv <- function(x) {
  check_not_missing(x)
  x <- as_pk_numeric(x)
  check_numeric(x)
  (sqrt(exp(stats::sd(log(x[x > 0]), na.rm = TRUE)^2) - 1)) * 100
}

#' @rdname pk_helpers
#' @examples
#' geom_mean(c(1, 2, 3, 4, 5))
#'
#' @export
geom_mean <- function(x, na.rm = TRUE) {
  check_not_missing(x)
  x <- as_pk_numeric(x)
  check_numeric(x)
  check_scalar_logical(na.rm)
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  # no usable values (e.g. all NA) or any non-positive value -> undefined
  if (length(x) == 0 || any(x <= 0)) {
    return(NA_real_)
  }
  exp(mean(log(x)))
}

#' @rdname pk_helpers
#' @examples
#' fmt_3sig(0.001234)
#' fmt_3sig(123456)
#' fmt_3sig(c(0.001234, NA, 123456))
#'
#' @export
fmt_3sig <- function(x) {
  check_not_missing(x)
  x <- as_pk_numeric(x)
  check_numeric(x, allow_empty = TRUE)
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- gsub(
    "\\.$", "",
    formatC(signif(x[ok], 3), digits = 3, format = "fg", flag = "#")
  )
  out
}

#' @rdname pk_helpers
#' @examples
#' fmt_pct(45.678)
#' fmt_pct(c(45.678, NA))
#'
#' @export
fmt_pct <- function(x) {
  check_not_missing(x)
  x <- as_pk_numeric(x)
  check_numeric(x, allow_empty = TRUE)
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x) & is.finite(x)
  out[ok] <- sprintf("%.1f", x[ok])
  out
}

#' @rdname pk_helpers
#'
#' @details
#' `pk_imputation_rules()` applies BLQ (Below Limit of Quantification) imputation
#' rules to a formatted PK summary statistic based on the proportion of BLQ
#' observations and dosing timing. When too many observations are BLQ, summary
#' statistics that cannot be meaningfully reported are replaced by `"ND"` (not
#' determined), and a geometric mean that could not be computed is replaced by
#' `"NE"` (not estimable).
#'
#' @examples
#' pk_imputation_rules("1.23", "Mean", blq_ratio = 0.5, postdose = TRUE, rule = "1/3")
#'
#' @export
pk_imputation_rules <- function(stat_val, label, blq_ratio, postdose, rule = "1/3") {
  check_not_missing(label)
  check_not_missing(blq_ratio)
  check_not_missing(postdose)
  check_string(label)
  check_scalar_logical(postdose)
  if (is.null(rule) || is.na(blq_ratio)) {
    return(stat_val)
  }
  check_scalar_range(blq_ratio, range = c(0, 1), include_bounds = c(TRUE, TRUE))
  check_string(rule)

  # statistics that remain reportable once the BLQ threshold is exceeded.
  # the 1/3 rule keeps separate sets for pre-dose and post-dose timepoints
  # (currently identical, split so PK SMEs can tune each independently).
  keep_half <- c("Max", "No. obs.", "Number of LTR/BLQ")
  keep_third_predose <- c("Median", "Max", "Geom_mean", "No. obs.", "Number of LTR/BLQ")
  keep_third_postdose <- c("Median", "Max", "Geom_mean", "No. obs.", "Number of LTR/BLQ")

  keep_third <- if (postdose) keep_third_postdose else keep_third_predose

  dplyr::case_when(
    # a geometric mean that could not be computed is not estimable
    label == "Geom_mean" & (is.na(stat_val) | stat_val == "NA") ~ "NE",

    # 1/2 rule: more than half of observations are BLQ
    rule == "1/2" & blq_ratio > 0.5 ~
      dplyr::if_else(label %in% keep_half, stat_val, "ND"),

    # 1/3 rule: more than a third of observations are BLQ
    rule == "1/3" & blq_ratio > 1 / 3 ~
      dplyr::if_else(label %in% keep_third, stat_val, "ND"),
    TRUE ~ stat_val
  )
}
