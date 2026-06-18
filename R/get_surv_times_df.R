#' Generate Table of Survival Estimates at Specific Times
#'
#' @description
#' This function extracts survival probabilities, confidence intervals, and
#' numbers at risk from a `survfit` object at specified time points. It returns
#' a formatted `data.frame` that can be manipulated before passing to
#' `tbl_surv_times()`.
#'
#' @param fit_km (`survfit`)\cr
#'   A fitted Kaplan-Meier object of class `survfit`.
#' @param times (`numeric`)\cr
#'   A numeric vector of time points at which to evaluate survival estimates.
#' @param conf_int (`numeric`)\cr
#'   The confidence level to use for the intervals. Defaults to `0.95`.
#' @param scale (`numeric`)\cr
#'   A scaling factor for the survival estimates. For example, `100` converts
#'   probabilities to percentages. Defaults to `1`.
#'
#' @return A `data.frame` with columns for `Strata`, `Time`, `N at Risk`,
#'   `Survival`, and `XX% CI`.
#'
#' @examples
#' library(survival)
#' surv_data <- lung
#' surv_data$status <- surv_data$status - 1
#'
#' # Example: Handling advanced arguments natively in survfit.
#' # Instead of passing `method.args`, apply them directly to survfit().
#' # Here we set a 99% CI and specify a cluster ID (inst).
#' fit_complex <- survfit(
#'   Surv(time, status) ~ sex,
#'   data = surv_data,
#'   id = inst,
#'   conf.int = 0.99
#' )
#'
#' get_surv_times_df(fit_complex, times = c(100, 200), conf_int = 0.99)
#'
#' @export
get_surv_times_df <- function(fit_km, times, conf_int = 0.95, scale = 1) {
  # Enforce rigorous type-checking using rlang
  if (!inherits(fit_km, "survfit")) {
    rlang::abort("`fit_km` must be a survfit object.")
  }

  if (!is.numeric(times) || length(times) == 0) {
    rlang::abort("`times` must be a non-empty numeric vector.")
  }

  # Extract summary at specific times, extending to last known survival
  # handles times beyond the max observed time gracefully.
  summ <- summary(fit_km, times = times, extend = TRUE)

  strata_levels <- if (!is.null(summ$strata)) {
    as.character(summ$strata)
  } else {
    "All"
  }

  # Remove variable prefix from strata to maintain clean downstream headers
  # without cluttering tables with 'arm=A', 'arm=B' etc.
  if (!is.null(fit_km$strata)) {
    strata_levels <- sub("^[^=]+=", "", strata_levels)
  }

  df <- data.frame(
    Strata = strata_levels,
    Time = summ$time,
    `N at Risk` = summ$n.risk,
    Survival = sprintf("%.2f", summ$surv * scale),
    CI = paste0(
      "(", sprintf("%.2f", summ$lower * scale),
      ", ", sprintf("%.2f", summ$upper * scale), ")"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Rename the general 'CI' column to reflect the exact interval used
  # giving users an accurate header prior to any manual renaming steps
  ci_col_name <- paste0(conf_int * 100, "% CI")
  names(df)[names(df) == "CI"] <- ci_col_name

  df
}
