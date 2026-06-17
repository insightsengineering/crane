#' Extract Survival Differences at Specific Times
#'
#' @description
#' Calculates survival differences, confidence intervals, and p-values at
#' specified time points using `cardx::ard_survival_survfit_diff()`. The
#' resulting `data.frame` is structurally identical to the output of
#' `get_surv_times_df()`, allowing them to be easily bound together before
#' passing into `tbl_survfit_times()`.
#'
#' @param fit_km (`survfit`)\cr
#'   A fitted Kaplan-Meier object of class `survfit`.
#' @param times (`numeric`)\cr
#'   A numeric vector of time points at which to evaluate differences.
#' @param reference (`character`)\cr
#'   The name of the reference strata level to calculate differences against.
#' @param conf_int (`numeric`)\cr
#'   The confidence level to use for the intervals. Defaults to `0.95`.
#' @param scale (`numeric`)\cr
#'   A scaling factor for the survival estimates. For example, `100` converts
#'   probabilities to percentages. Defaults to `1`.
#'
#' @return A `data.frame` with columns for `Strata`, `Time`, `Survival`,
#'   `XX% CI`, and `p-value`.
#'
#' @examples
#' library(survival)
#' surv_data <- lung
#' surv_data$status <- surv_data$status - 1
#' surv_data$sex <- factor(surv_data$sex, labels = c("Male", "Female"))
#'
#' # 1. Fit the model
#' fit_km <- survfit(Surv(time, status) ~ sex, data = surv_data)
#'
#' # 2. Extract standard survival times
#' df_surv <- get_surv_times_df(fit_km, times = c(100, 200))
#'
#' # 3. Extract survival differences (Female vs Male)
#' df_diff <- get_surv_diff_df(
#'   fit_km,
#'   times = c(100, 200),
#'   reference = "Male"
#' )
#'
#' # 4. Combine and render
#' combined_df <- dplyr::bind_rows(df_surv, df_diff)
#' tbl_survfit_times(combined_df)
#'
#' @export
get_surv_diff_df <- function(fit_km, times, reference, conf_int = 0.95,
                             scale = 1) {
  if (!inherits(fit_km, "survfit")) {
    rlang::abort("`fit_km` must be a survfit object.")
  }

  if (!is.numeric(times) || length(times) == 0) {
    rlang::abort("`times` must be a non-empty numeric vector.")
  }

  # Calculate differences using cardx's Analysis Results Data (ARD) framework
  ard_res <- cardx::ard_survival_survfit_diff(
    x = fit_km,
    times = times,
    conf.level = conf_int
  )

  # Extract the underlying list data into a flat format.
  # Note: cardx stores the numeric time points in `variable_level`.
  flat_df <- ard_res |>
    dplyr::filter(!.data$stat_name %in% c("method", "reference_level")) |>
    dplyr::mutate(
      time = as.numeric(unlist(.data$variable_level)),
      comp_level = as.character(unlist(.data$group1_level)),
      value = as.numeric(unlist(.data$stat))
    ) |>
    dplyr::select("time", "comp_level", "stat_name", "value")

  # Pivot wider to build standard columns matching get_surv_times_df()
  wide_df <- flat_df |>
    tidyr::pivot_wider(
      names_from = "stat_name",
      values_from = "value"
    )

  ci_col_name <- paste0(conf_int * 100, "% CI")

  # Construct the final data.frame, ensuring the strata name clearly denotes
  # that this row represents a calculated difference versus the reference.
  res_df <- data.frame(
    Strata = paste(wide_df$comp_level, "vs", reference, "(Diff)"),
    Time = wide_df$time,
    `N at Risk` = NA_character_,
    Survival = sprintf("%.2f", wide_df$estimate * scale),
    CI = paste0(
      "(", sprintf("%.2f", wide_df$conf.low * scale),
      ", ", sprintf("%.2f", wide_df$conf.high * scale), ")"
    ),
    `p-value` = sprintf("%.3f", wide_df$p.value),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  names(res_df)[names(res_df) == "CI"] <- ci_col_name

  res_df
}
