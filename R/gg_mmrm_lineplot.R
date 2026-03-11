#' Create MMRM Line Plot (MMRMG01)
#'
#' @param mmrm_df Output from `get_mmrm_results()`
#' @param arm String, column name for treatment arm
#' @param visit String, column name for visits
#' @param error_bar String, either "ci" (95% CI) or "se" (Standard Error)
#' @param dodge_width Numeric, amount to jitter x-axis to prevent overlap
#' @param hline Numeric, y-intercept for horizontal reference line (default is 0)
#' @param legend_pos String, "bottom" or "right"
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("mmrm", quietly = TRUE)
#' library(mmrm)
#' fv_dt <- mmrm::fev_data |>
#'   dplyr::mutate(
#'     ARMCD = sprintf(
#'       "%s\n(N = %d)", ARMCD,
#'       table(mmrm::fev_data$ARMCD)[ARMCD]
#'     ),
#'     ARMCD = factor(ARMCD)
#'   )
#' # Fit an MMRM model using the FEV data
#' fit_mmrm <- mmrm::mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID), # us -> unstructured cov structure
#'   data = fv_dt
#' )
#' mmrm_results <- get_mmrm_results(fit_mmrm, arm = "ARMCD", visit = "AVISIT", conf_level = 0.95)
#'
#' # Create a plot with SE bars, jittered by 0.2, legend on the bottom
#' my_plot <- gg_lineplot_mmrm(
#'   mmrm_df = mmrm_results,
#'   arm = "ARMCD",
#'   visit = "AVISIT",
#'   error_bar = "se",       # Switch to "ci" for Confidence Intervals
#'   dodge_width = 0.2,      # Jitters the x-axis to prevent overlap
#'   legend_pos = "bottom"   # Moves legend
#' )
#'
#' print(my_plot + nestcolor::theme_nest())
#' @return A ggplot object
#' @export
gg_lineplot_mmrm <- function(mmrm_df, arm, visit, error_bar = c("ci", "se"),
                             dodge_width = 0.15, hline = 0, legend_pos = "bottom") {

  error_bar <- match.arg(error_bar)

  # 1. Standardize column names for easy plotting
  plot_df <- mmrm_df |>
    dplyr::select(
      Arm = dplyr::all_of(arm),
      Visit = dplyr::all_of(visit),
      est = estimate_est,
      se = se_est,
      lcl = lower_cl_est,
      ucl = upper_cl_est
    )

  # 2. Inject the Baseline zero-point for Change from Baseline
  base_rows <- plot_df |>
    dplyr::distinct(Arm) |>
    dplyr::mutate(
      Visit = "Baseline",
      est = 0,
      se = 0,
      lcl = 0,
      ucl = 0
    )

  # Combine and ensure Baseline is the first factor level
  plot_df <- dplyr::bind_rows(base_rows, plot_df) |>
    dplyr::mutate(
      Visit = fct_relevel(factor(Visit), "Baseline")
    )

  # 3. Calculate plotting bounds based on SE or CI preference
  if (error_bar == "se") {
    plot_df <- plot_df |>
      dplyr::mutate(
        ymin = - est - se,
        ymax = - est + se
      )
    y_label <- "Adjusted Mean Change from Baseline (\u00B1 SE)"
  } else {
    plot_df <- plot_df |>
      dplyr::mutate(
        ymin = lcl,
        ymax = ucl
      )
    y_label <- "Adjusted Mean Change from Baseline (\u00B1 95% CI)"
  }

  # 4. Build the plot with jittering (position_dodge)
  pd <- ggplot2::position_dodge(width = dodge_width)

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = Visit, y = - est, group = Arm, color = Arm, shape = Arm, linetype = Arm)
  )

  # Optional Reference Line
  if (!is.null(hline)) {
    p <- p + ggplot2::geom_hline(yintercept = hline, linetype = "dashed", color = "gray50", alpha = 0.7)
  }

  # Add Lines, Points, and Error Bars
  p <- p +
    ggplot2::geom_line(position = pd, linewidth = 1) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax), width = 0.15, position = pd, linewidth = 0.8) +
    ggplot2::geom_point(position = pd, size = 3, fill = "white", stroke = 1.5) +

    # Formatting and Theming
    ggplot2::labs(
      x = "Visit",
      y = y_label,
      color = "Treatment",
      shape = "Treatment",
      linetype = "Treatment"
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = legend_pos,
      legend.title = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "gray90"),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 1)
    )

  p
}
