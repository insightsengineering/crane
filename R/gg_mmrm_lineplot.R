#' Create MMRM Line Plot
#'
#' Generates a standardized line plot for Mixed-Effect Repeated Measures Model (MMRM)
#' analysis. It displays adjusted means (change from baseline)
#' along with either 95% Confidence Intervals (CI) or Standard Errors (SE) over time.
#'
#' @param mmrm_df (`data.frame`)\cr
#'   A tidy data frame containing the MMRM results. Usually the output of [get_mmrm_results()].
#' @param arm (`string`)\cr
#'   The column in `mmrm_df` that identifies the treatment arms.
#' @param visit (`string`)\cr
#'   The column in `mmrm_df` that identifies the visits.
#' @param error_bar (`string`)\cr
#'   Type of error bars to display. Either `"ci"` (95% Confidence Interval) or `"se"` (Standard Error).
#'   Default is `"ci"`.
#' @param dodge_width (`numeric`)\cr
#'   The amount to jitter the x-axis points to prevent overlapping error bars. Default is `0.15`.
#' @param hline (`numeric` or `NULL`)\cr
#'   The y-intercept for a horizontal reference line. Set to `NULL` to remove. Default is `0`.
#' @param legend_pos (`numeric` vector or `string`)\cr
#'   The position of the legend. To place it inside the plot on the bottom left, use `c(0.02, 0.02)`.
#'   Can also be standard string positions like `"bottom"`, `"right"`, `"none"`.
#'
#' @return A `ggplot` object.
#'
#' @seealso [get_mmrm_results()] to get the mmrm resuls, and [tbl_mmrm()] for a summary table of the MMRM results.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("mmrm", quietly = TRUE)
#' library(mmrm)
#' fv_dt <- mmrm::fev_data |>
#'   dplyr::mutate(
#'     ARMCD = factor(ARMCD)
#'   )
#' # Fit an MMRM model using the FEV data
#' fit_mmrm <- mmrm::mmrm(
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fv_dt
#' )
#' mmrm_results <- get_mmrm_results(fit_mmrm, arm = "ARMCD", visit = "AVISIT", conf_level = 0.95)
#'
#' # Create a plot with SE bars, jittered by 0.2, legend inside bottom-left
#' my_plot <- gg_lineplot_mmrm(
#'   mmrm_df = mmrm_results,
#'   arm = "ARMCD",
#'   visit = "AVISIT",
#'   error_bar = "se", # Switch to "ci" for Confidence Intervals
#'   dodge_width = 0.2, # Jitters the x-axis to prevent overlap
#'   legend_pos = c(0.02, 0.02)
#' )
#'
#' print(my_plot)
#'
#' @export
gg_lineplot_mmrm <- function(mmrm_df, arm, visit, error_bar = c("ci", "se"),
                             dodge_width = 0.15, hline = 0, legend_pos = c(0.02, 0.02)) {
  error_bar <- match.arg(error_bar)

  # 1. Mandatory Arguments & Tidy-selection
  check_not_missing(mmrm_df)
  check_not_missing(arm)
  check_not_missing(visit)

  cards::process_selectors(
    mmrm_df,
    arm = {{ arm }}, visit = {{ visit }}
  )

  # 2. Type & Class Checking
  check_data_frame(mmrm_df)
  check_class(mmrm_df, "mmrm_df")
  check_string(arm)
  check_string(visit)

  # 3. Graphical Parameter Validation
  if (!is.numeric(dodge_width) || length(dodge_width) != 1) {
    stop("`dodge_width` must be a single numeric value.")
  }
  if (!is.null(hline) && (!is.numeric(hline) || length(hline) != 1)) {
    stop("`hline` must be a single numeric value or NULL.")
  }
  if (!is.character(legend_pos) && !is.numeric(legend_pos)) {
    stop("`legend_pos` must be a character string (e.g., 'bottom') or a numeric vector of length 2.")
  }

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
  # Note: Since estimates are inverted (-est), the CI bounds MUST also be inverted and swapped.
  if (error_bar == "se") {
    plot_df <- plot_df |>
      dplyr::mutate(
        plot_y = -est,
        ymin = -est - se,
        ymax = -est + se
      )
    y_label <- "Mean (\u00B1 SE) Change from Baseline"
  } else {
    plot_df <- plot_df |>
      dplyr::mutate(
        plot_y = -est,
        ymin = -ucl, # Negated and swapped because of the axis inversion
        ymax = -lcl
      )
    y_label <- "Mean (\u00B1 95% CI) Change from Baseline"
  }

  # 4. Build the plot with jittering (position_dodge)
  pd <- ggplot2::position_dodge(width = dodge_width)

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = Visit, y = plot_y, group = Arm, color = Arm, shape = Arm, linetype = Arm)
  )

  # Optional Reference Line
  if (!is.null(hline)) {
    p <- p + ggplot2::geom_hline(yintercept = hline, linetype = "dashed", color = "gray50", alpha = 0.7)
  }

  # Add Lines, Points, and Error Bars
  p <- p +
    ggplot2::geom_line(position = pd, linewidth = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax), width = 0.15, position = pd, linewidth = 0.6) +
    ggplot2::geom_point(position = pd, size = 1.5, fill = "white", stroke = 1) +

    # Formatting and Theming
    ggplot2::labs(
      x = "Visit",
      y = y_label,
      color = "Treatment",
      shape = "Treatment",
      linetype = "Treatment"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = legend_pos,
      legend.justification = if (is.numeric(legend_pos)) c(0, 0) else "center",
      legend.background = ggplot2::element_rect(
        fill = ggplot2::alpha("white", 0.9), color = "grey50",
        linewidth = 0.5
      ),
      legend.title = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "gray90"),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
    )

  p
}
