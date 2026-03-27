#' Plot Pharmacokinetic Concentration Time Profile
#'
#' @description
#' Creates a standard Pharmacokinetic (PK) concentration-time profile plot.
#' This function wraps `ggplot2` calls to consistently format PK profiles,
#' handling log transformations, various summary statistics, and variability measures.
#'
#' @param data (`data.frame`)\cr
#'    The dataset containing PK data.
#' @param time_var ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'    The time variable (x-axis).
#' @param analyte_var ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'    The concentration/analyte variable (y-axis).
#' @param group ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'    The grouping/treatment variable.
#' @param stat (`string`)\cr
#'    Primary summary statistic: `"mean"` or `"median"`. Default is `"mean"`.
#' @param variability (`string`)\cr
#'    Variability measure: `"sd"`, `"se"`, `"ci"`, `"iqr"`, or `"none"`. Default is `"sd"`.
#' @param log_y (`logical`)\cr
#'    Whether to apply log10 scale to the y-axis. Default is `TRUE`.
#' @param lloq (`numeric` or `NULL`)\cr
#'    Lower Limit of Quantification. Default is `NA_real_`.
#'
#' @returns A `ggplot` object.
#'
#' @examples
#' # Prepare PK Data using the built-in Theoph dataset
#' df_pk <- Theoph
#' df_pk$Time_Nominal <- round(df_pk$Time)
#' # Filter to specific timepoints to keep the table clean
#' df_pk <- df_pk[df_pk$Time_Nominal %in% c(0, 2, 4, 8, 24), ]
#' # Create a mock treatment group based on Dose
#' df_pk$Dose_Group <- ifelse(df_pk$Dose > 4.5, "High Dose", "Low Dose")
#'
#' # Linear Scale Example (Baseline 0 is included)
#' gg_pkc_lineplot(
#'   data = df_pk,
#'   time_var = Time_Nominal,
#'   analyte_var = conc,
#'   group = Dose_Group,
#'   stat = "mean",
#'   variability = "sd",
#'   log_y = FALSE
#' )
#'
#' # Log Scale Example (Filter out 0s first to avoid log(0) warnings)
#' df_pk |>
#'   dplyr::filter(conc > 0) |>
#'   gg_pkc_lineplot(
#'     time_var = Time_Nominal,
#'     analyte_var = conc,
#'     group = Dose_Group,
#'     stat = "mean",
#'     variability = "se",
#'     log_y = TRUE,
#'     lloq = 2.0
#'   )
#'
#' # Title, subtitle, axes labels and legend position customization
#' gg_pkc_lineplot(
#'   data = df_pk,
#'   time_var = Time_Nominal,
#'   analyte_var = conc,
#'   group = Dose_Group,
#'   stat = "mean",
#'   variability = "sd",
#'   log_y = FALSE
#' ) +
#'   ggplot2::labs(
#'     x = "Nominal time (hr)",
#'     y = "Concentration (ng/mL)",
#'     title = "Title",
#'     subtitle = "Subtitle"
#'   ) +
#'   ggplot2::theme(
#'     legend.position = "top"
#'   )
#'
#' @export
gg_pkc_lineplot <- function(data,
                            time_var,
                            analyte_var,
                            group,
                            stat = c("mean", "median"),
                            variability = c("sd", "se", "ci", "iqr", "none"),
                            log_y = TRUE,
                            lloq = NA_real_) {
  stat <- match.arg(stat)
  variability <- match.arg(variability)

  # Mandatory Arguments Validation
  check_not_missing(data)
  check_not_missing(time_var)
  check_not_missing(analyte_var)
  check_not_missing(group)
  check_data_frame(data)

  # Tidy-selection processing
  cards::process_selectors(
    data,
    time_var = {{ time_var }},
    analyte_var = {{ analyte_var }},
    group = {{ group }}
  )

  # Ensure only single columns were selected
  check_string(time_var)
  check_string(analyte_var)
  check_string(group)

  # ----------------------------------------------------------------------------
  # BUILD PLOT
  # ----------------------------------------------------------------------------
  pd <- ggplot2::position_dodge(width = 0.2)

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[[time_var]],
      y = .data[[analyte_var]],
      color = .data[[group]],
      shape = .data[[group]],
      linetype = .data[[group]]
    )
  ) +
    ggplot2::stat_summary(fun = stat, geom = "line", linewidth = 0.8, position = pd) +
    ggplot2::stat_summary(fun = stat, geom = "point", size = 2, position = pd)

  # Add Variability (Error Bars)
  if (stat == "mean" && variability %in% c("sd", "se", "ci")) {
    p <- p + ggplot2::stat_summary(
      fun.data = function(val) {
        m <- mean(val, na.rm = TRUE)
        se <- stats::sd(val, na.rm = TRUE) / sqrt(sum(!is.na(val)))

        err <- switch(variability,
          "sd" = stats::sd(val, na.rm = TRUE),
          "se" = se,
          "ci" = stats::qt(0.975, df = max(1, sum(!is.na(val)) - 1)) * se
        )

        data.frame(y = m, ymin = m - err, ymax = m + err)
      },
      geom = "errorbar", width = 0.2, position = pd
    )
  } else if (stat == "median" && variability == "iqr") {
    p <- p + ggplot2::stat_summary(
      fun.data = function(val) {
        m <- stats::median(val, na.rm = TRUE)
        ymin_val <- unname(stats::quantile(val, 0.25, na.rm = TRUE))
        ymax_val <- unname(stats::quantile(val, 0.75, na.rm = TRUE))

        data.frame(y = m, ymin = ymin_val, ymax = ymax_val)
      },
      geom = "errorbar", width = 0.2, position = pd
    )
  } else if (variability != "none") {
    cli::cli_abort("Invalid combination of stat ({.val {stat}}) and variability ({.val {variability}}).")
  }

  # ----------------------------------------------------------------------------
  # Log Scale & LLOQ
  # ----------------------------------------------------------------------------
  if (log_y) {
    p <- p + ggplot2::scale_y_log10()
  }

  if (!is.na(lloq)) {
    p <- p + ggplot2::geom_hline(
      yintercept = lloq,
      linetype = "dashed",
      color = "gray50"
    )
  }

  # ----------------------------------------------------------------------------
  # THEMING & SCALES
  # ----------------------------------------------------------------------------
  p <- p +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title.position = "top",
      legend.title.align = 0.5,
      legend.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.5),
      plot.title = ggplot2::element_text(face = "bold")
    )
  new_x_scale <- ggplot2::scale_x_continuous(
    breaks = unique(data[[time_var]]),
    expand = ggplot2::expansion(mult = 0.05)
  )
  p <- p +
    new_x_scale + ggplot2::coord_cartesian(xlim = range(data[[time_var]]))

  class(p) <- c("crane_gg_pkc", class(p))

  p
}
