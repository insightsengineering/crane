#' Plot Pharmacokinetic Concentration Time Profile
#'
#' @description
#' Creates a standard Pharmacokinetic (PK) concentration-time profile plot.
#' This function wraps `ggplot2` calls to consistently format PK profiles,
#' handling log transformations, various summary statistics, and variability measures.
#'
#' @param data (`data.frame`)\cr The dataset containing PK data.
#' @param time_var ([`tidy-select`][dplyr::dplyr_tidy_select])\cr The time variable (x-axis).
#' @param analyte_var ([`tidy-select`][dplyr::dplyr_tidy_select])\cr The concentration/analyte variable (y-axis).
#' @param group ([`tidy-select`][dplyr::dplyr_tidy_select])\cr The grouping/treatment variable.
#' @param stat (`string`)\cr Primary summary statistic: `"mean"` or `"median"`. Default is `"mean"`.
#' @param variability (`string`)\cr Variability measure: `"sd"`, `"se"`, `"ci"`, `"iqr"`, or `"none"`. Default is `"sd"`.
#' @param log_y (`logical`)\cr Whether to apply log10 scale to the y-axis. Default is `TRUE`.
#' @param lloq (`numeric` or `NULL`)\cr Lower Limit of Quantification. Default is `NA_real_`.
#' @param x_label (`string`)\cr X-axis label. Default is `"Nominal time (hr)"`.
#' @param y_label (`string`)\cr Y-axis label. Default is `"Concentration (ng/mL)"`.
#' @param legend_pos (`string`)\cr Standard ggplot2 legend position options. Default is `"bottom"`.
#' @param title (`string` or `NULL`)\cr Plot title.
#' @param subtitle (`string` or `NULL`)\cr Plot subtitle.
#'
#' @returns A `ggplot` object.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' 
#'df_pk <- tibble::tribble(
#'  ~USUBJID, ~TRT,     ~ATPTN, ~AVAL,
#'  "P1",     "Drug A", 0,      0,
#'  "P1",     "Drug A", 4,      10,
#'  "P1",     "Drug A", 12,     1,   
#'  "P2",     "Drug A", 0,      0,
#'  "P2",     "Drug A", 4,      12,
#'  "P2",     "Drug A", 12,     10,  
#'  "P3",     "Drug B", 0,      0,
#'  "P3",     "Drug B", 4,      20,
#'  "P3",     "Drug B", 12,     10,
#'  "P4",     "Drug B", 0,      0,
#'  "P4",     "Drug B", 4,      18,
#'  "P4",     "Drug B", 12,     12
#')
#' 
#' # Linear Scale Example (Baseline 0 is included)
#' gg_pkc_lineplot(
#'   data = df_pk, 
#'   time_var = ATPTN, 
#'   analyte_var = AVAL, 
#'   group = TRT,
#'   stat = "mean",
#'   variability = "sd",
#'   log_y = FALSE
#' )
#' 
#' # Log Scale Example (Filter out 0s first to avoid log(0) warnings)
#' df_pk |>
#'   dplyr::filter(AVAL > 0) |>
#'   gg_pkc_lineplot(
#'     time_var = ATPTN,
#'     analyte_var = AVAL,
#'     group = TRT,
#'     stat = "mean",
#'     variability = "se",
#'     log_y = TRUE,
#'     lloq = 2.0
#'   )
gg_pkc_lineplot <- function(data,
                            time_var,
                            analyte_var,
                            group,
                            stat = c("mean", "median"),
                            variability = c("sd", "se", "ci", "iqr", "none"),
                            log_y = TRUE,
                            lloq = NA_real_,
                            x_label = "Nominal time (hr)",
                            y_label = "Concentration (ng/mL)",
                            legend_pos = "bottom",
                            title = NULL,
                            subtitle = NULL) {
  
  # Match standard arguments
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

  # 4Build Plot
  pd <- ggplot2::position_dodge(width = 0.2)

  # Base Plot (Lines and Points are drawn regardless of variability setting)
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
        se <- sd(val, na.rm = TRUE) / sqrt(sum(!is.na(val)))
        
        err <- switch(variability,
          "sd" = sd(val, na.rm = TRUE),
          "se" = se,
          "ci" = qt(0.975, df = max(1, sum(!is.na(val)) - 1)) * se
        )
        
        # Floor ymin to 1e-5 to prevent log(negative) errors
        data.frame(y = m, ymin = max(m - err, 1e-5), ymax = m + err)
      },
      geom = "errorbar", width = 0.2, position = pd
    )
  } else if (stat == "median" && variability == "iqr") {
    p <- p + ggplot2::stat_summary(
      fun.data = function(val) {
        data.frame(
          y = median(val, na.rm = TRUE),
          ymin = quantile(val, 0.25, na.rm = TRUE),
          ymax = quantile(val, 0.75, na.rm = TRUE)
        )
      },
      geom = "errorbar", width = 0.2, position = pd
    )
  } else if (variability != "none") {
    # Safety catch for invalid combinations (e.g., stat = "median", variability = "sd")
    cli::cli_abort("Invalid combination of stat ({.val {stat}}) and variability ({.val {variability}}).")
  }

  # Log Scale & LLOQ
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

  # Theming
  p <- p +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = legend_pos,
      legend.title.position = "top",
      legend.title.align = 0.5,
      legend.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.5),
      plot.title = ggplot2::element_text(face = "bold")
    )

  p
}