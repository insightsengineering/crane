#' Plot Pharmacokinetic Concentration Time Profile
#'
#' @description
#' Creates a standard Pharmacokinetic (PK) concentration-time profile plot.
#' This function wraps `ggplot2` calls to consistently format PK profiles,
#' handling log transformations, various summary statistics, and variability measures.
#'
#' @param data (`data.frame`)\cr
#'   The dataset containing PK data.
#' @param time_var ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The time variable (x-axis).
#' @param analyte_var ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The concentration/analyte variable (y-axis).
#' @param group ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The grouping/treatment variable.
#' @param stat (`string`)\cr
#'   Primary summary statistic: `"mean"` or `"median"`. Default is `"mean"`.
#' @param variability (`string`)\cr
#'   Variability measure: `"sd"`, `"se"`, `"ci"`, `"iqr"`, or `"none"`. Default is `"sd"`.
#' @param conf_level (`numeric`)\cr
#'   Confidence level for error bars when `variability = "ci"` (default: `0.95`).
#' @param log_y (`logical`)\cr
#'   Whether to apply log10 scale to the y-axis. Default is `TRUE`.
#' @param lloq (`numeric` or `NULL`)\cr
#'   Lower Limit of Quantification. Default is `NA_real_`.
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
#' @export
#' @importFrom rlang .data
gg_pkc_lineplot <- function(data,
                            time_var,
                            analyte_var,
                            group,
                            stat = c("mean", "median"),
                            variability = c("sd", "se", "ci", "iqr", "none"),
                            conf_level = 0.95,
                            log_y = TRUE,
                            lloq = NA_real_) {
  # Match standard arguments
  stat <- match.arg(stat)
  variability <- match.arg(variability)

  # Prevent mathematically invalid combinations
  if (stat == "mean" && variability == "iqr") {
    cli::cli_abort(
      paste0(
        "Invalid combination of stat ({.val {stat}})",
        "and variability ({.val {variability}})."
      )
    )
  } else if (stat == "median" && variability %in% c("sd", "se", "ci")) {
    cli::cli_abort(
      paste0(
        "Invalid combination of stat ({.val {stat}})",
        "and variability ({.val {variability}})."
      )
    )
  }

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

  # change from factor to numeric
  # time_var can be factor or numeric - factor allow for correct n of decimals
  # in the summary table
  # factor is
  if (!is.numeric(data[[time_var]])) {
    data <- data |>
      mutate(!!time_var := as.numeric(as.character(.data[[time_var]])))
  } else if (is.numeric(data[[time_var]])) {
    cli::cli_warn(
      "i" = paste0(
        "We encourage to supply `time_var` as a factor, since it supports ",
        "correct decimals formatting in the summary table."
      )
    )
  }

  # Ensure only single columns were selected
  check_string(time_var)
  check_string(analyte_var)
  check_string(group)

  pd <- ggplot2::position_dodge(width = 0.2)

  # Base Plot
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
    ggplot2::stat_summary(
      fun = stat, geom = "line", linewidth = 0.8, position = pd, na.rm = TRUE
    ) +
    ggplot2::stat_summary(
      fun = stat, geom = "point", size = 2, position = pd, na.rm = TRUE
    )

  # Add Variability (Error Bars) using our unified math engine
  if (variability != "none") {
    p <- p + ggplot2::stat_summary(
      fun.data = gg_get_summary_stats(
        stat = stat,
        variability = variability,
        conf_level = conf_level
      ),
      geom = "errorbar",
      width = 0.2,
      position = pd,
      na.rm = TRUE
    )
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
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title.position = "top",
      legend.title.align = 0.5,
      legend.background = ggplot2::element_rect(
        fill = "white",
        color = "black",
        linewidth = 0.5
      ),
      plot.title = ggplot2::element_text(face = "bold")
    )

  # Aligning plot to actual timepoints in the data frame to ensure
  # categorical mapping scales appropriately for cowplot alignments downstream
  new_x_scale <- ggplot2::scale_x_continuous(
    breaks = data[[time_var]],
    expand = ggplot2::expansion(mult = 0.05)
  )

  p <- p +
    new_x_scale + ggplot2::coord_cartesian(xlim = range(data[[time_var]]))

  class(p) <- c("crane_gg_pkc", class(p))

  return(p)
}
