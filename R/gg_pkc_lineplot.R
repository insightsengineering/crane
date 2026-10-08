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
#' @param x_breaks (`numeric`, `function`, or `NULL`)\cr
#'   Breaks for a numeric x-axis, passed to [ggplot2::scale_x_continuous()].
#'   The default `NULL` puts a break on every observed timepoint, which is
#'   readable over a short window but not over a long one. Supply a vector
#'   (e.g. `seq(0, 600, by = 100)`) or a break function
#'   (e.g. `scales::breaks_pretty()`) for wider time ranges. Ignored when
#'   `time_var` is a factor.
#' @param errorbar_width (`numeric`)\cr
#'   Width of the horizontal caps on the variability error bars. Default is
#'   `0.45`. Set to `0` for no caps. Ignored when `variability = "none"`.
#' @param dodge_width (`numeric`)\cr
#'   Horizontal separation between groups at each timepoint. Default is `0.2`.
#'   Increase if error bar caps are wider than the dodge width and overlap.
#'
#' @returns A `ggplot` object.
#' @seealso [annotate_pkc_df()] for related functionalities.
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
                            conf_level = 0.95,
                            log_y = TRUE,
                            lloq = NA_real_,
                            x_breaks = NULL,
                            errorbar_width = 0.45,
                            dodge_width = 0.2) {
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

  # time_var can be supplied as factor or numeric; convert factor to numeric
  # when possible, otherwise keep it as a factor for discrete plotting
  if (!is.numeric(data[[time_var]])) {
    # 1. "Test" the conversion silently to see if it results in NAs
    test_numeric <- suppressWarnings(as.numeric(as.character(data[[time_var]])))

    # 2. Only overwrite the data if the conversion was 100% successful
    if (!any(is.na(test_numeric) & !is.na(data[[time_var]]))) {
      data <- data |>
        dplyr::mutate(!!time_var := as.numeric(as.character(.data[[time_var]])))
    } else {
      # If it has text like "week 1", leave it as a factor and let ggplot2 handle it natively!
      cli::cli_inform(
        c("i" = "Categorical X-axis detected. Leaving as factor for discrete plotting.")
      )
    }
  }

  # Ensure only single columns were selected
  check_string(time_var)
  check_string(analyte_var)
  check_string(group)

  pd <- ggplot2::position_dodge(width = dodge_width)

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
    p <- p |>
      gg_add_stats(stat, variability, conf_level, position = pd, width = errorbar_width)
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

  if (is.numeric(data[[time_var]])) {
    # Aligning plot to actual timepoints in the data frame to ensure
    # categorical mapping scales appropriately for cowplot alignments downstream.
    # A single scale is added: adding two would make ggplot2 drop the first one
    # and warn on every call.
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = x_breaks %||% sort(unique(data[[time_var]])),
        expand = ggplot2::expansion(mult = 0.05)
      ) +
      ggplot2::coord_cartesian(xlim = range(data[[time_var]]))
  }

  class(p) <- c("crane_gg_pkc", class(p))

  p
}
