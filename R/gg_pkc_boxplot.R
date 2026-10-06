#' Plot Pharmacokinetic Concentrations as Box Plots
#'
#' @description
#' Creates a standard Pharmacokinetic (PK) concentration box plot, summarizing
#' the distribution of concentrations at each timepoint and treatment group.
#' This function wraps `ggplot2` calls to consistently format PK box plots,
#' handling log transformations, whisker definitions, and outlier detection.
#'
#' @param data (`data.frame`)\cr
#'   The dataset containing PK data.
#' @param time_var ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The time variable (x-axis).
#' @param analyte_var ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The concentration/analyte variable (y-axis).
#' @param group ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The grouping/treatment variable.
#' @param whisker (`string`)\cr
#'   Method used to compute the box whiskers: `"percentile"` (5% and 95%
#'   percentiles), `"tukey"` (1.5 * IQR beyond the hinges, the conventional
#'   boxplot definition), or `"minmax"` (full data range, so no points are
#'   flagged as outliers). Default is `"percentile"`.
#' @param quantile_type (`integer`)\cr
#'   Quantile algorithm `type` passed to [stats::quantile()]. Default is `1`.
#' @param log_y (`logical`)\cr
#'   Whether to apply log10 scale to the y-axis. Default is `TRUE`.
#' @param show_mean (`logical`)\cr
#'   Whether to overlay a mean marker on each box. Default is `TRUE`.
#' @param outlier_size (`numeric`)\cr
#'   Size of the outlier point markers. Default is `2`.
#'
#' @returns A `ggplot` object of class `crane_gg_pkc_box`.
#' @seealso [gg_pkc_lineplot()] for related functionalities.
#'
#' @examples
#' # Prepare PK Data using the built-in Theoph dataset
#' df_pk <- Theoph
#' df_pk$Time_Nominal <- round(df_pk$Time)
#' # Filter to specific timepoints to keep the plot clean
#' df_pk <- df_pk[df_pk$Time_Nominal %in% c(0, 2, 4, 8, 24), ]
#' # Create a mock treatment group based on Dose
#' df_pk$Dose_Group <- ifelse(df_pk$Dose > 4.5, "High Dose", "Low Dose")
#'
#' # Default Plot: percentile whiskers on a log10 y-axis
#' gg_pkc_boxplot(
#'   data = df_pk,
#'   time_var = Time_Nominal,
#'   analyte_var = conc,
#'   group = Dose_Group
#' )
#'
#' # Tukey whiskers on a linear y-axis, without the mean marker
#' gg_pkc_boxplot(
#'   data = df_pk,
#'   time_var = Time_Nominal,
#'   analyte_var = conc,
#'   group = Dose_Group,
#'   whisker = "tukey",
#'   log_y = FALSE,
#'   show_mean = FALSE
#' )
#'
#' @export
gg_pkc_boxplot <- function(
  data,
  time_var,
  analyte_var,
  group,
  whisker = c("percentile", "tukey", "minmax"),
  quantile_type = 1,
  log_y = TRUE,
  show_mean = TRUE,
  outlier_size = 2
) {
  # Match standard arguments
  whisker <- match.arg(whisker)

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

  # A log10 y-axis cannot plot zero/negative values, so drop them up front
  # rather than letting ggplot2 silently remove them layer by layer.
  if (log_y) {
    data <- data |>
      dplyr::filter(!is.na(.data[[analyte_var]]), .data[[analyte_var]] > 0)
  }

  # Computes the five summary values `geom = "boxplot"` expects (ymin, lower,
  # middle, upper, ymax) for one numeric vector. The hinges (lower/middle/
  # upper) are always quartiles; only the whisker limits (ymin/ymax) change
  # with the `whisker` method, so outlier detection and box drawing stay
  # consistent with each other.
  box_stats <- function(x) {
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      # No non-missing values for this timepoint/group: return NAs so
      # ggplot2 drops the box instead of erroring.
      return(c(
        ymin = NA_real_, lower = NA_real_, middle = NA_real_,
        upper = NA_real_, ymax = NA_real_
      ))
    }

    # Box hinges: 25th/50th (median)/75th percentiles
    q <- stats::quantile(
      x,
      probs = c(0.25, 0.50, 0.75),
      type = quantile_type,
      names = FALSE
    )

    limits <- switch(whisker,
      # 5% and 95% percentiles of the data
      percentile = stats::quantile(
        x,
        probs = c(0.05, 0.95),
        type = quantile_type,
        names = FALSE
      ),
      # Full data range: no point can fall outside, so there are no outliers
      minmax = range(x),
      # Standard 1.5 * IQR rule, clipped to the observed data range
      tukey = c(
        max(min(x), q[[1]] - 1.5 * (q[[3]] - q[[1]])),
        min(max(x), q[[3]] + 1.5 * (q[[3]] - q[[1]]))
      )
    )

    c(
      ymin = limits[[1]],
      lower = q[[1]],
      middle = q[[2]],
      upper = q[[3]],
      ymax = limits[[2]]
    )
  }

  # Flags values of x that fall outside the whisker limits computed by
  # `box_stats()`, so they can be drawn as individual points instead of
  # being absorbed into the whiskers.
  is_outlier <- function(x) {
    x <- x[!is.na(x)]

    if (whisker == "minmax") {
      return(rep(FALSE, length(x)))
    }

    limits <- box_stats(x)[c("ymin", "ymax")]
    x < limits[[1]] | x > limits[[2]]
  }

  # Convert time_var to an ordered factor so every timepoint gets its own
  # discrete x position and dodged group of boxes, regardless of whether the
  # original time_var is numeric or character.
  data <- data |>
    dplyr::mutate(
      .time = factor(.data[[time_var]], levels = sort(unique(.data[[time_var]])))
    )

  # Pre-compute, per timepoint and group, which observations are outliers.
  # Non-outliers are set to NA so they are skipped by the outlier point layer
  # below (`geom_point(na.rm = TRUE)`), which otherwise overlays every raw
  # observation on top of the boxes.
  outliers <- data |>
    dplyr::group_by(.data$.time, .data[[group]]) |>
    dplyr::mutate(.outlier = dplyr::if_else(
      is_outlier(.data[[analyte_var]]),
      .data[[analyte_var]],
      NA_real_
    )) |>
    dplyr::ungroup()

  # A single shared dodge width keeps the boxes, whisker caps, outlier points
  # and mean marker aligned within the same timepoint.
  dodge <- ggplot2::position_dodge(width = 0.8)

  # Base Plot: draw the box (hinges + whiskers) via `box_stats()` instead of
  # geom_boxplot()'s built-in stat, so the whisker definition can vary with
  # `whisker`.
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$.time,
      y = .data[[analyte_var]],
      fill = .data[[group]]
    )
  ) +
    ggplot2::stat_summary(
      fun.data = box_stats,
      geom = "boxplot",
      position = dodge
    ) +
    # geom_boxplot() draws the whisker lines but no end caps; add a matching
    # errorbar layer (same box_stats) purely to draw horizontal caps at
    # ymin/ymax for readability.
    ggplot2::stat_summary(
      fun.data = box_stats,
      geom = "errorbar",
      width = 0.8,
      position = dodge
    ) +
    # Overlay the pre-flagged outlier values on top of the boxes; na.rm drops
    # the non-outlier rows that were set to NA above.
    ggplot2::geom_point(
      data = outliers,
      ggplot2::aes(y = .data$.outlier),
      shape = 1,
      size = outlier_size,
      position = dodge,
      na.rm = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title.position = "top",
      legend.title.align = 0.5,
      legend.background = ggplot2::element_rect(
        fill = "white", color = "black", linewidth = 0.5
      )
    )

  # Optional mean marker, dodged the same way so it lines up with its group's
  # box rather than sitting at the center of the timepoint.
  if (show_mean) {
    p <- p +
      ggplot2::stat_summary(
        fun = "mean",
        geom = "point",
        color = "black",
        shape = 8,
        size = 3,
        position = dodge
      )
  }

  # Log Scale
  if (log_y) {
    p <- p + ggplot2::scale_y_log10()
  }

  class(p) <- c("crane_gg_pkc_box", class(p))
  p
}
