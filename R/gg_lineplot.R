#' Generate a Summary Line Plot from Raw Data
#'
#' @description Calculates summary statistics inline using `ggplot2::stat_summary()`,
#'   generating a line plot directly from raw data. Supports configurable central
#'   tendencies and dispersion metrics.
#'
#' @param data (`data.frame`)\cr
#'   The raw data frame (e.g., ADaM dataset).
#' @param x ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name for the x-axis timepoints (e.g., `AVISIT`).
#' @param y ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column name for the continuous variable to summarize (e.g., `AVAL`).
#' @param group ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Optional column name for the grouping/treatment variable.
#' @param stat (`string`)\cr
#'   Primary summary statistic: `"mean"` or `"median"`. Default is `"mean"`.
#' @param variability (`string`)\cr
#'   Variability measure: `"sd"`, `"se"`, `"ci"`, `"iqr"`, or `"none"`.
#'   Default is `"ci"`.
#' @param conf_level (`numeric`)\cr
#'   Confidence level for error bars when `variability = "ci"` (default: `0.95`).
#'
#' @return A `ggplot` object of class `crane_gg_line`.
#'
#' @seealso [annotate_lineplot_df()] for related functionalities.
#'
#' @examples
#' set.seed(123)
#' mock_adlb <- data.frame(
#'   ARM = rep(c("Treatment A", "Treatment B"), each = 30),
#'   AVISIT = rep(c(0, 4, 8), 20),
#'   AVAL = rnorm(60, mean = 10, sd = 2)
#' )
#'
#' # 1. Default Plot: Mean with 95% Confidence Intervals
#' gg_lineplot(
#'   data = mock_adlb,
#'   x = AVISIT,
#'   y = AVAL,
#'   group = ARM
#' )
#'
#' # 2. Median with Interquartile Range (IQR)
#' gg_lineplot(
#'   data = mock_adlb,
#'   x = AVISIT,
#'   y = AVAL,
#'   group = ARM,
#'   stat = "median",
#'   variability = "iqr"
#' )
#'
#' # 3. Ungrouped data with Mean and Standard Deviation +
#' # Change legend position to top and add horizontal reference line
#' gg_lineplot(
#'   data = mock_adlb,
#'   x = AVISIT,
#'   y = AVAL,
#'   group = ARM,
#'   stat = "mean",
#'   variability = "sd"
#' ) +
#'   ggplot2::theme(legend.position = "top") +
#'   ggplot2::geom_hline(
#'     yintercept = 30,
#'     linetype = "dashed",
#'     color = "gray50"
#'   )
#' @export
gg_lineplot <- function(data,
                        x,
                        y,
                        group = NULL,
                        stat = c("mean", "median"),
                        variability = c("ci", "sd", "se", "iqr", "none"),
                        conf_level = 0.95) {
  # 1. Argument Matching and Validation
  stat <- match.arg(stat)
  variability <- match.arg(variability)

  # Prevent mathematically invalid combinations
  if (stat == "mean" && variability == "iqr") {
    cli::cli_abort("Invalid combination: Cannot plot IQR around a mean.")
  } else if (stat == "median" && variability %in% c("sd", "se", "ci")) {
    cli::cli_abort(
      paste0(
        "Invalid combination of stat ({.val {stat}}) ",
        "and variability ({.val {variability}})."
      )
    )
  }

  cards::process_selectors(
    data,
    x = {{ x }},
    y = {{ y }},
    group = {{ group }}
  )

  # change from factor to numeric
  # x can be factor or numeric - factor allow for correct n of decimals
  # in the summary table
  if (!is.numeric(data[[x]])) {
    # 1. "Test" the conversion silently to see if it results in NAs
    test_numeric <- suppressWarnings(as.numeric(as.character(data[[x]])))

    # 2. Only overwrite the data if the conversion was 100% successful
    if (!any(is.na(test_numeric) & !is.na(data[[x]]))) {
      data <- data |>
        dplyr::mutate(!!x := as.numeric(as.character(.data[[x]])))
    } else {
      # If it has text like "week 1", leave it as a factor and let ggplot2 handle it natively!
      cli::cli_inform(
        c("i" = "Categorical X-axis detected. Leaving as factor for discrete plotting.")
      )
    }
  } else {
    cli::cli_inform(
      c(
        "i" = paste0(
          "We encourage you to supply `x` as a factor, since it supports ",
          "correct decimals formatting in the summary table."
        )
      )
    )
  }

  # 2. Data Preprocessing
  # Expand grid ensures missing timepoints in specific groups are explicitly
  # populated with NAs. This prevents ggplot from drawing misleading lines
  # that bridge across missing visits.
  if (length(group) > 0) {
    df_plot <- tidyr::expand(data, .data[[group]], .data[[x]]) |>
      dplyr::left_join(data, by = c(group, x))
  } else {
    df_plot <- tidyr::expand(data, .data[[x]]) |>
      dplyr::left_join(data, by = x)
  }

  pd <- ggplot2::position_dodge(width = 0.4)

  # 3. Build Plot: Use clean aes() mapping so gg_varname_extraction succeeds
  if (length(group) > 0) {
    p <- ggplot2::ggplot(
      data = df_plot,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        color = .data[[group]],
        shape = .data[[group]],
        linetype = .data[[group]],
        group = .data[[group]]
      )
    )
  } else {
    p <- ggplot2::ggplot(
      data = df_plot,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]]
      )
    )
  }

  p <- p +
    ggplot2::stat_summary(
      fun = stat,
      geom = "point",
      position = pd,
      size = 2,
      na.rm = TRUE
    )

  if (length(group) > 0) {
    p <- p + ggplot2::stat_summary(
      fun = stat,
      geom = "line",
      position = pd,
      na.rm = TRUE
    )
  }

  # 4. Add Variability Layer conditionally to avoid drawing degenerate lines
  if (variability != "none") {
    p <- p |>
      gg_add_stats(stat, variability, conf_level)
  }

  # 5. Theming
  p <- p +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title.position = "top",
      legend.title.align = 0.5,
      legend.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.5),
      plot.title = ggplot2::element_text(face = "bold")
    )

  # Dynamically label the legends to overwrite the ugly ".data[[group]]" default
  if (length(group) > 0) {
    p <- p + ggplot2::labs(color = group, linetype = group, shape = group)
  }


  # If the x-axis is numeric, force it to only show ticks for actual data points
  if (is.numeric(data[[x]])) {
    p <- p + ggplot2::scale_x_continuous(
      breaks = sort(unique(data[[x]]))
    )
  }

  class(p) <- c("crane_gg_line", class(p))

  p
}
