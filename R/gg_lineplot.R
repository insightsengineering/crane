#' @title Calculate Summary Statistics for a Vector
#'
#' @description Calculates N, mean, standard deviation, and a two-sided confidence
#' interval for the mean of a numeric vector.
#'
#' @param x (`numeric`)\cr vector to summarize.
#' @param conf_level (`probability`)\cr The confidence level for the interval (default is 0.95).
#' @param decimal_places (`integer`)\cr Number of decimal places for numeric values in the table.
#' @return A `list` with statistics: `n`, `mean`, `mean_ci_lwr`, `mean_ci_upr`, `median`, `sd`.
#' @export
#' @examples
#' x <- c(10, 20, 40, NA, 30)
#' calc_stats(x, conf_level = 0.95, decimal_places = 2)
calc_stats <- function(x, conf_level = 0.95, decimal_places = 2) {
  n <- sum(!is.na(x))
  if (n == 0) {
    return(list(n = 0, mean = NA, mean_ci_lwr = NA, mean_ci_upr = NA, median = NA, sd = NA))
  }

  m <- mean(x, na.rm = TRUE)
  se <- stats::sd(x, na.rm = TRUE) / sqrt(n)
  ci_val <- stats::qt((1 + conf_level) / 2, df = n - 1) * se
  mean_ci_lwr <- round(m - ci_val, decimal_places)
  mean_ci_upr <- round(m + ci_val, decimal_places)

  list(
    n = n,
    mean = round(m, decimal_places),
    mean_ci = paste(
      mean_ci_lwr,
      mean_ci_upr
    ),
    mean_ci_lwr = mean_ci_lwr,
    mean_ci_upr = mean_ci_upr,
    median = stats::median(x, na.rm = TRUE),
    sd = stats::sd(x, na.rm = TRUE)
  )
}


#' @title Generate a Line Plot from Pre-calculated Statistics
#'
#' @description Creates a `ggplot` line plot of mean (or other mid-point) and
#' confidence intervals from a summary data frame.
#'
#' @param df_stats (`data.frame`)\cr containing pre-calculated statistics (e.g., mean, CIs, N).
#' @param x (`string`)\cr Column name for the x-axis (must be in `df_stats`).
#' @param mid (`string`)\cr Column name for the y-axis middle point (e.g., "mean").
#' @param strata_N (`string`)\cr Column name for the stratification variable used for grouping/coloring (can be `NULL`).
#' @param whiskers (`character`)\cr A vector of two column names for the lower and upper error bar limits.
#' @param mid_type (`string`)\cr String indicating whether to plot points ("p"), lines ("l"), or both ("pl").
#' @param mid_point_size (`numeric`)\cr Numeric value for the size of points.
#' @param position (`position`)\cr Position adjustment for dodging points and lines (default: `position_dodge(width = 0.4)`).
#' @param errorbar_width (`numeric`)\cr Width of error bars (default: 0.45).
#' @param col (`character`)\cr Vector of color values for manual color scaling.
#' @param linetype (`character`)\cr Vector of line type values for manual line type scaling.
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Create example statistics data frame
#' df_stats <- data.frame(
#'   AVISIT = factor(c("Baseline", "Week 4", "Week 8")),
#'   ARM = c("Treatment", "Treatment", "Treatment"),
#'   mean = c(10.5, 12.3, 14.1),
#'   mean_ci_lwr = c(9.2, 11.0, 12.8),
#'   mean_ci_upr = c(11.8, 13.6, 15.4)
#' )
#'
#' # Basic line plot without stratification
#' g_lineplot(
#'   df_stats = df_stats,
#'   x = "AVISIT",
#'   mid = "mean",
#'   whiskers = c("mean_ci_lwr", "mean_ci_upr")
#' )
#'
#' # Line plot with stratification
#' df_stats_strat <- rbind(
#'   transform(df_stats, ARM = "Treatment A"),
#'   transform(df_stats,
#'     ARM = "Treatment B", mean = mean + 2,
#'     mean_ci_lwr = mean_ci_lwr + 2, mean_ci_upr = mean_ci_upr + 2
#'   )
#' )
#' g_lineplot(
#'   df_stats = df_stats_strat,
#'   x = "AVISIT",
#'   mid = "mean",
#'   strata_N = "ARM",
#'   whiskers = c("mean_ci_lwr", "mean_ci_upr")
#' )
g_lineplot <- function(df_stats,
                                     x = "AVISIT",
                                     mid = "mean",
                                     strata_N = NULL,
                                     whiskers = c("mean_ci_lwr", "mean_ci_upr"),
                                     mid_type = "pl",
                                     mid_point_size = 2,
                                     position = position_dodge(width = 0.4),
                                     errorbar_width = 0.45,
                                     col = NULL,
                                     linetype = NULL) {
  p <- ggplot(
    data = df_stats,
    aes(
      x = .data[[x]],
      y = .data[[mid]],
      color = if (is.null(strata_N)) NULL else .data[[strata_N]],
      shape = if (is.null(strata_N)) NULL else .data[[strata_N]],
      lty = if (is.null(strata_N)) NULL else .data[[strata_N]],
      group = if (is.null(strata_N)) NULL else .data[[strata_N]]
    )
  )

  # Add points
  if (grepl("p", mid_type, fixed = TRUE)) {
    p <- p + geom_point(position = position, size = mid_point_size, na.rm = TRUE)
  }

  # Add lines
  if (grepl("l", mid_type, fixed = TRUE) && !is.null(strata_N)) {
    p <- p + geom_line(position = position, na.rm = TRUE)
  }

  # Add error bars
  if (!is.null(whiskers) && length(whiskers) >= 2) {
    p <- p + geom_errorbar(
      aes(ymin = .data[[whiskers[1]]], ymax = .data[[whiskers[2]]]),
      width = errorbar_width,
      position = position,
      na.rm = TRUE
    )
  }

  # Add labels and theme
  p <- p +
    scale_y_continuous(labels = scales::label_comma())


  if (!is.null(col)) {
    p <- p + scale_color_manual(values = col)
  }
  if (!is.null(linetype)) {
    p <- p + scale_linetype_manual(values = linetype)
  }

  return(p)
}

#' @title Generate a ggplot Table from Pre-calculated Statistics
#'
#' @description Creates a ggplot object containing text data for use as a table
#' component beneath a main plot.
#'
#' @param df_stats (`data.frame`)\cr containing pre-calculated statistics.
#' @param x (`string`)\cr Column name for the x-axis (must be in `df_stats`).
#' @param group_var (`string`)\cr Column name for the grouping/stratification variable (can be `NULL`).
#' @param table (`character`)\cr A character vector of statistic column names to display in the table (e.g., c(`"n"`, `"mean"`, `"mean_ci"`)).
#' @param table_font_size (`integer`)\cr Font size for the table text.
#' @param decimal_places (`integer`)\cr Integer specifying the number of decimal places for numeric statistics (like 'mean').
#' @return A `ggplot` object formatted as a table.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(tidyr)
#'
#' # Create example statistics data frame
#' df_stats <- data.frame(
#'   AVISIT = factor(c("Baseline", "Week 4", "Week 8")),
#'   ARM = c("Treatment", "Treatment", "Treatment"),
#'   n = c(50, 48, 45),
#'   mean = c(10.5, 12.3, 14.1),
#'   mean_ci = c("9.20 11.80", "11.00 13.60", "12.80 15.40")
#' )
#'
#' # Generate table with n and mean
#' g_lineplot_table(
#'   df_stats = df_stats,
#'   x = "AVISIT",
#'   group_var = "ARM",
#'   table = c("n", "mean")
#' )
g_lineplot_table <- function(df_stats,
                             x = "AVISIT",
                             group_var = NULL,
                             table = c("n", "mean"),
                             table_font_size = 3,
                             decimal_places = 2) {
  # Format numeric columns to character strings with specified decimal places
  fmt <- paste0("%.", decimal_places, "f")
  df_stats_formatted <- df_stats |>
    mutate(across(all_of(table) & where(is.numeric), ~ sprintf(fmt, .)))

  # Select and pivot the formatted data
  df_stats_table <- df_stats_formatted |>
    select(all_of(c(group_var, x, table))) |>
    tidyr::pivot_longer(
      cols = -all_of(c(group_var, x)),
      names_to = "stat",
      values_to = "value"
    )

  tbl <- ggplot(df_stats_table, aes(x = .data[[x]], y = .data[["stat"]], label = .data[["value"]])) +
    geom_text(size = table_font_size) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none"
    )

  if (!is.null(group_var)) {
    tbl <- tbl + facet_wrap(stats::as.formula(paste("~", group_var)), ncol = 1)
  }
  return(tbl)
}

#' @title Preprocess Data for Line Plot Statistics
#'
#' @description Cleans, expands, joins, and computes summary statistics
#' (using [calc_stats]) from the raw data frame, preparing it for plotting.
#'
#' @param df (`data.frame`)\cr The primary data frame containing the data to plot (e.g., ADaM BDS).
#' @param alt_counts_df (`data.frame`)\cr An optional data frame for calculating N counts (e.g., ADSL).
#' @param x (`string`)\cr Column name for the x-axis (e.g., `"AVISIT"`).
#' @param y (`string`)\cr Column name for the y-axis values (e.g., `"AVAL"`).
#' @param group_var (`string`)\cr Column name for the grouping variable (e.g., `"ARM"`).
#' @param subject_var (`string`)\cr Column name for the subject ID (e.g., `"USUBJID"`).
#' @param mid (`string`)\cr Column name for the mean/median statistic to be plotted (e.g., `"mean"`).
#' @param calc_stats_func (`function`)\cr A function to calculate summary statistics, defaulting to the provided `calc_stats` function.
#' @param ... Additional arguments passed to the `calc_stats` function (e.g., `conf_level`, `decimal_places`).
#' @return A data frame (`df_stats`) containing the calculated statistics, ready for plotting.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' # Create example ADaM-like data
#' set.seed(123)
#' adlb <- data.frame(
#'   USUBJID = rep(paste0("SUBJ-", 1:20), each = 3),
#'   ARM = rep(c(rep("Treatment A", 10), rep("Treatment B", 10)), each = 3),
#'   AVISIT = rep(factor(c("Baseline", "Week 4", "Week 8")), 20),
#'   AVAL = rnorm(60, mean = 12, sd = 2)
#' )
#'
#' adsl <- data.frame(
#'   USUBJID = paste0("SUBJ-", 1:20),
#'   ARM = c(rep("Treatment A", 10), rep("Treatment B", 10))
#' )
#'
#' # Preprocess data for line plot with default confidence level
#' df_stats <- preprocess_lineplot_data(
#'   df = adlb,
#'   alt_counts_df = adsl,
#'   x = "AVISIT",
#'   y = "AVAL",
#'   group_var = "ARM",
#'   subject_var = "USUBJID"
#' )
#'
#' # Custom confidence level using ...
#' df_stats_90ci <- preprocess_lineplot_data(
#'   df = adlb,
#'   alt_counts_df = adsl,
#'   x = "AVISIT",
#'   y = "AVAL",
#'   group_var = "ARM",
#'   subject_var = "USUBJID",
#'   conf_level = 0.90
#' )
#'
#' # Custom decimal places using ...
#' df_stats_3dec <- preprocess_lineplot_data(
#'   df = adlb,
#'   alt_counts_df = adsl,
#'   x = "AVISIT",
#'   y = "AVAL",
#'   group_var = "ARM",
#'   subject_var = "USUBJID",
#'   decimal_places = 3
#' )
#'
#' # Without grouping variable
#' df_stats_ungrouped <- preprocess_lineplot_data(
#'   df = adlb,
#'   x = "AVISIT",
#'   y = "AVAL",
#'   group_var = NULL
#' )
preprocess_lineplot_data <- function(df,
                                     alt_counts_df = NULL,
                                     x = "AVISIT",
                                     y = "AVAL",
                                     group_var = "ARM",
                                     subject_var = "USUBJID",
                                     mid = "mean",
                                     calc_stats_func = calc_stats,
                                     ...) {
  # Remove unused factor levels
  if (is.factor(df[[x]])) {
    df[[x]] <- droplevels(df[[x]])
  }

  # Expand grid for all combinations (Handles NULL group_var)
  if (!is.null(group_var)) {
    df_grp <- tidyr::expand(df, .data[[group_var]], .data[[x]])
  } else {
    df_grp <- tidyr::expand(df, .data[[x]])
  }

  # Join with actual data and compute statistics
  df_grp <- df_grp |>
    dplyr::full_join(df[, c(group_var, x, y)], by = c(group_var, x), multiple = "all") |>
    dplyr::group_by(across(all_of(c(group_var, x))))

  # 2. CALCULATE STATISTICS
  # Ensure stats column contains only vectors
  df_stats <- df_grp |>
    dplyr::summarise(
      stats = list(as.list(calc_stats_func(.data[[y]], ...))), # Convert to list to ensure compatibility
      .groups = "drop"
    ) |>
    tidyr::unnest_wider(all_of("stats"))

  # Remove NA rows where the midpoint statistic is missing
  df_stats <- df_stats[!is.na(df_stats[[mid]]), ]

  # Add N counts to group labels if alt_counts_df provided
  strata_N <- NULL
  if (!is.null(group_var) && !is.null(alt_counts_df)) {
    strata_N <- paste0(group_var, "_N")

    df_N <- alt_counts_df |>
      dplyr::group_by(.data[[group_var]]) |>
      dplyr::summarise(N = dplyr::n_distinct(.data[[subject_var]]), .groups = "drop") |>
      dplyr::mutate(!!strata_N := paste0(.data[[group_var]], " (N = ", .data[["N"]], ")"))

    df_stats <- df_stats |>
      dplyr::left_join(df_N[, c(group_var, strata_N)], by = group_var) |>
      dplyr::mutate(!!strata_N := factor(.data[[strata_N]], levels = unique(df_N[[strata_N]])))
  }

  return(df_stats)
}
