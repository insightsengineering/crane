#' @title Calculate Summary Statistics for a Vector
#'
#' @description Calculates N, mean, standard deviation, and a two-sided confidence
#' interval for the mean of a numeric vector.
#'
#' @param x A numeric vector.
#' @param conf_level The confidence level for the interval (default is 0.95).
#' @param decimal_places Number of decimal places for numeric values in the table.
#' @return A list with statistics: n, mean, mean_ci_lwr, mean_ci_upr, median, sd.
#' @export
#' @examples
#' set.seed(123)
#' x <- rnorm(100, mean = 10, sd = 2)
#' calc_stats(x, conf_level = 0.95, decimal_places = 2)
calc_stats <- function(x, conf_level = 0.95, decimal_places = 2) {
  n <- sum(!is.na(x))
  if (n == 0) return(list(n = 0, mean = NA, mean_ci_lwr = NA, mean_ci_upr = NA, median = NA, sd = NA))

  m <- mean(x, na.rm = TRUE)
  se <- sd(x, na.rm = TRUE) / sqrt(n)
  ci_val <- qt((1 + conf_level) / 2, df = n - 1) * se
  mean_ci_lwr <- round(m - ci_val, decimal_places)
  mean_ci_upr <- round(m + ci_val, decimal_places)

  list(
    n = n,
    mean = round(m, decimal_places),
    mean_ci = paste(mean_ci_lwr,
                    mean_ci_upr),
    mean_ci_lwr = mean_ci_lwr,
    mean_ci_upr = mean_ci_upr,
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
}


#' @title Generate a Line Plot from Pre-calculated Statistics
#'
#' @description Creates a ggplot line plot of mean (or other mid-point) and
#' confidence intervals from a summary data frame.
#'
#' @param df_stats A data frame containing pre-calculated statistics (e.g., mean, CIs, N).
#' @param x Column name for the x-axis (must be in df_stats).
#' @param mid Column name for the y-axis middle point (e.g., "mean").
#' @param strata_N Column name for the stratification variable used for grouping/coloring (can be NULL).
#' @param whiskers A vector of two column names for the lower and upper error bar limits.
#' @param mid_type String indicating whether to plot points ("p"), lines ("l"), or both ("pl").
#' @param mid_point_size Numeric value for the size of points.
#' @param position Position adjustment for dodging points and lines (default: position_dodge(width = 0.4)).
#' @param legend_title Title for the legend.
#' @param legend_position Position of the legend (default: "bottom").
#' @param ggtheme ggplot2 theme to apply (default: theme_bw()).
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param caption Plot caption.
#' @param errorbar_width Width of error bars (default: 0.45).
#' @param col Vector of color values for manual color scaling.
#' @param linetype Vector of line type values for manual line type scaling.
#' @return A ggplot object.
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
#' g_lineplot_without_table(
#'   df_stats = df_stats,
#'   x = "AVISIT",
#'   mid = "mean",
#'   whiskers = c("mean_ci_lwr", "mean_ci_upr")
#' )
#'
#' # Line plot with stratification
#' df_stats_strat <- rbind(
#'   transform(df_stats, ARM = "Treatment A"),
#'   transform(df_stats, ARM = "Treatment B", mean = mean + 2,
#'             mean_ci_lwr = mean_ci_lwr + 2, mean_ci_upr = mean_ci_upr + 2)
#' )
#' g_lineplot_without_table(
#'   df_stats = df_stats_strat,
#'   x = "AVISIT",
#'   mid = "mean",
#'   strata_N = "ARM",
#'   whiskers = c("mean_ci_lwr", "mean_ci_upr"),
#'   title = "Mean Values by Visit and Treatment"
#' )
g_lineplot_without_table <- function(df_stats,
                                     x = "AVISIT",
                                     mid = "mean",
                                     strata_N = NULL,
                                     whiskers = c("mean_ci_lwr", "mean_ci_upr"),
                                     mid_type = "pl",
                                     mid_point_size = 2,
                                     position = position_dodge(width = 0.4),
                                     legend_title = NULL,
                                     legend_position = "bottom",
                                     ggtheme = theme_bw(),
                                     x_lab = NULL,
                                     y_lab = NULL,
                                     title = "Plot of Mean and 95% Confidence Limits by Visit",
                                     subtitle = "",
                                     caption = NULL,
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
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      color = legend_title,
      lty = legend_title,
      shape = legend_title,
      x = x_lab,
      y = y_lab
    )

  if (!is.null(col)) {
    p <- p + scale_color_manual(values = col)
  }
  if (!is.null(linetype)) {
    p <- p + scale_linetype_manual(values = linetype)
  }

  if (!is.null(ggtheme)) {
    p <- p + ggtheme
  }

  p <- p + theme(legend.position = legend_position)

  return(p)
}

#' @title Generate a ggplot Table from Pre-calculated Statistics
#'
#' @description Creates a ggplot object containing text data for use as a table
#' component beneath a main plot.
#'
#' @param df_stats A data frame containing pre-calculated statistics.
#' @param x Column name for the x-axis (must be in df_stats).
#' @param group_var Column name for the grouping/stratification variable (can be NULL).
#' @param table A character vector of statistic column names to display in the table (e.g., c("n", "mean", "mean_ci")).
#' @param table_font_size Font size for the table text.
#' @param decimal_places Integer specifying the number of decimal places for numeric statistics (like 'mean').
#' @return A ggplot object formatted as a table.
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
  df_stats_formatted <- df_stats %>%
    mutate(across(all_of(table) & where(is.numeric), ~ sprintf(fmt, .)))

  # Select and pivot the formatted data
  df_stats_table <- df_stats_formatted %>%
    select(all_of(c(group_var, x, table))) %>%
    pivot_longer(
      cols = -all_of(c(group_var, x)),
      names_to = "stat",
      values_to = "value"
    )

  tbl <- ggplot(df_stats_table, aes(x = .data[[x]], y = stat, label = value)) +
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
    tbl <- tbl + facet_wrap(as.formula(paste("~", group_var)), ncol = 1)
  }
  return(tbl)
}

#' @title Generate Line Plot with Optional Summary Table
#'
#' @description Generates and combines a line plot and an optional summary table
#' from a pre-calculated statistics data frame (`df_stats`).
#'
#' @param df_stats The pre-calculated data frame containing summary statistics.
#' @param x Column name for the x-axis (e.g., "AVISIT").
#' @param group_var Column name for the grouping variable (e.g., "ARM").
#' @param strata_N Column name for the group label including N (usually generated by preprocess_lineplot_data).
#' @param mid Column name for the plot midpoint (e.g., "mean").
#' @param whiskers A vector of two column names for the lower and upper error bar limits.
#' @param table A character vector of statistic column names to display in the table (e.g., c("n", "mean")).
#' @param mid_type String indicating whether to plot points ("p"), lines ("l"), or both ("pl").
#' @param mid_point_size Numeric value for the size of points.
#' @param position Position adjustment for dodging points and lines.
#' @param legend_title Title for the legend.
#' @param legend_position Position of the legend.
#' @param ggtheme ggplot2 theme to apply (default: nestcolor::theme_nest()).
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param caption Plot caption.
#' @param table_font_size Font size for the table text.
#' @param decimal_places Integer specifying the number of decimal places for numeric statistics in the table.
#' @param errorbar_width Width of error bars.
#' @param col Vector of color values for manual color scaling.
#' @param linetype Vector of line type values for manual line type scaling.
#' @param rel_height_plot Relative height of the plot component compared to the table (default: 0.5).
#' @return A combined plot/table (cowplot) or just the plot (ggplot).
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(cowplot)
#'
#' # Create example statistics data frame
#' df_stats <- data.frame(
#'   AVISIT = factor(c("Baseline", "Week 4", "Week 8", "Baseline", "Week 4", "Week 8")),
#'   ARM = c(rep("Treatment A", 3), rep("Treatment B", 3)),
#'   ARM_N = c(rep("Treatment A (N = 50)", 3), rep("Treatment B (N = 48)", 3)),
#'   n = c(50, 48, 45, 48, 46, 44),
#'   mean = c(10.5, 12.3, 14.1, 10.8, 11.5, 12.2),
#'   mean_ci_lwr = c(9.2, 11.0, 12.8, 9.5, 10.2, 10.9),
#'   mean_ci_upr = c(11.8, 13.6, 15.4, 12.1, 12.8, 13.5),
#'   mean_ci = c("9.20 11.80", "11.00 13.60", "12.80 15.40",
#'               "9.50 12.10", "10.20 12.80", "10.90 13.50")
#' )
#' df_stats$ARM_N <- factor(df_stats$ARM_N)
#'
#' # Line plot without table
#' g_lineplot_with_table(
#'   df_stats = df_stats,
#'   x = "AVISIT",
#'   group_var = "ARM",
#'   strata_N = "ARM_N",
#'   table = NULL
#' )
#'
#' # Line plot with summary table
#' g_lineplot_with_table(
#'   df_stats = df_stats,
#'   x = "AVISIT",
#'   group_var = "ARM",
#'   strata_N = "ARM_N",
#'   table = c("n", "mean", "mean_ci"),
#'   title = "Mean Values with 95% CI by Visit",
#'   rel_height_plot = 0.6
#' )
g_lineplot_with_table <- function(df_stats,
                                  x = "AVISIT",
                                  group_var = "ARM",
                                  strata_N = NULL,
                                  mid = "mean",
                                  whiskers = c("mean_ci_lwr", "mean_ci_upr"),
                                  table = NULL,
                                  mid_type = "pl",
                                  mid_point_size = 2,
                                  position = position_dodge(width = 0.4),
                                  legend_title = NULL,
                                  legend_position = "bottom",
                                  ggtheme = nestcolor::theme_nest(),
                                  x_lab = NULL,
                                  y_lab = NULL,
                                  title = "Plot of Mean and 95% Confidence Limits by Visit",
                                  subtitle = "",
                                  caption = NULL,
                                  table_font_size = 3,
                                  decimal_places = 2,
                                  errorbar_width = 0.45,
                                  col = NULL,
                                  linetype = NULL,
                                  rel_height_plot = 0.5) {

  # 1. GENERATE PLOT -----------------------------------------------------------------
  p <- g_lineplot_without_table(
    df_stats = df_stats,
    x = x,
    mid = mid,
    strata_N = strata_N,
    whiskers = whiskers,
    mid_type = mid_type,
    mid_point_size = mid_point_size,
    position = position,
    legend_title = legend_title,
    legend_position = legend_position,
    ggtheme = ggtheme,
    x_lab = x_lab,
    y_lab = y_lab,
    title = title,
    subtitle = subtitle,
    caption = caption,
    errorbar_width = errorbar_width,
    col = col,
    linetype = linetype
  )

  # 2. GENERATE TABLE and COMBINE ----------------------------------------------
  if (!is.null(table)) {
    tbl <- g_lineplot_table(
      df_stats = df_stats,
      x = x,
      group_var = group_var,
      table = table,
      table_font_size = table_font_size,
      decimal_places = decimal_places
    )

    # Combine plot and table using cowplot::plot_grid
    return(cowplot::plot_grid(p, tbl, ncol = 1, align = "v", axis = "tblr",
                              rel_heights = c(rel_height_plot, 1 - rel_height_plot)))
  } else {
    return(p)
  }
}


#' @title Preprocess Data for Line Plot Statistics
#'
#' @description Cleans, expands, joins, and computes summary statistics
#' (using [calc_stats]) from the raw data frame, preparing it for plotting.
#'
#' @param df The primary data frame containing the data to plot (e.g., ADaM BDS).
#' @param alt_counts_df An optional data frame for calculating N counts (e.g., ADSL).
#' @param x Column name for the x-axis (e.g., "AVISIT").
#' @param y Column name for the y-axis values (e.g., "AVAL").
#' @param group_var Column name for the grouping variable (e.g., "ARM").
#' @param subject_var Column name for the subject ID (e.g., "USUBJID").
#' @param mid Column name for the mean/median statistic to be plotted (e.g., "mean").
#' @param calc_stats A function to calculate summary statistics, defaulting to the provided `calc_stats` function.
#' @param ... Additional arguments passed to the `calc_stats` function (e.g., `conf_level`, `decimal_places`).
#' @return A data frame (`df_stats`) containing the calculated statistics, ready for plotting.
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
                                     calc_stats = calc_stats,
                                     ...) { # Allowing for custom statistic function, defaulting to calc_stats

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
  df_grp <- df_grp %>%
    full_join(df[, c(group_var, x, y)], by = c(group_var, x), multiple = "all") %>%
    group_by(across(all_of(c(group_var, x))))

  # 2. CALCULATE STATISTICS
  df_stats <- df_grp %>%
    summarise(
      stats = list(calc_stats(.data[[y]], ...)), # Calls external calc_stats with ...
      .groups = "drop"
    ) %>%
    tidyr::unnest_wider(stats)

  # Remove NA rows where the midpoint statistic is missing
  df_stats <- df_stats[!is.na(df_stats[[mid]]), ]

  # Add N counts to group labels if alt_counts_df provided
  strata_N <- NULL
  if (!is.null(group_var) && !is.null(alt_counts_df)) {
    strata_N <- paste0(group_var, "_N")

    df_N <- alt_counts_df %>%
      group_by(.data[[group_var]]) %>%
      summarise(N = n_distinct(.data[[subject_var]]), .groups = "drop") %>%
      mutate(!!strata_N := paste0(.data[[group_var]], " (N = ", N, ")"))

    df_stats <- df_stats %>%
      left_join(df_N[, c(group_var, strata_N)], by = group_var) %>%
      mutate(!!strata_N := factor(.data[[strata_N]], levels = unique(df_N[[strata_N]])))
  }

  return(df_stats)
}
