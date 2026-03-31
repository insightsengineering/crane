#' Deprecated functions
#'
#' `r lifecycle::badge('deprecated')`\cr
#' Some functions have been deprecated and are no longer being actively
#' supported.
#'
#' @name deprecated
#' @keywords internal
NULL

# v0.2.0 -----------------------------------------------------------------------
#' @rdname deprecated
#' @export
tbl_demographics <- function(..., nonmissing = "always") {
  lifecycle::deprecate_soft(
    "0.2.0",
    "crane::tbl_demographics()",
    "tbl_roche_summary()"
  )

  tbl_roche_summary(..., nonmissing = nonmissing)
}

# v0.3.1.9017 ------------------------------------------------------------------
#' @rdname deprecated
#' @export
calc_stats <- function(x, conf_level = 0.95, decimal_places = 2) {
  lifecycle::deprecate_soft(
    "0.3.1.9017",
    "crane::calc_stats()"
  )

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

#' @rdname deprecated
#' @export
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
  lifecycle::deprecate_soft(
    "0.3.1.9017",
    "crane::g_lineplot()",
    "crane::gg_lineplot_2"
  )
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

#' @rdname deprecated
#' @export
g_lineplot_table <- function(df_stats,
                             x = "AVISIT",
                             group_var = NULL,
                             table = c("n", "mean"),
                             table_font_size = 3,
                             decimal_places = 2) {
  lifecycle::deprecate_soft(
    "0.3.1.9017",
    "crane::g_lineplot_table()",
    "crane::annotate_gg()"
  )

  # Format numeric columns to character strings with specified decimal places
  fmt <- paste0("%.", decimal_places, "f")
  df_stats_formatted <- df_stats |>
    mutate(across(all_of(table) & where(is.numeric), ~ sprintf(fmt, .)))

  # Select and pivot the formatted data
  df_stats_table <- df_stats_formatted |>
    dplyr::select(all_of(c(group_var, x, table))) |>
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

#' @rdname deprecated
#' @export
preprocess_lineplot_data <- function(df,
                                     alt_counts_df = NULL,
                                     x = "AVISIT",
                                     y = "AVAL",
                                     group_var = "ARM",
                                     subject_var = "USUBJID",
                                     mid = "mean",
                                     calc_stats_func = calc_stats,
                                     ...) {
  lifecycle::deprecate_soft(
    "0.3.1.9017",
    "crane::preprocess_lineplot_data()",
    "crane::annotate_gg()"
  )

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
