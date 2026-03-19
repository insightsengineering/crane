#' Convert Summary Table and Combine with a ggplot2 Object
#'
#' @param df (`data.frame` or `tibble`)\cr
#'   The summary table. The first column is used as row labels (Y-axis).
#'   Remaining column names must be coercible to numeric X-axis values.
#' @param gg_plt (`ggplot2`)\cr
#'   The main ggplot object. The table will be aligned to its X-axis and
#'   stacked underneath it.
#' @param text_size (`numeric`)\cr
#'   Base text size for the table values. Defaults to 3.5.
#' @param rel_height_plot (`numeric`)\cr
#'   A single numeric value defining the relative height of the main plot area
#'   compared to the summary table. Defaults to `0.75`.
#'
#' @return A combined `patchwork` object.
#' @keywords internal
df2gg_2 <- function(df, gg_plt, text_size = 3.5, rel_height_plot = 0.75) {
  # 1. Input Validation
  if (!is.data.frame(df)) rlang::abort("`df` must be a data.frame or tibble.")
  if (is.null(gg_plt) || !inherits(gg_plt, "gg")) {
    rlang::abort("`gg_plt` must be a valid ggplot2 object.")
  }

  first_col <- names(df)[1]
  x_cols <- names(df)[-1]

  # Ensure x_cols can be treated as numeric
  x_num <- suppressWarnings(as.numeric(x_cols))
  if (any(is.na(x_num))) {
    rlang::warn("Some column names cannot be coerced to numeric. They will be omitted.")
    x_cols <- x_cols[!is.na(x_num)]
  }

  # --- 2. Format Y-axis labels using plotmath expressions ---
  raw_labels <- as.character(df[[first_col]])
  trimmed_labels <- trimws(raw_labels)

  known_stats <- c("n", "Mean", "SD", "Median", "IQR")
  is_stat <- trimmed_labels %in% known_stats

  # Use ~ for rigid plotmath spacing, and bold() for cohorts
  # We use double quotes around the strings in case the label itself has a space
  formatted_labels <- ifelse(
    is_stat,
    paste0('~~~~"', trimmed_labels, '"'),
    paste0('bold("', trimmed_labels, '")')
  )
  # ---------------------------------------------------------

  # 3. Pivot data to long format
  df_long <- df |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(x_cols),
      names_to = "x_chr",
      values_to = "value"
    ) |>
    dplyr::mutate(x = as.numeric(x_chr))

  # 4. Build the base ggplot table
  p_tbl <- ggplot2::ggplot(df_long, ggplot2::aes(x = x, y = row_id, label = value)) +
    ggplot2::geom_text(size = text_size, vjust = 0.5, hjust = 0.5) +
    ggplot2::scale_y_reverse(
      breaks = 1:nrow(df),
      # parse() converts the string logic into actual R graphical expressions
      labels = parse(text = formatted_labels)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      # Back to standard element_text! No warnings, no ggtext needed.
      axis.text.y = ggplot2::element_text(
        hjust = 0,
        size = 10,
        color = "black",
        margin = ggplot2::margin(r = 15)
      ),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 0, r = 5, b = 5, l = 5)
    )

  # 5. Snap X-axis to the provided plot and fix Y-axis cropping
  plot_build <- ggplot2::ggplot_build(gg_plt)
  x_range <- plot_build$layout$panel_params[[1]]$x.range

  p_tbl <- p_tbl +
    ggplot2::coord_cartesian(
      xlim = x_range,
      ylim = c(nrow(df) + 0.5, 0.5),
      expand = FALSE
    )

  # 6. Combine using patchwork
  table_height <- 1 - rel_height_plot

  combined_plot <- gg_plt / p_tbl +
    patchwork::plot_layout(heights = c(rel_height_plot, table_height))

  return(combined_plot)
}

#' Create and Stack an X-Axis Aligned Table
#'
#' @description Generates a text table from a dataframe, perfectly aligns its
#'   X-axis to match a provided ggplot, and seamlessly stacks them vertically.
#'
#' @param df (`data.frame`)\cr The summary table to render.
#' @param gg_plt (`ggplot2`)\cr The main plot to align to and stack on top of.
#' @param y_col (`character` or `NULL`)\cr The name of the column containing Y-axis
#'   labels. If `NULL`, the `rownames(df)` are used.
#' @param y_labels (`character` or `expression`)\cr Optional. A vector of formatted
#'   labels (e.g., parsed plotmath) to override the raw strings extracted from `y_col`.
#' @param title (`character` or `NULL`)\cr Optional title for the table.
#' @param xlab (`character` or `NULL`)\cr Optional X-axis label for the table.
#' @param show_xaxis (`logical`)\cr Should the table display an X-axis line, ticks,
#'   and numeric text? Useful for risk tables. Defaults to `FALSE`.
#' @param text_size (`numeric`)\cr Size of the data values in the table.
#' @param label_size (`numeric`)\cr Size of the axis and title text.
#' @param rel_height_plot (`numeric`)\cr Proportion of vertical space given to the main plot.
#'
#' @return A combined `patchwork` object.
#' @export
df2gg_aligned <- function(df, gg_plt,
                          y_col = NULL,
                          y_labels = NULL,
                          title = NULL,
                          xlab = NULL,
                          show_xaxis = FALSE,
                          text_size = 3.5,
                          label_size = 10,
                          rel_height_plot = 0.75) {
  # 1. Input Validation
  if (!is.data.frame(df)) rlang::abort("`df` must be a data.frame or tibble.")
  if (is.null(gg_plt) || !inherits(gg_plt, "gg")) rlang::abort("`gg_plt` must be a valid ggplot2 object.")

  # 2. Extract Y-axis labels and isolate X-columns
  if (is.null(y_col)) {
    raw_y_labels <- rownames(df)
    df_x <- df
  } else {
    if (!y_col %in% names(df)) rlang::abort(paste("Column", y_col, "not found in df."))
    raw_y_labels <- as.character(df[[y_col]])
    df_x <- df[, names(df) != y_col, drop = FALSE]
  }

  final_y_labels <- if (!is.null(y_labels)) y_labels else raw_y_labels

  # 3. Ensure remaining columns are numeric timepoints
  x_cols <- names(df_x)
  x_num <- suppressWarnings(as.numeric(x_cols))
  if (any(is.na(x_num))) {
    rlang::warn("Some columns cannot be coerced to numeric. Omitting them.")
    x_cols <- x_cols[!is.na(x_num)]
    df_x <- df_x[, x_cols, drop = FALSE]
  }

  # 4. Pivot data to long format
  df_long <- df_x |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(x_cols),
      names_to = "x_chr",
      values_to = "value"
    ) |>
    dplyr::mutate(x = as.numeric(x_chr))

  df_long$value[is.na(df_long$value)] <- "" # Clean NAs

  # 5. Extract Limits & Breaks safely from the built plot
  plot_build <- ggplot2::ggplot_build(gg_plt)
  x_range <- plot_build$layout$panel_params[[1]]$x.range
  x_breaks <- plot_build$layout$panel_params[[1]]$x$breaks
  x_breaks <- x_breaks[!is.na(x_breaks)] # Drop NA breaks if they exist

  # 6. Build the aligned table
  p_tbl <- ggplot2::ggplot(df_long, ggplot2::aes(x = x, y = row_id, label = value)) +
    ggplot2::geom_text(size = text_size, vjust = 0.5, hjust = 0.5) +
    ggplot2::scale_y_reverse(breaks = 1:nrow(df_x), labels = final_y_labels) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::coord_cartesian(xlim = x_range, ylim = c(nrow(df_x) + 0.5, 0.5), expand = FALSE, clip = "off") +
    ggplot2::labs(title = title, x = xlab) +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = label_size, hjust = 1, margin = ggplot2::margin(r = 10), color = "black"),
      plot.title = if (!is.null(title)) ggplot2::element_text(size = label_size, vjust = 3, face = "bold", hjust = 0) else ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = if (show_xaxis || !is.null(title)) 10 else 0, r = 5, b = 5, l = 5)
    )

  # 7. Apply specific formatting based on show_xaxis
  if (show_xaxis) {
    p_tbl <- p_tbl + ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = label_size, margin = ggplot2::margin(t = 2), color = "black"),
      axis.ticks.x = ggplot2::element_line(color = "black"),
      axis.line.x = ggplot2::element_line(color = "black"),
      axis.title.x = if (!is.null(xlab)) ggplot2::element_text(size = label_size, margin = ggplot2::margin(t = 5)) else ggplot2::element_blank()
    )
  }

  # 8. Combine using patchwork
  table_height <- 1 - rel_height_plot
  gg_plt <- gg_plt + ggplot2::theme(legend.position = "bottom")

  combined_plot <- gg_plt / p_tbl +
    patchwork::plot_layout(heights = c(rel_height_plot, table_height))

  return(combined_plot)
}
