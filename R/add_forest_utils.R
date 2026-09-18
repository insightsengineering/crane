# Utility functions for adding forest plot to gtsummary tables
#

# Default sizes for various elements in the forest plot
.get_default_forest_sizes <- function() {
  list(
    # Lines & Strokes (Standard ggplot sizes)
    line_axis = 0.5,
    line_ref = 0.4,
    errorbar_size = 0.6,
    stroke = 0.8,
    tick_width = 0.4,

    # Ticks (Standard units)
    tick_short = unit(0.1, "cm"),
    tick_mid = unit(0.15, "cm"),
    tick_long = unit(0.2, "cm"),

    # Text
    text_size = 9, # Standard point size
    text_margin = 3,

    # Dots
    dot_max = 6, # Max dot size (e.g. 6pt)
    dot_base = 2, # Default dot size if p-value missing

    # x-axis plot margins
    axis_plot_margins = margin(t = 0, r = 5, b = 3, l = 5, unit = "pt")
  )
}


# Pulls the two treatment labels from the spanning headers, NULL if not found.
.determine_ggplot_header <- function(tbl) {
  raw_headers <- tbl$table_styling$spanning_header |>
    dplyr::filter(.data$column != "label", !is.na(.data$spanning_header)) |>
    dplyr::filter(grepl(.data$column, pattern = "stat")) |>
    dplyr::arrange(.data$column) |>
    dplyr::pull(.data$spanning_header) |>
    unique()

  # Clean up headers (remove **bolding** syntax if present)
  clean_headers <- gsub("\\*\\*", "", raw_headers)

  # Fallback: If no spanning headers exist (e.g. no 'by' variable), use generic defaults
  if (length(clean_headers) > 2) {
    cli::cli_warn(
      "More than 2 spanning headers detected. Only the first two will be used for the forest plot header."
    )
    clean_headers <- clean_headers[1:2]
  }

  if (length(clean_headers) < 2) {
    cli::cli_warn(
      "Less than 2 spanning headers detected. The forest plot column will have an empty header."
    )
    return(NULL)
  }

  list(left = clean_headers[1], right = clean_headers[2])
}

# Header for the forest plot column. Reuses the body plots' scale and margins so
# the panel geometry is identical and the labels align in every output format.
.forest_header_plot <- function(header_parts, limits, margins, sizes) {
  # geometric midpoint of each half, i.e. the visual centre on a log axis
  left_at <- sqrt(limits[1] * 1)
  right_at <- sqrt(1 * limits[2])
  text_size <- sizes$text_size / ggplot2::.pt

  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = c(left_at, left_at, right_at, right_at),
      y = c(0.62, 0.38, 0.62, 0.38),
      label = c(header_parts$left, "Better", header_parts$right, "Better"),
      hjust = 0.5, vjust = c(0, 1, 0, 1), size = text_size
    ) +
    ggplot2::scale_x_log10(limits = limits) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = margins)
}

# Function to generate a clean, centered X-axis
.plot_centered_axis <- function(limits, mean_estimate, sizes) {
  # create dummy data just to initialize ggplot
  ggplot(data.frame(x = limits), aes(x = .data$x)) +
    geom_blank() +

    # LOG SCALE WITH DYNAMIC BREAKS
    scale_x_log10(
      limits = limits,
      # 'n = 5' suggests roughly 5 numbers (e.g. 0.2, 0.5, 1, 2, 5)
      breaks = scales::breaks_log(n = 5),
      # Format labels to avoid scientific notation (e.g., "0.5" instead of "5e-1")
      labels = scales::label_number(drop0trailing = TRUE)
    ) +

    # ADD THE "COMB" TICKS (The small ticks between numbers)
    ggplot2::annotation_logticks(
      sides = "b",
      outside = TRUE,
      short = sizes$tick_short,
      mid = sizes$tick_mid,
      long = sizes$tick_long,
      linewidth = sizes$tick_width
    ) +

    # Add reference lines
    ggplot2::geom_vline(xintercept = mean_estimate, linetype = "dashed", color = "black", linewidth = sizes$line_ref) +
    ggplot2::geom_vline(xintercept = 1, color = "black", linewidth = sizes$line_ref) +
    ggplot2::geom_vline(xintercept = 0.2, linewidth = sizes$line_ref) +
    ggplot2::theme_void() +

    # COORDINATES (Crucial so ticks don't get clipped)
    ggplot2::coord_cartesian(xlim = limits, clip = "off") +
    ggplot2::theme(
      # Draw the main horizontal axis line
      axis.line.x = element_line(color = "black", linewidth = sizes$line_axis),
      axis.ticks.x = element_line(color = "black", linewidth = sizes$line_axis),
      axis.ticks.length.x = sizes$tick_long, # Match the 'long' logtick
      # Text styling
      axis.text.x = element_text(
        size = sizes$text_size,
        color = "black",
        margin = margin(t = sizes$text_margin)
      ),
      plot.margin = sizes$axis_plot_margins
    )
}
