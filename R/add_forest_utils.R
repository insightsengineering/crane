# Utility functions for adding forest plot to gtsummary tables
#

# Default sizes for various elements in the forest plot
.get_default_forest_sizes <- function(table_engine) {
  if (table_engine == "gt") {
    out <- list(
      # Lines & Strokes
      line_axis = 8,
      line_ref = 6,
      errorbar_size = 8,
      stroke = 5,
      tick_width = 6,

      # Ticks (Large units for high-res HTML)
      tick_short = unit(0.7, "cm"),
      tick_mid = unit(1.3, "cm"),
      tick_long = unit(2.5, "cm"),

      # Text
      text_size = 120,
      text_margin = 60,

      # Dots (Scale factor for p-values)
      dot_max = 120,
      dot_base = 40,

      # x-axis plot margins
      axis_plot_margins = margin(t = 0, r = 5, b = 25, l = 5, unit = "pt")
    )
  } else {
    out <- list(
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
  out
}


# The headers are stored in x$table_styling$header
# We look for columns that are NOT the label column, and get their 'spanning_header'
.determine_ggplot_header <- function(tbl, header_spaces, table_engine) {
  raw_headers <- tbl$table_styling$spanning_header |>
    dplyr::filter(column != "label", !is.na(spanning_header)) |>
    dplyr::filter(grepl(column, pattern = "stat")) |>
    dplyr::arrange(column) |>
    dplyr::pull(spanning_header) |>
    unique()

  # Clean up headers (remove **bolding** syntax if present)
  clean_headers <- gsub("\\*\\*", "", raw_headers)

  # Fallback: If no spanning headers exist (e.g. no 'by' variable), use generic defaults
  if (length(clean_headers) < 2) {
    cli::cli_warn(
      "Less than 2 spanning headers detected. Using default headers 'Group 1' and 'Group 2' for the forest plot header."
    )
    clean_headers <- c("Group 1", "Group 2")
  }
  if (length(clean_headers) > 2) {
    cli::cli_warn(
      "More than 2 spanning headers detected. Only the first two will be used for the forest plot header."
    )
    clean_headers <- clean_headers[1:2]
  }

  left_text <- clean_headers[1]
  right_text <- clean_headers[2]
  header_spacer_btm <- nchar(left_text) + header_spaces - nchar("Better") + 1

  # B. Build the String
  if (table_engine == "gt") {
    # GT (HTML)
    spacer <- paste0(rep("&nbsp;", header_spaces), collapse = "")
    spacer_btm <- paste0(rep("&nbsp;", header_spacer_btm), collapse = "")

    header_text <- gt::html(paste0(
      left_text, spacer, right_text, "<br>",
      "Better", spacer_btm, "Better"
    ))
  } else {
    # FLEXTABLE (Text)
    spacer <- paste0(rep("\u00A0", header_spaces), collapse = "")
    spacer_btm <- paste0(rep("\u00A0", header_spacer_btm), collapse = "")

    header_text <- paste0(
      left_text, spacer, right_text, "\n",
      "Better", spacer_btm, "Better"
    )
  }

  header_text
}

# Function to generate a clean, centered X-axis
.plot_centered_axis <- function(limits, mean_estimate, sizes) {
  # create dummy data just to initialize ggplot
  ggplot(data.frame(x = limits), aes(x = x)) +
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
