# Function to generate a clean, centered X-axis
.plot_centered_axis <- function(limits, mean_estimate) {
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
      sides = "b",       # 'b' = bottom
      outside = TRUE,    # Draw them touching the axis line
      short = unit(0.7, "cm"),
      mid = unit(1.3, "cm"),
      long = unit(2.5, "cm"),
      linewidth = 6
    ) +

    # Add reference lines
    geom_vline(xintercept = mean_estimate, linetype = "dashed", color = "black", linewidth = 6) +
    geom_vline(xintercept = 1, color = "black", linewidth = 6) +
    ggplot2::geom_vline(xintercept = 0.2, linewidth = 6) +

    theme_void() +

    # COORDINATES (Crucial so ticks don't get clipped)
    coord_cartesian(xlim = limits, clip = "off") +
    ggplot2::scale_x_continuous(limits = limits) +

    theme(
      # Draw the main horizontal axis line
      axis.line.x  = element_line(color = "black", linewidth = 8),
      # Hide standard ticks because annotation_logticks handles them
      axis.ticks.x = element_line(color = "black", linewidth = 8),
      axis.ticks.length.x = unit(2.5, "cm"),
      # Text styling
      axis.text.x  = element_text(size = 120, color = "black", margin = margin(t = 60)),
      # Margins to center it in the table row
      plot.margin = margin(t = 0, r = 5, b = 25, l = 5, unit = "pt")
    )
}


#' Add a Forest Plot
#'
#' @param x a gtsummary table
#'
#' @return a gt table
#' @export
#'
#' @examples
#' # Add example
add_forest <- function(x, estimate = "estimate", conf.low = "conf.low", conf.high = "conf.high",
                       pvalue = NULL,
                       after = "label", table_engine = c("gt", "flextable")) {
  set_cli_abort_call()
  global_limits <-  c(min(c(x$table_body[[conf.low]], 0.2), na.rm = TRUE),
               max(x$table_body[[conf.high]], na.rm = TRUE))
  mean_estimate <- mean(x$table_body[[estimate]], na.rm = TRUE)
  global_margins <- margin(t = 0, r = 5, b = 0, l = 5, unit = "pt")
  table_engine <- arg_match(table_engine, error_call = get_cli_abort_call())
  if ()

  # create ggplots -------------------------------------------------------------
  lst_ggplots <-
    seq_len(nrow(x$table_body)) |>
    lapply(
      function(i) {

        # Handle missing estimates by creating an empty plot with just reference lines
        if (is.na(x$table_body[[estimate]][i]) ||
            is.na(x$table_body[[conf.low]][i]) ||
            is.na(x$table_body[[conf.high]][i])) {

          # Create an empty plot with just the reference lines
          out <- ggplot2::ggplot() +
            ggplot2::geom_vline(xintercept = mean_estimate, linetype = "dashed", linewidth = 8) +
            ggplot2::geom_vline(xintercept = 1, linewidth = 6) +
            ggplot2::geom_vline(xintercept = 0.2, linewidth = 6) +
            ggplot2::scale_x_continuous(limits = global_limits) +
            ggplot2::theme_void() +
            ggplot2::theme(
              axis.title = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              plot.margin = global_margins
            )
          return(out)
        }

        # Extract the row data
        dt <- x$table_body[i, ]

        # Dots sizes
        pvalue_size_i <- ifelse(is.null(dt[[pvalue]]) || is.na(dt[[pvalue]]), 40, 120 * dt[[pvalue]])

        # Create the forest plot for this row
        out <- dt |>
          dplyr::mutate(y = 1L) |>
          ggplot2::ggplot(
            ggplot2::aes(
              y = .data$y,
              x = .data[[estimate]],
              xmin = .data[[conf.low]],
              xmax = .data[[conf.high]]
            )
          ) +
          ggplot2::geom_errorbar(height = 0, size = 8) +
          ggplot2::geom_vline(xintercept = mean_estimate, linetype = "dashed", linewidth = 8) +
          ggplot2::geom_vline(xintercept = 1, linewidth = 6) +
          ggplot2::geom_vline(xintercept = 0.2, linewidth = 6) +
          ggplot2::geom_point(size = pvalue_size_i, shape = 21, fill = "white", stroke = 5) +
          ggplot2::scale_x_continuous(limits = global_limits) +
          ggplot2::theme_void() +
          ggplot2::theme(
            axis.title = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.margin = global_margins
          )

        out
      }
    )

  # Create the axis plot (using the function from the previous step)
  p_axis <- .plot_centered_axis(limits = global_limits, mean_estimate = mean_estimate)

  # 2. Append it to your list of plots
  lst_ggplots_final <- c(lst_ggplots, list(p_axis))

  # add plots to table ---------------------------------------------------------
  if (table_engine == "gt") {
    # Define the HTML string separately for cleanliness
    header_html <- "
<div style='display: flex; width: 100%; align-items: flex-end;'>
  <div style='flex: 1; text-align: center;'>
    TRT<br>Better
  </div>
  <div style='flex: 1; text-align: center;'>
    CTRL<br>Better
  </div>
</div>
"

    out <- x |>
      gtsummary::modify_table_body(
        ~ .x |>
          dplyr::add_row() |>  # <--- A. Add the extra row for the axis here
          dplyr::mutate(ggplot = NA, .after = .data[[after]])
      ) |>
      gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
      # gtsummary::modify_footnote(gtsummary::everything() ~ NA, abbreviation = TRUE) |>
      gtsummary::modify_header(ggplot = gt::html(header_html)) |>
      gtsummary::as_gt() |>
      gt::text_transform(
        locations = gt::cells_body(columns = .data$ggplot),
        fn = function(x) {
          lst_ggplots_final |>
            gt::ggplot_image(height = gt::px(28), aspect_ratio = 8)
        }
      ) |>
      gt::cols_width(ggplot ~ gt::px(250)) |>
      gt::opt_table_lines(extent = "none") |>
      gt::tab_options(
        data_row.padding = gt::px(0),      # Squash rows together
        table.border.top.style = "hidden", # Hide outer borders
        table.border.bottom.style = "hidden",
        column_labels.border.bottom.style = "hidden"
      ) |>
      # B. CSS HACK: KILL THE "GHOST" LINE
      # This forces images to behave like blocks (stacking) rather than text (lines)
      gt::opt_css("
        .gt_table img {
          display: block;
          vertical-align: bottom;
          margin: 0 auto;
        }
      ") |>
      # Force all text columns to keep content on a single line
      gt::tab_style(
        style = gt::cell_text(whitespace = "nowrap"),
        locations = gt::cells_body() # Applies to all columns
      ) |>
      identity()
  } else if (table_engine == "flextable") {
    # Create a "Plot" that is just the header text
    # We use the SAME global_limits and margins as your other plots
    p_header <- ggplot(data.frame(x = global_limits), aes(x = x, y = 1)) +
      geom_blank() +
      scale_x_log10(limits = global_limits) +
      theme_void() +
      theme(
        # MATCH THE MARGINS of your sparklines exactly
        plot.margin = margin(t = 0, r = 10, b = 5, l = 10, unit = "pt")
      ) +
      # Place text visually in the "middle" of the two log-halves
      # Adjust x=0.5 and x=5 roughly to where you want the labels center
      annotate("text", x = 0.4, y = 1, label = "TRT\nBetter", size = 3.5, fontface = "bold", lineheight = 0.8) +
      annotate("text", x = sum(1, global_limits) / 2, y = 1, label = "CTRL\nBetter", size = 3.5, fontface = "bold", lineheight = 0.8)

    out <- x %>%
      # --- A. GTSUMMARY PREP (Keep this part) ---
      gtsummary::modify_table_body(
        ~ .x %>%
          dplyr::add_row() %>%
          dplyr::mutate(ggplot = NA, .after = .data[[after]])
      ) %>%
      gtsummary::modify_footnote(gtsummary::everything() ~ NA) %>%
      # We just set a placeholder label here; we will overwrite it with a plot later
      gtsummary::modify_header(ggplot = " ") %>%

      # --- B. CONVERT TO FLEXTABLE ---
      gtsummary::as_flex_table() %>%

      # --- C. INSERT PLOTS INTO BODY ---
      # We replace the content of the 'ggplot' column with our list of sparklines
      flextable::mk_par(
        j = "ggplot",
        value = flextable::as_paragraph(
          flextable::gg_chunk(value = lst_ggplots_final, height = 0.4, width = 2) # approx px(30) / px(250)
        )
      ) %>%

      # --- D. INSERT HEADER PLOT ---
      # We replace the header text with the header plot we created above
      flextable::mk_par(
        part = "header",
        j = "ggplot",
        value = flextable::as_paragraph(
          flextable::gg_chunk(value = list(p_header), height = 0.5, width = 2.5)
        )
      ) %>%

      # --- E. STYLING (Remove Padding & Borders) ---
      # 1. Set column width (Crucial for images)
      flextable::width(j = "ggplot", width = 2.5) %>%

      # 2. Kill all padding to make lines touch
      flextable::padding(padding = 0, part = "all") %>%

      # 4. Vertical Align (Bottom is usually best for sparklines)
      flextable::valign(valign = "bottom", part = "body") |>
      identity()
  }

  out
}
