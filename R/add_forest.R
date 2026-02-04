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

    theme_void() +

    # COORDINATES (Crucial so ticks don't get clipped)
    ggplot2::coord_cartesian(xlim = limits, clip = "off") +

    ggplot2::theme(
      # Draw the main horizontal axis line
      axis.line.x  = element_line(color = "black", linewidth = sizes$line_axis),
      axis.ticks.x = element_line(color = "black", linewidth = sizes$line_axis),
      axis.ticks.length.x = sizes$tick_long, # Match the 'long' logtick
      # Text styling
      axis.text.x  = element_text(
        size = sizes$text_size,
        color = "black",
        margin = margin(t = sizes$text_margin)
      ),
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

  # 1. SETUP DEFAULTS ----------------------------------------------------------
  # Define two sets of sizes: "Huge" for GT (HTML) and "Standard" for Flextable (Word/PPT)
  table_engine <- arg_match(table_engine, error_call = get_cli_abort_call())

  sizes <- if (table_engine == "gt") {
    list(
      # Lines & Strokes
      line_axis = 8,
      line_ref = 6,
      errorbar_size = 8,
      stroke = 5,
      tick_width = 6,

      # Ticks (Large units for high-res HTML)
      tick_short = unit(0.7, "cm"),
      tick_mid   = unit(1.3, "cm"),
      tick_long  = unit(2.5, "cm"),

      # Text
      text_size = 120,
      text_margin = 60,

      # Dots (Scale factor for p-values)
      dot_max = 120,
      dot_base = 40
    )
  } else {
    list(
      # Lines & Strokes (Standard ggplot sizes)
      line_axis = 0.5,
      line_ref = 0.4,
      errorbar_size = 0.6,
      stroke = 0.8,
      tick_width = 0.4,

      # Ticks (Standard units)
      tick_short = unit(0.1, "cm"),
      tick_mid   = unit(0.2, "cm"),
      tick_long  = unit(0.3, "cm"),

      # Text
      text_size = 9,      # Standard point size
      text_margin = 5,

      # Dots
      dot_max = 6,        # Max dot size (e.g. 6pt)
      dot_base = 2        # Default dot size if p-value missing
    )
  }
  # 2. DATA PREP ---------------------------------------------------------------
  global_limits <- c(min(c(x$table_body[[conf.low]], 0.2), na.rm = TRUE),
                     max(x$table_body[[conf.high]], na.rm = TRUE))
  mean_estimate <- mean(x$table_body[[estimate]], na.rm = TRUE)
  global_margins <- margin(t = 0, r = 5, b = 0, l = 5, unit = "pt")

  # 3. GENERATE PLOTS ----------------------------------------------------------
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
            ggplot2::geom_vline(xintercept = mean_estimate, linetype = "dashed", linewidth = sizes$line_ref) +
            ggplot2::geom_vline(xintercept = 1, linewidth = sizes$line_ref) +
            ggplot2::geom_vline(xintercept = 0.2, linewidth = sizes$line_ref) +
            ggplot2::scale_x_log10(limits = global_limits) +
            ggplot2::theme_void() +
            ggplot2::theme(
              axis.title = element_blank(), axis.line = element_blank(),
              axis.ticks = element_blank(), axis.text = element_blank(),
              plot.margin = global_margins
            )
          return(out)
        }

        # Extract the row data
        dt <- x$table_body[i, ]

        # Dots sizes
        pvalue_size_i <- ifelse(
          is.null(dt[[pvalue]]) || is.na(dt[[pvalue]]),
          sizes$dot_base,
          sizes$dot_max * dt[[pvalue]]
        )

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
          ggplot2::geom_errorbar(height = 0, size = sizes$errorbar_size) +
          ggplot2::geom_vline(xintercept = mean_estimate, linetype = "dashed", linewidth = sizes$line_ref) +
          ggplot2::geom_vline(xintercept = 1, linewidth = sizes$line_ref) +
          ggplot2::geom_vline(xintercept = 0.2, linewidth = sizes$line_ref) +
          ggplot2::geom_point(size = pvalue_size_i, shape = 21, fill = "white", stroke = sizes$stroke) +
          ggplot2::scale_x_log10(limits = global_limits) + # Ensure log scale matches axis!
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

  # 4. CREATE AXIS & APPEND ----------------------------------------------------
  # Create the axis plot (using the function from the previous step)
  p_axis <- .plot_centered_axis(limits = global_limits, mean_estimate = mean_estimate, sizes = sizes)

  # Append it to your list of plots
  lst_ggplots_final <- c(lst_ggplots, list(p_axis))

  # A. Extract the Spanning Headers from gtsummary metadata
  # The headers are stored in x$table_styling$header
  # We look for columns that are NOT the label column, and get their 'spanning_header'
  raw_headers <- x$table_styling$header |>
    dplyr::filter(column != "label", !is.na(spanning_header)) |>
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

  left_text  <- clean_headers[1]
  right_text <- clean_headers[2]

  # B. Build the String
  if (table_engine == "gt") {
    # GT (HTML)
    spacer <- paste0(rep("&nbsp;", header_spaces), collapse = "")

    header_text <- gt::html(paste0(
      left_text, spacer, right_text, "<br>",
      "<span style='font-size: smaller'>Better", spacer, "Better</span>"
    ))

  } else {
    # FLEXTABLE (Text)
    spacer <- paste0(rep(" ", header_spaces), collapse = "")

    header_text <- paste0(
      left_text, spacer, right_text, "\n",
      "Better", spacer, "Better"
    )
  }
  if (table_engine == "gt") {
    out <- x |>
      gtsummary::modify_table_body(~ .x |> dplyr::add_row() |> dplyr::mutate(ggplot = NA, .after = .data[[after]])) |>
      gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
      gtsummary::modify_header(ggplot = header_text) |>
      gtsummary::as_gt() |>
      gt::text_transform(
        locations = gt::cells_body(columns = .data$ggplot),
        fn = function(x) {
          lst_ggplots_final |> gt::ggplot_image(height = gt::px(28), aspect_ratio = 8)
        }
      ) |>
      gt::cols_width(ggplot ~ gt::px(250)) |>
      gt::opt_table_lines(extent = "none") |>
      gt::tab_options(
        data_row.padding = gt::px(0),
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        column_labels.border.bottom.style = "hidden"
      ) |>
      gt::opt_css("
        .gt_table img { display: block; vertical-align: bottom; margin: 0 auto; }
      ") |>
      gt::tab_style(style = gt::cell_text(whitespace = "nowrap"), locations = gt::cells_body())

  } else if (table_engine == "flextable") {
    out <- x |>
      gtsummary::modify_table_body(~ .x |> dplyr::add_row() |> dplyr::mutate(ggplot = NA, .after = .data[[after]])) |>
      gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
      gtsummary::modify_header(ggplot = header_text) |>
      gtsummary::as_flex_table() |>
      flextable::mk_par(
        j = "ggplot",
        value = flextable::as_paragraph(
          flextable::gg_chunk(value = lst_ggplots_final, height = 0.4, width = 2.5)
        )
      ) |>
      flextable::align(j = "ggplot", align = "center", part = "header") |>
      flextable::width(j = "ggplot", width = 2.5) |>
      flextable::padding(padding = 0, part = "all") |>
      flextable::border_remove() |>
      flextable::valign(valign = "bottom", part = "body")
  }

  out
}
