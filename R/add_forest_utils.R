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


# flextable needs magick to read gg_chunk() images back when it draws the table
# (gen_grob(), save_as_image()); without it the forest column renders blank
.warn_if_no_magick <- function(installed = is_installed("magick")) {
  if (!installed) {
    cli::cli_warn(
      c(
        "Install {.pkg magick} to render the forest plot in PDF/PNG/SVG exports.",
        "i" = "Without it the forest plot column is left blank in those formats. Word, RTF, and HTML output are unaffected."
      ),
      .frequency = "once",
      .frequency_id = "crane_add_forest_magick"
    )
  }
  invisible(installed)
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
# Both labels are wrapped to the same width, centered line by line, placed either
# side of the line at 1 and bottom-aligned, in Arial at the column labels' font
# size. The picture height it needs (inches) is returned as attr "height_in".
.forest_header_plot <- function(header_parts, limits, margins, sizes, width_in = 2.5, gap_in = 0.05,
                                font_size = sizes$text_size) {
  pt <- font_size
  line_in <- pt * 1.15 / 72
  panel_in <- width_in - 10 / 72
  # x range of the panel in log10 units, with ggplot's default 5% expansion
  xr <- log10(limits) + c(-0.05, 0.05) * diff(log10(limits))
  one_in <- -xr[1] / diff(xr) * panel_in # position of the line at 1, inches
  # common wrap width: the narrower side's room, but never below the longest word
  words <- unlist(strsplit(c(header_parts$left, header_parts$right), "\\s+"))
  room <- max(min(one_in, panel_in - one_in) - gap_in, .forest_text_width(words, pt))
  wrap_to <- function(label, max_in) {
    lines <- character(0)
    cur <- ""
    for (wd in strsplit(label, "\\s+")[[1]]) {
      try <- if (nzchar(cur)) paste(cur, wd) else wd
      if (nzchar(cur) && .forest_text_width(try, pt) > max_in) {
        lines <- c(lines, cur)
        cur <- wd
      } else {
        cur <- try
      }
    }
    c(lines, cur)
  }
  left <- c(wrap_to(header_parts$left, room), "Better")
  right <- c(wrap_to(header_parts$right, room), "Better")
  n_lines <- max(length(left), length(right))
  # center of each label block (inches), either side of the line at 1; if one
  # block would run off the picture, both move together (they never overlap)
  w_left <- max(.forest_text_width(left, pt))
  w_right <- max(.forest_text_width(right, pt))
  x_left <- one_in - gap_in - w_left / 2
  x_right <- one_in + gap_in + w_right / 2
  shift <- max(0, w_left / 2 - x_left) - max(0, x_right + w_right / 2 - panel_in)
  x_left <- x_left + shift
  x_right <- x_right + shift
  to_x <- function(x_in) 10^(xr[1] + x_in / panel_in * diff(xr))
  df <- rbind(
    data.frame(x = to_x(x_left), y = length(left) - seq_along(left) + 0.5, label = left),
    data.frame(x = to_x(x_right), y = length(right) - seq_along(right) + 0.5, label = right)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label)) +
    ggplot2::geom_text(size = pt / ggplot2::.pt, hjust = 0.5, vjust = 0.5, family = "Arial") +
    ggplot2::scale_x_log10(limits = limits) +
    ggplot2::scale_y_continuous(limits = c(0, n_lines), expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = margins)
  attr(p, "height_in") <- n_lines * line_in
  p
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
    ggplot2::geom_vline(xintercept = mean_estimate, linetype = "dotted", color = "black", linewidth = sizes$line_ref) +
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

# Compact rows ----------------------------------------------------------------

# Picture paragraphs: single spacing from the Word template's "header" style
# instead of a direct w:line, which Word and LibreOffice read differently
.forest_picture_par <- function(ft, i = NULL, part) {
  ft |>
    flextable::style(
      i = i, j = "ggplot", part = part,
      pr_p = officer::fp_par(text.align = "center", word_style = "header")
    ) |>
    flextable::line_spacing(i = i, j = "ggplot", space = NA, part = part)
}

# Text width and height (in) and font size (pt) of a citril page size: A4
# landscape ("L6" to "L10": 11.69 x 8.27in minus margins 3.30 + 3.35cm wide and
# 3.66 + 2.11cm high) or portrait ("P6" to "P10": 8.27 x 11.69in minus 3.66 + 2.11cm
# and 3.55 + 3.30cm); the number is the font size decorate_tlg() uses.
# A number is taken as the width in inches.
.forest_page <- function(table_width) {
  if (is.null(table_width)) {
    return(list(width = NULL, height = NULL, font_size = NULL))
  }
  if (is.numeric(table_width)) {
    return(list(width = table_width, height = NULL, font_size = NULL))
  }
  code <- toupper(table_width)
  if (!grepl("^[LP]([6-9]|10)$", code)) {
    cli::cli_abort(
      "{.arg table_width} must be a page size {.val L6}-{.val L10} or {.val P6}-{.val P10}, or a width in inches.",
      call = get_cli_abort_call()
    )
  }
  landscape <- startsWith(code, "L")
  list(
    width = if (landscape) 11.69 - (3.30 + 3.35) / 2.54 else 8.27 - (3.66 + 2.11) / 2.54,
    height = if (landscape) 8.27 - (3.66 + 2.11) / 2.54 else 11.69 - (3.55 + 3.30) / 2.54,
    font_size = as.numeric(substring(code, 2))
  )
}

# Whether the body rows fit on one page with ~2in left for titles, column labels
# and footnotes (unknown page: assume they fit)
.forest_fits_one_page <- function(row_heights, page, reserve_in = 2) {
  is.null(page$height) || sum(row_heights) <= page$height - reserve_in
}

# One picture for the whole forest column: every data row's CI at the center of
# its own row band (same dots, bars and reference lines as the per-row plots),
# and crane's axis plot underneath for the last (axis) row.
.forest_column_plot <- function(x, estimate, conf_low, conf_high, pvalue, limits, mean_estimate,
                                sizes, margins, row_heights, p_axis) {
  n <- length(row_heights)
  h <- row_heights[-n]
  rows <- seq_along(h)
  p <- if (length(pvalue) > 0) x$table_body[[pvalue]][rows] else rep(NA_real_, length(h))
  d <- data.frame(
    y = cumsum(h) - h / 2,
    est = x$table_body[[estimate]][rows], lo = x$table_body[[conf_low]][rows], hi = x$table_body[[conf_high]][rows],
    dot = ifelse(!is.na(p), sizes$dot_max * p, sizes$dot_base),
    ok = !vapply(rows, function(i) .is_na_or_chr(x, i, estimate, conf_low, conf_high), logical(1))
  )
  d <- d[d$ok, , drop = FALSE]
  rows_plot <- ggplot2::ggplot(d) +
    ggplot2::geom_errorbar(ggplot2::aes(y = .data$y, xmin = .data$lo, xmax = .data$hi),
      width = 0, linewidth = sizes$errorbar_size, orientation = "y"
    ) +
    ggplot2::geom_vline(xintercept = mean_estimate, linetype = "dotted", linewidth = sizes$line_ref) +
    ggplot2::geom_vline(xintercept = 1, linewidth = sizes$line_ref) +
    ggplot2::geom_vline(xintercept = 0.2, linewidth = sizes$line_ref) +
    ggplot2::geom_point(ggplot2::aes(y = .data$y, x = .data$est, size = .data$dot),
      shape = 21, fill = "white", stroke = sizes$stroke
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_x_log10(limits = limits) +
    ggplot2::scale_y_reverse(limits = c(sum(h), 0), expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = margins, legend.position = "none")
  cowplot::plot_grid(rows_plot, p_axis, ncol = 1, rel_heights = c(sum(h), row_heights[n]), align = "v", axis = "lr")
}

# Column widths that fill `table_width` inches: the forest column keeps its
# width; the other columns get at least their longest value (and longest header
# word) and share the rest. When that does not fit, the label column (first)
# shrinks to 1in, then the forest column to 1.5in, then all text columns are
# narrowed and their text wraps (the row heights follow).
.forest_fit_widths <- function(ft, table_width, font_size = NULL, forest_key = "ggplot") {
  keys <- setdiff(ft$col_keys, forest_key)
  forest_w <- ft$body$colwidths[ft$col_keys == forest_key]
  need <- vapply(keys, function(k) .forest_min_width(ft, k, font_size), numeric(1))
  if (sum(need) + forest_w <= table_width) {
    need <- need * (table_width - forest_w) / sum(need)
  } else {
    need[1] <- max(table_width - forest_w - sum(need[-1]), min(need[1], 1))
    forest_w <- max(min(forest_w, table_width - sum(need)), 1.5)
    if (sum(need) + forest_w > table_width) {
      cli::cli_warn(c(
        "The table needs {round(sum(need) + forest_w, 2)}in but the page has {round(table_width, 2)}in.",
        "i" = "Text columns are narrowed and their text wraps. A landscape page size would avoid this."
      ))
      need <- need * (table_width - forest_w) / sum(need)
    }
  }
  ft <- flextable::width(ft, j = c(keys, forest_key), width = unname(c(need, forest_w)))
  flextable::set_table_properties(ft, layout = "fixed")
}

.forest_min_width <- function(ft, key, font_size = NULL, slack_in = 0.06) {
  j <- which(ft$col_keys == key)
  part_need <- function(part, split_words) {
    p <- ft[[part]]
    if (is.null(p) || nrow(p$dataset) == 0) {
      return(0)
    }
    out <- 0
    for (i in seq_len(nrow(p$dataset))) {
      txt <- p$content$data[[i, j]]$txt
      txt <- paste(txt[!is.na(txt)], collapse = "")
      if (p$spans$rows[i, j] != 1 || !nzchar(trimws(txt))) next
      pieces <- if (split_words) strsplit(trimws(txt), "\\s+")[[1]] else txt
      size <- font_size %||% p$styles$text$font.size$data[i, j]
      w <- max(.forest_text_width(pieces, size))
      pad <- (p$styles$pars$padding.left$data[i, j] + p$styles$pars$padding.right$data[i, j]) / 72
      out <- max(out, w + pad)
    }
    out
  }
  max(part_need("body", FALSE), part_need("header", TRUE)) + slack_in
}

# Width of text in inches, with the pdf device's Helvetica metrics (Arial has the
# same widths), so no font files are needed
.forest_text_width <- function(x, size_pt) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::strwidth(x, units = "inches", family = "sans", cex = size_pt / 12)
}

# One height per body row (inches): row_height, plus one line pitch per extra
# wrapped text line (at the page's font size when known); the last (axis) row
# gets what the axis plot needs.
.forest_row_heights <- function(ft, row_height, p_axis, font_size = NULL, forest_key = "ggplot", safety_in = 0.03) {
  b <- ft$body
  st <- b$styles
  n <- nrow(b$dataset)
  extra_pt <- numeric(n)
  for (i in seq_len(n)) {
    for (j in which(ft$col_keys != forest_key)) {
      span <- b$spans$rows[i, j]
      txt <- paste(stats::na.omit(b$content$data[[i, j]]$txt), collapse = "")
      if (span == 0 || !nzchar(trimws(txt))) next
      size <- font_size %||% st$text$font.size$data[i, j]
      pad_in <- (st$pars$padding.left$data[i, j] + st$pars$padding.right$data[i, j]) / 72
      avail_in <- sum(b$colwidths[j:(j + span - 1)]) - pad_in
      lines <- .forest_line_count(txt, avail_in - safety_in, size)
      pitch <- size * 1.15 * st$pars$line_spacing$data[i, j] # Arial: single line = 1.15 em
      extra_pt[i] <- max(extra_pt[i], (lines - 1) * pitch)
    }
  }
  h <- row_height + extra_pt / 72
  h[n] <- .forest_axis_height(p_axis)
  round(h * 1440) / 1440 # whole twips, as Word stores them
}

# Lines a text needs with greedy word wrapping
.forest_line_count <- function(text, limit_in, size_pt) {
  limit_in <- max(limit_in, 0.01)
  n <- 0L
  for (piece in strsplit(text, "\n", fixed = TRUE)[[1]]) {
    words <- strsplit(trimws(piece), "\\s+")[[1]]
    words <- words[nzchar(words)]
    if (length(words) == 0) {
      n <- n + 1L
      next
    }
    lines <- 1L
    cur <- words[1]
    for (w in words[-1]) {
      if (.forest_text_width(paste(cur, w), size_pt) > limit_in) {
        lines <- lines + 1L
        cur <- w
      } else {
        cur <- paste(cur, w)
      }
    }
    lines <- lines + sum(pmax(ceiling(.forest_text_width(words, size_pt) / limit_in) - 1, 0))
    n <- n + lines
  }
  max(n, 1L)
}

# Axis row: what ggplot2 needs for the axis, plus a 3pt stub of reference lines
.forest_axis_height <- function(p_axis, stub_pt = 3) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  g <- ggplot2::ggplotGrob(p_axis)
  sum(grid::convertHeight(g$heights, "in", valueOnly = TRUE)) + stub_pt / 72
}
