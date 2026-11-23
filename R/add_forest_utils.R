# Utility functions for adding forest plot to gtsummary tables
#
# add_forest() builds the table and draws the forest column; adjust_forest_widths() redraws it later.
# Drawing (.forest_refresh()) has three steps:
#   1. .forest_plan():  layout from the table, attr(x, "forest") and the page height; no side effects
#   2. .forest_draw():  renders the pictures, only when stale (.forest_is_stale())
#   3. .forest_style(): re-applies what decoration resets (merges, row heights, 1pt fonts, HTML), every call

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

    # plot margins: the row and header plots, and the x-axis plot
    plot_margins = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt"),
    axis_plot_margins = margin(t = 0, r = 5, b = 3, l = 5, unit = "pt")
  )
}


# Header label font; ragg draws it with the system's font or a fallback, measured alike (.forest_label_width())
.forest_label_family <- "Arial"

# flextable needs magick to read gg_chunk() images back when it draws the table
# (gen_grob(), save_as_image()); without it the forest column renders blank
.warn_if_no_magick <- function(installed = rlang::is_installed("magick")) {
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

# Plot data ---------------------------------------------------------------------

# What the plot is drawn from, as plain data that survives saveRDS(). citril reads `version`, `col_key` and
# `width` (inches). The axis height depends on the data only, so it is measured once, here.
.forest_inputs <- function(x, estimate, conf_low, conf_high, pvalue, row_height, pagesize) {
  tb <- x$table_body
  # Extract only finite numbers that are ALSO <= 999.99
  finite <- function(col) tb[[col]][is.finite(tb[[col]]) & tb[[col]] <= 999.99]
  fin_low <- finite(conf_low)
  fin_high <- finite(conf_high)
  fin_est <- finite(estimate)
  # rows without a usable estimate and CI get only the reference lines
  ok <- !vapply(seq_len(nrow(tb)), function(i) .is_na_or_chr(x, i, estimate, conf_low, conf_high), logical(1))
  plotted <- function(col) {
    values <- tb[[col]]
    values[!ok] <- NA
    as.numeric(values)
  }
  limits <- c(
    # Calculate limits (with fallbacks to prevent warnings if vectors are empty)
    if (length(fin_low) > 0 && min(fin_low) > 0) min(fin_low, 0.2) else 0.2, # at least 0.2 because of log scale
    if (length(fin_high) > 0) max(fin_high, 1.2) else 2.0 # Default upper limit if all are filtered out
  )
  mean_estimate <- if (length(fin_est) > 0) mean(fin_est) else 1.0

  list(
    version = 1L,
    col_key = "ggplot",
    width = c(min = 1.5, natural = 2.5),
    data = data.frame(
      estimate = plotted(estimate),
      conf_low = plotted(conf_low),
      conf_high = plotted(conf_high),
      pvalue = if (length(pvalue) > 0) as.numeric(tb[[pvalue]]) else rep(NA_real_, nrow(tb))
    ),
    limits = limits,
    mean_estimate = mean_estimate,
    header = .determine_ggplot_header(x),
    row_height = row_height,
    axis_height = .forest_axis_height(.plot_centered_axis(limits, mean_estimate, .get_default_forest_sizes())),
    pagesize = pagesize
  )
}

# The body rows as their dataset values (untouched by compose(), mk_par() and decoration), to detect row edits.
# Stored as is: hashes and text conversions vary across package versions and locales.
.forest_row_values <- function(dataset, columns) {
  n <- nrow(dataset)
  lapply(stats::setNames(nm = columns), function(k) if (k %in% names(dataset)) as.vector(dataset[[k]]) else rep(NA, n))
}

# Plots ---------------------------------------------------------------------------

# CIs of body rows `rows`, each centred in its band of `heights`, over the reference lines
.forest_rows_plot <- function(forest, rows, heights, sizes) {
  d <- forest$data[rows, , drop = FALSE]
  d$y <- cumsum(heights) - heights / 2
  d$dot <- ifelse(is.na(d$pvalue), sizes$dot_base, sizes$dot_max * d$pvalue)
  d <- d[!is.na(d$estimate), , drop = FALSE]
  ggplot2::ggplot(d) +
    ggplot2::geom_errorbar(ggplot2::aes(y = .data$y, xmin = .data$conf_low, xmax = .data$conf_high),
      width = 0, linewidth = sizes$errorbar_size, orientation = "y"
    ) +
    ggplot2::geom_vline(xintercept = forest$mean_estimate, linetype = "dotted", linewidth = sizes$line_ref) +
    ggplot2::geom_vline(xintercept = 1, linewidth = sizes$line_ref) +
    ggplot2::geom_vline(xintercept = 0.2, linewidth = sizes$line_ref) +
    ggplot2::geom_point(ggplot2::aes(y = .data$y, x = .data$estimate, size = .data$dot),
      shape = 21, fill = "white", stroke = sizes$stroke
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_x_log10(limits = forest$limits) +
    ggplot2::scale_y_reverse(limits = c(sum(heights), 0), expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = sizes$plot_margins, legend.position = "none")
}

# One picture for the whole column (reference lines drawn once, so they stay solid), axis plot underneath
.forest_column_plot <- function(forest, heights, sizes, p_axis) {
  n <- length(heights)
  rows_plot <- .forest_rows_plot(forest, seq_len(n - 1), heights[-n], sizes)
  cowplot::plot_grid(rows_plot, p_axis,
    ncol = 1, rel_heights = c(sum(heights[-n]), heights[n]), align = "v", axis = "lr"
  )
}

# Header picture (.forest_header_layout()), on the body plots' scale and margins so the labels align
.forest_header_plot <- function(header, limits, sizes) {
  label_df <- function(x, labels) data.frame(x = x, y = length(labels) - seq_along(labels) + 0.5, label = labels)
  df <- rbind(label_df(header$x[1], header$left), label_df(header$x[2], header$right))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label)) +
    ggplot2::geom_text(
      size = header$font_size / ggplot2::.pt, hjust = 0.5, vjust = 0.5, family = .forest_label_family
    ) +
    # in a narrow column a label can be centered outside the limits: draw it, don't drop it
    ggplot2::scale_x_log10(limits = limits, oob = scales::oob_keep) +
    ggplot2::scale_y_continuous(limits = c(0, max(length(header$left), length(header$right))), expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = sizes$plot_margins)
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

# Page and widths -----------------------------------------------------------------

.forest_pagesizes <- c(paste0("L", 6:10), paste0("P", 6:10))

# `pagesize` is NULL or a citril page size code, upper case as in citril
.check_pagesize <- function(pagesize, call = get_cli_abort_call()) {
  check_string(pagesize, allow_empty = TRUE, call = call)
  if (!is.null(pagesize)) arg_match(pagesize, values = .forest_pagesizes, error_call = call)
  invisible(pagesize)
}

# Text width and height (in) and font size (pt) of a citril page size: A4 minus citril's margins. NULL if unknown.
.forest_page <- function(pagesize) {
  if (!isTRUE(pagesize %in% .forest_pagesizes)) {
    return(NULL)
  }
  landscape <- startsWith(pagesize, "L")
  list(
    width = if (landscape) 11.69 - (3.30 + 3.35) / 2.54 else 8.27 - (3.66 + 2.11) / 2.54,
    height = if (landscape) 8.27 - (3.66 + 2.11) / 2.54 else 11.69 - (3.55 + 3.30) / 2.54,
    font_size = as.numeric(substring(pagesize, 2))
  )
}

# The table's page size: citril's attr(x, "pagesize") if known, else add_forest()'s; NULL for none
.forest_pagesize <- function(x) {
  pagesize <- attr(x, "pagesize")
  if (isTRUE(pagesize %in% .forest_pagesizes)) pagesize else attr(x, "forest")$pagesize
}

# Text height (in) of the table's page, else A4 landscape; an unknown attr(x, "pagesize") warns once
.forest_page_height <- function(x) {
  pagesize <- attr(x, "pagesize")
  page <- .forest_page(.forest_pagesize(x)) %||% .forest_page("L8")
  if (!is.null(pagesize) && is.null(.forest_page(pagesize))) {
    .forest_warn_once(
      c(
        paste(
          "{.code attr(x, \"pagesize\")} is {.val {format(pagesize)}}, not a page size crane knows",
          "({.val L6} to {.val L10}, {.val P6} to {.val P10})."
        ),
        "i" = paste(
          "The forest plot is fitted to a {round(page$height, 2)}in high page: the page size of",
          "{.fun add_forest}, else A4 landscape. Use {.arg page_height} to give the page's text height."
        )
      ),
      id = paste0("crane_forest_pagesize_", rlang::hash(pagesize))
    )
  }
  page$height
}

# cli_warn(.frequency = "once") that skips cli's formatting (~15ms) once given, as no-op calls are frequent
.forest_warn_once <- function(message, id, .envir = parent.frame()) {
  if (identical(getOption("rlib_warning_verbosity", "default"), "default")) {
    if (isTRUE(.forest_env[[id]])) {
      return(invisible())
    }
    .forest_env[[id]] <- TRUE
  }
  cli::cli_warn(message, .frequency = "once", .frequency_id = id, .envir = .envir)
}

# Warns when the table is wider than the page's text width: every time for a user's `total_width` (`given`), else
# once per session and width. `autofit`: `table_width` is what the text of an autofit table needs.
.forest_warn_wider <- function(table_width, pagesize, given = TRUE, autofit = FALSE) {
  page <- .forest_page(pagesize)
  if (is.null(page) || table_width <= page$width + 0.005) {
    return(invisible())
  }
  msg <- if (autofit) "The table needs {round(table_width, 2)}in," else "The table is {round(table_width, 2)}in wide,"
  msg <- paste(msg, "wider than the {round(page$width, 2)}in text width of a {.val {pagesize}} page.")
  if (given) {
    cli::cli_warn(c(msg, "i" = "{.arg total_width} is kept, so the table runs into the page margins."))
  } else {
    hint <- "Set narrower widths, e.g. with {.arg total_width} or {.fun citril::set_column_widths}."
    if (autofit) {
      hint <- paste(
        "Word narrows an autofit table to the page, and the exact row heights cut off the text that then wraps.",
        "Set the widths, e.g. with {.arg total_width} or {.fun citril::set_column_widths}."
      )
    }
    .forest_warn_once(c(msg, "i" = hint), id = sprintf("crane_forest_wider_%s_%s_%.3f", pagesize, autofit, table_width))
  }
  invisible()
}

# Widths filling `table_width`: the forest column keeps its natural width, the text columns share the rest. If
# that does not fit, the label column wraps (down to 1in), the forest column narrows (to its minimum, or to what its
# labels need), then text columns wrap between words; words break, with a warning, only as a last resort.
.forest_fit_widths <- function(ft, table_width, forest, header_row) {
  keys <- setdiff(ft$col_keys, forest$col_key)
  natural <- forest$width[["natural"]]
  floor_w <- forest$width[["min"]]
  .forest_measuring({
    need <- .forest_min_widths(ft, keys)
    # same slack as .forest_part_lines(): a column at its longest word does not break it
    word <- .forest_min_widths(ft, keys, words = TRUE, slack_in = .forest_line_safety)
    arial <- .forest_min_widths(ft, keys, words = TRUE, slack_in = .forest_line_safety, system_fonts = FALSE)
    header <- .forest_header_layout(ft, forest, floor_w, header_row)
    if (sum(need) + natural > table_width && !is.null(header) && !header$fits) {
      label_w <- .forest_header_need(forest$header, forest$limits, header$font_size)
      if (label_w <= natural && sum(word) + label_w <= table_width) floor_w <- label_w
    }
  })
  forest_w <- natural
  if (sum(need) + natural > table_width) {
    need[1] <- max(table_width - natural - sum(need[-1]), min(need[1], max(1, word[1])))
    forest_w <- max(min(natural, table_width - sum(need)), floor_w)
  }
  room <- table_width - forest_w
  short <- sum(need) - room
  if (short > 1e-9) {
    floor_in <- if (sum(word) <= room) word else if (sum(arial) <= room) arial
    cli::cli_warn(c(
      "The table needs {round(sum(need) + forest_w, 2)}in but the page has {round(table_width, 2)}in.",
      "i" = if (!is.null(floor_in)) {
        "Text columns are narrowed and their text wraps. A landscape page size would avoid this."
      } else {
        paste(
          "Text columns are narrowed, their text wraps and words too long for their column break",
          "({round(sum(arial) + forest_w, 2)}in would keep them whole). A landscape page size would avoid this."
        )
      }
    ))
    need <- if (is.null(floor_in)) need * room / sum(need) else need - (need - floor_in) * short / sum(need - floor_in)
  } else {
    need <- need * room / sum(need)
  }
  ft <- flextable::width(ft, j = c(keys, forest$col_key), width = unname(c(need, forest_w)))
  # Word and HTML keep the widths of a fixed layout only
  .forest_table_properties(ft, layout = "fixed")
}

# Width (in) each column of `keys` needs: its longest body line (or word, with `words`) and longest header word,
# plus padding. `system_fonts = FALSE` measures in Arial only. Cells spanning several columns do not count.
.forest_min_widths <- function(ft, keys, words = FALSE, slack_in = 0.06, system_fonts = TRUE) {
  j <- match(keys, ft$col_keys)
  part_need <- function(part, split_words) {
    p <- ft[[part]]
    cells <- .forest_text_cells(p, skip = setdiff(seq_along(ft$col_keys), j), one_column = TRUE)
    if (length(cells$txt) == 0) {
      return(rep(0, length(j)))
    }
    if (!system_fonts) cells$family <- NULL
    pieces <- lapply(cells$txt, if (split_words) .forest_words else .forest_lines)
    cell <- factor(rep(seq_along(pieces), lengths(pieces)), seq_along(pieces))
    widest <- vapply(split(.forest_piece_widths(pieces, cells), cell), max, numeric(1))
    pad <- (p$styles$pars$padding.left$data[cells$at] + p$styles$pars$padding.right$data[cells$at]) / 72
    need <- tapply(widest + pad, factor(cells$at[, 2], j), max)
    ifelse(is.na(need), 0, need)
  }
  stats::setNames(pmax(part_need("body", words), part_need("header", TRUE)) + slack_in, keys)
}

# Whether the text of a cell is bold: one of its text chunks is (by its own setting, else by the cell's)
.forest_is_bold <- function(chunks, cell_bold) {
  bold <- chunks$bold
  bold[is.na(bold)] <- cell_bold
  isTRUE(any(bold[!is.na(chunks$txt)]))
}

# Plan ----------------------------------------------------------------------------

# Page height (in) a merged picture leaves free: citril's empty first paragraph (0.26in) and the line under the
# header picture (0.04in)
.forest_page_reserve <- 0.3

# Layout of the forest column for the table as it is now: column width, row heights, header picture and its row
# (`header_row`, NULL for none), table height, and whether one merged picture fits the page
.forest_plan <- function(x, forest, page_height, header_row = .forest_header_row(x, forest)) {
  j <- match(forest$col_key, x$col_keys)
  width <- x$body$colwidths[[j]]
  .forest_measuring({
    # cached by add_forest()
    axis_height <- forest$axis_height %||%
      .forest_axis_height(.plot_centered_axis(forest$limits, forest$mean_estimate, .get_default_forest_sizes()))
    heights <- .forest_row_heights(x, j, forest$row_height, axis_height)
    header <- .forest_header_layout(x, forest, width, header_row)
    picture <- if (!is.null(header)) {
      # a footnote mark next to the picture takes a line of its own
      mark_in <- if (.forest_has_text(.forest_cell_text(x$header, header_row, j))) header$font_size * 1.15 / 72 else 0
      c(header_row, j, header$height + mark_in)
    }
    table_height <- .forest_part_height(x, "header", picture) + sum(heights) + .forest_part_height(x, "footer")
    list(
      width = width, heights = heights, merged = table_height + .forest_page_reserve <= page_height,
      header = header, header_row = if (!is.null(header)) header_row, table_height = table_height,
      page_height = page_height
    )
  })
}

# What the pictures depend on: when it changes, they are drawn again
.forest_drawing <- function(plan, forest) {
  c(plan[c("width", "heights", "merged", "header")], forest[c("data", "limits", "mean_estimate")])
}

# Body row heights (in): row_height plus a line pitch per wrapped line; the axis row fits the axis. In whole twips.
.forest_row_heights <- function(x, j, row_height, axis_height) {
  text <- .forest_part_lines(x, "body", skip = j) # the forest column holds pictures
  extra_pt <- apply(pmax(text$lines - 1, 0) * text$pitch, 1, max)
  h <- row_height + extra_pt / 72
  h[length(h)] <- axis_height
  round(h * 1440) / 1440
}

# Header picture layout at the font size of `header_row`'s labels; NULL without labels or picture row
.forest_header_layout <- function(x, forest, width_in, header_row) {
  if (is.null(forest$header) || is.null(header_row)) {
    return(NULL)
  }
  pt <- .forest_label_font(x, header_row, match(forest$col_key, x$col_keys))
  .forest_label_layout(forest$header, forest$limits, width_in, pt)
}

# Header labels (each over "Better") either side of the line at 1: one line each if that fits, else wrapped to the
# narrower side. `fits` is FALSE when a label is cut off at the edge (`cut_off`) or crosses the line (`crosses`).
.forest_label_layout <- function(labels, limits, width_in, pt, gap_in = 0.05) {
  measure <- function(text) .forest_label_width(text, pt)
  margin_in <- 5 / 72 # the plots' 5pt left and right margins
  tolerance <- 0.005
  panel_in <- width_in - 2 * margin_in
  # x range of the panel in log10 units, with ggplot's default 5% expansion
  xr <- log10(limits) + c(-0.05, 0.05) * diff(log10(limits))
  one_in <- -xr[1] / diff(xr) * panel_in # position of the line at 1, inches
  place <- function(max_in) {
    left <- c(.forest_wrap(labels$left, max_in, measure), "Better")
    right <- c(.forest_wrap(labels$right, max_in, measure), "Better")
    # center of each label block (inches, panel coordinates)
    w <- c(max(measure(left)), max(measure(right)))
    centers <- one_in + c(-1, 1) * (gap_in + w / 2)
    centers <- centers + max(0, w[1] / 2 - margin_in - centers[1]) -
      max(0, centers[2] + w[2] / 2 - panel_in - margin_in)
    edges <- centers[c(1, 1, 2, 2)] + c(-1, 1, -1, 1) * w[c(1, 1, 2, 2)] / 2
    cut_off <- edges[1] < -margin_in - tolerance || edges[4] > panel_in + margin_in + tolerance
    crosses <- edges[2] > one_in + tolerance || edges[3] < one_in - tolerance
    list(
      font_size = pt, left = left, right = right,
      x = 10^(xr[1] + centers / panel_in * diff(xr)),
      height = max(length(left), length(right)) * pt * 1.15 / 72,
      fits = !cut_off && !crosses, cut_off = cut_off, crosses = crosses
    )
  }
  layout <- place(Inf)
  if (!layout$fits) {
    words <- unlist(strsplit(c(labels$left, labels$right), "\\s+"))
    layout <- place(max(min(one_in, panel_in - one_in) - gap_in + margin_in + tolerance, measure(words)))
  }
  layout
}

# Narrowest forest column (in, to 0.01in) whose header labels fit at `pt`, by bisection
.forest_header_need <- function(labels, limits, pt) {
  fits <- function(width_in) .forest_label_layout(labels, limits, width_in, pt)$fits
  .forest_measuring({
    low <- 0
    high <- 2.5
    while (!fits(high) && high < 100) {
      low <- high
      high <- 2 * high
    }
    while (high - low > 0.005) {
      mid <- (low + high) / 2
      if (fits(mid)) high <- mid else low <- mid
    }
  })
  ceiling(high * 100) / 100
}

# Height (in) of the header or footer rows as Word lays them out (at least one line per row, exact and atleast
# heights honoured). `picture`: c(row, column, height) of the picture cell.
.forest_part_height <- function(x, part, picture = NULL) {
  p <- x[[part]]
  if (nrow(p$dataset) == 0) {
    return(0)
  }
  text <- .forest_part_lines(x, part)
  pad_pt <- p$styles$pars$padding.top$data + p$styles$pars$padding.bottom$data
  cell_pt <- pmax(text$lines, 1) * text$pitch + pad_pt
  if (length(picture) == 3) cell_pt[picture[1], picture[2]] <- picture[3] * 72 + pad_pt[picture[1], picture[2]]
  content_in <- apply(cell_pt, 1, max) / 72
  rows_in <- ifelse(p$hrule == "atleast", pmax(p$rowheights, content_in), content_in)
  sum(ifelse(p$hrule == "exact", p$rowheights, rows_in))
}

# Slack (in) per text line, for differences between Word's and crane's measuring
.forest_line_safety <- 0.03

# Wrapped line count of each cell of a table part (0 for empty, merged-away and `skip` cells) and the line pitch
# (pt; Arial: 1.15em). Texts are measured whole; only those too wide for their cell are wrapped word by word.
.forest_part_lines <- function(x, part, skip = integer(0), safety_in = .forest_line_safety) {
  p <- x[[part]]
  st <- p$styles
  spacing <- st$pars$line_spacing$data
  spacing[is.na(spacing)] <- 1 # picture cells: single spacing from their paragraph style
  size <- st$text$font.size$data
  lines <- matrix(0, nrow(p$dataset), length(x$col_keys))
  cells <- .forest_text_cells(p, skip)
  if (length(cells$txt) > 0) {
    at <- cells$at
    span <- p$spans$rows[at]
    pad_in <- (st$pars$padding.left$data[at] + st$pars$padding.right$data[at]) / 72
    room_in <- vapply(seq_len(nrow(at)), function(k) sum(p$colwidths[at[k, 2] + seq_len(span[k]) - 1]), numeric(1)) -
      pad_in
    pieces <- lapply(cells$txt, .forest_lines)
    cell <- rep(seq_along(pieces), lengths(pieces))
    piece_in <- .forest_piece_widths(pieces, cells)
    fits <- piece_in <= pmax(room_in - safety_in, 0.01)[cell]
    fits <- vapply(split(fits, factor(cell, seq_along(pieces))), all, logical(1))
    n <- pmax(lengths(pieces), 1)
    for (k in which(!fits)) {
      measure <- function(text) .forest_text_width(text, cells$pt[k], cells$bold[k], cells$family[k])
      n[k] <- .forest_line_count(cells$txt[[k]], room_in[k], measure, safety_in)
    }
    lines[at] <- n
  }
  list(lines = lines, pitch = size * 1.15 * spacing)
}

# Cells of part `p` holding text (outside `skip`, not merged away): positions, text, font size, face and family.
# Texts stay separate: mixed encodings in one vector break regular expressions outside UTF-8 locales.
.forest_text_cells <- function(p, skip = integer(0), one_column = FALSE) {
  st <- p$styles$text
  at <- which((if (one_column) p$spans$rows == 1 else p$spans$rows > 0) & p$spans$columns > 0, arr.ind = TRUE)
  at <- at[!at[, 2] %in% skip, , drop = FALSE]
  chunks <- lapply(seq_len(nrow(at)), function(k) p$content$data[[at[k, 1], at[k, 2]]])
  txt <- lapply(chunks, function(ch) paste(ch$txt[!is.na(ch$txt)], collapse = ""))
  keep <- .forest_has_text(as.character(txt))
  at <- at[keep, , drop = FALSE]
  list(
    at = at, txt = txt[keep], pt = st$font.size$data[at], family = st$font.family$data[at],
    bold = as.logical(mapply(.forest_is_bold, chunks[keep], st$bold$data[at]))
  )
}

# Lines of a text, words joined by single spaces
.forest_lines <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  vapply(lines, function(line) paste(.forest_words(line), collapse = " "), character(1), USE.NAMES = FALSE)
}

# Words of a text (a non-breaking space joins words)
.forest_words <- function(text) {
  words <- strsplit(text, "\\s+")[[1]]
  words[nzchar(words)]
}

# Whether texts hold more than white space (byte-wise, for any encoding)
.forest_has_text <- function(x) grepl("[^ \t\r\n]", x, useBytes = TRUE)

# Widths (in) of `pieces` (texts per cell of `cells`), each in its cell's font, measured font by font
.forest_piece_widths <- function(pieces, cells) {
  cell <- rep(seq_along(pieces), lengths(pieces))
  font <- paste(cells$pt, cells$bold, cells$family)[cell]
  texts <- unlist(pieces)
  width <- numeric(length(texts))
  for (f in unique(font)) {
    k <- cell[match(f, font)]
    width[font == f] <- .forest_text_width(texts[font == f], cells$pt[k], cells$bold[k], cells$family[k])
  }
  width
}

# Lines a text needs in `room_in` inches, as Word and browsers wrap it: between words, and a word wider than the
# room character by character. `measure` gives text widths in inches.
.forest_line_count <- function(text, room_in, measure, safety_in = .forest_line_safety) {
  limit_in <- max(room_in - safety_in, 0.01)
  room_in <- max(room_in, limit_in)
  n <- 0L
  for (piece in strsplit(text, "\n", fixed = TRUE)[[1]]) {
    words <- .forest_words(piece)
    width <- measure(words)
    if (all(width <= limit_in)) {
      n <- n + length(.forest_wrap(piece, limit_in, measure))
      next
    }
    space <- measure(" ")
    line <- NA # width of the line being filled, NA before its first word
    for (k in seq_along(words)) {
      if (!is.na(line) && line + space + width[k] <= limit_in) {
        line <- line + space + width[k]
        next
      }
      n <- n + !is.na(line)
      line <- width[k]
      if (width[k] > limit_in) {
        line <- 0
        for (char in measure(strsplit(words[k], "")[[1]])) {
          if (line > 0 && line + char > room_in) {
            n <- n + 1L
            line <- 0
          }
          line <- line + char
        }
      }
    }
    n <- n + 1L
  }
  max(n, 1L)
}

# Greedy word wrap to lines of at most `max_in` inches (a longer word gets its own line)
.forest_wrap <- function(text, max_in, measure) {
  words <- .forest_words(text)
  if (length(words) == 0) {
    return("")
  }
  lines <- character(0)
  while (length(words) > 0) {
    runs <- vapply(seq_along(words), function(k) paste(words[seq_len(k)], collapse = " "), character(1))
    k <- max(1, sum(measure(runs) <= max_in))
    lines <- c(lines, runs[k])
    words <- words[-seq_len(k)]
  }
  lines
}

# Axis row: what ggplot2 needs for the axis, plus a 3pt stub of reference lines
.forest_axis_height <- function(p_axis, stub_pt = 3) {
  .forest_measuring({
    g <- ggplot2::ggplotGrob(p_axis)
    sum(grid::convertHeight(g$heights, "in", valueOnly = TRUE)) + stub_pt / 72
  })
}

# Draw and style ----------------------------------------------------------------

# Second half of add_forest(), and all of adjust_forest_widths(): plan, redraw if stale, restyle.
# attr(x, "forest")$drawn records what the pictures were drawn from (.forest_drawing()).
.forest_refresh <- function(x, page_height = NULL, header_row = .forest_header_row(x, attr(x, "forest"))) {
  .forest_measuring({
    forest <- attr(x, "forest")
    plan <- .forest_plan(x, forest, page_height %||% .forest_page_height(x), header_row)
    if (.forest_is_stale(x, forest, plan)) {
      x <- .forest_draw(x, forest, plan)
      forest$drawn <- .forest_drawing(plan, forest)
      attr(x, "forest") <- forest
    }
    .forest_style(x, forest, plan)
  })
}

# Stale: the drawing inputs changed, a picture file is gone (gg_chunk() keeps only a tempfile() path, gone in a
# new session), or a body cell of the forest column holds text
.forest_is_stale <- function(x, forest, plan) {
  if (!identical(.forest_drawing(plan, forest), forest$drawn)) {
    return(TRUE)
  }
  j <- match(forest$col_key, x$col_keys)
  if (length(.forest_body_text(x, j)) > 0) {
    return(TRUE)
  }
  cells <- lapply(if (plan$merged) 1L else seq_along(plan$heights), function(i) .forest_cell_images(x$body, i, j))
  if (!is.null(plan$header)) cells <- c(cells, list(.forest_cell_images(x$header, plan$header_row, j)))
  !all(vapply(cells, function(path) length(path) == 1 && file.exists(path), logical(1)))
}

# Renders the merged picture (or one per row) and the header picture. Warns when the header labels do not fit, and
# when body cells of the forest column held text, which is dropped.
.forest_draw <- function(x, forest, plan) {
  j <- match(forest$col_key, x$col_keys)
  sizes <- .get_default_forest_sizes()
  p_axis <- .plot_centered_axis(forest$limits, forest$mean_estimate, sizes)
  n <- length(plan$heights)
  # as text, so cli pluralizes on the number of rows
  text_rows <- as.character(.forest_body_text(x, j))
  if (length(text_rows) > 0) {
    cli::cli_warn(c(
      "The text in the forest plot column of body row{?s} {text_rows} is dropped: the plots fill these cells.",
      "i" = "Put footnote marks and other text of a row in another column, e.g. that of its estimate."
    ))
  }
  if (plan$merged) {
    column_plot <- .forest_column_plot(forest, plan$heights, sizes, p_axis)
    x <- .forest_picture(x, 1, j, column_plot, plan$width, sum(plan$heights))
    if (n > 1) x <- flextable::mk_par(x, i = 2:n, j = j, value = flextable::as_paragraph(""), part = "body")
  } else {
    for (i in seq_len(n)) {
      plot <- if (i < n) .forest_rows_plot(forest, i, plan$heights[i], sizes) else p_axis
      x <- .forest_picture(x, i, j, plot, plan$width, plan$heights[i])
    }
  }
  if (!is.null(plan$header)) {
    if (!isTRUE(plan$header$fits)) {
      need <- .forest_header_need(forest$header, forest$limits, plan$header$font_size)
      problem <- c(if (plan$header$cut_off) "are cut off at its edge", if (plan$header$crosses) "cross the line at 1")
      cli::cli_warn(c(
        "The forest plot header labels do not fit the {round(plan$width, 2)}in forest column: they {problem}.",
        "i" = paste(
          "At {plan$header$font_size}pt they need a column of at least {need}in",
          "({.code attr(x, \"forest\")$width[\"min\"]} is {forest$width[['min']]}in):",
          "make the column wider, or the font size of the column labels smaller."
        )
      ))
    }
    x <- .forest_picture(
      x, plan$header_row, j, .forest_header_plot(plan$header, forest$limits, sizes), plan$width, plan$header$height,
      part = "header"
    )
  }
  x
}

# Body rows whose forest cell holds text, e.g. a footnote mark next to its picture
.forest_body_text <- function(x, j) {
  rows <- seq_len(nrow(x$body$dataset))
  rows[vapply(rows, function(i) .forest_has_text(.forest_cell_text(x$body, i, j)), logical(1))]
}

# The text of a cell, all its chunks together
.forest_cell_text <- function(part, i, j) {
  txt <- part$content$data[[i, j]]$txt
  paste(txt[!is.na(txt)], collapse = "")
}

# Puts the picture of `plot` in cell (i, j); in a header cell it replaces only the old picture (keeps a footnote mark)
.forest_picture <- function(x, i, j, plot, width, height, part = "body") {
  picture <- flextable::gg_chunk(value = list(plot), width = width, height = height)
  chunks <- x[[part]]$content$data[[i, j]]
  images <- which(vapply(chunks$img_data, is.character, logical(1)))
  value <- if (part == "body" || length(images) == 0) {
    flextable::as_paragraph(picture)
  } else {
    keep <- setdiff(seq_len(nrow(chunks)), images[-1])
    flextable::as_paragraph(list_values = lapply(keep, function(k) {
      if (k == images[1]) {
        return(picture)
      }
      structure(chunks[k, names(chunks) != ".chunk_index"], class = c("chunk", "data.frame"))
    }))
  }
  flextable::mk_par(x, i = i, j = j, part = part, value = value)
}

# Re-applied every call, as decoration resets them: the forest column's merge (only its spans: merge_none() would
# clear every merge), exact row heights, 1pt picture cells without padding, header text, HTML. The paragraph style
# is set once in add_forest(): style(pr_p =) would reset paginate()'s keep_with_next.
.forest_style <- function(x, forest, plan) {
  j <- match(forest$col_key, x$col_keys)
  n <- length(plan$heights)
  if (plan$merged) {
    x <- flextable::merge_at(x, i = seq_len(n), j = j, part = "body")
  } else {
    x$body$spans$columns[, j] <- 1
  }
  x <- x |>
    flextable::height(height = plan$heights, part = "body") |>
    flextable::hrule(rule = "exact", part = "body") |>
    flextable::fontsize(j = j, size = 1, part = "body") |>
    flextable::padding(j = j, padding = 0, part = "body")
  if (!is.null(plan$header)) {
    x <- x |>
      flextable::fontsize(i = plan$header_row, j = j, size = 1, part = "header") |>
      flextable::padding(i = plan$header_row, j = j, padding.left = 0, padding.right = 0, part = "header")
  }
  x <- .forest_header_text(x, j)
  .forest_html(x, forest, plan)
}

# Header row whose forest cell holds a picture (file present or not), wherever titles put it; NULL when there is
# none, e.g. after set_header_labels(ggplot = ...)
.forest_header_row <- function(x, forest) {
  j <- match(forest$col_key, x$col_keys)
  p <- x$header
  rows <- seq_len(nrow(p$dataset))
  rows <- rows[vapply(rows, function(i) {
    p$spans$rows[i, j] > 0 && p$spans$columns[i, j] > 0 && length(.forest_cell_images(p, i, j)) > 0
  }, logical(1))]
  if (length(rows) > 0) max(rows)
}

# Paths of the pictures in a cell (gg_chunk() keeps only a tempfile() path)
.forest_cell_images <- function(part, i, j) {
  images <- unlist(lapply(part$content$data[[i, j]]$img_data, function(v) if (is.character(v)) v))
  images[!is.na(images) & nzchar(images)]
}

# Font size of the column labels of header row `i`: its first cell outside the forest column `j`
.forest_label_font <- function(x, i, j) {
  x$header$styles$text$font.size$data[[i, setdiff(seq_along(x$col_keys), j)[1]]]
}

# Text in header cells of the forest column (a label replacing the picture, a footnote mark, a copied row) gets its
# row's label font size instead of the picture cell's 1pt, and single line spacing (gen_grob() fails without it)
.forest_header_text <- function(x, j) {
  p <- x$header
  for (i in seq_len(nrow(p$dataset))) {
    chunks <- p$content$data[[i, j]]
    text <- !is.na(chunks$txt)
    if (p$spans$rows[i, j] == 0 || p$spans$columns[i, j] == 0 || !any(text)) next
    if (is.na(p$styles$pars$line_spacing$data[[i, j]])) {
      x <- flextable::line_spacing(x, i = i, j = j, space = 1, part = "header")
    }
    size <- .forest_label_font(x, i, j)
    if (length(.forest_cell_images(p, i, j)) > 0) {
      chunks$font.size[text & is.na(chunks$font.size)] <- size
      x$header$content$data[[i, j]] <- chunks
    } else if (p$styles$text$font.size$data[[i, j]] == 1 && .forest_has_text(.forest_cell_text(p, i, j))) {
      x <- flextable::fontsize(x, i = i, j = j, size = size, part = "header")
    }
  }
  x
}

# HTML: each forest table gets its own class, added to the table's (citril's) class and CSS, replacing only crane's
# earlier rules. The rules show the pictures at their drawn width (flextable writes 72 px/in, 75% in a browser) and
# set table-layout:auto, overriding citril's `table{table-layout:fixed}`, which squeezes the forest column. Fixed
# layouts also pin the table width; autofit ones keep body text on one line.
.forest_html <- function(x, forest, plan) {
  html <- x$properties$opts_html
  # `&` is the class selector; the header picture is matched by its width, so other header images keep their size
  picture <- sprintf(
    paste0(
      "& thead img[style*=\";width:%.0fpx;\"],& tbody img{display:block;margin:0 auto;width:%.3fin !important;",
      "height:auto !important;max-width:none !important;}"
    ),
    plan$width * 72, plan$width
  )
  rules <- if (identical(x$properties$layout, "fixed")) {
    c(
      sprintf("&{table-layout:auto;width:%1$.3fin;min-width:%1$.3fin;}", sum(x$body$colwidths)),
      "& td,& th{overflow-wrap:anywhere;}",
      picture
    )
  } else {
    c(
      "&{table-layout:auto;width:auto;min-width:0;}",
      "& tbody td{white-space:nowrap;}",
      sprintf("& thead [colspan=\"%1$d\"],& tfoot [colspan=\"%1$d\"]{width:1px;}", length(x$col_keys)),
      picture
    )
  }
  class <- paste0("crane-forest-", substr(rlang::hash(list(forest[c("data", "limits", "header")], rules)), 1, 8))
  classes <- unlist(strsplit(paste(html$extra_class, collapse = " "), "\\s+"))
  classes <- classes[nzchar(classes) & !grepl("^crane-forest(-[0-9a-f]{8})?$", classes)]
  .forest_table_properties(x, opts_html = list(
    extra_css = paste0(
      gsub("\\.crane-forest-[0-9a-f]{8}[^{]*\\{[^}]*\\}", "", html$extra_css %||% ""),
      gsub("&", paste0(".", class), paste(rules, collapse = ""), fixed = TRUE)
    ),
    scroll = html$scroll,
    extra_class = paste(c(classes, "crane-forest", class), collapse = " ")
  ))
}

# set_table_properties() resets every property it is not given: pass the others back as they are
.forest_table_properties <- function(x, layout = x$properties$layout, opts_html = x$properties$opts_html) {
  p <- x$properties
  flextable::set_table_properties(
    x,
    layout = layout %||% "fixed", width = p$width %||% 0, align = p$align,
    opts_html = unclass(opts_html) %||% list(), opts_word = unclass(p$opts_word) %||% list(),
    opts_pdf = unclass(p$opts_pdf) %||% list(), word_title = p$word_title, word_description = p$word_description
  )
}

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

# Checks --------------------------------------------------------------------------

# The "x" bullet of an error: the value supplied, or its type
.forest_supplied <- function(x) {
  if ((is.character(x) || is.numeric(x)) && length(x) == 1L) {
    "You supplied {.val {x}}."
  } else {
    "You supplied {.obj_type_friendly {x}}."
  }
}

# A single positive, finite number of inches (or NULL, when allowed)
.forest_check_inches <- function(x, allow_null = FALSE, arg_name = rlang::caller_arg(x),
                                 call = get_cli_abort_call()) {
  if ((allow_null && is.null(x)) || (is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0)) {
    return(invisible(x))
  }
  cli::cli_abort(
    c("{.arg {arg_name}} must be a single positive number of inches.", "x" = .forest_supplied(x)),
    call = call
  )
}

# NULL, or positive, finite column shares: one per column, or one for all columns
.forest_check_widths <- function(x, n_col, arg_name = rlang::caller_arg(x), call = get_cli_abort_call()) {
  if (is.null(x) || (is.numeric(x) && length(x) %in% c(1L, n_col) && all(is.finite(x)) && all(x > 0))) {
    return(invisible(x))
  }
  cli::cli_abort(
    c(
      "{.arg {arg_name}} must be positive column shares, one value per column ({n_col}) or a single value.",
      "x" = if (is.numeric(x)) "You supplied {length(x)} value{?s}: {.val {x}}." else .forest_supplied(x)
    ),
    call = call
  )
}

# A flextable made by add_forest(), with its forest column, whose body cells are not merged across that column
.forest_check_table <- function(x, call = get_cli_abort_call()) {
  check_class(x, "flextable", call = call)
  forest <- attr(x, "forest")
  if (!is.list(forest) || !identical(forest$version, 1L)) {
    cli::cli_abort(
      c(
        "{.arg x} must be a table made by {.fun add_forest}.",
        "i" = "It has no forest plot data of version 1 in {.code attr(x, \"forest\")}."
      ),
      call = call
    )
  }
  j <- which(x$col_keys == forest$col_key)
  if (length(j) != 1L) {
    cli::cli_abort("The forest plot column {.val {forest$col_key}} is missing from {.arg x}.", call = call)
  }
  # as text, so cli pluralizes on the number of rows
  across <- as.character(which(x$body$spans$rows[, j] != 1))
  if (length(across) > 0) {
    cli::cli_abort(
      "Body row{?s} {across} {?is/are} merged across the forest plot column; unmerge {?it/them} first.",
      call = call
    )
  }
  invisible(x)
}

# The body rows must be the ones add_forest() drew, in order: the plots are matched to them by position
.forest_check_rows <- function(x, call = get_cli_abort_call()) {
  drawn <- attr(x, "forest")$rows
  rows <- .forest_row_values(x$body$dataset, names(drawn))
  n_rows <- nrow(x$body$dataset)
  n_drawn <- length(drawn[[1]])
  if (identical(rows, drawn)) {
    return(invisible(x))
  }
  # first differing row (equal values of another type count as the same)
  n <- min(n_rows, n_drawn)
  same <- rep(TRUE, n)
  for (k in names(drawn)) {
    a <- rows[[k]][seq_len(n)]
    b <- drawn[[k]][seq_len(n)]
    same <- same & ((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
  }
  i <- which(!same)[1]
  if (is.na(i) && n_rows == n_drawn) {
    return(invisible(x))
  }
  if (is.na(i)) i <- n + 1L
  bullets <- "The body rows of {.arg x} are not the rows its forest plot was drawn for."
  if (i <= n_rows) {
    label <- trimws(as.character(rows[[1]][i]))
    bullets <- c(bullets, "x" = if (!is.na(label) && nzchar(label)) {
      "Body row {i} ({.val {label}}) is not the row the plot was drawn for."
    } else {
      "Body row {i} is not the row the plot was drawn for."
    })
  }
  if (n_rows != n_drawn) {
    bullets <- c(bullets, "x" = "{.arg x} has {n_rows} body row{?s}, the plot was drawn for {n_drawn}.")
  }
  cli::cli_abort(
    c(bullets, "i" = "Add, remove or reorder rows in the {.pkg gtsummary} table, before {.fun add_forest}."),
    call = call
  )
}

# Measuring -----------------------------------------------------------------------

.forest_env <- new.env(parent = emptyenv())

# Runs `code` on one pdf measuring device (Helvetica metrics, as Arial's), opened once per top-level call and
# reused by nested calls; afterwards the caller's device is active again
.forest_measuring <- function(code) {
  device <- .forest_env$device
  if (!is.null(device) && device %in% grDevices::dev.list()) {
    # nested: gg_chunk() may have activated another device
    if (grDevices::dev.cur() != device) grDevices::dev.set(device)
    return(code)
  }
  previous <- grDevices::dev.cur()
  measuring <- .forest_open_device()
  .forest_env$device <- measuring
  on.exit({
    if (measuring %in% grDevices::dev.list()) grDevices::dev.off(measuring)
    .forest_env$device <- NULL
    .forest_reactivate(previous)
  })
  code
}

# Opens the measuring device and returns its number
.forest_open_device <- function() {
  grDevices::pdf(NULL)
  grDevices::dev.cur()
}

# Text width (in): the wider of Arial (as Word draws it) and, with systemfonts, the cell's `family` as installed here
.forest_text_width <- function(x, size_pt, bold = FALSE, family = NULL) {
  width <- .forest_measuring(
    graphics::strwidth(x, units = "inches", family = "sans", font = if (bold) 2L else 1L, cex = size_pt / 12)
  )
  if (is.null(family) || !requireNamespace("systemfonts", quietly = TRUE)) {
    return(width)
  }
  pmax(width, .forest_font_width(x, size_pt, family, bold), na.rm = TRUE)
}

# Text width (in) in `family` as installed (systemfonts); NA for non-ASCII native text outside UTF-8 locales
.forest_font_width <- function(x, size_pt, family, bold = FALSE) {
  # systemfonts 1.3.0 replaced `bold` with `weight`
  face <- if ("weight" %in% names(formals(systemfonts::string_width))) {
    list(weight = if (bold) "bold" else "normal")
  } else {
    list(bold = bold)
  }
  # high resolution, so hinting does not round the widths
  width <- rlang::inject(systemfonts::string_width(x, family = family, size = size_pt, res = 1200, !!!face)) / 1200
  if (!l10n_info()[["UTF-8"]]) width[Encoding(x) == "unknown" & is.na(iconv(x, "ASCII", "ASCII"))] <- NA
  width
}

# Header label width (in) as ragg draws it (system font or fallback); the measuring device without systemfonts
.forest_label_width <- function(x, size_pt) {
  width <- rep(NA_real_, length(x))
  if (requireNamespace("systemfonts", quietly = TRUE)) width <- .forest_font_width(x, size_pt, .forest_label_family)
  if (anyNA(width)) width[is.na(width)] <- .forest_text_width(x[is.na(width)], size_pt)
  width
}

# Reactivates `device` if still open: closing a device (as gg_chunk() does) activates the next one, not the caller's
.forest_reactivate <- function(device) {
  if (device > 1 && device %in% grDevices::dev.list()) grDevices::dev.set(device)
  invisible()
}
