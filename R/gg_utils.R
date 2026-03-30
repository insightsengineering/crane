#' Create and Stack an X-Axis Aligned Table
#'
#' @description Generates a text table from a dataframe, perfectly aligns its
#'   X-axis to match a provided ggplot, and seamlessly stacks them vertically.
#'   Tailored specifically for Kaplan-Meier ("KM") or Pharmacokinetic ("PK")
#'   table structures. Supports both continuous and discrete X-axes.
#'
#' @param df (`data.frame`)\cr
#'   The summary table to render.
#' @param gg_plt (`ggplot2`)\cr
#'   The main plot to align to and stack on top of.
#' @param type (`character`)\cr
#'   The type of plot/table. Either `"KM"` (uses rownames for Y-axis) or `"PK"`
#'   (uses the first column for Y-axis).
#' @param y_labels (`character` or `expression`)\cr
#'   Formatted labels override.
#' @param title (`character` or `NULL`)\cr
#'   Optional title for the table.
#' @param xlab (`character` or `NULL`)\cr
#'   Optional X-axis label for the table.
#' @param show_xaxis (`logical`)\cr
#'   Should the table display an X-axis line?
#' @param text_size (`numeric`)\cr
#'   Size of the data values in the table.
#' @param label_size (`numeric`)\cr
#'   Size of the axis and title text.
#' @param rel_height_plot (`numeric`)\cr
#'   Vertical space for the main plot.
#'
#' @examples
#' \dontrun{
#' # 1. Create a base plot with a continuous X-axis
#' p_base <- ggplot(mtcars, aes(x = mpg, y = disp)) +
#'   geom_point() +
#'   scale_x_continuous(limits = c(10, 35), breaks = c(10, 20, 30))
#'
#' # 2. Create a mock summary table matching the X-axis breaks
#' mock_df <- data.frame(
#'   `10` = c("15", "12"),
#'   `20` = c("10", "8"),
#'   `30` = c("5", "3"),
#'   check.names = FALSE
#' )
#' rownames(mock_df) <- c("Group A", "Group B")
#'
#' # 3. Align and stack the table under the plot using the "KM" engine
#' df2gg_aligned(
#'   df = mock_df,
#'   gg_plt = p_base,
#'   type = "KM",
#'   title = "Subjects at Risk",
#'   show_xaxis = TRUE
#' )
#' }
#' @return A combined `cowplot` object.
#' @keywords internal
df2gg_aligned <- function(df,
                          gg_plt,
                          type = c("KM", "PK", "GEN"),
                          y_labels = NULL,
                          title = NULL,
                          xlab = NULL,
                          show_xaxis = FALSE,
                          text_size = 3.5,
                          label_size = 10,
                          rel_height_plot = 0.75) {
  # 1. Input Validation---------------------------------------------------------
  type <- match.arg(type)

  if (!is.data.frame(df)) {
    rlang::abort("`df` must be a data.frame or tibble.")
  }

  if (is.null(gg_plt) || !inherits(gg_plt, "gg")) {
    rlang::abort("`gg_plt` must be a valid ggplot2 object.")
  }

  # 2. Extract Y-axis labels and isolate X-columns based on `type`--------------
  if (type == "KM") {
    raw_y_labels <- rownames(df)
    df_x <- df
  } else if (type %in% c("PK", "GEN")) {
    if (ncol(df) < 2) {
      rlang::abort(
        "PK and GEN tables must have at least 2 columns (labels + data)."
      )
    }
    y_col_name <- names(df)[1]
    raw_y_labels <- as.character(df[[y_col_name]])
    df_x <- df[, names(df) != y_col_name, drop = FALSE]
  }

  final_y_labels <- if (!is.null(y_labels)) y_labels else raw_y_labels

  # 3. Extract Plot Mapping Details & Map X-Coordinates-------------------------
  plot_build <- ggplot2::ggplot_build(gg_plt)
  x_params <- plot_build$layout$panel_params[[1]]
  x_range <- x_params$x.range
  x_scale <- plot_build$layout$panel_scales_x[[1]]

  x_cols <- names(df_x)
  x_num <- suppressWarnings(as.numeric(x_cols))

  if (isTRUE(x_scale$is_discrete())) {
    # Discrete Axis Handling
    x_limits <- x_scale$get_limits()

    if (is.null(x_limits) || length(x_limits) == 0) {
      rlang::abort("Cannot extract discrete limits from the plot for alignment.")
    }

    valid_cols <- intersect(x_cols, x_limits)
    if (length(valid_cols) == 0) {
      rlang::abort("None of the table columns match the categorical X-axis levels in the plot.")
    }

    col_x_map <- stats::setNames(match(valid_cols, x_limits), valid_cols)

    x_breaks <- match(x_scale$get_breaks(), x_limits)
    x_breaks <- x_breaks[!is.na(x_breaks)]
  } else {
    # Continuous Axis Handling
    valid_cols <- x_cols[!is.na(x_num)]
    if (length(valid_cols) == 0) {
      rlang::abort("None of the table columns could be coerced to numeric coordinates.")
    }

    col_x_map <- stats::setNames(as.numeric(valid_cols), valid_cols)

    x_breaks <- x_params$x$breaks
    x_breaks <- x_breaks[!is.na(x_breaks)]
  }

  df_x <- df_x[, valid_cols, drop = FALSE]

  # 4. Pivot data to long format------------------------------------------------
  df_long <- df_x |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(valid_cols),
      names_to = "x_chr",
      values_to = "value"
    ) |>
    dplyr::mutate(x = col_x_map[.data$x_chr])

  df_long$value[is.na(df_long$value)] <- ""

  # 5. Build the aligned table--------------------------------------------------
  p_tbl <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = .data$x, y = .data$row_id, label = .data$value)
  ) +
    ggplot2::geom_text(size = text_size, vjust = 0.5, hjust = 0.5) +
    ggplot2::scale_y_reverse(
      breaks = seq_len(nrow(df_x)),
      labels = final_y_labels
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks) +
    ggplot2::coord_cartesian(
      xlim = x_range,
      ylim = c(nrow(df_x) + 0.5, 0.5),
      expand = FALSE,
      clip = "off"
    ) +
    ggplot2::labs(title = title, x = xlab) +
    ggplot2::theme_void() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = label_size,
        hjust = 1,
        margin = ggplot2::margin(r = 10),
        color = "black"
      ),
      plot.title = if (!is.null(title)) {
        ggplot2::element_text(
          size = label_size, vjust = 3, face = "bold", hjust = 0
        )
      } else {
        ggplot2::element_blank()
      },
      plot.margin = ggplot2::margin(
        t = if (show_xaxis || !is.null(title)) 10 else 0,
        r = 5,
        b = 5,
        l = 5
      )
    )

  # 6. Apply specific formatting based on show_xaxis----------------------------
  if (show_xaxis) {
    p_tbl <- p_tbl + ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = label_size,
        margin = ggplot2::margin(t = 2),
        color = "black"
      ),
      axis.ticks.x = ggplot2::element_line(color = "black"),
      axis.line.x = ggplot2::element_line(color = "black"),
      axis.title.x = if (!is.null(xlab)) {
        ggplot2::element_text(
          size = label_size, margin = ggplot2::margin(t = 5)
        )
      } else {
        ggplot2::element_blank()
      }
    )
  }

  # 7. Combine using cowplot----------------------------------------------------
  table_height <- 1 - rel_height_plot

  combined_plot <- cowplot::plot_grid(
    gg_plt,
    p_tbl,
    ncol = 1,
    axis = "rl",
    align = "v",
    rel_heights = c(rel_height_plot, table_height)
  )

  combined_plot
}

#' @title Convert Data Frame to Floating ggplot2 Table Graphic
#'
#' @description
#' Creates a standalone `ggplot2` table and overlays it onto a base plot
#' using `cowplot` at specified coordinates.
#'
#' @param df (`data.frame`)\cr
#'   The data frame to render.
#' @param gg_plt (`ggplot2`)\cr
#'   The main plot to overlay the table onto.
#' @param x (`numeric`)\cr
#'   X-coordinate for the box anchor position (0 to 1).
#' @param y (`numeric`)\cr
#'   Y-coordinate for the box anchor position (0 to 1).
#' @param w (`numeric`)\cr
#'   Width of the annotation box (0 to 1).
#' @param h (`numeric`)\cr
#'   Height of the annotation box (0 to 1).
#' @param colwidths (`numeric`)\cr
#'   Numeric vector of relative column widths.
#' @param font_size (`numeric`)\cr
#'   Base font size.
#' @param col_labels (`logical`)\cr
#'   Whether to display column labels (header).
#' @param col_lab_fontface (`character`)\cr
#'   String for the font face of column labels (e.g., "bold").
#' @param hline (`logical`)\cr
#'   Whether to draw a horizontal line below the column labels.
#' @param bg_fill (`character`)\cr
#'   Optional color string for the plot background.
#'
#' @examples
#' \dontrun{
#'
#' # 1. Create a base plot
#' p_base <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_classic()
#'
#' # 2. Create a mock summary table
#' mock_df <- data.frame(
#'   Statistic = c("N", "Median", "Mean"),
#'   Value = c("32", "19.2", "20.1")
#' )
#'
#' # 3. Float the table on the plot
#' df2gg_floating(
#'   df = mock_df,
#'   gg_plt = p_base,
#'   x = 0.75,
#'   y = 0.75,
#'   w = 0.35,
#'   h = 0.25,
#'   bg_fill = "white"
#' )
#' }
#'
#' @return A `cowplot` object representing the combined plot and table.
#'
#' @keywords internal
df2gg_floating <- function(df,
                           gg_plt,
                           x = 0.8,
                           y = 0.8,
                           w = 0.3,
                           h = 0.2,
                           colwidths = NULL,
                           font_size = 10,
                           col_labels = TRUE,
                           col_lab_fontface = "bold",
                           hline = TRUE,
                           bg_fill = NULL) {
  # Convert all values to character, replacing NAs with "NA"
  df_char <- as.data.frame(apply(df, 1:2, function(val) {
    if (is.na(val)) {
      "NA"
    } else {
      as.character(val)
    }
  }))

  if (col_labels) {
    df_char <- as.matrix(df_char)
    df_char <- rbind(colnames(df_char), df_char)
  }

  if (is.null(colwidths)) {
    colwidths <- apply(df_char, 2, function(val) max(nchar(val), na.rm = TRUE))
  }

  tot_width <- sum(colwidths)

  # Create the table graphic
  tbl_p <- ggplot2::ggplot(data = as.data.frame(df_char)) +
    ggplot2::theme_void() +
    ggplot2::scale_x_continuous(limits = c(0, tot_width)) +
    # FIX: Add 0.5 padding to top and bottom so text isn't on the exact edge
    ggplot2::scale_y_continuous(limits = c(0.5, nrow(df_char) + 0.5)) +
    # FIX: Force ggplot to stop hiding elements near the borders
    ggplot2::coord_cartesian(clip = "off")

  for (i in seq_len(ncol(df_char))) {
    line_pos <- c(
      if (i == 1) 0 else sum(colwidths[1:(i - 1)]),
      sum(colwidths[1:i])
    )

    # NEW: Use italics for the first column's data rows
    col_data_face <- if (i == 1) "italic" else "plain"

    font_faces <- if (col_labels) {
      c(col_lab_fontface, rep(col_data_face, nrow(df_char) - 1))
    } else {
      rep(col_data_face, nrow(df_char))
    }

    tbl_p <- tbl_p + ggplot2::annotate(
      "text",
      x = mean(line_pos),
      y = rev(seq_len(nrow(df_char))),
      label = df_char[, i],
      size = font_size / ggplot2::.pt,
      fontface = font_faces
    )
  }

  if (hline) {
    tbl_p <- tbl_p + ggplot2::annotate(
      "segment",
      # NEW: Start the line precisely AFTER the first column
      x = colwidths[1],
      xend = tot_width - 0.1 * utils::tail(colwidths, 1),
      y = nrow(df_char) - 0.5,
      yend = nrow(df_char) - 0.5
    )
  }

  if (!is.null(bg_fill)) {
    tbl_p <- tbl_p + ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg_fill)
    )
  }

  # Draw over the main plot using cowplot
  res <- cowplot::ggdraw(gg_plt) +
    cowplot::draw_plot(
      tbl_p,
      x = x,
      y = y,
      width = w,
      height = h,
      vjust = 0.5,
      hjust = 0.5
    )

  res
}

#' Extract Variable Names from ggplot2 Aesthetic Mappings
#'
#' @description
#' This internal helper function safely extracts variable names as character
#' strings from `ggplot2` aesthetic mappings (quosures). It is designed to
#' handle both simple symbols (e.g., `aes(x = TIME)`) and `.data` pronouns (e.g.,
#' `aes(x = .data[["TIME"]])`). Complex expressions or mathematical operations
#' (e.g., `aes(x = TIME / 24)`) will safely fall back to returning `NULL`.
#'
#' @param mapping_quo (`quosure`)\cr
#'   A quosure extracted from a `ggplot2` aesthetic mapping list (for example,
#'   `gg_plt$mapping$x`).
#'
#' @return A single character string containing the extracted variable name, or
#'   `NULL` if the variable cannot be safely extracted or is missing.
#'
#' @examples
#' \dontrun{
#'
#' # Create a plot with both standard and .data pronoun mappings
#' p <- ggplot(mtcars, aes(x = mpg, y = .data[["wt"]]))
#'
#' # Extracts "mpg"
#' gg_varname_extraction(p$mapping$x)
#'
#' # Extracts "wt"
#' gg_varname_extraction(p$mapping$y)
#' }
#'
#' @keywords internal
gg_varname_extraction <- function(mapping_quo) {
  if (is.null(mapping_quo) || !rlang::is_quosure(mapping_quo)) {
    return(NULL)
  }
  expr <- rlang::quo_get_expr(mapping_quo)
  if (rlang::is_symbol(expr)) {
    return(as.character(expr))
  }
  if (rlang::is_call(expr, "[[") && identical(expr[[2]], quote(.data))) {
    return(rlang::eval_bare(expr[[3]]))
  }
  return(NULL)
}

#' @title Generate Summary Statistic Function for ggplot2
#'
#' @description
#' A function factory that produces a summary closure compatible with
#' `ggplot2::stat_summary(fun.data = ...)`. It computes a central tendency
#' (mean or median) alongside a specified measure of dispersion.
#'
#' @param stat (`string`)\cr
#'   Primary statistic to calculate (`"mean"` or `"median"`).
#' @param variability (`string`)\cr
#'   Measure of variability (`"sd"`, `"se"`, `"ci"`, `"iqr"`, or `"none"`).
#' @param conf_level (`numeric`)\cr
#'   Confidence level for `"ci"` (default `0.95`).
#'
#' @return A `function` that takes a numeric vector and returns a `data.frame`
#'   with `y`, `ymin`, and `ymax`.
#' @examples
#' \dontrun{
#' # Generate a closure for Mean and 90% CI
#' my_summary_fun <- gg_get_summary_stats(
#'   stat = "mean",
#'   variability = "ci",
#'   conf_level = 0.90
#' )
#'
#' # Apply to a vector
#' my_summary_fun(rnorm(100, mean = 10, sd = 2))
#' }
#'
#' @keywords internal
gg_get_summary_stats <- function(
  stat = c("mean", "median"),
  variability = c("sd", "se", "ci", "iqr", "none"),
  conf_level = 0.95
) {
  stat <- match.arg(stat)
  variability <- match.arg(variability)

  # Return the mathematical closure that ggplot2 will evaluate during rendering
  function(val) {
    val <- stats::na.omit(val)
    n <- length(val)

    # Return NAs to prevent ggplot2 from drawing artifact geometries on empty subsets
    if (n == 0) {
      return(data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_))
    }

    if (stat == "median") {
      y_val <- stats::median(val)

      if (variability == "iqr") {
        ymin_val <- stats::quantile(val, 0.25)
        ymax_val <- stats::quantile(val, 0.75)
      } else {
        ymin_val <- y_val
        ymax_val <- y_val
      }
    } else if (stat == "mean") {
      y_val <- mean(val)
      se <- stats::sd(val) / sqrt(n)

      # Determine the error margin based on the requested dispersion metric
      err <- switch(variability,
        "sd" = stats::sd(val),
        "se" = se,
        "ci" = stats::qt((1 + conf_level) / 2, df = max(1, n - 1)) * se,
        "none" = 0,
        0
      )

      ymin_val <- y_val - err
      ymax_val <- y_val + err
    }

    data.frame(y = y_val, ymin = ymin_val, ymax = ymax_val)
  }
}
