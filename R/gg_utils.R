#' Create and Stack an X-Axis Aligned Table
#'
#' @description Generates a text table from a dataframe, perfectly aligns its
#'   X-axis to match a provided ggplot, and seamlessly stacks them vertically.
#'   Tailored specifically for Kaplan-Meier ("KM") or Pharmacokinetic ("PK")
#'   table structures.
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
#' library(ggplot2)
#'
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
#'}
#' @return A combined `patchwork` object.
#' @keywords internal
df2gg_aligned <- function(df,
                          gg_plt,
                          type = c("KM", "PK"),
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
    # KM Tables use rownames for their Y-axis labels
    raw_y_labels <- rownames(df)
    df_x <- df
  } else if (type == "PK") {
    # PK Tables use the very first column for their Y-axis labels
    if (ncol(df) < 2) {
      rlang::abort("PK tables must have at least 2 columns (labels + data).")
    }
    y_col_name <- names(df)[1]
    raw_y_labels <- as.character(df[[y_col_name]])
    df_x <- df[, names(df) != y_col_name, drop = FALSE]
  }

  final_y_labels <- if (!is.null(y_labels)) y_labels else raw_y_labels

  # 3. Ensure remaining columns are numeric timepoints--------------------------
  x_cols <- names(df_x)
  x_num <- suppressWarnings(as.numeric(x_cols))

  if (any(is.na(x_num))) {
    rlang::warn("Some columns cannot be coerced to numeric. Omitting them.")
    x_cols <- x_cols[!is.na(x_num)]
    df_x <- df_x[, x_cols, drop = FALSE]
  }

  # 4. Pivot data to long format------------------------------------------------
  df_long <- df_x |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(x_cols),
      names_to = "x_chr",
      values_to = "value"
    ) |>
    dplyr::mutate(x = as.numeric(.data$x_chr))

  df_long$value[is.na(df_long$value)] <- ""

  # 5. Extract Limits & Breaks safely from the built plot-----------------------
  plot_build <- ggplot2::ggplot_build(gg_plt)
  x_range <- plot_build$layout$panel_params[[1]]$x.range
  x_breaks <- plot_build$layout$panel_params[[1]]$x$breaks
  x_breaks <- x_breaks[!is.na(x_breaks)]

  # 6. Build the aligned table--------------------------------------------------
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

  # 7. Apply specific formatting based on show_xaxis----------------------------
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

  # 8. Combine using cowplot----------------------------------------------------
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
#' library(ggplot2)
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
#'}
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

    font_faces <- if (col_labels) {
      c(col_lab_fontface, rep("plain", nrow(df_char) - 1))
    } else {
      rep("plain", nrow(df_char))
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
      x = 0 + 0.2 * colwidths[2],
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
