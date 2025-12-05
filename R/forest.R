#' Create a Custom Forest Plot
#'
#' Generates a forest plot using \code{ggplot2} from a data frame containing
#' estimates, confidence intervals, and sample sizes. This function is designed
#' to be a component of a combined table/plot output (e.g., used by \code{g_forest}).
#'
#' @param data A data frame (tibble) containing the plot data. It must include
#'   columns: \code{group} (for y-axis labels), \code{estimate}, \code{ci_lower},
#'   \code{ci_upper}, and \code{n} (for point size).
#' @param xlim A numeric vector of length 2 specifying the limits of the x-axis
#'   (e.g., \code{c(0.1, 10)}).
#' @param logx A logical value indicating whether the x-axis should be log-transformed
#'   (i.e., using \code{scale_x_log10}). The default is \code{TRUE}, which is typical
#'   for effect measures like Odds Ratios or Hazard Ratios.
#' @param vline A numeric value specifying the x-intercept for the vertical
#'   reference line (line of no effect). The default is \code{1}.
#'
#' @return A \code{ggplot} object representing the forest plot.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Assuming 'forest_data' is structured correctly:
#' forest_data <- data.frame(
#'   group = c("A vs B", "C vs D"),
#'   estimate = c(0.5, 2.0),
#'   ci_lower = c(0.2, 1.5),
#'   ci_upper = c(0.9, 3.5),
#'   n = c(100, 250)
#' )
#'
#' create_forest_plot(forest_data)
#' create_forest_plot(forest_data, xlim = c(0.05, 50), vline = 1)
#' }
create_forest_plot <- function(data,
                               xlim = c(0.1, 10),
                               logx = TRUE,
                               vline = 1) {
  forest_header <- c("Comparison\nBetter", "Treatment\nBetter")
  # Calculate y positions (reverse order for top-to-bottom display)
  data <- data %>%
    mutate(y_pos = rev(dplyr::row_number()))

  # Apply log transformation if needed
  if (logx) {
    #   data <- data %>%
    #     mutate(
    #       estimate_log = log(estimate),
    #       ci_lower_log = log(ci_lower),
    #       ci_upper_log = log(ci_upper)
    #     )
    #   x_aesthetic_vars <- aes(x = estimate_log, xend = ci_lower_log, yend = ci_upper_log)
    #   x_scale <- scale_x_log10(limits = xlim, expand = c(0.01, 0))
    # } else {
    data <- data %>%
      mutate(
        estimate_log = estimate,
        ci_lower_log = ci_lower,
        ci_upper_log = ci_upper
      )
    x_aesthetic_vars <- aes(x = estimate, xend = ci_lower_log, yend = ci_upper_log)
    x_scale <- scale_x_log10(limits = xlim, expand = c(0.01, 0))
  }

  # Create plot
  ggplot(data) +
    # Background rectangle
    annotate("rect",
      xmin = xlim[1], xmax = xlim[2], ymin = 0.5, ymax = nrow(data) + 0.5,
      fill = "grey92", alpha = 0.5
    ) +
    # CI lines with arrows
    geom_errorbarh(aes(xmin = ci_lower_log, xmax = ci_upper_log, y = y_pos),
      height = 0.2, color = "black"
    ) +
    # Points
    geom_point(aes(x = estimate_log, y = y_pos, size = n),
      color = "#343cff", shape = 19
    ) +
    # Reference line
    geom_vline(xintercept = vline, linewidth = 1) +
    # Forest header text
    annotate("text",
      x = mean(c(xlim[1], vline)), y = nrow(data) + 1.25,
      label = forest_header[1], size = 3.5
    ) +
    annotate("text",
      x = mean(c(vline, xlim[2])), y = nrow(data) + 1.25,
      label = forest_header[2], size = 3.5
    ) +
    # Scales and theme
    x_scale +
    scale_y_continuous(
      limits = c(0, nrow(data) + 1.5), breaks = data$y_pos,
      labels = data$group, expand = c(0, 0)
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(0.1, 0.1, 0.05, 0, "npc")
    )
}

#' Extract Data for Forest Plot from gtsummary Table
#'
#' Converts the table body (\code{tbl$table_body}) of a \code{gtsummary} object
#' into a data frame suitable for plotting with \code{create_forest_plot}.
#' It selects and renames the necessary columns for the plot.
#'
#' @param tbl A \code{gtsummary} object (e.g., from \code{tbl_regression} or \code{tbl_uvregression}).
#'
#' @return A data frame (tibble) with the columns:
#'   \code{group} (from \code{term}), \code{estimate}, \code{ci_lower} (from \code{conf.low}),
#'   \code{ci_upper} (from \code{conf.high}), and \code{n} (from \code{N_obs}).
#' @keywords internal
extract_plot_data <- function(tbl) {
  ret <- tbl$table_body %>%
    select(
      group = term,
      estimate = estimate,
      ci_lower = conf.low,
      ci_upper = conf.high,
      n = N_obs
    )
  return(ret)
}

#' Create a Combined gtsummary Table and Forest Plot
#'
#' This is the main wrapper function that takes a \code{gtsummary} object,
#' converts it to a \code{ggplot} table, extracts the necessary data, creates
#' a forest plot, and combines the two plots side-by-side using \code{+}.
#' This likely relies on the \code{patchwork} package for plot combination.
#'
#' @param tbl A \code{gtsummary} object (e.g., from \code{tbl_regression}).
#'
#' @return A combined \code{ggplot} object (likely a \code{patchwork} object)
#'   showing the table on the left and the forest plot on the right.
#'
#' @seealso \code{\link{gtsummary2gg}}, \code{\link{extract_plot_data}}, \code{\link{create_forest_plot}}
#'
#' @export
#' @examples
#' tbl <-
#'   trial %>%
#'   tbl_subgroups(
#'     subgroups = c("grade", "stage"),
#'     ~ glm(response ~ trt, data = .x) %>%
#'       gtsummary::tbl_regression(
#'         show_single_row = trt,
#'         exponentiate = TRUE
#'       )
#'   )
#' g_forest(tbl)
g_forest <- function(tbl) {
  table_plot <- gtsummary2gg(tbl)
  forest_data <- extract_plot_data(tbl)
  forest_plot <- create_forest_plot(forest_data)
  table_plot + forest_plot
}

#' Convert gtsummary Object into a ggplot Table
#'
#' Renders the formatted content of a \code{gtsummary} object as a
#' \code{ggplot} object. This allows the text-based table to be combined
#' seamlessly with a plot (like a forest plot) using \code{patchwork} or \code{cowplot}.
#'
#' NOTE: The current implementation of this function includes commented-out code
#' for drawing the header labels and separating line. As written, it only draws
#' the table body content.
#'
#' @param tbl A \code{gtsummary} object.
#' @param fontsize The size of the text used in the table (in points). Default is 12.
#' @param header_line_color The color of the line to be drawn under the header (if uncommented).
#'
#' @return A \code{ggplot} object representing the formatted table.
#' @keywords internal
gtsummary2gg <- function(tbl, fontsize = 12, header_line_color = "gray30") {
  .pt <- 2.834646

  # 1. Convert gtsummary object to a tibble with formatting
  tbl_df_raw <- as_tibble(tbl)
  print(tbl_df_raw)
  # 2. Extract column names (final header labels) and body strings
  # The table body contains the cell content, the column headers are in the column names.
  body_strings <- tbl_df_raw %>%
    select(-starts_with("label")) %>%
    mutate(across(everything(), as.character))

  # Extract final column labels for the header
  header_labels <- names(body_strings)
  print("here2")

  # Extract the row labels and make them the first column for plotting
  # label_strings <- tbl_df_raw %>%
  #   select(starts_with("label")) %>%
  #   tidyr::unite("label_col", everything(), sep = " ") %>%
  #   pull(label_col)
  #
  label_strings <- tbl_df_raw[, 1]
  print("here1")

  # Combine label column and body
  plot_df <- cbind(data.frame(label_col = label_strings), body_strings)

  # 3. Determine Layout and Dimensions
  n_rows <- nrow(plot_df)
  n_cols <- ncol(plot_df)

  # Rough approximation of column widths based on max character length
  col_widths <- apply(plot_df, 2, function(x) max(nchar(x), na.rm = TRUE)) + 2

  # Set the first column (labels) to a wider width for better spacing
  col_widths[1] <- max(col_widths[1] + 10, 30)

  # Calculate cumulative positions for text placement
  # The x-position will be the midpoint of each column's width
  x_pos <- cumsum(col_widths) - col_widths / 2

  # Calculate the x-position for the column boundaries (for vertical lines, if desired)
  x_boundaries <- cumsum(col_widths)
  x_boundaries <- c(0, x_boundaries)

  # The y-positions (reversed, so row 1 is at the top of the plot)
  y_pos <- rev(seq_len(n_rows + 1)) # +1 to account for the header row

  # Total width for the plot canvas
  tot_width <- max(x_boundaries)
  print("here")
  # 4. Initialize ggplot
  res <- ggplot() +
    theme_void() +
    # Set the limits of the plot area
    scale_x_continuous(limits = c(0, tot_width)) +
    scale_y_continuous(limits = c(0, n_rows + 1.5))

  # 5. Add **Header Labels**
  # res <- res +
  #   annotate(
  #     "text",
  #     x = x_pos,
  #     y = y_pos[1], # Top row
  #     label = header_labels,
  #     fontface = "bold",
  #     size = fontsize / .pt
  #   )
  # +
  #   # Add a horizontal line under the header
  #   annotate(
  #     "segment",
  #     x = 0, xend = tot_width,
  #     y = y_pos[1] - 0.5, yend = y_pos[1] - 0.5,
  #     color = header_line_color
  #   )
  print(res)
  # 6. Add **Table Body Content** (Row by Row, Column by Column)
  for (i in seq_len(n_cols)) {
    # Assuming the first column (label_col) is left-aligned and the rest are right-aligned
    hjust_val <- if (i == 1) 0 else 1

    # Calculate x position: use the start of the column for left-aligned text,
    # and the end of the column for right-aligned text.
    x_val <- if (i == 1) x_boundaries[i] else x_boundaries[i + 1]

    res <- res +
      annotate(
        "text",
        # We need to skip the first row since that is where the header is placed
        x = x_val,
        y = y_pos[-1],
        label = plot_df[, i],
        hjust = hjust_val,
        size = fontsize / .pt
      )
  }

  # Return the final ggplot object
  return(res)
}
