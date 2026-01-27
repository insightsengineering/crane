#' Convert Data Frame to ggplot2 Table Graphic
#'
#' @description Creates a `ggplot2` object that renders a data frame as a table graphic.
#'
#' @param df The data frame to render.
#' @param colwidths Numeric vector of relative column widths. If \code{NULL}, determined by max character length.
#' @param font_size Numeric base font size.
#' @param col_labels Logical, whether to display column labels (header).
#' @param col_lab_fontface Character string for the font face of column labels (e.g., "bold").
#' @param hline Logical, whether to draw a horizontal line below the column labels.
#' @param bg_fill Optional color string for the plot background.
#' @param add_proper_xaxis Logical, whether to add a proper x-axis with column values.
#'
#' @return A \code{ggplot2} object representing the table.
#'
#' @keywords internal
df2gg <- function(df, colwidths = NULL, font_size = 10, col_labels = TRUE,
                  col_lab_fontface = "bold", hline = TRUE, bg_fill = NULL, add_proper_xaxis = FALSE) {
  # Convert all values to character, replacing NAs with "NA"
  df <- as.data.frame(apply(df, 1:2, function(x) {
    if (is.na(x)) {
      "NA"
    } else {
      as.character(x)
    }
  }))

  # Add column labels as first row if specified
  if (col_labels) {
    df <- as.matrix(df)
    df <- rbind(colnames(df), df)
  }

  # Create ggplot2 object with x-axis specified in df
  if (add_proper_xaxis) {
    # Determine column widths if not provided
    if (is.null(colwidths)) {
      tot_width <- max(colnames(df) |> as.numeric(), na.rm = TRUE)
      colwidths <- rep(floor(tot_width / ncol(df)), ncol(df))
    } else {
      tot_width <- sum(colwidths)
    }

    df_long <- df |>
      as.data.frame() |>
      # 1. Ensure the row names ('A', 'B', 'C') are a column named 'row_name'
      dplyr::mutate(row_name = row.names(df)) |>
      # 2. Pivot the remaining columns (starting from '0' to the end) longer
      tidyr::pivot_longer(
        cols = -"row_name", # Select all columns EXCEPT 'row_name'
        names_to = "col_name", # Name the new column containing the old column headers
        values_to = "value" # Name the new column containing the data values
      ) |>
      dplyr::arrange("row_name", "col_name") |>
      dplyr::mutate(
        col_name = as.numeric(.data$col_name),
        row_name = factor(.data$row_name, levels = row.names(df))
      )
    res <- ggplot2::ggplot(data = df_long) +
      ggplot2::theme_void() +
      ggplot2::annotate("text",
        x = df_long$col_name, y = rev(df_long$row_name), # why rev?
        label = df_long$value, size = font_size / .pt
      )

    # Create ggplot2 object with a specific x-axis based on column widths
  } else {
    # Determine column widths if not provided
    if (is.null(colwidths)) {
      colwidths <- apply(df, 2, function(x) max(nchar(x), na.rm = TRUE))
    }
    tot_width <- sum(colwidths)

    res <- ggplot2::ggplot(data = df) +
      ggplot2::theme_void() +
      ggplot2::scale_x_continuous(limits = c(0, tot_width)) +
      ggplot2::scale_y_continuous(limits = c(1, nrow(df)))


    for (i in seq_len(ncol(df))) {
      line_pos <- c(
        if (i == 1) {
          0
        } else {
          sum(colwidths[1:(i - 1)])
        },
        sum(colwidths[1:i])
      )
      res <- res + ggplot2::annotate("text",
        x = mean(line_pos), y = rev(seq_len(nrow(df))),
        label = df[, i], size = font_size / .pt, fontface = if (col_labels) {
          c(col_lab_fontface, rep("plain", nrow(df) - 1))
        } else {
          rep("plain", nrow(df))
        }
      )
    }
  }

  # Add horizontal line if specified
  if (hline) {
    res <- res + ggplot2::annotate(
      "segment",
      x = 0 + 0.2 * colwidths[2], xend = tot_width - 0.1 * tail(colwidths, 1),
      y = nrow(df) - 0.5, yend = nrow(df) - 0.5
    )
  }

  # Set background fill if specified
  if (!is.null(bg_fill)) {
    res <- res + ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg_fill))
  }

  res
}

#' Calculate X-axis Ticks
#'
#' @description Determines the positions for x-axis ticks based on the data and user input.
#'
#' @param data A data frame containing a `"time""` column.
#' @param xticks A numeric vector of specific tick positions, a single number for the interval, or
#'   `NULL` for auto-calculation.
#' @param max_time Optional numeric value specifying the maximum time to consider for tick range.
#'
#' @return A numeric vector of x-axis tick positions.
#'
#' @keywords internal
h_xticks <- function(data, xticks = NULL, max_time = NULL) {
  if (is.null(xticks)) {
    if (is.null(max_time)) {
      labeling::extended(range(data$time)[1], range(data$time)[2], m = 5)
    } else {
      labeling::extended(range(data$time)[1], max(range(data$time)[2], max_time), m = 5)
    }
  } else if (is.numeric(xticks) && length(xticks) == 1 && !is.na(xticks)) {
    if (is.null(max_time)) {
      seq(0, max(data$time), xticks)
    } else {
      seq(0, max(data$time, max_time), xticks)
    }
  } else if (is.numeric(xticks)) {
    xticks
  } else {
    stop(
      paste(
        "xticks should be either `NULL`",
        "or a single number (interval between x ticks)",
        "or a numeric vector (position of ticks on the x axis)"
      )
    )
  }
}

#' @title Median Survival Summary Table
#'
#' @description Extracts and formats the median survival time and its confidence interval
#' from a fitted Kaplan-Meier object.
#'
#' @inheritParams gg_km
#'
#' @return A data frame with columns "N", "Median", and the confidence interval label.
#'
#' @keywords internal
h_tbl_median_surv <- function(fit_km, strata_levels = "All") {
  y <- if (is.null(fit_km$strata)) {
    as.data.frame(t(summary(fit_km)$table), row.names = strata_levels)
  } else {
    tbl <- summary(fit_km)$table
    rownames_lst <- strsplit(sub("=", "equals", rownames(tbl)), "equals")
    rownames(tbl) <- matrix(unlist(rownames_lst), ncol = 2, byrow = TRUE)[, 2]
    as.data.frame(tbl)
  }
  conf.int <- summary(fit_km)$conf.int # nolint
  y$records <- round(y$records)
  y$median <- signif(y$median, 4)
  y$`CI` <- paste0(
    "(", signif(y[[paste0(conf.int, "LCL")]], 4), ", ", signif(y[[paste0(conf.int, "UCL")]], 4), ")"
  )
  stats::setNames(
    y[c("records", "median", "CI")],
    c("N", "Median", paste0(conf.int * 100, "% CI"))
  )
}
