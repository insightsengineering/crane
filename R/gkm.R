#' @title Convert Data Frame to ggplot2 Table Graphic
#'
#' @description Creates a \code{ggplot2} object that renders a data frame as a table graphic.
#'
#' @param df The data frame to render.
#' @param colwidths Numeric vector of relative column widths. If \code{NULL}, determined by max character length.
#' @param font_size Numeric base font size.
#' @param col_labels Logical, whether to display column labels (header).
#' @param col_lab_fontface Character string for the font face of column labels (e.g., "bold").
#' @param hline Logical, whether to draw a horizontal line below the column labels.
#' @param bg_fill Optional color string for the plot background.
#' @keywords internal
#' @return A \code{ggplot2} object representing the table.
df2gg <- function(df, colwidths = NULL, font_size = 10, col_labels = TRUE,
                  col_lab_fontface = "bold", hline = TRUE, bg_fill = NULL) {
  # ... (function body remains the same)
  df <- as.data.frame(apply(df, 1:2, function(x) {
    if (is.na(x)) {
      "NA"
    } else {
      as.character(x)
    }
  }))
  if (col_labels) {
    df <- as.matrix(df)
    df <- rbind(colnames(df), df)
  }
  if (is.null(colwidths)) {
    colwidths <- apply(df, 2, function(x) max(nchar(x), na.rm = TRUE))
  }
  tot_width <- sum(colwidths)
  res <- ggplot2::ggplot(data = df) +
    ggplot2::theme_void() +
    ggplot2::scale_x_continuous(limits = c(
      0,
      tot_width
    )) +
    ggplot2::scale_y_continuous(limits = c(1, nrow(df)))
  if (!is.null(bg_fill)) {
    res <- res + ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg_fill))
  }
  if (hline) {
    res <- res + ggplot2::annotate("segment",
      x = 0 + 0.2 * colwidths[2],
      xend = tot_width - 0.1 * tail(colwidths, 1), y = nrow(df) -
        0.5, yend = nrow(df) - 0.5
    )
  }
  for (i in seq_len(ncol(df))) {
    line_pos <- c(if (i == 1) {
      0
    } else {
      sum(colwidths[1:(i -
        1)])
    }, sum(colwidths[1:i]))
    res <- res + ggplot2::annotate("text",
      x = mean(line_pos), y = rev(seq_len(nrow(df))),
      label = df[, i], size = font_size / .pt, fontface = if (col_labels) {
        c(col_lab_fontface, rep("plain", nrow(df) - 1))
      } else {
        rep("plain", nrow(df))
      }
    )
  }
  res
}

#' @title Calculate X-axis Ticks
#'
#' @description Determines the positions for x-axis ticks based on the data and user input.
#'
#' @param data A data frame containing a \code{time} column.
#' @param xticks A numeric vector of specific tick positions, a single number for the interval, or \code{NULL} for auto-calculation.
#' @param max_time Optional numeric value specifying the maximum time to consider for tick range.
#' @keywords internal
#' @return A numeric vector of x-axis tick positions.
h_xticks <- function(data, xticks = NULL, max_time = NULL) {
  # ... (function body remains the same)
  if (is.null(xticks)) {
    if (is.null(max_time)) {
      labeling::extended(range(data$time)[1], range(data$time)[2], m = 5)
    } else {
      labeling::extended(range(data$time)[1], max(range(data$time)[2], max_time), m = 5)
    }
  } else if (checkmate::test_number(xticks)) {
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
#' @param fit_km A fitted Kaplan-Meier object of class \code{survfit}.
#' @param armval Character string to use as the row name if \code{fit_km} has no strata (e.g., "All").
#' @keywords internal
#' @return A data frame with columns "N", "Median", and the confidence interval label.
h_tbl_median_surv <- function(fit_km, armval = "All") {
  # ... (function body remains the same)
  y <- if (is.null(fit_km$strata)) {
    as.data.frame(t(summary(fit_km)$table), row.names = armval)
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
    c("N", "Median", f_conf_level(conf.int))
  )
}

#' Perform Pairwise Cox Proportional Hazards Regression
#'
#' This function performs a pairwise comparison of treatment arms using the **Cox proportional hazards model** and calculates the corresponding **log-rank p-value**. Each comparison is made between a specified reference group and all other comparison groups in the dataset.
#'
#' @param model_formula A \code{\link[stats]{formula}} object specifying the survival model, typically in the form \code{Surv(time, status) ~ arm + covariates}.
#' @param data A \code{\link[base]{data.frame}} containing the survival data, including time, status, and the arm variable.
#' @param arm A character string specifying the name of the column in \code{data} that contains the grouping/treatment arm variable (must be a factor-like variable).
#' @param ref_group A character string specifying the level of the \code{arm} variable to be used as the **reference group** for all pairwise comparisons. If \code{NULL} (the default), the **first unique level** of the \code{arm} column is used as the reference group.
#'
#' @return A \code{\link[base]{data.frame}} with the results of the pairwise comparisons. The columns include:
#' \itemize{
#'   \item \code{arm}: The comparison arm being tested against the reference group.
#'   \item \code{hr}: The Hazard Ratio (HR) for the comparison arm vs. the reference arm, formatted to two decimal places.
#'   \item \code{ci}: The 95\% confidence interval for the HR, presented as a string in the format "(lower, upper)", with values formatted to two decimal places.
#'   \item \code{pval}: The log-rank p-value for the comparison.
#' }
#'
#' @details The function iterates through each unique arm (excluding the reference group), filters the data to include only the current comparison arm and the reference arm, and then fits a Cox model (\code{\link[survival]{coxph}}) and performs a log-rank test (\code{\link[survival]{survdiff}}). The Hazard Ratio and its 95\% confidence interval are extracted from the Cox model summary, and the p-value is calculated from the log-rank test.
#' @export
#' @examples
#' # Example data setup (assuming 'time' is event time, 'status' is event indicator (1=event),
#' # and 'arm' is the treatment group)
#' library(survival)
#' use_lung <- lung
#' use_lung$arm <- factor(sample(c("A", "B", "C"), nrow(use_lung), replace = TRUE))
#' use_lung$status <- use_lung$status - 1 # Convert status to 0/1
#' use_lung <- na.omit(use_lung)
#'
#' formula <- Surv(time, status) ~ arm
#' results_tbl <- get_cox_pairwise_tbl(
#'   model_formula = formula,
#'   data = use_lung,
#'   arm = "arm",
#'   ref_group = "A"
#' )
#' print(results_tbl)
get_cox_pairwise_tbl <- function(model_formula, data, arm, ref_group = NULL) {
  msg <- paste0(rlang::ensym(model_formula), " is not a formula")
  assertthat::assert_that(rlang::is_formula(model_formula), msg = msg)
  msg <- paste0(rlang::ensym(data), "[['", rlang::ensym(arm), "']] is not a factor")
  assertthat::assert_that(is.factor(data[[arm]]), msg = msg)
  ref_group <- if (!is.null(ref_group)) {
    ref_group
  } else {
    levels(data[[arm]])[1]
  }
  comp_group <- setdiff(levels(data[[arm]]), ref_group)

  ret <- c()
  for (current_arm in comp_group) {
    subset_arm <- c(ref_group, current_arm)
    assertthat::assert_that(length(subset_arm) == 2, msg = "Make sure 2 arms")
    comp_df <- data[as.character(data[[arm]]) %in% subset_arm, ]
    suppressWarnings(
      coxph_ans <- coxph(formula = model_formula, data = comp_df) %>% summary()
    )
    orginal_survdiff <- survdiff(formula = model_formula, data = comp_df)
    log_rank_pvalue <- 1 - stats::pchisq(orginal_survdiff$chisq, length(orginal_survdiff$n) -
      1)
    current_row <- data.frame(
      hr = sprintf("%.2f", coxph_ans$conf.int[1, 1]),
      ci = paste0(
        "(",
        sprintf("%.2f", coxph_ans$conf.int[1, 3]),
        ", ",
        sprintf("%.2f", coxph_ans$conf.int[1, 4]),
        ")"
      ),
      pval = log_rank_pvalue
    )
    rownames(current_row) <- current_arm
    ret <- rbind(ret, current_row)
  }

  return(ret)
}


#' @title Generate a Kaplan-Meier Plot
#'
#' @description The function \code{h_data_plot} takes a fitted \code{survfit} object and processes it into a data frame
#' suitable for plotting a Kaplan-Meier curve with \code{ggplot2}, including extending
#' the curve to time zero.
#'
#' @param fit_km A fitted Kaplan-Meier object of class \code{survfit}.
#' @param armval Character string for the strata level if \code{fit_km} has no strata (e.g., "All").
#' @param max_time Numeric, the maximum time point to include in the data, or \code{NULL} for no limit.
#'
#' @return The function \code{h_data_plot} returns a data frame containing the survival curve steps, confidence intervals, and censoring info.
#' @rdname gkm
#' @examples
#' # Example data setup (assuming 'time' is event time, 'status' is event indicator (1=event),
#' # and 'arm' is the treatment group)
#' library(survival)
#' use_lung <- lung
#' use_lung$arm <- factor(sample(c("A", "B", "C"), nrow(use_lung), replace = TRUE))
#' use_lung$status <- use_lung$status - 1 # Convert status to 0/1
#' use_lung <- na.omit(use_lung)
#' formula <- Surv(time, status) ~ arm
#' fit_kmg01 <- survfit(formula, use_lung)
#' surv_plot_data <- h_data_plot(fit_kmg01)
#' head(surv_plot_data)
#'
#' @export
h_data_plot <- function(fit_km,
                        armval = "All",
                        max_time = NULL) {
  # ... (function body remains the same)
  y <- broom::tidy(fit_km)

  if (!is.null(fit_km$strata)) {
    fit_km_var_level <- strsplit(sub("=", "equals", names(fit_km$strata)), "equals")
    strata_levels <- vapply(fit_km_var_level, FUN = "[", FUN.VALUE = "a", i = 2)
    strata_var_level <- strsplit(sub("=", "equals", y$strata), "equals")
    y$strata <- factor(
      vapply(strata_var_level, FUN = "[", FUN.VALUE = "a", i = 2),
      levels = strata_levels
    )
  } else {
    y$strata <- armval
  }

  y_by_strata <- split(y, y$strata)
  y_by_strata_extended <- lapply(
    y_by_strata,
    FUN = function(tbl) {
      first_row <- tbl[1L, ]
      first_row$time <- 0
      first_row$n.risk <- sum(first_row[, c("n.risk", "n.event", "n.censor")])
      first_row$n.event <- first_row$n.censor <- 0
      first_row$estimate <- first_row$conf.high <- first_row$conf.low <- 1
      first_row$std.error <- 0
      rbind(
        first_row,
        tbl
      )
    }
  )
  y <- do.call(rbind, y_by_strata_extended)

  y$censor <- ifelse(y$n.censor > 0, y$estimate, NA)
  if (!is.null(max_time)) {
    y <- y[y$time <= max(max_time), ]
  }
  y
}


#' @description The function \code{g_km} creates a comprehensive \code{ggplot2} object for a Kaplan-Meier
#' survival curve, with support for various customizations like censoring marks, CIs, and axis control.
#'
#' @param surv_plot_data A data frame containing the pre-processed survival data, ready for plotting.
#'   This data should be equivalent to the output of \code{h_data_plot}.
#' @param col A character vector of colors for the survival curves. Length should match number of arms.
#' @param lty A vector of line types for the survival curves, or \code{NULL} for default.
#' @param lwd Numeric value specifying line width for the survival curves.
#' @param censor_show Logical, whether to display censoring marks on the plot.
#' @param pch Plotting character for censoring marks.
#' @param size Size of the censoring marks.
#' @param max_time Numeric, the maximum time point to display on the x-axis.
#' @param xticks Numeric vector of x-axis tick positions, or a single number for the interval, or \code{NULL} for auto.
#' @param xlab Character string for the x-axis label.
#' @param yval Character string, either \code{"Survival"} or \code{"Failure"} to plot Survival or Failure probability.
#' @param ylab Character string for the y-axis label.
#' @param ylim Numeric vector of length 2 for y-axis limits.
#' @param title Character string for the plot title.
#' @param footnotes Character string for plot footnotes/caption.
#' @param font_size Numeric, base font size for the plot theme.
#' @param ci_ribbon Logical, whether to display confidence intervals as a ribbon (area).
#' @param legend_pos Numeric vector of length 2 for legend position (x, y) relative to the plot area (0 to 1), or \code{NULL} for auto-placement.
#' @param ggtheme An optional \code{ggplot2} theme to apply.
#'
#' @return The function \code{g_km} returns a \code{ggplot2} object of the KM plot.
#' @rdname gkm
#' @examples
#' # Example of making the KM plot
#' plt_kmg01 <- g_km(surv_plot_data,
#'   xlab = "Time (Days)"
#' )
#'
#' @export
g_km <- function(
  surv_plot_data,
  col = NULL,
  lty = NULL,
  lwd = 0.5,
  censor_show = TRUE,
  pch = 3,
  size = 2,
  max_time = NULL,
  xticks = NULL,
  xlab = "Days",
  yval = c("Survival", "Failure"),
  ylab = paste(yval, "Probability"),
  ylim = NULL,
  title = NULL,
  footnotes = NULL,
  font_size = 10,
  ci_ribbon = FALSE,
  legend_pos = NULL,
  ggtheme = NULL
) {
  checkmate::assert_data_frame(surv_plot_data, min.cols = 7, min.rows = 1)
  data <- surv_plot_data

  armval <- levels(data$strata)
  checkmate::assert_vector(col, len = length(armval), null.ok = TRUE)

  yval <- match.arg(yval)

  xticks <- h_xticks(data = data, xticks = xticks, max_time = max_time)

  if (yval == "Failure") {
    data[c("estimate", "conf.low", "conf.high", "censor")] <- list(
      1 - data$estimate, 1 - data$conf.low, 1 - data$conf.high, 1 - data$censor
    )
  }

  if (is.null(ylim)) {
    if (!is.null(max_time)) {
      y_lwr <- min(data[data$time < max_time, ][["estimate"]])
      y_upr <- max(data[data$time < max_time, ][["estimate"]])
    } else {
      y_lwr <- min(data[["estimate"]])
      y_upr <- max(data[["estimate"]])
    }
    ylim <- c(y_lwr, y_upr)
  }

  gg_plt <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data[["time"]], y = .data[["estimate"]], ymin = .data[["conf.low"]],
      ymax = .data[["conf.high"]], color = .data[["strata"]], fill = .data[["strata"]]
    )
  ) +
    ggplot2::theme_bw(base_size = font_size) +
    ggplot2::scale_y_continuous(limits = ylim, expand = c(0.025, 0)) +
    ggplot2::labs(title = title, x = xlab, y = ylab, caption = footnotes) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = font_size), axis.title = ggplot2::element_text(size = font_size),
      legend.title = ggplot2::element_blank(), legend.text = ggplot2::element_text(size = font_size),
      legend.box.background = ggplot2::element_rect(fill = "white", linewidth = 0.5),
      legend.background = ggplot2::element_blank(), legend.position = "inside",
      legend.spacing.y = ggplot2::unit(-0.02, "npc"), panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (!is.null(max_time) && !is.null(xticks)) {
    gg_plt <- gg_plt + ggplot2::scale_x_continuous(
      breaks = xticks, limits = c(min(0, xticks), max(c(xticks, max_time))), expand = c(0.025, 0)
    )
  } else if (!is.null(xticks)) {
    if (max(data$time) <= max(xticks)) {
      gg_plt <- gg_plt + ggplot2::scale_x_continuous(
        breaks = xticks, limits = c(min(0, min(xticks)), max(xticks)), expand = c(0.025, 0)
      )
    } else {
      gg_plt <- gg_plt + ggplot2::scale_x_continuous(breaks = xticks, expand = c(0.025, 0))
    }
  } else if (!is.null(max_time)) {
    gg_plt <- gg_plt + ggplot2::scale_x_continuous(limits = c(0, max_time), expand = c(0.025, 0))
  }

  if (!is.null(legend_pos)) {
    gg_plt <- gg_plt + ggplot2::theme(legend.position.inside = legend_pos)
  } else {
    max_time2 <- sort(
      data$time,
      partial = nrow(data) - length(armval) - 1
    )[nrow(data) - length(armval) - 1]

    y_rng <- ylim[2] - ylim[1]

    if (yval == "Survival" && all(data$estimate[data$time == max_time2] > ylim[1] + 0.09 * y_rng) &&
      all(data$estimate[data$time == max_time2] < ylim[1] + 0.5 * y_rng)) { # nolint
      gg_plt <- gg_plt +
        ggplot2::theme(
          legend.position.inside = c(1, 0.5),
          legend.justification = c(1.1, 0.6)
        )
    } else {
      gg_plt <- gg_plt +
        ggplot2::theme(
          legend.position.inside = c(1, 0),
          legend.justification = c(1.1, -0.4)
        )
    }
  }

  gg_plt <- if (is.null(lty)) {
    gg_plt + ggplot2::geom_step(linewidth = lwd, na.rm = TRUE)
  } else if (length(lty) == 1) {
    gg_plt + ggplot2::geom_step(linewidth = lwd, lty = lty, na.rm = TRUE)
  } else {
    gg_plt +
      ggplot2::geom_step(ggplot2::aes(lty = .data[["strata"]]), linewidth = lwd, na.rm = TRUE) +
      ggplot2::scale_linetype_manual(values = lty)
  }

  if (censor_show) {
    gg_plt <- gg_plt + ggplot2::geom_point(
      data = data[data$n.censor != 0, ],
      ggplot2::aes(x = .data[["time"]], y = .data[["censor"]], shape = "Censored"),
      size = size,
      na.rm = TRUE
    ) +
      ggplot2::scale_shape_manual(name = NULL, values = pch) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = NA)))
  }

  if (ci_ribbon) gg_plt <- gg_plt + ggplot2::geom_ribbon(alpha = 0.3, lty = 0, na.rm = TRUE)

  if (!is.null(col)) {
    gg_plt <- gg_plt +
      ggplot2::scale_color_manual(values = col) +
      ggplot2::scale_fill_manual(values = col)
  }
  if (!is.null(ggtheme)) gg_plt <- gg_plt + ggtheme

  gg_plt
}

#' @description The \code{annot_surv_med} function adds a median survival time summary table as an annotation box on a
#' Kaplan-Meier plot using \code{cowplot}.
#'
#' @param gg_plt A \code{ggplot2} or \code{cowplot} object of the Kaplan-Meier plot.
#' @param fit_km A fitted Kaplan-Meier object of class \code{survfit}, used to generate the table data.
#' @param control_annot_surv_med A list of control parameters for the annotation box.
#' @param font_size Numeric, base font size for the annotation table.
#'
#' @return The \code{annot_surv_med} function returns a \code{cowplot} object with the median survival table annotation added.
#' @rdname gkm
#' @examples
#' # Annotate Kaplan-Meier Plot with Median Survival Table
#' annot_surv_med(plt_kmg01, fit_kmg01)
#'
#' @export
annot_surv_med <- function(gg_plt, fit_km, control_annot_surv_med = control_surv_med_annot(), font_size = 10) {
  # Determine armval for h_tbl_median_surv, assuming it's available in the calling environment or logic should be updated
  # For now, keeping as is, but this typically requires armval or inferring it from fit_km
  armval <- if (is.null(fit_km$strata)) "All" else levels(fit_km$strata) # Placeholder for armval

  surv_med_tbl <- h_tbl_median_surv(fit_km = fit_km, armval = armval)
  bg_fill <- if (isTRUE(control_annot_surv_med[["fill"]])) "#00000020" else control_annot_surv_med[["fill"]]

  gg_surv_med <- df2gg(surv_med_tbl, font_size = font_size, colwidths = c(1, 1, 2), bg_fill = bg_fill) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = font_size, face = "italic", hjust = 1),
      plot.margin = ggplot2::margin(0, 2, 0, 5)
    ) +
    ggplot2::coord_cartesian(clip = "off", ylim = c(0.5, nrow(surv_med_tbl) + 1.5))
  gg_surv_med <- suppressMessages(
    gg_surv_med +
      ggplot2::scale_x_continuous(expand = c(0.025, 0)) +
      ggplot2::scale_y_continuous(labels = rev(rownames(surv_med_tbl)), breaks = seq_len(nrow(surv_med_tbl)))
  )

  gg_plt <- cowplot::ggdraw(gg_plt) +
    cowplot::draw_plot(
      gg_surv_med, control_annot_surv_med[["x"]], control_annot_surv_med[["y"]],
      width = control_annot_surv_med[["w"]], height = control_annot_surv_med[["h"]],
      vjust = 0.5, hjust = 0.5
    )
  gg_plt
}

#' @description The function \code{annot_cox_ph} adds a Cox Proportional Hazards summary table created by the function \code{\link{get_cox_pairwise_tbl}} as an annotation box on a
#' Kaplan-Meier plot using \code{cowplot}.
#'
#' @param gg_plt A \code{ggplot2} or \code{cowplot} object of the Kaplan-Meier plot.
#' @param coxph_tbl A data frame containing pre-calculated Cox-PH results.
#' @param control_annot_coxph A list of control parameters for the annotation box.
#' @param font_size Numeric, base font size for the annotation table.
#'
#' @return The function \code{annot_surv_med} returns a \code{cowplot} object with the Cox-PH table annotation added.
#' @rdname gkm
#' @export
#' @examples
#' # Annotate Kaplan-Meier Plot with Cox-PH Table
#' coxph_tbl <- get_cox_pairwise_tbl(formula, data = use_lung, arm = "arm", ref_group = "A")
#' annot_surv_med(plt_kmg01, coxph_tbl)
#'
annot_cox_ph <- function(gg_plt, coxph_tbl, control_annot_coxph = control_coxph_annot(), font_size = 10) {
  # ... (function body remains the same)
  bg_fill <- if (isTRUE(control_annot_coxph[["fill"]])) "#00000020" else control_annot_coxph[["fill"]]

  gg_coxph <- df2gg(coxph_tbl, font_size = font_size, colwidths = c(1.1, 1, 3), bg_fill = bg_fill) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = font_size, face = "italic", hjust = 1),
      plot.margin = ggplot2::margin(0, 2, 0, 5)
    ) +
    ggplot2::coord_cartesian(clip = "off", ylim = c(0.5, nrow(coxph_tbl) + 1.5))
  gg_coxph <- suppressMessages(
    gg_coxph +
      ggplot2::scale_x_continuous(expand = c(0.025, 0)) +
      ggplot2::scale_y_continuous(labels = rev(rownames(coxph_tbl)), breaks = seq_len(nrow(coxph_tbl)))
  )

  gg_plt <- cowplot::ggdraw(gg_plt) +
    cowplot::draw_plot(
      gg_coxph, control_annot_coxph[["x"]], control_annot_coxph[["y"]],
      width = control_annot_coxph[["w"]], height = control_annot_coxph[["h"]],
      vjust = 0.5, hjust = 0.5
    )
  gg_plt
}


#' @description The function \code{annot_at_risk} adds a "Numbers at Risk" table below a Kaplan-Meier plot using \code{cowplot::plot_grid}.
#'
#' @param gg_plt A \code{ggplot2} or \code{cowplot} object of the Kaplan-Meier plot.
#' @param fit_km A fitted Kaplan-Meier object of class \code{survfit}, used to generate the table data.
#' @param font_size Numeric, base font size for the table.
#' @param annot_at_risk_title Logical, whether to include the title "Patients at Risk:".
#' @param rel_height_plot Numeric, relative height of the main plot area compared to the 'at-risk' table (0 to 1).
#' @param xlab Character string for the x-axis label on the 'at-risk' table (typically time).
#' @rdname gkm
#' @return The function \code{annot_at_risk} returns a \code{cowplot} object combining the KM plot and the 'Numbers at Risk' table.
#' @export
#' @examples
#' # Annotate Plot with Numbers at Risk Table
#' annot_at_risk(plt_kmg01, fit_kmg01)
#'
annot_at_risk <- function(gg_plt, fit_km, font_size = 10, annot_at_risk_title = TRUE, rel_height_plot = 0.75, xlab = "Days") {
  data <- broom::tidy(fit_km)
  xticks <- h_xticks(data = data)
  annot_tbl <- summary(fit_km, times = xticks, extend = TRUE)

  # Placeholder for armval, should be retrieved from fit_km or passed as argument
  armval <- if (is.null(fit_km$strata)) "All" else levels(fit_km$strata)

  annot_tbl <- if (is.null(fit_km$strata)) {
    data.frame(
      n.risk = annot_tbl$n.risk,
      time = annot_tbl$time,
      strata = armval
    )
  } else {
    strata_lst <- strsplit(sub("=", "equals", levels(annot_tbl$strata)), "equals")
    levels(annot_tbl$strata) <- matrix(unlist(strata_lst), ncol = 2, byrow = TRUE)[, 2]
    data.frame(
      n.risk = annot_tbl$n.risk,
      time = annot_tbl$time,
      strata = annot_tbl$strata
    )
  }

  at_risk_tbl <- as.data.frame(tidyr::pivot_wider(annot_tbl, names_from = "time", values_from = "n.risk")[, -1])
  at_risk_tbl[is.na(at_risk_tbl)] <- 0
  rownames(at_risk_tbl) <- levels(annot_tbl$strata)

  gg_at_risk <- df2gg(
    at_risk_tbl,
    font_size = font_size, col_labels = FALSE, hline = FALSE,
    colwidths = rep(1, ncol(at_risk_tbl))
  ) +
    ggplot2::labs(title = if (annot_at_risk_title) "Patients at Risk:" else NULL, x = xlab) +
    ggplot2::theme_bw(base_size = font_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = font_size, vjust = 3, face = "bold"),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = font_size, face = "italic", hjust = 1),
      axis.text.x = ggplot2::element_text(size = font_size),
      axis.line.x = ggplot2::element_line()
    ) +
    ggplot2::coord_cartesian(clip = "off", ylim = c(0.5, nrow(at_risk_tbl)))
  gg_at_risk <- suppressMessages(
    gg_at_risk +
      ggplot2::scale_x_continuous(expand = c(0.1, 0), breaks = seq_along(at_risk_tbl) - 0.5, labels = xticks) +
      ggplot2::scale_y_continuous(labels = rev(levels(annot_tbl$strata)), breaks = seq_len(nrow(at_risk_tbl)))
  )

  gg_plt <- cowplot::plot_grid(
    gg_plt, gg_at_risk,
    align = "v", axis = "tblr", ncol = 1,
    rel_heights = c(rel_height_plot, 1 - rel_height_plot)
  )
  gg_plt
}
