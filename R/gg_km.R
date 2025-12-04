#' Kaplan-Meier Plot
#'
#' @description
#' This set of functions facilitates the creation of Kaplan-Meier survival plots using `ggplot2`. Use
#' `process_survfit()` to prepare the survival data from a fitted `survfit` object, and then
#' `gg_km()` to generate the Kaplan-Meier plot with various customization options. Additional functions
#' like `annot_surv_med()`, `annot_cox_ph()`, and `annot_at_risk()` allow for adding summary tables and
#' annotations to the plot.
#'
#' @name gg_km
NULL

#' @describeIn gg_km takes a fitted [survfit] object and processes it into a data frame
#'   suitable for plotting a Kaplan-Meier curve with `ggplot2`. Time zero is also added to the data.
#'
#' @param fit_km A fitted Kaplan-Meier object of class `survfit`.
#' @param armval (`string`)\cr
#'   A single character string used as the strata level if the input `fit_km` object
#'   has no strata (e.g., `"All"`).
#' @param max_time (`numeric` or `NULL`)\cr
#'   A single numeric value defining the **maximum time point** to include in the data,
#'   or `NULL` for no time limit.
#'
#' @return The function `process_survfit` returns a data frame containing the survival
#'   curve steps, confidence intervals, and censoring info.
#'
#' @details
#' Data setup assumes `"time"` is event time, `"status"` is event indicator (`1` represents an event),
#' while `"arm"` is the treatment group.
#'
#' @examples
#' # Data preparation for KM plot
#' use_lung <- lung
#' use_lung$arm <- factor(sample(c("A", "B", "C"), nrow(use_lung), replace = TRUE))
#' use_lung$status <- use_lung$status - 1 # Convert status to 0/1
#' use_lung <- na.omit(use_lung)
#'
#' # Fit Kaplan-Meier model
#' formula <- Surv(time, status) ~ arm
#' fit_kmg01 <- survfit(formula, use_lung)
#'
#' # Process survfit data for plotting
#' surv_plot_data <- process_survfit(fit_kmg01)
#' head(surv_plot_data)
#'
#' @export
process_survfit <- function(fit_km,
                                 armval = "All",
                                 max_time = NULL) {
  set_cli_abort_call()

  # Input checks
  if (!inherits(fit_km, "survfit")) {
    cli::cli_abort(
      "The input {.arg fit_km} must be a fitted Kaplan-Meier object of class {.cls survfit}.",
      call = get_cli_abort_call()
    )
  }
  check_string(armval)
  check_numeric(max_time, allow_empty = TRUE)

  y <- broom::tidy(fit_km)

  # Handle strata factor levels
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

  # Extend to time zero
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

  # Censoring points
  y$censor <- ifelse(y$n.censor > 0, y$estimate, NA)

  # Apply max_time filter if provided
  if (!is.null(max_time)) {
    y <- y[y$time <= max(max_time), ]
  }

  y
}


#' @describeIn gg_km creates a Kaplan-Meier survival curve, with
#'   support for various customizations like censoring marks, Confidence  Intervals (CIs), and axis control.
#'
#' @param surv_plot_data (`data.frame`)\cr
#'   A data frame containing the pre-processed survival data, ready for plotting.
#'   This data should be equivalent to the output of `process_survfit`.
#' @param lty (`numeric` or `NULL`)\cr
#'   A numeric vector of **line types** (e.g., `1` for solid, `2` for dashed) for the survival curves, or `NULL` for `ggplot2` defaults.
#'   The length should match the number of arms/groups.
#' @param lwd (`numeric`)\cr
#'   A single numeric value specifying the **line width** for the survival curves.
#' @param censor_show (`logical`)\cr
#'   A single logical value indicating whether to display **censoring marks** on the plot. Defaults to `TRUE`.
#' @param pch (`numeric`)\cr
#'   A single numeric value specifying the **plotting character** (point shape code) for censoring marks.
#' @param size (`numeric`)\cr
#'   A single numeric value specifying the **size** of the censoring marks.
#' @param max_time (`numeric`)\cr
#'   A single numeric value defining the **maximum time point** to display on the x-axis. Data points beyond this time will be clipped.
#' @param xticks (`numeric` or `NULL`)\cr
#'   A numeric vector of explicit **x-axis tick positions**, or a single numeric value representing the **interval** between ticks, or `NULL` for automatic `ggplot2` scaling.
#' @param xlab (`character`)\cr
#'   A single character string for the **x-axis label**.
#' @param yval (`character`)\cr
#'   A single character string, either `"Survival"` or `"Failure"` to plot the corresponding probability. Case sensitive.
#' @param ylab (`character`)\cr
#'   A single character string for the **y-axis label**.
#' @param ylim (`numeric`)\cr
#'   A **numeric vector of length 2** defining the lower and upper limits of the y-axis (e.g., `c(0, 1)`).
#' @param font_size (`numeric`)\cr
#'   A single numeric value specifying the **base font size** for the plot theme elements.
#' @param legend_pos (`numeric` or `NULL`)\cr
#'   A **numeric vector of length 2** defining the **legend position** as (x, y) coordinates relative to the plot area (ranging from 0 to 1), or `NULL` for external, automatic placement.
#'
#' @return The function `gg_km` returns a `ggplot2` object of the KM plot.
#'
#' @examples
#' # Example of making the KM plot
#' plt_kmg01 <- gg_km(surv_plot_data,
#'   xlab = "Time (Days)"
#' )
#'
#' # Confidence Interval as Ribbon
#' plt_kmg01 +
#'   ggplot2::geom_ribbon(alpha = 0.3, lty = 0, na.rm = TRUE)
#'
#' # Adding Title and Footnotes
#' plt_kmg01 +
#'   ggplot2::labs(title = "title", caption = "footnotes")
#'
#' @export
gg_km <- function(surv_plot_data,
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
                  font_size = 10,
                  legend_pos = NULL) {
  set_cli_abort_call()

  # Input checks
  check_data_frame(surv_plot_data)
  needed_cols <- c("time", "estimate", "conf.low", "conf.high", "strata", "n.censor", "censor")
  if (!all(needed_cols %in% colnames(surv_plot_data))) {
    cli::cli_abort(
      "The input {.arg surv_plot_data} must contain the following columns: ",
      "{.code time}, {.code estimate}, {.code conf.low}, {.code conf.high}, {.code strata}, {.code n.censor}, and {.code censor}.",
      call = get_cli_abort_call()
    )
  }
  if (nrow(surv_plot_data) < 1) {
    cli::cli_abort(
      "The input {.arg surv_plot_data} must contain at least one row of data.",
      call = get_cli_abort_call()
    )
  }

  data <- surv_plot_data

  armval <- levels(data$strata)

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
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = font_size),
      axis.title = ggplot2::element_text(size = font_size),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = font_size),
      legend.box.background = ggplot2::element_rect(fill = "white", linewidth = 0.5),
      legend.background = ggplot2::element_blank(),
      legend.position = "inside",
      legend.spacing.y = ggplot2::unit(-0.02, "npc"),
      panel.grid.major = ggplot2::element_blank(),
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

  gg_plt
}

#' @describeIn gg_km The \code{annot_surv_med} function adds a median survival time summary table as an annotation box on a
#' Kaplan-Meier plot using \code{cowplot}.
#'
#' @param gg_plt A \code{ggplot2} or \code{cowplot} object of the Kaplan-Meier plot.
#' @param fit_km A fitted Kaplan-Meier object of class \code{survfit}, used to generate the table data.
#' @param control_annot_surv_med A list of control parameters for the annotation box.
#' @param font_size Numeric, base font size for the annotation table.
#'
#' @return The \code{annot_surv_med} function returns a \code{cowplot} object with the median survival table annotation added.
#'
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

#' @describeIn gg_km The function \code{annot_cox_ph} adds a Cox Proportional Hazards summary table created by the function \code{\link{get_cox_pairwise_tbl}} as an annotation box on a
#' Kaplan-Meier plot using \code{cowplot}.
#'
#' @param gg_plt A \code{ggplot2} or \code{cowplot} object of the Kaplan-Meier plot.
#' @param coxph_tbl A data frame containing pre-calculated Cox-PH results.
#' @param control_annot_coxph A list of control parameters for the annotation box.
#' @param font_size Numeric, base font size for the annotation table.
#'
#' @return The function \code{annot_surv_med} returns a \code{cowplot} object with the Cox-PH table annotation added.
#'
#' @examples
#' # Annotate Kaplan-Meier Plot with Cox-PH Table
#' coxph_tbl <- get_cox_pairwise_tbl(formula, data = use_lung, arm = "arm", ref_group = "A")
#' annot_cox_ph(plt_kmg01, coxph_tbl)
#'
#' @export
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


#' @describeIn gg_km The function \code{annot_at_risk} adds a "Numbers at Risk" table below a Kaplan-Meier plot using \code{cowplot::plot_grid}.
#'
#' @param gg_plt A \code{ggplot2} or \code{cowplot} object of the Kaplan-Meier plot.
#' @param fit_km A fitted Kaplan-Meier object of class \code{survfit}, used to generate the table data.
#' @param font_size Numeric, base font size for the table.
#' @param annot_at_risk_title Logical, whether to include the title "Patients at Risk:".
#' @param rel_height_plot Numeric, relative height of the main plot area compared to the 'at-risk' table (0 to 1).
#' @param xlab Character string for the x-axis label on the 'at-risk' table (typically time).
#' @return The function \code{annot_at_risk} returns a \code{cowplot} object combining the KM plot and the 'Numbers at Risk' table.
#'
#' @examples
#' # Annotate Plot with Numbers at Risk Table
#' annot_at_risk(plt_kmg01, fit_kmg01)
#'
#' @export
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
