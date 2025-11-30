#' @title Control parameters for Cox Proportional Hazards model
#'
#' @description Creates a list of control parameters for Cox Proportional Hazards (Cox-PH)
#' analysis, used by functions like \code{\link{h_tbl_coxph_pairwise}}.
#'
#' @param pval_method A character string specifying the method for calculating the p-value.
#'   Must be one of \code{"log-rank"}, \code{"wald"}, or \code{"likelihood"}.
#' @param ties A character string specifying the method for handling tied failure times.
#'   Must be one of \code{"efron"}, \code{"breslow"}, or \code{"exact"}.
#' @param conf_level A numeric value between 0 and 1, specifying the confidence level.
#'
#' @return A list with elements \code{pval_method}, \code{ties}, and \code{conf_level}.
control_coxph <- function(pval_method = c("log-rank", "wald", "likelihood"),
                          ties = c("efron", "breslow", "exact"), conf_level = 0.95) {
  pval_method <- match.arg(pval_method)
  ties <- match.arg(ties)
  assert_proportion_value(conf_level) # Assuming assert_proportion_value is defined elsewhere
  list(pval_method = pval_method, ties = ties, conf_level = conf_level)
}

#' @title Control parameters for Median Survival Annotation Box
#'
#' @description Creates a list of control parameters for positioning and styling the
#' median survival annotation box on a plot.
#'
#' @param x A numeric value (0 to 1) for the x-coordinate of the box center (relative to plot area).
#' @param y A numeric value (0 to 1) for the y-coordinate of the box center (relative to plot area).
#' @param w A numeric value (0 to 1) for the width of the box (relative to plot area).
#' @param h A numeric value (0 to 1) for the height of the box (relative to plot area).
#' @param fill A logical value (\code{TRUE} for a default light gray fill) or a color string for the box background.
#'
#' @return A list with elements \code{x}, \code{y}, \code{w}, \code{h}, and \code{fill}.
control_surv_med_annot <- function(x = 0.8, y = 0.85, w = 0.32, h = 0.16, fill = TRUE) {
  assert_proportion_value(x)
  assert_proportion_value(y)
  assert_proportion_value(w)
  assert_proportion_value(h)

  list(x = x, y = y, w = w, h = h, fill = fill)
}

#' @title Control parameters for Cox-PH Annotation Box
#'
#' @description Creates a list of control parameters for positioning and styling the
#' Cox Proportional Hazards annotation box on a plot.
#'
#' @param x A numeric value (0 to 1) for the x-coordinate of the box center (relative to plot area).
#' @param y A numeric value (0 to 1) for the y-coordinate of the box center (relative to plot area).
#' @param w A numeric value (0 to 1) for the width of the box (relative to plot area).
#' @param h A numeric value (0 to 1) for the height of the box (relative to plot area).
#' @param fill A logical value (\code{TRUE} for a default light gray fill) or a color string for the box background.
#' @param ref_lbls A logical flag indicating whether to append "vs. ref group" to row names.
#'
#' @return A list with elements \code{x}, \code{y}, \code{w}, \code{h}, \code{fill}, and \code{ref_lbls}.
control_coxph_annot <- function(x = 0.29, y = 0.51, w = 0.4, h = 0.125, fill = TRUE, ref_lbls = FALSE) {
  checkmate::assert_logical(ref_lbls, any.missing = FALSE)

  res <- c(control_surv_med_annot(x = x, y = y, w = w, h = h), list(ref_lbls = ref_lbls))
  res
}


## Helper Functions (Formatting, Data Preparation, Plotting Utilities)

#' @title Format Confidence Level String
#' @description Converts a confidence level (e.g., 0.95) to a formatted string (e.g., "95% CI").
#' @param conf_level A numeric confidence level (proportion, 0 to 1).
#' @return A character string.
f_conf_level <- function(conf_level) {
  assert_proportion_value(conf_level) # Assuming assert_proportion_value is defined elsewhere
  paste0(conf_level * 100, "% CI")
}

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
#'
#' @return A \code{ggplot2} object representing the table.
#' @importFrom ggplot2 ggplot theme_void scale_x_continuous scale_y_continuous theme element_rect annotate element_text .pt
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
#'
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
#'
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

#' @title Pairwise Cox Proportional Hazards Model Summary Table
#'
#' @description This function computes and formats the results of a pairwise Cox Proportional
#' Hazards (Cox-PH) regression analysis between different treatment arms.
#'
#' @param df A data frame containing the survival data.
#' @param variables A named list specifying the column names for time-to-event (\code{tte}),
#' treatment arm (\code{arm}), event status (\code{is_event}), and optional strata (\code{strata}) in \code{df}.
#' @param ref_group_coxph An optional string specifying the reference group for the Cox-PH model.
#'   If \code{NULL}, the first factor level of the arm variable is used as the reference group.
#' @param control_coxph_pw A list of control parameters for the Cox-PH model, typically
#'   generated by \code{\link{control_coxph}}, controlling the p-value method, ties handling, and confidence level.
#' @param annot_coxph_ref_lbls A logical flag indicating whether to append "vs. ref group"
#'   to the row names in the resulting table.
#'
#' @return A data frame summarizing the pairwise Cox-PH results, including Hazard Ratio (HR),
#'   its confidence interval, and the p-value.
#' @export
h_tbl_coxph_pairwise <- function(df,
                                 variables,
                                 ref_group_coxph = NULL,
                                 control_coxph_pw = control_coxph(),
                                 annot_coxph_ref_lbls = FALSE) {
  # ... (function body remains the same)
  assert_df_with_variables(df, variables) # Assuming assert_df_with_variables is defined elsewhere
  checkmate::assert_choice(ref_group_coxph, levels(df[[variables$arm]]), null.ok = TRUE)
  checkmate::assert_flag(annot_coxph_ref_lbls)

  arm <- variables$arm
  df[[arm]] <- factor(df[[arm]])

  ref_group <- if (!is.null(ref_group_coxph)) ref_group_coxph else levels(df[[variables$arm]])[1]
  comp_group <- setdiff(levels(df[[arm]]), ref_group)

  results <- Map(function(comp) {
    res <- s_coxph_pairwise(
      df = df[df[[arm]] == comp, , drop = FALSE],
      .ref_group = df[df[[arm]] == ref_group, , drop = FALSE],
      .in_ref_col = FALSE,
      .var = variables$tte,
      is_event = variables$is_event,
      strata = variables$strata,
      control = control_coxph_pw
    )
    res_df <- data.frame(
      hr = format(round(res$hr, 2), nsmall = 2),
      hr_ci = paste0(
        "(", format(round(res$hr_ci[1], 2), nsmall = 2), ", ",
        format(round(res$hr_ci[2], 2), nsmall = 2), ")"
      ),
      pvalue = if (res$pvalue < 0.0001) "<0.0001" else format(round(res$pvalue, 4), 4),
      stringsAsFactors = FALSE
    )
    # Assuming obj_label is defined elsewhere and hr_ci is the label for the CI
    colnames(res_df) <- c("HR", vapply(res[c("hr_ci", "pvalue")], obj_label, FUN.VALUE = "character")) # nolint
    row.names(res_df) <- comp
    res_df
  }, comp_group)
  if (annot_coxph_ref_lbls) names(results) <- paste(comp_group, "vs.", ref_group)

  do.call(rbind, results)
}

#' @title Prepare Kaplan-Meier Data for Plotting
#'
#' @description Takes a fitted \code{survfit} object and processes it into a data frame
#' suitable for plotting a Kaplan-Meier curve with \code{ggplot2}, including extending
#' the curve to time zero.
#'
#' @param fit_km A fitted Kaplan-Meier object of class \code{survfit}.
#' @param armval Character string for the strata level if \code{fit_km} has no strata (e.g., "All").
#' @param max_time Numeric, the maximum time point to include in the data, or \code{NULL} for no limit.
#'
#' @return A data frame containing the survival curve steps, confidence intervals, and censoring info.
#' @importFrom broom tidy
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


## Core Statistical Function

#' @title Pairwise Cox Proportional Hazards Model Calculation
#'
#' @description Performs a Cox Proportional Hazards model calculation comparing two groups
#' (a reference group and a comparison group). This is an internal function used by
#' \code{\link{h_tbl_coxph_pairwise}}.
#'
#' @param df Data frame for the comparison group.
#' @param .ref_group Data frame for the reference group.
#' @param .in_ref_col Logical, if \code{TRUE} returns empty results (for internal table building).
#' @param .var Character string for the time-to-event variable name.
#' @param is_event Character string for the event status variable name.
#' @param strata Optional character vector of stratification variable names.
#' @param control A list of control parameters from \code{\link{control_coxph}}.
#' @param ... Additional arguments (not used).
#'
#' @return A list containing the p-value, Hazard Ratio (HR), confidence interval (HR\_CI),
#'   and total counts.
#' @importFrom survival Surv coxph survdiff
#' @importFrom stats as.formula
s_coxph_pairwise <-
  function(df, .ref_group, .in_ref_col, .var, is_event, strata = NULL,
           control = control_coxph(), ...) {
    # ... (function body remains the same)
    checkmate::assert_string(.var)
    checkmate::assert_numeric(df[[.var]])
    checkmate::assert_logical(df[[is_event]])
    assert_df_with_variables(df, list(tte = .var, is_event = is_event))
    pval_method <- control$pval_method
    ties <- control$ties
    conf_level <- control$conf_level
    if (.in_ref_col) {
      # ... (returns empty list for reference column)
      return(list(pvalue = with_label(
        numeric(),
        paste0("p-value (", pval_method, ")")
      ), hr = with_label(
        numeric(),
        "Hazard Ratio"
      ), hr_ci = with_label(
        numeric(),
        f_conf_level(conf_level)
      ), hr_ci_3d = with_label(
        numeric(),
        paste0(
          "Hazard Ratio (", f_conf_level(conf_level),
          ")"
        )
      ), n_tot = with_label(
        numeric(),
        "Total n"
      ), n_tot_events = with_label(
        numeric(),
        "Total events"
      )))
    }
    data <- rbind(.ref_group, df)
    group <- factor(rep(c("ref", "x"), c(nrow(.ref_group), nrow(df))),
      levels = c("ref", "x")
    )
    df_cox <- data.frame(
      tte = data[[.var]], is_event = data[[is_event]],
      arm = group
    )
    if (is.null(strata)) {
      formula_cox <- survival::Surv(tte, is_event) ~ arm
    } else {
      formula_cox <- stats::as.formula(paste0(
        "survival::Surv(tte, is_event) ~ arm + survival::strata(",
        paste(strata, collapse = ","), ")"
      ))
      df_cox <- cbind(df_cox, data[strata])
    }
    cox_fit <- survival::coxph(
      formula = formula_cox, data = df_cox,
      ties = ties
    )
    sum_cox <- summary(cox_fit, conf.int = conf_level, extend = TRUE)
    orginal_survdiff <- survival::survdiff(formula_cox, data = df_cox)
    log_rank_pvalue <- 1 - stats::pchisq(orginal_survdiff$chisq, length(orginal_survdiff$n) -
      1)
    pval <- switch(pval_method,
      wald = sum_cox$waldtest["pvalue"],
      `log-rank` = log_rank_pvalue,
      likelihood = sum_cox$logtest["pvalue"]
    )
    # Assuming with_label is defined elsewhere
    list(pvalue = with_label(unname(pval), paste0(
      "p-value (",
      pval_method, ")"
    )), hr = with_label(sum_cox$conf.int[
      1,
      1
    ], "Hazard Ratio"), hr_ci = with_label(unname(sum_cox$conf.int[
      1,
      3:4
    ]), f_conf_level(conf_level)), hr_ci_3d = with_label(c(sum_cox$conf.int[
      1,
      1
    ], unname(sum_cox$conf.int[1, 3:4])), paste0(
      "Hazard Ratio (",
      f_conf_level(conf_level), ")"
    )), n_tot = with_label(
      sum_cox$n,
      "Total n"
    ), n_tot_events = with_label(
      sum_cox$nevent,
      "Total events"
    ))
  }


## Core Plotting and Annotation Functions

#' @title Generate a Kaplan-Meier Plot
#'
#' @description This function creates a comprehensive \code{ggplot2} object for a Kaplan-Meier
#' survival curve, with support for various customizations like censoring marks, CIs, and axis control.
#'
#' @param surv_plot_data A data frame containing the pre-processed survival data, ready for plotting.
#'   This data should be equivalent to the output of \code{\link{h_data_plot}}.
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
#' @return A \code{ggplot2} object of the KM plot.
#' @importFrom ggplot2 ggplot aes theme_bw scale_y_continuous labs theme element_text element_blank element_rect element_line geom_step geom_point scale_shape_manual guides guide_legend geom_ribbon scale_color_manual scale_fill_manual scale_linetype_manual coord_cartesian
#' @importFrom rlang .data
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
    ggtheme = NULL) {
  # ... (function body remains the same)
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

#' @title Annotate Kaplan-Meier Plot with Median Survival Table
#'
#' @description Adds a median survival time summary table as an annotation box on a
#' Kaplan-Meier plot using \code{cowplot}.
#'
#' @param gg_plt A \code{ggplot2} or \code{cowplot} object of the Kaplan-Meier plot.
#' @param fit_km A fitted Kaplan-Meier object of class \code{survfit}, used to generate the table data.
#' @param control_annot_surv_med A list of control parameters for the annotation box,
#'   typically generated by \code{\link{control_surv_med_annot}}.
#' @param font_size Numeric, base font size for the annotation table.
#'
#' @return A \code{cowplot} object with the median survival table annotation added.
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom ggplot2 theme element_text coord_cartesian scale_x_continuous scale_y_continuous margin
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

#' @title Annotate Kaplan-Meier Plot with Cox-PH Table
#'
#' @description Adds a Cox Proportional Hazards summary table as an annotation box on a
#' Kaplan-Meier plot using \code{cowplot}.
#'
#' @param gg_plt A \code{ggplot2} or \code{cowplot} object of the Kaplan-Meier plot.
#' @param coxph_tbl A data frame containing pre-calculated Cox-PH results,
#'   typically generated by \code{\link{h_tbl_coxph_pairwise}}.
#' @param control_annot_coxph A list of control parameters for the annotation box,
#'   typically generated by \code{\link{control_coxph_annot}}.
#' @param font_size Numeric, base font size for the annotation table.
#'
#' @return A \code{cowplot} object with the Cox-PH table annotation added.
#' @importFrom cowplot ggdraw draw_plot
#' @importFrom ggplot2 theme element_text coord_cartesian scale_x_continuous scale_y_continuous margin
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

#' @title Annotate Plot with Numbers at Risk Table
#'
#' @description Adds a "Numbers at Risk" table below a Kaplan-Meier plot using \code{cowplot::plot_grid}.
#'
#' @param gg_plt A \code{ggplot2} or \code{cowplot} object of the Kaplan-Meier plot.
#' @param fit_km A fitted Kaplan-Meier object of class \code{survfit}, used to generate the table data.
#' @param font_size Numeric, base font size for the table.
#' @param annot_at_risk_title Logical, whether to include the title "Patients at Risk:".
#' @param rel_height_plot Numeric, relative height of the main plot area compared to the 'at-risk' table (0 to 1).
#' @param xlab Character string for the x-axis label on the 'at-risk' table (typically time).
#'
#' @return A \code{cowplot} object combining the KM plot and the 'Numbers at Risk' table.
#' @importFrom broom tidy
#' @importFrom tidyr pivot_wider
#' @importFrom cowplot plot_grid
#' @importFrom ggplot2 labs theme_bw theme element_text element_blank element_line coord_cartesian scale_x_continuous scale_y_continuous
#' @export
annot_at_risk <- function(gg_plt, fit_km, font_size = 10, annot_at_risk_title = TRUE, rel_height_plot = 0.75, xlab = "Days") {
  # ... (function body remains the same)
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

# styler: off
# nocov start



  ## Control and Internal KM Fit (from `tern` or similar)

  #' @title Control parameters for Survival Timepoint Estimation
  #'
  #' @description Creates a list of control parameters for \code{survival::survfit} when used
  #' for timepoint estimation.
  #'
  #' @param conf_level A numeric value (0 to 1) for the confidence level.
  #' @param conf_type A character string specifying the type of confidence interval.
  #'   Must be one of \code{"plain"}, \code{"log"}, or \code{"log-log"}.
  #'
  #' @return A list with elements \code{conf_level} and \code{conf_type}.
  control_surv_timepoint <- function(conf_level = 0.95, conf_type = c("plain", "log", "log-log")) {
    conf_type <- match.arg(conf_type)
    assert_proportion_value(conf_level) # Assuming assert_proportion_value is defined elsewhere
    list(conf_level = conf_level, conf_type = conf_type)
  }


#' @title Kaplan-Meier Survival Curve Fitting
#'
#' @description This helper function fits a Kaplan-Meier survival curve model
#' using the formula \code{survival::Surv(tte, is_event) ~ arm}.
#' It is designed to be a prerequisite for plotting functions like \code{\link{g_km}}.
#'
#' @param df A data frame containing time-to-event (tte), event status (\code{is_event}),
#'   and treatment arm (\code{arm}) variables.
#' @param variables A named list specifying the column names for time-to-event (\code{tte}),
#'   event status (\code{is_event}), and treatment arm (\code{arm}) in \code{df}.
#' @param control_surv A list of control parameters for the \code{survival::survfit} function,
#'   typically generated by \code{\link{control_surv_timepoint}}, controlling confidence level
#'   and confidence interval type.
#'
#' @return An object of class \code{survfit} from the \code{survival} package, containing
#'   the fitted Kaplan-Meier curves.
#' @importFrom survival survfit Surv
#' @importFrom stats as.formula
h_km_fit <- function(df, variables, control_surv = control_surv_timepoint()) {
  tte <- variables$tte
  is_event <- variables$is_event
  arm <- variables$arm

  # Assuming assert_valid_factor and assert_df_with_variables are defined elsewhere
  assert_valid_factor(df[[arm]])
  assert_df_with_variables(df, list(tte = tte, is_event = is_event, arm = arm))

  formula <- stats::as.formula(paste0("survival::Surv(", tte, ", ", is_event, ") ~ ", arm))
  fit_km <- survival::survfit(
    formula = formula,
    data = df,
    conf.int = control_surv$conf_level,
    conf.type = control_surv$conf_type
  )
  return(fit_km)
}
# nocov end
# styler: on
