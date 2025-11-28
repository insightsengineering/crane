control_surv_timepoint <- function(conf_level = 0.95, conf_type = c("plain", "log", "log-log")) {
  conf_type <- match.arg(conf_type)
  assert_proportion_value(conf_level)
  list(conf_level = conf_level, conf_type = conf_type)
}

control_coxph <- function(pval_method = c("log-rank", "wald", "likelihood"),
                          ties = c("efron", "breslow", "exact"), conf_level = 0.95) {
  pval_method <- match.arg(pval_method)
  ties <- match.arg(ties)
  assert_proportion_value(conf_level)
  list(pval_method = pval_method, ties = ties, conf_level = conf_level)
}

control_surv_med_annot <- function(x = 0.8, y = 0.85, w = 0.32, h = 0.16, fill = TRUE) {
  assert_proportion_value(x)
  assert_proportion_value(y)
  assert_proportion_value(w)
  assert_proportion_value(h)

  list(x = x, y = y, w = w, h = h, fill = fill)
}

control_coxph_annot <- function(x = 0.29, y = 0.51, w = 0.4, h = 0.125, fill = TRUE, ref_lbls = FALSE) {
  checkmate::assert_logical(ref_lbls, any.missing = FALSE)

  res <- c(control_surv_med_annot(x = x, y = y, w = w, h = h), list(ref_lbls = ref_lbls))
  res
}


## ----------------------------------------------------------------------------
## 3. Helper Functions (Formatting, Data Preparation, Plotting Utilities)
## ----------------------------------------------------------------------------

f_conf_level <- function(conf_level) {
  assert_proportion_value(conf_level)
  paste0(conf_level * 100, "% CI")
}

df2gg <- function(df, colwidths = NULL, font_size = 10, col_labels = TRUE,
                  col_lab_fontface = "bold", hline = TRUE, bg_fill = NULL) {
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
    theme_void() +
    scale_x_continuous(limits = c(
      0,
      tot_width
    )) +
    scale_y_continuous(limits = c(1, nrow(df)))
  if (!is.null(bg_fill)) {
    res <- res + theme(plot.background = element_rect(fill = bg_fill))
  }
  if (hline) {
    res <- res + annotate("segment",
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
    res <- res + annotate("text",
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

h_xticks <- function(data, xticks = NULL, max_time = NULL) {
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

h_tbl_median_surv <- function(fit_km, armval = "All") {
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
#' If \code{NULL}, the first factor level of the arm variable is used as the reference group.
#' @param control_coxph_pw A list of control parameters for the Cox-PH model, typically
#' generated by \code{\link{control_coxph}}, controlling the p-value method, ties handling, and confidence level.
#' @param annot_coxph_ref_lbls A logical flag indicating whether to append "vs. ref group"
#' to the row names in the resulting table.
#'
#' @return A data frame summarizing the pairwise Cox-PH results, including Hazard Ratio (HR),
#' its confidence interval, and the p-value.
#' @export
h_tbl_coxph_pairwise <- function(df,
                                 variables,
                                 ref_group_coxph = NULL,
                                 control_coxph_pw = control_coxph(),
                                 annot_coxph_ref_lbls = FALSE) {
  assert_df_with_variables(df, variables)
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
    colnames(res_df) <- c("HR", vapply(res[c("hr_ci", "pvalue")], obj_label, FUN.VALUE = "character")) # nolint
    row.names(res_df) <- comp
    res_df
  }, comp_group)
  if (annot_coxph_ref_lbls) names(results) <- paste(comp_group, "vs.", ref_group)

  do.call(rbind, results)
}

h_data_plot <- function(fit_km,
                        armval = "All",
                        max_time = NULL) {
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


## ----------------------------------------------------------------------------
## 4. Core Statistical Function
## ----------------------------------------------------------------------------

s_coxph_pairwise <-
  function(df, .ref_group, .in_ref_col, .var, is_event, strata = NULL,
           control = control_coxph(), ...) {
    checkmate::assert_string(.var)
    checkmate::assert_numeric(df[[.var]])
    checkmate::assert_logical(df[[is_event]])
    assert_df_with_variables(df, list(tte = .var, is_event = is_event))
    pval_method <- control$pval_method
    ties <- control$ties
    conf_level <- control$conf_level
    if (.in_ref_col) {
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
        "survival::Surv(tte, is_event) ~ arm + strata(",
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
    log_rank_pvalue <- 1 - pchisq(orginal_survdiff$chisq, length(orginal_survdiff$n) -
      1)
    pval <- switch(pval_method,
      wald = sum_cox$waldtest["pvalue"],
      `log-rank` = log_rank_pvalue,
      likelihood = sum_cox$logtest["pvalue"]
    )
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


#' @title Kaplan-Meier Survival Curve Fitting
#'
#' @description This helper function fits a Kaplan-Meier survival curve model
#' using the formula \code{survival::Surv(tte, is_event) ~ arm}.
#' It is designed to be a prerequisite for plotting functions like \code{\link{g_km}}.
#'
#' @param df A data frame containing time-to-event (tte), event status (\code{is_event}),
#' and treatment arm (\code{arm}) variables.
#' @param variables A named list specifying the column names for time-to-event (\code{tte}),
#' event status (\code{is_event}), and treatment arm (\code{arm}) in \code{df}.
#' For example: \code{list(tte = "time_column", is_event = "status_column", arm = "group_column")}.
#' @param control_surv A list of control parameters for the \code{survival::survfit} function,
#' typically generated by \code{\link{control_surv_timepoint}}, controlling confidence level
#' and confidence interval type.
#'
#' @return An object of class \code{survfit} from the \code{survival} package, containing
#' the fitted Kaplan-Meier curves.
#' @export
h_km_fit <- function(df, variables, control_surv = control_surv_timepoint()) {
  tte <- variables$tte
  is_event <- variables$is_event
  arm <- variables$arm

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

#' @title Generate a Kaplan-Meier Plot with Annotations
#'
#' @description This function creates a comprehensive ggplot2 object for a Kaplan-Meier
#' survival curve, optionally including annotations for median survival and Cox-PH results,
#' and a 'Numbers at Risk' table below the main plot.
#'
#' @param fit_km A fitted Kaplan-Meier object of class \code{survfit}, typically
#' generated by \code{\link{h_km_fit}}.
#' @param variables A named list specifying the survival and grouping variables (needed
#' for accessing the column names, even if the fit is provided).
#' @param coxph_tbl An optional data frame containing pre-calculated Cox-PH results,
#' typically generated by \code{\link{h_tbl_coxph_pairwise}}. If provided, a Cox-PH annotation table is added.
#' @param control_surv A list of control parameters for the survival model, from \code{\link{control_surv_timepoint}}.
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
#' @param annot_at_risk Logical, whether to include the 'Numbers at Risk' table below the plot.
#' @param annot_at_risk_title Logical, whether to include the title "Patients at Risk:" in the table.
#' @param annot_surv_med Logical, whether to include the median survival time annotation table.
#' @param control_annot_surv_med A list of control parameters for the median survival annotation box,
#' typically generated by \code{\link{control_surv_med_annot}}.
#' @param control_annot_coxph A list of control parameters for the Cox-PH annotation box,
#' typically generated by \code{\link{control_coxph_annot}}.
#' @param legend_pos Numeric vector of length 2 for legend position (x, y) relative to the plot area (0 to 1), or \code{NULL} for auto-placement.
#' @param rel_height_plot Numeric, relative height of the main plot area compared to the 'at-risk' table (0 to 1).
#' @param ggtheme An optional \code{ggplot2} theme to apply.
#' @param as_list Logical, if \code{TRUE}, returns a list containing the plot and the 'at-risk' table as separate \code{cowplot} objects; otherwise returns the combined \code{cowplot} object.
#'
#' @return A \code{cowplot} object combining the KM plot and the 'Numbers at Risk' table, or a list if \code{as_list = TRUE}.
#' @importFrom ggplot2 ggplot aes theme_bw scale_y_continuous labs theme element_text element_blank element_rect element_line geom_step geom_point scale_shape_manual guides guide_legend geom_ribbon scale_color_manual scale_fill_manual scale_linetype_manual coord_cartesian
#' @importFrom cowplot ggdraw draw_plot plot_grid
#' @importFrom tidyr pivot_wider
#' @importFrom survival survfit
#' @importFrom broom tidy
#' @importFrom stats as.formula
#' @export
g_km <- function(fit_km,
                 variables,
                 coxph_tbl = NULL, # New argument for pre-calculated Cox-PH table
                 control_surv = control_surv_timepoint(),
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
                 annot_at_risk = TRUE,
                 annot_at_risk_title = TRUE,
                 annot_surv_med = TRUE,
                 control_annot_surv_med = control_surv_med_annot(),
                 control_annot_coxph = control_coxph_annot(),
                 legend_pos = NULL,
                 rel_height_plot = 0.75,
                 ggtheme = NULL,
                 as_list = FALSE) {
  # --- Data Extraction and Assertions ---
  checkmate::assert_class(fit_km, "survfit")
  checkmate::assert_list(variables)
  checkmate::assert_subset(c("tte", "arm", "is_event"), names(variables))

  # 1. Extract arm values (strata names) from the fitted object
  # h_data_plot is used here only to consistently get the unique strata levels
  armval <- if (is.null(fit_km$strata)) "All" else levels(h_data_plot(fit_km, max_time = 1)$strata)
  checkmate::assert_vector(col, len = length(armval), null.ok = TRUE)

  # Check if a Cox-PH table was provided (replaces annot_coxph flag)
  if (!is.null(coxph_tbl)) {
    checkmate::assert_data_frame(coxph_tbl)
  }

  # --- Data Processing ---
  yval <- match.arg(yval)
  data <- h_data_plot(fit_km, armval = armval, max_time = max_time)
  xticks <- h_xticks(data = data, xticks = xticks, max_time = max_time)

  # change estimates of survival to estimates of failure (1 - survival)
  if (yval == "Failure") {
    data[c("estimate", "conf.low", "conf.high", "censor")] <- list(
      1 - data$estimate, 1 - data$conf.low, 1 - data$conf.high, 1 - data$censor
    )
  }

  # derive y-axis limits
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

  # --- ggplot Initialization and Aesthetics (Unchanged) ---
  gg_plt <- ggplot2::ggplot(
    data = data,
    mapping = aes(
      x = .data[["time"]], y = .data[["estimate"]], ymin = .data[["conf.low"]],
      ymax = .data[["conf.high"]], color = .data[["strata"]], fill = .data[["strata"]]
    )
  ) +
    theme_bw(base_size = font_size) +
    scale_y_continuous(limits = ylim, expand = c(0.025, 0)) +
    labs(title = title, x = xlab, y = ylab, caption = footnotes) +
    theme(
      axis.text = element_text(size = font_size), axis.title = element_text(size = font_size),
      legend.title = element_blank(), legend.text = element_text(size = font_size),
      legend.box.background = element_rect(fill = "white", linewidth = 0.5),
      legend.background = element_blank(), legend.position = "inside",
      legend.spacing.y = unit(-0.02, "npc"), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  # derive x-axis limits
  if (!is.null(max_time) && !is.null(xticks)) {
    gg_plt <- gg_plt + scale_x_continuous(
      breaks = xticks, limits = c(min(0, xticks), max(c(xticks, max_time))), expand = c(0.025, 0)
    )
  } else if (!is.null(xticks)) {
    if (max(data$time) <= max(xticks)) {
      gg_plt <- gg_plt + scale_x_continuous(
        breaks = xticks, limits = c(min(0, min(xticks)), max(xticks)), expand = c(0.025, 0)
      )
    } else {
      gg_plt <- gg_plt + scale_x_continuous(breaks = xticks, expand = c(0.025, 0))
    }
  } else if (!is.null(max_time)) {
    gg_plt <- gg_plt + scale_x_continuous(limits = c(0, max_time), expand = c(0.025, 0))
  }

  # set legend position (unchanged logic)
  if (!is.null(legend_pos)) {
    gg_plt <- gg_plt + theme(legend.position.inside = legend_pos)
  } else {
    max_time2 <- sort(
      data$time,
      partial = nrow(data) - length(armval) - 1
    )[nrow(data) - length(armval) - 1]

    y_rng <- ylim[2] - ylim[1]

    if (yval == "Survival" && all(data$estimate[data$time == max_time2] > ylim[1] + 0.09 * y_rng) &&
      all(data$estimate[data$time == max_time2] < ylim[1] + 0.5 * y_rng)) { # nolint
      gg_plt <- gg_plt +
        theme(
          legend.position.inside = c(1, 0.5),
          legend.justification = c(1.1, 0.6)
        )
    } else {
      gg_plt <- gg_plt +
        theme(
          legend.position.inside = c(1, 0),
          legend.justification = c(1.1, -0.4)
        )
    }
  }

  # add lines, censor marks, ci ribbon, and colors (unchanged)
  gg_plt <- if (is.null(lty)) {
    gg_plt + geom_step(linewidth = lwd, na.rm = TRUE)
  } else if (length(lty) == 1) {
    gg_plt + geom_step(linewidth = lwd, lty = lty, na.rm = TRUE)
  } else {
    gg_plt +
      geom_step(aes(lty = .data[["strata"]]), linewidth = lwd, na.rm = TRUE) +
      scale_linetype_manual(values = lty)
  }

  if (censor_show) {
    gg_plt <- gg_plt + geom_point(
      data = data[data$n.censor != 0, ],
      aes(x = .data[["time"]], y = .data[["censor"]], shape = "Censored"),
      size = size,
      na.rm = TRUE
    ) +
      scale_shape_manual(name = NULL, values = pch) +
      guides(fill = guide_legend(override.aes = list(shape = NA)))
  }

  if (ci_ribbon) gg_plt <- gg_plt + geom_ribbon(alpha = 0.3, lty = 0, na.rm = TRUE)

  if (!is.null(col)) {
    gg_plt <- gg_plt +
      scale_color_manual(values = col) +
      scale_fill_manual(values = col)
  }
  if (!is.null(ggtheme)) gg_plt <- gg_plt + ggtheme

  # --- Annotation Tables ---

  # 2. Median survival time annotation table
  if (annot_surv_med) {
    surv_med_tbl <- h_tbl_median_surv(fit_km = fit_km, armval = armval)
    bg_fill <- if (isTRUE(control_annot_surv_med[["fill"]])) "#00000020" else control_annot_surv_med[["fill"]]

    gg_surv_med <- df2gg(surv_med_tbl, font_size = font_size, colwidths = c(1, 1, 2), bg_fill = bg_fill) +
      theme(
        axis.text.y = element_text(size = font_size, face = "italic", hjust = 1),
        plot.margin = margin(0, 2, 0, 5)
      ) +
      coord_cartesian(clip = "off", ylim = c(0.5, nrow(surv_med_tbl) + 1.5))
    gg_surv_med <- suppressMessages(
      gg_surv_med +
        scale_x_continuous(expand = c(0.025, 0)) +
        scale_y_continuous(labels = rev(rownames(surv_med_tbl)), breaks = seq_len(nrow(surv_med_tbl)))
    )

    gg_plt <- cowplot::ggdraw(gg_plt) +
      cowplot::draw_plot(
        gg_surv_med, control_annot_surv_med[["x"]], control_annot_surv_med[["y"]],
        width = control_annot_surv_med[["w"]], height = control_annot_surv_med[["h"]],
        vjust = 0.5, hjust = 0.5
      )
  }

  # 3. Cox-PH annotation table
  if (!is.null(coxph_tbl)) {
    # coxph_tbl is pre-computed outside g_km, just plot it
    bg_fill <- if (isTRUE(control_annot_coxph[["fill"]])) "#00000020" else control_annot_coxph[["fill"]]

    gg_coxph <- df2gg(coxph_tbl, font_size = font_size, colwidths = c(1.1, 1, 3), bg_fill = bg_fill) +
      theme(
        axis.text.y = element_text(size = font_size, face = "italic", hjust = 1),
        plot.margin = margin(0, 2, 0, 5)
      ) +
      coord_cartesian(clip = "off", ylim = c(0.5, nrow(coxph_tbl) + 1.5))
    gg_coxph <- suppressMessages(
      gg_coxph +
        scale_x_continuous(expand = c(0.025, 0)) +
        scale_y_continuous(labels = rev(rownames(coxph_tbl)), breaks = seq_len(nrow(coxph_tbl)))
    )

    gg_plt <- cowplot::ggdraw(gg_plt) +
      cowplot::draw_plot(
        gg_coxph, control_annot_coxph[["x"]], control_annot_coxph[["y"]],
        width = control_annot_coxph[["w"]], height = control_annot_coxph[["h"]],
        vjust = 0.5, hjust = 0.5
      )
  }

  # add at risk annotation table (unchanged logic)
  if (annot_at_risk) {
    annot_tbl <- summary(fit_km, times = xticks, extend = TRUE)
    annot_tbl <- if (is.null(fit_km$strata)) {
      data.frame(
        n.risk = annot_tbl$n.risk, time = annot_tbl$time, strata = armval
      )
    } else {
      strata_lst <- strsplit(sub("=", "equals", levels(annot_tbl$strata)), "equals")
      levels(annot_tbl$strata) <- matrix(unlist(strata_lst), ncol = 2, byrow = TRUE)[, 2]
      data.frame(
        n.risk = annot_tbl$n.risk, time = annot_tbl$time, strata = annot_tbl$strata
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
      labs(title = if (annot_at_risk_title) "Patients at Risk:" else NULL, x = xlab) +
      theme_bw(base_size = font_size) +
      theme(
        plot.title = element_text(size = font_size, vjust = 3, face = "bold"),
        panel.border = element_blank(), panel.grid = element_blank(),
        axis.title.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = font_size, face = "italic", hjust = 1),
        axis.text.x = element_text(size = font_size), axis.line.x = element_line()
      ) +
      coord_cartesian(clip = "off", ylim = c(0.5, nrow(at_risk_tbl)))
    gg_at_risk <- suppressMessages(
      gg_at_risk +
        scale_x_continuous(expand = c(0.025, 0), breaks = seq_along(at_risk_tbl) - 0.5, labels = xticks) +
        scale_y_continuous(labels = rev(levels(annot_tbl$strata)), breaks = seq_len(nrow(at_risk_tbl)))
    )

    if (!as_list) {
      gg_plt <- cowplot::plot_grid(
        gg_plt, gg_at_risk,
        align = "v", axis = "tblr", ncol = 1,
        rel_heights = c(rel_height_plot, 1 - rel_height_plot)
      )
    }
  }

  if (as_list) {
    list(plot = gg_plt, table = gg_at_risk)
  } else {
    gg_plt
  }
}
