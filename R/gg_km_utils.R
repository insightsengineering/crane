f_conf_level <- function(conf_level) {
  # assert_proportion_value(conf_level) # Assuming assert_proportion_value is defined elsewhere
  paste0(conf_level * 100, "% CI")
}

control_surv_med_annot <- function(x = 0.8, y = 0.85, w = 0.32, h = 0.16, fill = TRUE) {
  list(x = x, y = y, w = w, h = h, fill = fill)
}

control_coxph_annot <- function(x = 0.29, y = 0.51, w = 0.4, h = 0.125, fill = TRUE, ref_lbls = FALSE) {
  checkmate::assert_logical(ref_lbls, any.missing = FALSE)

  res <- c(control_surv_med_annot(x = x, y = y, w = w, h = h), list(ref_lbls = ref_lbls))
  res
}

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
#'
#' @return A \code{ggplot2} object representing the table.
#'
#' @keywords internal
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
#' @param model_formula A [stats::formula] object specifying the survival model, typically in the form \code{Surv(time, status) ~ arm + covariates}.
#' @param data A `data.frame` containing the survival data, including time, status, and the arm variable.
#' @param arm A character string specifying the name of the column in \code{data} that contains the grouping/treatment arm variable (must be a factor-like variable).
#' @param ref_group A character string specifying the level of the \code{arm} variable to be used as the **reference group** for all pairwise comparisons. If \code{NULL} (the default), the **first unique level** of the \code{arm} column is used as the reference group.
#'
#' @return A `data.frame` with the results of the pairwise comparisons. The columns include:
#' \itemize{
#'   \item \code{arm}: The comparison arm being tested against the reference group.
#'   \item \code{hr}: The Hazard Ratio (HR) for the comparison arm vs. the reference arm, formatted to two decimal places.
#'   \item \code{ci}: The 95\% confidence interval for the HR, presented as a string in the format "(lower, upper)", with values formatted to two decimal places.
#'   \item \code{pval}: The log-rank p-value for the comparison.
#' }
#'
#' @details The function iterates through each unique arm (excluding the reference group), filters the data to include only the current comparison arm and the reference arm, and then fits a Cox model (\code{\link[survival]{coxph}}) and performs a log-rank test (\code{\link[survival]{survdiff}}). The Hazard Ratio and its 95\% confidence interval are extracted from the Cox model summary, and the p-value is calculated from the log-rank test.
#'
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
#'
#' @export
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

