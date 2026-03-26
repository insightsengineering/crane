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
