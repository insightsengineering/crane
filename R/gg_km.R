#' Kaplan-Meier Plot
#'
#' @description
#' This set of functions facilitates the creation of Kaplan-Meier survival plots using `ggplot2`. Use
#' `process_survfit()` to prepare the survival data from a fitted `survfit` object, and then
#' `gg_km()` to generate the Kaplan-Meier plot with various customization options. Additional functions
#' like `annot_surv_med()`, `annot_cox_ph()`, and `annotate_riskdf()` allow for adding summary tables and
#' annotations to the plot.
#'
#' @name gg_km
NULL

#' @describeIn gg_km takes a fitted [survfit] object and processes it into a data frame
#'   suitable for plotting a Kaplan-Meier curve with `ggplot2`. Time zero is also added to the data.
#'
#' @param fit_km A fitted Kaplan-Meier object of class `survfit`.
#' @param strata_levels (`string`)\cr
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
#' use_lung <- survival::lung
#' use_lung$arm <- factor(sample(c("A", "B", "C"), nrow(use_lung), replace = TRUE))
#' use_lung$status <- use_lung$status - 1 # Convert status to 0/1
#' use_lung <- na.omit(use_lung)
#'
#' # Fit Kaplan-Meier model
#' formula <- survival::Surv(time, status) ~ arm
#' fit_kmg01 <- survival::survfit(formula, use_lung)
#'
#' # Process survfit data for plotting
#' surv_plot_data <- process_survfit(fit_kmg01)
#' head(surv_plot_data)
#'
#' @export
process_survfit <- function(fit_km,
                            strata_levels = "All",
                            max_time = NULL) {
  set_cli_abort_call()

  # Input checks
  if (!inherits(fit_km, "survfit")) {
    cli::cli_abort(
      "The input {.arg fit_km} must be a fitted Kaplan-Meier object of class {.cls survfit}.",
      call = get_cli_abort_call()
    )
  }
  check_string(strata_levels)
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
    y$strata <- strata_levels
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
#'   A numeric vector of **line types** (e.g., `1` for solid, `2` for dashed) for the survival curves, or `NULL` for
#'   `ggplot2` defaults. The length should match the number of arms/groups.
#' @param lwd (`numeric`)\cr
#'   A single numeric value specifying the **line width** for the survival curves.
#' @param censor_show (`logical`)\cr
#'   A single logical value indicating whether to display **censoring marks** on the plot. Defaults to `TRUE`.
#' @param size (`numeric`)\cr
#'   A single numeric value specifying the **size** of the censoring marks.
#' @param max_time (`numeric`)\cr
#'   A single numeric value defining the **maximum time point** to display on the x-axis.
#' @param xticks (`numeric` or `NULL`)\cr
#'   A numeric vector of explicit **x-axis tick positions**, or a single numeric value representing the **interval**
#'   between ticks, or `NULL` for automatic `ggplot2` scaling.
#' @param yval (`character`)\cr
#'   A single character string, either `"Survival"` or `"Failure"` to plot the corresponding probability.
#' @param ylim (`numeric`)\cr
#'   A **numeric vector of length 2** defining the lower and upper limits of the y-axis (e.g., `c(0, 1)`).
#' @param font_size (`numeric`)\cr
#'   A single numeric value specifying the **base font size** for the plot theme elements.
#' @param legend_pos (`numeric` or `NULL`)\cr
#'   A **numeric vector of length 2** defining the **legend position** as (x, y) coordinates relative to the plot
#'   area (ranging from 0 to 1), or `NULL` for automatic placement.
#'
#' @return The function `gg_km` returns a `ggplot2` object of the KM plot.
#'
#' @examples
#' # Example of making the KM plot
#' plt_kmg01 <- gg_km(surv_plot_data)
#'
#' # Confidence Interval as Ribbon
#' plt_kmg01 +
#'   ggplot2::geom_ribbon(alpha = 0.3, lty = 0, na.rm = TRUE)
#'
#' # Adding Title and Footnotes
#' plt_kmg01 +
#'   ggplot2::labs(title = "title", caption = "footnotes")
#'
#' # Changing xlab and ylab
#' plt_kmg01 +
#'   ggplot2::xlab("Another Day") +
#'   ggplot2::ylab("THE Survival Probability")
#'
#' @export
gg_km <- function(surv_plot_data,
                  lty = NULL,
                  lwd = 0.5,
                  censor_show = TRUE,
                  size = 2,
                  max_time = NULL,
                  xticks = NULL,
                  yval = c("Survival", "Failure"),
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
      "{.code time}, {.code estimate}, {.code conf.low}, {.code conf.high}, ",
      "{.code strata}, {.code n.censor}, and {.code censor}.",
      call = get_cli_abort_call()
    )
  }
  if (nrow(surv_plot_data) < 1) {
    cli::cli_abort(
      "The input {.arg surv_plot_data} must contain at least one row of data.",
      call = get_cli_abort_call()
    )
  }
  check_numeric(ylim, allow_empty = TRUE)
  check_scalar_logical(censor_show)

  data <- surv_plot_data
  strata_levels <- levels(data$strata)

  yval <- match.arg(yval)
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

  xticks <- h_xticks(data = data, xticks = xticks, max_time = max_time)

  gg_plt <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data[["time"]], y = .data[["estimate"]], ymin = .data[["conf.low"]],
      ymax = .data[["conf.high"]], color = .data[["strata"]], fill = .data[["strata"]]
    )
  ) +
    ggplot2::theme_bw(base_size = font_size) +
    ggplot2::scale_y_continuous(limits = ylim) +
    ggplot2::labs(x = "Days", y = paste(yval, "Probability")) +
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
      breaks = xticks, limits = c(min(0, xticks), max(c(xticks, max_time)))
    )
  } else if (!is.null(xticks)) {
    if (max(data$time) <= max(xticks)) {
      gg_plt <- gg_plt + ggplot2::scale_x_continuous(
        breaks = xticks, limits = c(min(0, min(xticks)), max(xticks))
      )
    } else {
      gg_plt <- gg_plt + ggplot2::scale_x_continuous(breaks = xticks)
    }
  } else if (!is.null(max_time)) {
    gg_plt <- gg_plt + ggplot2::scale_x_continuous(limits = c(0, max_time))
  }

  if (!is.null(legend_pos)) {
    gg_plt <- gg_plt + ggplot2::theme(legend.position.inside = legend_pos)
  } else {
    max_time2 <- sort(
      data$time,
      partial = nrow(data) - length(strata_levels) - 1
    )[nrow(data) - length(strata_levels) - 1]

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

  if (isTRUE(censor_show)) {
    gg_plt <- gg_plt + ggplot2::geom_point(
      data = data[data$n.censor != 0, ],
      ggplot2::aes(x = .data[["time"]], y = .data[["censor"]], shape = "Censored"),
      size = size,
      na.rm = TRUE
    ) +
      ggplot2::scale_shape_manual(values = 3) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = NA)))
  }

  gg_plt
}
