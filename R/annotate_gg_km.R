#' Annotate Kaplan-Meier Plot
#'
#' @description
#' These functions provide capabilities to annotate Kaplan-Meier plots ([gg_km()]) with additional summary tables,
#' including median survival times, numbers at risk, and cox proportional hazards results.
#' The annotations are added using the `cowplot` package for flexible placement.
#'
#' @seealso [gg_km()], [process_survfit()], and [get_cox_pairwise_df()] for related functionalities.
#'
#' @examples
#' # Preparing the Kaplan-Meier Plot
#' use_lung <- survival::lung
#' use_lung$arm <- factor(sample(c("A", "B", "C"), nrow(use_lung), replace = TRUE))
#' use_lung$status <- use_lung$status - 1 # Convert status to 0/1
#' use_lung <- na.omit(use_lung)
#'
#' formula <- survival::Surv(time, status) ~ arm
#' fit_kmg01 <- survival::survfit(formula, use_lung)
#' surv_plot_data <- process_survfit(fit_kmg01)
#'
#' plt_kmg01 <- gg_km(surv_plot_data)
#'
#' @name annotate_gg_km
NULL

#' @describeIn annotate_gg_km The `annotate_surv_med` function adds a median survival time summary table as an
#'   annotation box.
#'
#' @param gg_plt (`ggplot2` or `cowplot`)\cr
#'   The primary plot object (either a `ggplot2` or `cowplot` object) of the Kaplan-Meier plot to which the median
#'   survival table annotation will be added.
#' @param fit_km (`survfit`)\cr
#'   A fitted Kaplan-Meier object of class `survfit` (from the `survival` package). This object contains the necessary
#'   survival data used to calculate and generate the content displayed in the annotation table.
#' @param ... Additional arguments passed to the control list for the annotation box.
#'   These arguments override the default values.
#'   Accepted arguments include:
#'   \itemize{
#'     \item \code{x} (\code{numeric}): X-coordinate for the box anchor position (0 to 1). Default is
#'       \code{0.8} (\code{0.29} for `annotate_coxph`).
#'     \item \code{y} (\code{numeric}): Y-coordinate for the box anchor position (0 to 1). Default is
#'       \code{0.85} (\code{0.51} for `annotate_coxph`).
#'     \item \code{w} (\code{numeric}): Width of the annotation box (0 to 1). Default is
#'       \code{0.32} (\code{0.4} for `annotate_coxph`).
#'     \item \code{h} (\code{numeric}): Height of the annotation box (0 to 1). Default
#'       is \code{0.16} (\code{0.125} for `annotate_coxph`).
#'     \item \code{fill} (\code{logical}): Whether the annotation box should have a background fill. Default is
#'       \code{TRUE}.
#'     \item \code{font_size} (\code{numeric}): Base font size for the text inside the annotation box. Default
#'       is \code{10}.
#'   }
#'
#' @return The function `annotate_surv_med` returns a `cowplot` object with the median survival table annotation
#'   added, ready for final display or saving.
#'
#' @examples
#' # Annotate Kaplan-Meier Plot with Median Survival Table
#' annotate_surv_med(plt_kmg01, fit_kmg01)
#'
#' @export
annotate_surv_med <- function(gg_plt, fit_km, ...) {
  set_cli_abort_call()
  default_eargs <- list(
    x = 0.8,
    y = 0.85,
    w = 0.32,
    h = 0.16,
    font_size = 10,
    fill = TRUE
  )
  eargs <- list(...)
  eargs <- utils::modifyList(default_eargs, eargs)

  # Checks
  check_class(fit_km, "survfit")
  check_class(gg_plt, c("gg", "ggplot", "cowplot"))

  # Check position/size (x, y, w, h, font_size) - Must be single non-missing numeric
  for (arg_name in c("x", "y", "w", "h", "font_size")) {
    check_numeric(eargs[[arg_name]])
  }
  check_logical(eargs[["fill"]])

  # Determine strata_levels for h_tbl_median_surv, assuming it's available in the calling environment or logic should
  # be updated. For now, keeping as is, but this typically requires strata_levels or inferring it from fit_km
  strata_levels <- if (is.null(fit_km$strata)) "All" else levels(fit_km$strata) # Placeholder for strata_levels

  surv_med_tbl <- h_tbl_median_surv(fit_km = fit_km, strata_levels = strata_levels)
  bg_fill <- if (isTRUE(eargs[["fill"]])) "#00000020" else eargs[["fill"]]

  gg_surv_med <- df2gg(surv_med_tbl, font_size = eargs[["font_size"]], colwidths = c(1, 1, 2), bg_fill = bg_fill) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = eargs[["font_size"]], face = "italic", hjust = 1),
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
      gg_surv_med, eargs[["x"]], eargs[["y"]],
      width = eargs[["w"]], height = eargs[["h"]],
      vjust = 0.5, hjust = 0.5
    )
  gg_plt
}

#' @describeIn annotate_gg_km The function `annotate_risk` adds a "Numbers at Risk" table below a
#'   Kaplan-Meier plot ([gg_km()]) using `cowplot::plot_grid`.
#'
#' @param gg_plt (`ggplot2` or `cowplot`)\cr
#'   The primary plot object (either a `ggplot2` or `cowplot` object) of the Kaplan-Meier plot.
#' @param fit_km (`survfit`)\cr
#'   A fitted Kaplan-Meier object of class `survfit` (from the `survival` package). This object contains
#'   the necessary survival data used to calculate the numbers at risk.
#' @param title (`string`)\cr
#'   A single logical value indicating whether to include a above the table. Defaults to
#'   `""Patients at Risk:""`. If `NULL`, no title is added.
#' @param rel_height_plot (`numeric`)\cr
#'   A single numeric value defining the **relative height** of the main Kaplan-Meier plot area compared
#'   to the 'at-risk' table. This value should be between 0 and 1, where a value closer to 1 gives the main plot
#'   more vertical space. Defaults to `0.75`.
#' @param xlab (`character`)\cr
#'   A single character string for the **x-axis label** on the 'at-risk' table. This typically represents
#'   time (e.g., "Time (Days)").
#' @return The function `annotate_risk` returns a `cowplot` object combining the KM plot and the 'Numbers at Risk'
#'   table.
#' @examples
#' # Annotate Plot with Numbers at Risk Table
#' annotate_risk(plt_kmg01, fit_kmg01)
#'
#' # Change order of y-axis (arm)
#' use_lung2 <- use_lung
#' use_lung2$arm <- factor(use_lung2$arm, levels = c("C", "B", "A"))
#' fit_kmg01 <- survival::survfit(formula, use_lung2)
#' annotate_risk(plt_kmg01, fit_kmg01) # rerun gg_km to change legend order
#'
#' @export
annotate_risk <- function(gg_plt, fit_km, title = "Patients at Risk:",
                          rel_height_plot = 0.75, xlab = "Days",
                          ...) {
  check_class(gg_plt, c("gg", "ggplot", "cowplot"))
  check_class(fit_km, "survfit")
  check_string(title, allow_empty = TRUE)
  check_scalar(rel_height_plot)
  check_numeric(rel_height_plot)
  if (rel_height_plot <= 0 || rel_height_plot >= 1) {
    cli::cli_abort(
      "{.arg rel_height_plot} must be a single {.cls numeric} value between 0 and 1 (exclusive).",
      call = get_cli_abort_call()
    )
  }
  check_string(xlab)
  default_eargs <- list(
    font_size = 10
  )
  eargs <- list(...)
  eargs <- utils::modifyList(default_eargs, eargs)
  font_size <- eargs[["font_size"]]
  check_numeric(font_size)

  data <- broom::tidy(fit_km)
  xticks <- h_xticks(data = data)
  annot_tbl <- summary(fit_km, times = xticks, extend = TRUE)

  # Placeholder for strata_levels, should be retrieved from fit_km or passed as argument
  strata_levels <- if (is.null(fit_km$strata)) "All" else levels(fit_km$strata)

  annot_tbl <- if (is.null(fit_km$strata)) {
    data.frame(
      n.risk = annot_tbl$n.risk,
      time = annot_tbl$time,
      strata = strata_levels
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

  at_risk_tbl <- as.data.frame(
    tidyr::pivot_wider(annot_tbl, names_from = "time", values_from = "n.risk")[, -1]
  )
  at_risk_tbl[is.na(at_risk_tbl)] <- 0
  rownames(at_risk_tbl) <- levels(annot_tbl$strata)

  gg_at_risk <- df2gg(
    at_risk_tbl,
    font_size = eargs$font_size, col_labels = FALSE, hline = FALSE,
    colwidths = rep(1, ncol(at_risk_tbl)),
    add_proper_xaxis = TRUE
  ) +
    ggplot2::labs(title = if (!is.null(title)) title else NULL, x = xlab) +
    ggplot2::theme_bw(base_size = eargs$font_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = eargs$font_size, vjust = 3, face = "bold"),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = eargs$font_size, face = "italic", hjust = 1),
      axis.text.x = ggplot2::element_text(size = eargs$font_size),
      axis.line.x = ggplot2::element_line()
    ) +
    ggplot2::coord_cartesian(clip = "off", ylim = c(0.5, nrow(at_risk_tbl)))

  gg_plt <- cowplot::plot_grid(
    gg_plt, gg_at_risk,
    align = "vh", axis = "b", ncol = 1,
    rel_heights = c(rel_height_plot, 1 - rel_height_plot)
  )
  gg_plt
}


#' @describeIn annotate_gg_km The function `annotate_coxph()` adds a Cox Proportional Hazards summary table created by
#' the function [get_cox_pairwise_df()] as an annotation box.
#'
#' @param gg_plt (`ggplot2` or `cowplot`)\cr
#'   The primary plot object (either a `ggplot2` or `cowplot` object) of the Kaplan-Meier plot to which
#'   the Cox-PH annotation table will be added.
#' @param coxph_tbl (`data.frame`)\cr
#'   A data frame containing the pre-calculated Cox-PH results, typically from a function like `get_cox_pairwise_df`.
#'   This data is used to generate the annotation table content.
#' @param ... Additional arguments passed to the control list for the annotation box.
#'   These arguments override the default values.
#'   Accepted arguments include:
#'   \itemize{
#'     \item \code{x} (`numeric`): X-coordinate for the box anchor position (0 to 1). Default is \code{0.29}.
#'     \item \code{y} (`numeric`): Y-coordinate for the box anchor position (0 to 1). Default is \code{0.51}.
#'     \item \code{w} (`numeric`): Width of the annotation box (0 to 1). Default is \code{0.4}.
#'     \item \code{h} (`numeric`): Height of the annotation box (0 to 1). Default is \code{0.125}.
#'   }
#'
#' @return The function `annotate_coxph` returns a `cowplot` object with the Cox-PH table annotation added.
#'
#' @examples
#' # Annotate Kaplan-Meier Plot with Cox-PH Table
#' coxph_tbl <- get_cox_pairwise_df(formula, data = use_lung, arm = "arm", ref_group = "A")
#' annotate_coxph(plt_kmg01, coxph_tbl)
#'
#' @export
annotate_coxph <- function(gg_plt, coxph_tbl, ...) {
  set_cli_abort_call()
  default_eargs <- list(
    x = 0.29,
    y = 0.51,
    w = 0.4,
    h = 0.125,
    fill = TRUE,
    font_size = 10
  )
  eargs <- list(...)
  eargs <- utils::modifyList(default_eargs, eargs)

  # Check position/size (x, y, w, h, font_size) - Must be single non-missing numeric
  for (arg_name in c("x", "y", "w", "h", "font_size")) {
    check_numeric(eargs[[arg_name]])
  }
  check_logical(eargs[["fill"]])

  bg_fill <- if (isTRUE(eargs[["fill"]])) "#00000020" else eargs[["fill"]]

  gg_coxph <- df2gg(coxph_tbl, font_size = eargs$font_size, colwidths = c(1.1, 1, 3), bg_fill = bg_fill) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = eargs$font_size, face = "italic", hjust = 1),
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
      gg_coxph, eargs[["x"]], eargs[["y"]],
      width = eargs[["w"]], height = eargs[["h"]],
      vjust = 0.5, hjust = 0.5
    )

  gg_plt
}
