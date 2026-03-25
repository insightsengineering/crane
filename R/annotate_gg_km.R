#' Annotate Kaplan-Meier Plot
#'
#' @description
#' These functions provide capabilities to annotate Kaplan-Meier plots ([gg_km()])
#' with additional summary tables, including median survival times, numbers at
#' risk, and cox proportional hazards results. The annotations are added using
#' the `cowplot` package for flexible placement.
#'
#' @param gg_plt (`ggplot2` or `cowplot`)\cr
#'   The primary plot object of the Kaplan-Meier plot. Note: While floating
#'   tables accept `cowplot` objects, aligned tables (like the risk table)
#'   require a pure `ggplot2` object.
#' @param fit_km (`survfit`)\cr
#'   A fitted Kaplan-Meier object of class `survfit` (from the `survival`
#'   package). This object contains the necessary survival data used to
#'   calculate and generate the content displayed in the annotation table.
#' @param coxph_tbl (`data.frame`)\cr
#'   A data frame containing the pre-calculated Cox-PH results, derived
#'   using function `get_cox_pairwise_df()`.
#' @param title (`character`)\cr
#'   A single string value indicating whether to include a title above the
#'   table. Defaults to `"Patients at Risk:"`. If `NULL`, no title is added.
#' @param rel_height_plot (`numeric`)\cr
#'   A single numeric value defining the **relative height** of the main
#'   Kaplan-Meier plot area compared to the 'at-risk' table. This value should
#'   be between 0 and 1, where a value closer to 1 gives the main plot more
#'   vertical space. Defaults to `0.75`.
#' @param xlab (`character`)\cr
#'   A single character string for the **x-axis label** on the 'at-risk' table.
#'   This typically represents time (e.g., "Days").
#' @param table_position (`numeric`)\cr
#'   A named numeric vector `c(x, y, w, h)` defining the position and size of
#'   the floating table. `x` and `y` are the coordinates (0 to 1),
#'   while `w` and `h` represent width and height (0 to 1). Defaults vary
#'   by function.
#' @param ... Additional arguments passed to the control list for the annotation
#'   box. These arguments override the default values.\cr
#'   Accepted arguments include:
#'   \itemize{
#'     \item \code{fill} (\code{logical}): Whether the annotation box should
#'       have a background fill. Default is \code{TRUE}.
#'     \item \code{font_size} (\code{numeric}): Base font size for the text
#'       inside the annotation box. Default is \code{10}.
#'   }
#'
#' @seealso [gg_km()], [process_survfit()], and [get_cox_pairwise_df()] for
#'   related functionalities.
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

#' @describeIn annotate_gg_km The function `annotate_riskdf` adds a "Numbers
#'   at Risk" table below a Kaplan-Meier plot using `patchwork`.\cr
#'   **Note:** For this specific function, `gg_plt` must be a pure `ggplot2`
#'   object (not a combined `cowplot` object) because it requires exact X-axis
#'   extraction.
#'
#' @return The function `annotate_riskdf` returns a `cowplot` object combining
#'   the KM plot and the 'Numbers at Risk' table.
#'
#' @examples
#' # Annotate Plot with Numbers at Risk Table
#' annotate_riskdf(plt_kmg01, fit_kmg01)
#'
#' # Change order of y-axis (arm)
#' use_lung2 <- use_lung
#' use_lung2$arm <- factor(use_lung2$arm, levels = c("C", "B", "A"))
#' fit_kmg01 <- survival::survfit(formula, use_lung2)
#' annotate_riskdf(plt_kmg01, fit_kmg01) # rerun gg_km to change legend order
#'
#' @export
annotate_riskdf <- function(gg_plt,
                            fit_km,
                            title = "Patients at Risk:",
                            rel_height_plot = 0.75,
                            xlab = "Days",
                            ...) {
  # 1. Input Checks - Strictly enforcing pure ggplot objects----------------------
  is_cowplot <- inherits(gg_plt, "ggplot") &&
    any(vapply(
      gg_plt$layers,
      function(l) inherits(l$geom, "GeomDrawGrob"),
      logical(1)
    ))

  if (!inherits(gg_plt, c("gg", "ggplot")) || is_cowplot) {
    rlang::abort(
      paste0(
        "`gg_plt` must be a pure ggplot object (not a cowplot object) for",
        "`annotate_riskdf`."
      ),
      "i" = paste0(
        "cowplot objects are not supported because",
        "exact X-axis extraction is required."
      )
    )
  }

  if (!inherits(fit_km, "survfit")) {
    rlang::abort("`fit_km` must be a survfit object.")
  }

  if (rel_height_plot <= 0 || rel_height_plot >= 1) {
    rlang::abort("`rel_height_plot` must be between 0 and 1.")
  }

  default_eargs <- list(font_size = 10)
  eargs <- utils::modifyList(default_eargs, list(...))

  # 2. Extract Data and Timepoints----------------------------------------------
  data <- broom::tidy(fit_km)
  xticks <- h_xticks(data = data)
  annot_tbl <- summary(fit_km, times = xticks, extend = TRUE)

  # 3. Format Strata Levels-----------------------------------------------------
  strata_levels <- if (is.null(fit_km$strata)) "All" else levels(fit_km$strata)

  annot_tbl <- if (is.null(fit_km$strata)) {
    data.frame(
      n.risk = annot_tbl$n.risk,
      time = annot_tbl$time,
      strata = strata_levels
    )
  } else {
    strata_lst <- strsplit(
      sub("=", "equals", levels(annot_tbl$strata)),
      "equals"
    )
    levels(annot_tbl$strata) <- matrix(
      unlist(strata_lst),
      ncol = 2,
      byrow = TRUE
    )[, 2]

    data.frame(
      n.risk = annot_tbl$n.risk,
      time = annot_tbl$time,
      strata = annot_tbl$strata
    )
  }

  # 4. Pivot to Wide Risk Table-------------------------------------------------
  at_risk_tbl <- annot_tbl |>
    tidyr::pivot_wider(names_from = "time", values_from = "n.risk") |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) |>
    tibble::column_to_rownames(var = "strata") |>
    as.data.frame()

  # 5. Format the Row Labels (Italics)------------------------------------------
  km_y_labels <- parse(text = paste0('italic("', rownames(at_risk_tbl), '")'))

  # 6. Enforce vertical stacking for the legend inside the main plot------------
  gg_plt <- gg_plt +
    ggplot2::theme(legend.direction = "vertical")

  # 7. Call the Engine----------------------------------------------------------
  res <- df2gg_aligned(
    df = at_risk_tbl,
    gg_plt = gg_plt,
    type = "KM",
    y_labels = km_y_labels,
    title = title,
    xlab = xlab,
    show_xaxis = TRUE,
    text_size = eargs$font_size / ggplot2::.pt,
    label_size = eargs$font_size,
    rel_height_plot = rel_height_plot
  )

  res
}

#' @describeIn annotate_gg_km The `annotate_surv_med` function adds a
#'   median survival time summary table as an annotation box.
#'
#' @return The function `annotate_surv_med` returns a `cowplot` object\cr
#'   with the median survival table annotation added.
#'
#' @examples
#' # Annotate Kaplan-Meier Plot with Median Survival Table
#' annotate_surv_med(plt_kmg01, fit_kmg01)
#'
#' @export
annotate_surv_med <- function(gg_plt,
                              fit_km,
                              table_position = c(
                                x = 0.8,
                                y = 0.85,
                                w = 0.32,
                                h = 0.16
                              ),
                              ...) {
  set_cli_abort_call()

  default_eargs <- list(
    font_size = 10,
    fill = TRUE
  )

  eargs <- list(...)
  eargs <- utils::modifyList(default_eargs, eargs)

  # Check explicitly allows cowplot objects for floating tables
  if (!inherits(gg_plt, c("gg", "ggplot", "cowplot"))) {
    rlang::abort("`gg_plt` must be a ggplot or cowplot object.")
  }

  if (!inherits(fit_km, "survfit")) {
    rlang::abort("`fit_km` must be a survfit object.")
  }


  strata_levels <- if (is.null(fit_km$strata)) "All" else levels(fit_km$strata)

  surv_med_tbl <- h_tbl_median_surv(
    fit_km = fit_km,
    strata_levels = strata_levels
  )

  if (!identical(rownames(surv_med_tbl), as.character(seq_len(nrow(surv_med_tbl))))) {
    surv_med_tbl <- data.frame(
      " " = rownames(surv_med_tbl),
      surv_med_tbl,
      check.names = FALSE
    )
  }

  bg_fill <- if (isTRUE(eargs[["fill"]])) "#00000020" else eargs[["fill"]]

  # Call the floating table engine
  res <- df2gg_floating(
    df = surv_med_tbl,
    gg_plt = gg_plt,
    x = table_position["x"],
    y = table_position["y"],
    w = table_position["w"],
    h = table_position["h"],
    font_size = eargs[["font_size"]],
    colwidths = NULL,
    bg_fill = bg_fill
  )

  res
}

#' @describeIn annotate_gg_km The function `annotate_coxph()` adds a Cox
#'   Proportional Hazards summary table as an annotation box.
#'
#'
#' @return The function `annotate_coxph` returns a `cowplot` object\cr
#'   with the Cox-PH table annotation added.
#'
#' @examples
#' # Annotate Kaplan-Meier Plot with Cox-PH Table
#' coxph_tbl <- get_cox_pairwise_df(
#'   formula,
#'   data = use_lung, arm = "arm", ref_group = "A"
#' )
#' annotate_coxph(plt_kmg01, coxph_tbl)
#'
#' @export
annotate_coxph <- function(gg_plt,
                           coxph_tbl,
                           table_position = c(
                             x = 0.29,
                             y = 0.51,
                             w = 0.4,
                             h = 0.125
                           ),
                           ...) {
  set_cli_abort_call()

  default_eargs <- list(
    fill = TRUE,
    font_size = 10
  )

  eargs <- list(...)
  eargs <- utils::modifyList(default_eargs, eargs)

  # Check explicitly allows cowplot objects for floating tables
  if (!inherits(gg_plt, c("gg", "ggplot", "cowplot"))) {
    rlang::abort("`gg_plt` must be a ggplot or cowplot object.")
  }

  if (!inherits(coxph_tbl, "data.frame")) {
    rlang::abort("`coxph_tbl` must be a data.frame.")
  }

  if (!identical(rownames(coxph_tbl), as.character(seq_len(nrow(coxph_tbl))))) {
    coxph_tbl <- data.frame(
      " " = rownames(coxph_tbl),
      coxph_tbl,
      check.names = FALSE
    )
  }

  bg_fill <- if (isTRUE(eargs[["fill"]])) "#00000020" else eargs[["fill"]]

  # Call the floating table engine
  res <- df2gg_floating(
    df = coxph_tbl,
    gg_plt = gg_plt,
    x = table_position["x"],
    y = table_position["y"],
    w = table_position["w"],
    h = table_position["h"],
    font_size = eargs[["font_size"]],
    colwidths = NULL,
    bg_fill = bg_fill
  )

  res
}
