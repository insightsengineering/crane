#' ANCOVA Table
#'
#' @description
#' Builds a table of adjusted means and treatment-vs-control contrasts
#' from a linear model (ANCOVA). The table displays, for each treatment group:
#' - n (number of observations)
#' - Adjusted Mean (least-squares mean)
#' - Difference in Adjusted Means (vs reference group)
#' - Confidence interval for the difference
#' - p-value for the difference
#'
#' The model is fit via [`cardx::construct_model()`] and least-squares
#' estimates are obtained with the `emmeans` package. Contrasts use
#' `emmeans::contrast(method = "trt.vs.ctrl")` so that each non-reference
#' group is compared to the reference group only.
#'
#' @param data (`data.frame`)\cr
#'   Analysis data set, typically one parameter/visit subset.
#' @param formula (`formula`)\cr
#'   Model formula passed to the fitting function. The left-hand side is the
#'   response (e.g. `CHG`), the right-hand side includes the treatment variable
#'   and any covariates (e.g. `CHG ~ TRT01A + BASE`).
#' @param by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   Column in `data` identifying treatment groups. This variable must also
#'   appear on the right-hand side of `formula`. Used for column headers.
#' @param ref_group (`string`)\cr
#'   Level of `by` that is the reference (control) group. Contrasts are
#'   computed as each non-reference group minus this group.
#' @param conf.level (scalar `numeric`)\cr
#'   Confidence level for the difference in adjusted means.
#'   Default is `0.95`.
#' @param method (`string`)\cr
#'   Modelling function name. Default is `"lm"`.
#' @param method.args (`list`)\cr
#'   Additional arguments passed to the modelling function.
#' @param package (`string`)\cr
#'   Package exporting `method`. Default is `"stats"`.
#' @param adjust (`string`)\cr
#'   Multiplicity adjustment method for contrasts, passed to
#'   [`emmeans::contrast()`]. Common choices: `"none"` (no adjustment,
#'   default), `"dunnett"` (Dunnett's method), `"bonferroni"`, `"tukey"`.
#'   See [`emmeans::summary.emmGrid()`] for all options.
#' @param label (`string`)\cr
#'   Label for the top-level row in the table. Defaults to the label attribute
#'   of the response variable, or the variable name if no label exists.
#'   When used inside `tbl_strata(strata = PARAM)`, pass the parameter value
#'   (e.g. `unique(data$PARAM)`) so each sub-table is labelled correctly.
#' @param denominator (`data.frame`)\cr
#'   Optional data frame used to compute the header Ns (typically `ADSL`).
#'   When supplied, the column headers show `(N = <count>)` from this data
#'   frame rather than from `data`.
#'
#' @return A `'gtsummary'` table of class `c("tbl_ancova", "gtsummary")`.
#' @name tbl_ancova
#'
#' @examples
#' # Simple ANCOVA with baseline covariate
#' cards::ADLB |>
#'   dplyr::filter(PARAMCD == "SODIUM", AVISIT == "Week 8") |>
#'   tbl_ancova(
#'     formula = CHG ~ TRTA + BASE,
#'     by = TRTA,
#'     ref_group = "Placebo"
#'   )
#'
#' # With denominator for header Ns
#' cards::ADLB |>
#'   dplyr::filter(PARAMCD == "SODIUM", AVISIT == "Week 8") |>
#'   tbl_ancova(
#'     formula = CHG ~ TRTA + BASE,
#'     by = TRTA,
#'     ref_group = "Placebo",
#'     denominator = cards::ADSL
#'   )
#'
#' # With custom label (e.g. parameter name inside tbl_strata)
#' cards::ADLB |>
#'   dplyr::filter(PARAMCD == "SODIUM", AVISIT == "Week 8") |>
#'   tbl_ancova(
#'     formula = CHG ~ TRTA + BASE,
#'     by = TRTA,
#'     ref_group = "Placebo",
#'     label = "Sodium (mmol/L)"
#'   )
#'
#' # With Dunnett's multiplicity adjustment
#' cards::ADLB |>
#'   dplyr::filter(PARAMCD == "SODIUM", AVISIT == "Week 8") |>
#'   tbl_ancova(
#'     formula = CHG ~ TRTA + BASE,
#'     by = TRTA,
#'     ref_group = "Placebo",
#'     adjust = "dunnett"
#'   )
#'
#' # Custom column order — set factor levels on the input data
#' cards::ADLB |>
#'   dplyr::filter(PARAMCD == "SODIUM", AVISIT == "Week 8") |>
#'   dplyr::mutate(TRTA = factor(TRTA, levels = c(
#'     "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"
#'   ))) |>
#'   tbl_ancova(
#'     formula = CHG ~ TRTA + BASE,
#'     by = TRTA,
#'     ref_group = "Placebo"
#'   )
#'
#' @export
tbl_ancova <- function(data,
                       formula,
                       by,
                       ref_group,
                       conf.level = 0.95,
                       method = "lm",
                       method.args = list(),
                       package = "stats",
                       adjust = "none",
                       label = NULL,
                       denominator = NULL) {
  set_cli_abort_call()

  # input checks ---------------------------------------------------------------
  check_not_missing(data)
  check_not_missing(formula)
  check_not_missing(ref_group)

  check_class(data, "data.frame")
  check_class(formula, "formula")
  check_string(ref_group)
  check_string(method)
  check_string(package)
  check_scalar(conf.level)
  check_range(conf.level, range = c(0, 1))
  check_string(adjust)
  if (!is.null(label)) check_string(label)
  check_pkg_installed("emmeans")

  by <- dplyr::select(data, {{ by }}) |> names()
  check_scalar(by, allow_empty = FALSE)

  if (!ref_group %in% unique(data[[by]])) {
    cli::cli_abort(
      "{.arg ref_group} value {.val {ref_group}} not found in column {.val {by}}.",
      call = get_cli_abort_call()
    )
  }

  # relevel so ref_group is first <U+2014> emmeans uses the first level as reference
  data[[by]] <- stats::relevel(factor(data[[by]]), ref = ref_group)

  # fit model and compute emmeans ----------------------------------------------
  mod <- cardx::construct_model(
    data = data, formula = formula, method = method,
    method.args = {{ method.args }}, package = package
  )

  emmeans_specs <- stats::reformulate(by)
  emm <- emmeans::emmeans(mod, specs = emmeans_specs)

  # LS means -------------------------------------------------------------------
  emm_summary <- summary(emm, calc = c(n = ".wgt.")) |>
    dplyr::as_tibble() |>
    dplyr::rename(
      estimate = dplyr::any_of(c("emmean", "prob")),
      n = dplyr::any_of("n")
    )

  # contrasts (treatment vs control) -------------------------------------------
  contr_summary <- emmeans::contrast(emm, method = "trt.vs.ctrl", ref = ref_group, adjust = adjust) |>
    summary(infer = TRUE, level = conf.level) |>
    dplyr::as_tibble() |>
    dplyr::rename(
      conf.low = dplyr::any_of(c("lower.CL", "asymp.LCL")),
      conf.high = dplyr::any_of(c("upper.CL", "asymp.UCL"))
    )


  # parse contrast labels to extract the non-reference group name
  # e.g. "Xanomeline High Dose - Placebo" -> "Xanomeline High Dose"
  contr_summary$trt_group <- sub(
    paste0("\\s*-\\s*", .escape_regex(ref_group), "$"), "",
    contr_summary$contrast
  )

  # build ARD for tbl_ard_summary ----------------------------------------------
  trt_levels <- levels(data[[by]])
  endpoint_var <- all.vars(formula)[1]
  endpoint_label <- label %||% attr(data[[endpoint_var]], "label") %||% endpoint_var

  ard <- .build_ancova_ard(
    emm_summary = emm_summary,
    contr_summary = contr_summary,
    by = by,
    trt_levels = trt_levels,
    ref_group = ref_group,
    endpoint_label = endpoint_label,
    conf.level = conf.level
  )

  # build gtsummary table ------------------------------------------------------
  ci_pct <- paste0(round(conf.level * 100), "%")

  tbl <- gtsummary::tbl_ard_summary(
    cards = ard,
    by = dplyr::all_of(by),
    type = everything() ~ "continuous2",
    statistic = everything() ~ c(
      "{n}",
      "{estimate}",
      "{mean.diff}",
      paste0("{conf.low}, {conf.high}"),
      "{p.value}"
    ),
  ) |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::mutate(
          label = dplyr::case_when(
            .data$label == "n" ~ "n",
            .data$label == "Mean" ~ "Adjusted Mean",
            .data$label == "Mean Difference" ~ "Difference in Adjusted Means",
            .data$label == "CI Lower Bound, CI Upper Bound" ~
              paste0(ci_pct, " CI for Difference in Adjusted Means"),
            .data$label == "p-value" ~ "p-value",
            TRUE ~ .data$label
          ),
          # blank out stats that don't apply to the reference group
          dplyr::across(
            dplyr::starts_with("stat_"),
            ~ dplyr::if_else(
              .data$label %in% c(
                "Difference in Adjusted Means",
                paste0(ci_pct, " CI for Difference in Adjusted Means"),
                "p-value"
              ) & grepl("NA", .x, fixed = TRUE),
              "",
              .x
            )
          )
        )
    )

  # add denominator header Ns if provided
  if (!is.null(denominator)) {
    check_class(denominator, "data.frame")
    if (by %in% names(denominator)) {
      # read the column-to-group mapping that tbl_ard_summary assigned
      col_map <- tbl$table_body |>
        dplyr::select(dplyr::starts_with("stat_")) |>
        names()
      # extract group labels from existing headers (format: "**Group**")
      existing_headers <- tbl$table_styling$header |>
        dplyr::filter(.data$column %in% col_map) |>
        dplyr::mutate(group = gsub("^\\*\\*|\\*\\*$", "", .data$label)) |>
        dplyr::select("column", "group")

      denom_n <- denominator |>
        dplyr::count(!!rlang::sym(by), name = "n")

      header_df <- existing_headers |>
        dplyr::left_join(denom_n, by = stats::setNames(by, "group"))

      header_labels <- stats::setNames(
        paste0("**", header_df$group, "**\n(N = ", header_df$n, ")"),
        header_df$column
      )
      tbl <- tbl |>
        gtsummary::modify_header(!!!header_labels)
    }
  }

  tbl |>
    structure(
      class = c("tbl_ancova", class(tbl)),
      by = by,
      ref_group = ref_group,
      conf.level = conf.level,
      adjust = adjust,
      method = method,
      package = package
    )
}


# Assembles an ARD (Analysis Results Dataset) from emmeans summaries.
# For each treatment level, creates rows for: n, adjusted mean (from emm_summary),
# and contrast stats mean difference, CI bounds, p-value (from contr_summary).
# Reference group gets NA for contrast stats so the table structure is uniform.
.build_ancova_ard <- function(emm_summary, contr_summary, by,
                              trt_levels, ref_group, endpoint_label,
                              conf.level) {
  ard_rows <- list()

  for (trt in trt_levels) {
    emm_row <- emm_summary[emm_summary[[by]] == trt, ]
    n_val <- as.integer(emm_row$n)
    est_val <- emm_row$estimate

    # base rows: n and adjusted mean
    ard_rows <- c(ard_rows, list(
      .ancova_ard_row(endpoint_label, by, trt, "n", n_val, "n", "integer"),
      .ancova_ard_row(endpoint_label, by, trt, "estimate", est_val, "Mean", "numeric")
    ))

    # contrast rows (only for non-reference groups)
    if (trt != ref_group) {
      contr_row <- contr_summary[contr_summary$trt_group == trt, ]
      if (nrow(contr_row) == 1) {
        ard_rows <- c(ard_rows, list(
          .ancova_ard_row(endpoint_label, by, trt, "mean.diff", contr_row$estimate, "Mean Difference", "numeric"),
          .ancova_ard_row(endpoint_label, by, trt, "conf.low", contr_row$conf.low, "CI Lower Bound", "numeric"),
          .ancova_ard_row(endpoint_label, by, trt, "conf.high", contr_row$conf.high, "CI Upper Bound", "numeric"),
          .ancova_ard_row(endpoint_label, by, trt, "p.value", contr_row$p.value, "p-value", "numeric")
        ))
      }
    } else {
      # reference group: fill with NA so the table structure is consistent
      ard_rows <- c(ard_rows, list(
        .ancova_ard_row(endpoint_label, by, trt, "mean.diff", NA_real_, "Mean Difference", "numeric"),
        .ancova_ard_row(endpoint_label, by, trt, "conf.low", NA_real_, "CI Lower Bound", "numeric"),
        .ancova_ard_row(endpoint_label, by, trt, "conf.high", NA_real_, "CI Upper Bound", "numeric"),
        .ancova_ard_row(endpoint_label, by, trt, "p.value", NA_real_, "p-value", "numeric")
      ))
    }
  }

  dplyr::bind_rows(ard_rows) |>
    dplyr::mutate(
      context = "continuous",
      fmt_fun = map(.data$stat_name, function(nm) {
        switch(nm,
          "n" = function(x) sprintf("%.0f", x),
          "estimate" = ,
          "mean.diff" = ,
          "conf.low" = ,
          "conf.high" = function(x) sub("^-0.00$", "0.00", sprintf("%.2f", x)),
          "p.value" = function(x) sprintf("%.4f", x),
          function(x) as.character(x)
        )
      })
    ) |>
    cards::as_card()
}


# Internal: create a single ARD row --------------------------------------------
.ancova_ard_row <- function(variable, group1, group1_level,
                            stat_name, stat, stat_label, var_type) {
  dplyr::tibble(
    variable = variable,
    var_type = var_type,
    var_label = variable,
    group1 = group1,
    group1_level = list(group1_level),
    stat_name = stat_name,
    stat_label = stat_label,
    stat = list(stat),
    warning = list(NULL),
    error = list(NULL)
  )
}


# Escapes regex special characters so group names can be used in sub() patterns.
# e.g. "Arm (High)" -> "Arm \\(High\\)", "Dose 1.5mg" -> "Dose 1\\.5mg"
.escape_regex <- function(x) {
  chars <- c(".", "|", "^", "$", "(", ")", "[", "]", "{", "}", "*", "+", "?", "\\")
  for (ch in chars) {
    x <- gsub(ch, paste0("\\", ch), x, fixed = TRUE)
  }
  x
}
