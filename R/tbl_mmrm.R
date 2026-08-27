#' Get and display MMRM Results in a Formatted Table
#'
#' These functions take a fitted MMRM model object and creates a formatted table,
#' following the style of the MMRM template. It combines baseline summary statistics
#' (if available) with the MMRM results, presenting them in a clear and organized manner.
#'
#' @param fit_mmrm (`mmrm` model object)\cr
#'   A fitted MMRM model object, typically created using the `mmrm` function from the `mmrm` package. This object
#'   should contain the necessary information to extract adjusted means, differences, confidence intervals, and
#'   p-values for the specified visits and arms.
#' @param conf_level (`numeric`)\cr
#'   The confidence level to use when calculating confidence intervals for the adjusted means and differences.
#'   Default is 0.95 for 95% confidence intervals.
#' @param weights (`string`)\cr
#'   The weighting scheme passed to [emmeans::emmeans()] when averaging the adjusted means over the levels of
#'   the model factors. Default is `"equal"`. Use `"proportional"` to weight by the observed group sizes.
#' @param mmrm_df (`data.frame`)\cr
#'   A tidy data frame containing the MMRM results. This should include
#'   columns for the visit, arm, adjusted means, differences, confidence intervals, and
#'   p-values. The data frame should be structured in a way that allows for stratification by visit and arm.
#'   Usually the output of [get_mmrm_results()].
#' @param base_df (`data.frame`)\cr
#'   A data frame containing baseline measurements. This should include columns for
#'   the visit, arm, and baseline values. The function will summarize this data
#'   to provide baseline statistics in the final table. If `base_df` is not provided
#'   or does not contain any rows, the function will simply omit the baseline summary
#'   section from the final table.
#' @param arm (`string`)\cr
#'   The column in `mmrm_df` and `base_df` that identifies the treatment arms.
#'   This will be used to divide the results in columns. First value is reference.
#' @param visit (`string`)\cr
#'   The column in `mmrm_df` and `base_df` that identifies the visits.
#'   This will be used to stratify the results in rows.
#' @param baseline_aval ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The column in `base_df` that contains the baseline values to be summarized.
#'   Unused if `base_df` is not provided or does not contain any rows.
#'
#' @seealso [gg_mmrm_lineplot()] for visualizing MMRM results.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("mmrm", quietly = TRUE)
#' library(mmrm)
#' fv_dt <- mmrm::fev_data |>
#'   dplyr::mutate(
#'     ARMCD = sprintf(
#'       "%s\n(N = %d)", ARMCD,
#'       table(mmrm::fev_data$ARMCD)[ARMCD]
#'     ),
#'     ARMCD = factor(ARMCD)
#'   )
#'
#' @name tbl_mmrm
NULL

#' @return A `data.frame` containing the estimated marginal means (adjusted means)
#' and contrasts (differences in adjusted means) for each visit and arm,
#' along with their standard errors, confidence intervals, degrees of freedom,
#' and sample sizes. This data frame is structured to facilitate the creation of a
#' formatted table using [tbl_mmrm()].
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("mmrm", quietly = TRUE)
#' # Fit an MMRM model using the FEV data
#' fit_mmrm <- mmrm::mmrm(
#'   # us -> unstructured cov structure
#'   formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
#'   data = fv_dt
#' )
#' mmrm_results <- get_mmrm_results(fit_mmrm, arm = "ARMCD", visit = "AVISIT", conf_level = 0.95)
#'
#' @rdname tbl_mmrm
#' @export
get_mmrm_results <- function(fit_mmrm, arm, visit, conf_level = 0.95,
                             weights = c("equal", "proportional", "outer", "cells", "flat")) {
  check_installed("emmeans")
  check_not_missing(fit_mmrm)
  check_not_missing(arm)
  check_not_missing(visit)
  check_not_missing(conf_level)
  check_string(arm)
  check_string(visit)
  check_class(fit_mmrm, "mmrm_fit")
  weights <- rlang::arg_match(weights)

  # NEW: Explicitly extract complete cases to perfectly match the model's dataset.
  # This guarantees emmeans calculates LS Means on the exact correct covariate averages.
  # -> To match standard clinical reporting (and SAS PROC MIXED), you must calculate LS Means on the
  #    complete cases only.
  # 1. Dynamically extract the raw data from the model call's environment
  # This prevents the user from having to pass 'data' manually!
  raw_data <- eval(fit_mmrm$call$data, envir = environment(stats::formula(fit_mmrm)))

  # 2. Extract complete cases to perfectly match the model's dataset
  # This guarantees emmeans calculates LS Means on the exact correct covariate averages
  model_vars <- all.vars(stats::formula(fit_mmrm))
  model_data <- stats::na.omit(raw_data[, model_vars, drop = FALSE])

  # Extract Statistics using emmeans
  emmeans_object <- emmeans::emmeans(
    fit_mmrm,
    data = model_data, # NEW: Pass our bulletproof complete cases
    specs = c(arm, visit),
    weights = weights
  )

  # Get n from the emmeans grid and rename the weight column to n
  visit_arm_grid <- emmeans_object@grid
  wgt_index <- match(".wgt.", names(visit_arm_grid))
  names(visit_arm_grid)[wgt_index] <- "n"
  visit_arm_grid$n <- as.integer(visit_arm_grid$n)
  # list with `object` (`emmGrid` object containing `emmeans` results) and `grid`
  # (`data.frame` containing the potential arm and the visit variables
  # together with the sample size `n` for each combination).
  emmeans_res <- list(object = emmeans_object, grid = visit_arm_grid)

  # Calculate confidence intervals for the emmeans results
  cis <- stats::confint(emmeans_res$object, level = conf_level)

  # Tidy up first part of the results and combine with confidence intervals and n
  estimates <- cbind(
    emmeans_res$grid[, setdiff(names(emmeans_res$grid), "n"), drop = FALSE],
    data.frame(estimate = cis$emmean, se = cis$SE, df = cis$df, lower_cl = cis$lower.CL, upper_cl = cis$upper.CL),
    emmeans_res$grid[, "n", drop = FALSE]
  )

  # Get least square means estimates for single visits, and possibly averaged visits.
  contrast_specs <- .get_single_visit_contrast_specs(emmeans_res, arm, visit)
  conts <- emmeans::contrast(
    emmeans_res$object,
    contrast_specs$coefs
  )
  cis <- stats::confint(conts, level = conf_level)
  contrast_estimates <- cbind(
    contrast_specs$grid,
    data.frame(
      estimate = cis$estimate,
      se = cis$SE,
      df = cis$df,
      lower_cl = cis$lower.CL,
      upper_cl = cis$upper.CL
    )
  )
  conts_df <- as.data.frame(conts)
  contrast_estimates$t_stat <- conts_df$t.ratio
  contrast_estimates$p_value <- conts_df$p.value

  # Merge the estimates and contrast estimates together
  relative_reduc_df <- .get_relative_reduc_df(estimates, arm, visit)
  contrast_estimates <- merge(
    contrast_estimates,
    relative_reduc_df,
    by = c(arm, visit),
    sort = FALSE
  )
  contrast_estimates[[arm]] <- factor(contrast_estimates[[arm]])
  contrast_estimates[[visit]] <- factor(contrast_estimates[[visit]])

  # Safe-net to ensure the arm variable in contrast_estimates has the same factor levels as in estimates
  contrast_estimates <- contrast_estimates |>
    dplyr::mutate(!!sym(arm) := factor(!!sym(arm), levels = levels(estimates[[arm]])))

  # Return a list containing the estimates, contrast estimates, averages, and weights
  # Left join estimates with contrasts
  out <- dplyr::full_join(
    estimates,
    contrast_estimates,
    by = c(arm, visit),
    suffix = c("_est", "_contr")
  ) |>
    dplyr::mutate(conf_level = conf_level) |>
    dplyr::arrange(!!sym(arm), !!sym(visit))

  class(out) <- c("mmrm_df", class(out))

  out
}

#' @param digits (`numeric`)\cr
#'   A numeric vector of length 3 specifying the number of decimal places for: 1) Estimates/CIs, 2) Standard
#'    Errors, and 3) P-values. Default is `c(2, 3, 4)`.
#' @param baseline_args (named `list`)\cr
#'   Arguments forwarded to the baseline [tbl_roche_summary()] call, letting you configure the baseline
#'   section (`statistic`, `digits`, `type`, `sort`, ...). Defaults to n and Mean (SE); only the elements you
#'   supply are overridden. For example,
#'   `baseline_args = list(statistic = ~ c("{N_nonmiss}", "{mean} ({sd})", "{median}", "{min} - {max}"))`
#'   shows n, Mean (SD), Median and Min - Max.
#' @param postbaseline_args (named `list`)\cr
#'   Configures the post-baseline (MMRM) section. Supported elements:
#'   * `stats` (`character`) - which statistics to show and in which order. Defaults to
#'     `c("n", "estimate_est", "lower_cl_est", "estimate_contr", "lower_cl_contr", "p_value")`.
#'     The log-scale statistics `"geom_mean_ratio"` and `"geom_mean_ratio_ci"` are also available
#'     (meaningful only for a log-transformed response); add them to `stats` to show them.
#'   * `label` (named `list`) - row-label overrides, keyed by statistic id, e.g.
#'     `label = list(p_value = "p")`.
#'   * `stat_fns` (named `list`) - formatting-function overrides, keyed by statistic id. Each
#'     function receives the row-group `data` and returns `list(my_stat = <string>)`.
#'   Only the elements you supply are overridden; the rest keep their defaults. For example,
#'   `postbaseline_args = list(stats = c("n", "estimate_est", "geom_mean_ratio", "geom_mean_ratio_ci"))`
#'   appends the geometric mean ratio and its CI.
#'
#' @return `tbl_mmrm` returns a 'gtsummary' table object.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("mmrm", quietly = TRUE)
#' tbl_mmrm(
#'   mmrm_results,
#'   fv_dt |> dplyr::mutate(AVISIT = "Baseline"),
#'   arm = "ARMCD", visit = "AVISIT", baseline_aval = "FEV1"
#' )
#'
#' # Append the geometric mean ratio and its CI (for a log-transformed response)
#' tbl_mmrm(
#'   mmrm_results,
#'   arm = "ARMCD", visit = "AVISIT",
#'   postbaseline_args = list(
#'     stats = c("n", "estimate_est", "geom_mean_ratio", "geom_mean_ratio_ci")
#'   )
#' )
#'
#' @rdname tbl_mmrm
#' @export
tbl_mmrm <- function(mmrm_df, base_df = NULL, arm, visit, baseline_aval = NULL, digits = c(2, 3, 4),
                     baseline_args = list(), postbaseline_args = list()) {
  check_not_missing(mmrm_df)
  check_not_missing(arm)
  check_not_missing(visit)
  check_not_missing(digits)
  check_class(baseline_args, "list", allow_empty = TRUE)
  check_class(postbaseline_args, "list", allow_empty = TRUE)

  # Baseline summary defaults (n and Mean (SE)); users override any of these via
  # `baseline_args`, which is forwarded to the baseline `tbl_roche_summary()` call.
  baseline_args <- utils::modifyList(
    list(
      type = ~"continuous2",
      statistic = ~ c("{N_nonmiss}", "{mean} ({se})"),
      digits = ~ c(0, digits[1], digits[2])
    ),
    baseline_args
  )

  # Post-baseline defaults. `stats` selects which statistics show and in which
  # order; `label` and `stat_fns` are named (by carrier column) overrides merged
  # over the built-in registry below. Defaults reproduce the historic output.
  postbaseline_args <- utils::modifyList(
    list(stats = .mmrm_default_stats, label = list(), stat_fns = list()),
    postbaseline_args
  )
  cards::process_selectors(
    mmrm_df,
    arm = {{ arm }}, visit = {{ visit }}
  )
  if (NROW(base_df) > 0) {
    cards::process_selectors(
      base_df,
      arm = all_of(arm), visit = all_of(visit), baseline_aval = {{ baseline_aval }}
    )
    check_data_frame(base_df)
    check_string(baseline_aval)
  }
  check_data_frame(mmrm_df)
  check_string(arm)
  check_string(visit)
  check_class(mmrm_df, "mmrm_df")
  check_integerish(digits)

  # Converts 0.95 into "95%"
  ci_pct_str <- sprintf("%.0f%%", mmrm_df$conf_level[1] * 100)

  # 3. Build Baseline Table (if baseline data exists)
  gts_baseline <- NULL
  if (NROW(base_df) > 0) {
    gts_baseline <- base_df |>
      gtsummary::tbl_strata(
        strata = any_of(visit), # or visit, depending on your column name
        .combine_with = "tbl_stack",
        .header = "{strata}",
        .tbl_fun = ~ rlang::inject(
          tbl_roche_summary(
            .x,
            by = any_of(arm), # or arm, matching the column containing your header
            include = all_of(baseline_aval), # Replace with the actual column name of the score (e.g., AVAL or BASE)
            !!!baseline_args
          )
        ) |>
          modify_table_body(
            ~ .x |>
              dplyr::mutate(
                label = dplyr::case_when(
                  label == "N Non-missing" ~ "n",
                  label == "Mean (se)" ~ "Mean (SE)",
                  TRUE ~ label
                )
              ) |>
              # Remove the original continuous2 header row
              dplyr::filter(row_type != "label") |>
              # Force the remaining stats to act like primary labels
              # so they align exactly with the MMRM table!
              dplyr::mutate(row_type = "label")
          ) |>
          modify_footnote(everything() ~ NA) |>
          gtsummary::modify_header(all_stat_cols() ~ "{level}")
      )
  }

  # 4. Build Post-Baseline MMRM Table
  # Resolve the statistics registry: which stats, their carrier columns, labels
  # and formatting functions. `stats` filters and orders; `stat_fns`/`label`
  # override the defaults from `.mmrm_stat_registry()`.
  reg <- .mmrm_resolve_stats(
    stats = postbaseline_args$stats,
    label = postbaseline_args$label,
    stat_fns = postbaseline_args$stat_fns,
    digits = digits,
    ci_pct_str = ci_pct_str
  )

  gts_mmrm <- mmrm_df |>
    gtsummary::tbl_strata(
      strata = all_of(visit),
      .combine_with = "tbl_stack",
      .header = "{strata}",
      .tbl_fun = ~ .x |>
        tbl_custom_summary(
          by = all_of(arm),
          include = all_of(reg$columns),
          # MANDATORY: This stops the variable labels from repeating across two lines!
          type = list(everything() ~ "continuous"),
          stat_fns = reg$stat_fns,
          statistic = ~"{my_stat}",
          missing = "no"
        ) |>
        modify_table_body(
          ~ .x |>
            dplyr::mutate(
              label = dplyr::coalesce(reg$labels[.data$variable], .data$label)
            )
        ) |>
        modify_footnote(everything() ~ NA) |>
        gtsummary::modify_header(all_stat_cols() ~ "{level}") |>
        gtsummary::modify_header(label = "")
    )

  # 5. Stack and Finalize Headers
  # If baseline exists, stack it with MMRM; otherwise just format MMRM
  if (!is.null(gts_baseline)) {
    final_table <- gtsummary::tbl_stack(list(gts_baseline, gts_mmrm))
  } else {
    final_table <- gts_mmrm
  }

  final_table <- final_table |>
    gtsummary::modify_header(
      groupname_col = "Visit",
      label = "Statistics"
    )

  final_table
}

# --- Post-baseline statistics registry ------------------------------------

# Statistics shown by default, in display order. Users pick a subset (and order)
# through `postbaseline_args$stats`.
.mmrm_default_stats <- c(
  "n", "estimate_est", "lower_cl_est",
  "estimate_contr", "lower_cl_contr", "p_value"
)

# Registry of every post-baseline statistic tbl_mmrm() can render. Each entry is
# keyed by a stat id (what the user selects in `stats`) and carries:
#   column - the mmrm_df column that carries the row in tbl_custom_summary().
#            Must be unique across the selected stats and hold non-missing values
#            for the arms that should display a value (contrast stats are NA on
#            the reference arm, which renders blank - same as the difference rows).
#   label  - default row label (CI labels are built from the confidence level).
#   fn     - stat_fn passed to tbl_custom_summary(); reads any mmrm_df column from
#            the row-group `data` and returns list(my_stat = <formatted string>).
# The geometric mean stats are off by default; select them via `stats`.
.mmrm_stat_registry <- function(digits, ci_pct_str) {
  list(
    n = list(
      column = "n",
      label = "n",
      fn = function(data, ...) .get_n(data)
    ),
    estimate_est = list(
      column = "estimate_est",
      label = "Adjusted Mean (SE)",
      fn = function(data, ...) .get_adj_mean_se(data, digits)
    ),
    lower_cl_est = list(
      column = "lower_cl_est",
      label = sprintf("%s CI for Adjusted Mean", ci_pct_str),
      fn = function(data, ...) .get_adj_mean_ci(data, digits)
    ),
    estimate_contr = list(
      column = "estimate_contr",
      label = "Difference in Adjusted Means (SE)",
      fn = function(data, ...) .get_diff_se(data, digits)
    ),
    lower_cl_contr = list(
      column = "lower_cl_contr",
      label = sprintf("%s CI for Difference in Adjusted Means", ci_pct_str),
      fn = function(data, ...) .get_diff_ci(data, digits)
    ),
    p_value = list(
      column = "p_value",
      label = "P-value",
      fn = function(data, ...) .get_pval(data, digits)
    ),
    # Derived, log-scale stats (off by default). Computed from the contrast
    # estimates, so they only make sense for log-transformed responses.
    geom_mean_ratio = list(
      column = "relative_reduc",
      label = "Geometric Mean Ratio",
      fn = function(data, ...) .get_geom_mean_ratio(data, digits)
    ),
    geom_mean_ratio_ci = list(
      column = "t_stat",
      label = sprintf("%s CI for Geometric Mean Ratio", ci_pct_str),
      fn = function(data, ...) .get_geom_mean_ratio_ci(data, digits)
    )
  )
}

# Resolve the user's post-baseline selection into the pieces tbl_custom_summary()
# needs: carrier columns (ordered), stat_fns (named by column) and labels (named
# by column). `label`/`stat_fns` overrides are keyed by stat id.
.mmrm_resolve_stats <- function(stats, label, stat_fns, digits, ci_pct_str) {
  registry <- .mmrm_stat_registry(digits, ci_pct_str)

  unknown <- setdiff(stats, names(registry))
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "Unknown {.arg stats} value{?s} in {.arg postbaseline_args}: {.val {unknown}}.",
        i = "Available statistics are {.val {names(registry)}}."
      ),
      call = get_cli_abort_call()
    )
  }

  selected <- registry[stats] # subset keeps the user-supplied order

  for (id in intersect(names(label), names(selected))) {
    selected[[id]]$label <- label[[id]]
  }
  for (id in intersect(names(stat_fns), names(selected))) {
    selected[[id]]$fn <- stat_fns[[id]]
  }

  columns <- vapply(selected, `[[`, character(1), "column")
  if (anyDuplicated(columns) > 0) {
    dup <- unique(columns[duplicated(columns)])
    cli::cli_abort(
      "Selected {.arg stats} map to the same carrier column ({.val {dup}}); pick at most one per column.",
      call = get_cli_abort_call()
    )
  }

  labels <- vapply(selected, `[[`, character(1), "label")
  names(labels) <- columns
  stat_fns_out <- stats::setNames(lapply(selected, `[[`, "fn"), columns)

  list(columns = unname(columns), stat_fns = stat_fns_out, labels = labels)
}

# --- Internal Helper Functions ---

se <- function(x, na.rm = TRUE) { # nolint
  if (na.rm) x <- stats::na.omit(x)
  stats::sd(x) / sqrt(length(x))
}

.get_relative_reduc_df <- function(estimates, arm, visit) {
  ref_arm_level <- levels(estimates[[arm]])[1L]

  estimates |>
    dplyr::select(dplyr::all_of(c(visit, arm)), "estimate") |>
    tidyr::pivot_wider(names_from = dplyr::all_of(arm), values_from = "estimate") |>
    dplyr::mutate(
      dplyr::across(
        -dplyr::all_of(c(visit, ref_arm_level)),
        ~ (.data[[ref_arm_level]] - .x) / .data[[ref_arm_level]],
        .names = "relative_reduc_{.col}"
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("relative_reduc_"),
      names_to = arm,
      names_prefix = "relative_reduc_",
      values_to = "relative_reduc"
    )
}

.get_single_visit_contrast_specs <- function(emmeans_res, arm, visit) {
  emmeans_res$grid$index <- seq_len(nrow(emmeans_res$grid))
  grid_by_visit <- split(emmeans_res$grid, emmeans_res$grid[[visit]])
  arm_levels <- emmeans_res$object@levels[[arm]]
  ref_arm_level <- arm_levels[1L]
  zeros_coefs <- numeric(nrow(emmeans_res$grid))
  overall_list <- list()
  arm_vec <- visit_vec <- c()

  for (j in seq_along(grid_by_visit)) {
    this_grid <- grid_by_visit[[j]]
    ref_index <- which(this_grid[[arm]] == ref_arm_level)
    this_visit <- names(grid_by_visit)[j]
    this_ref_coefs <- zeros_coefs
    this_ref_coefs[this_grid$index[ref_index]] <- -1
    this_list <- list()
    for (i in seq_len(nrow(this_grid))[-ref_index]) {
      this_coefs <- this_ref_coefs
      this_coefs[this_grid$index[i]] <- 1
      this_arm <- as.character(this_grid[[arm]][i])
      arm_vec <- c(arm_vec, this_arm)
      visit_vec <- c(visit_vec, this_visit)
      this_label <- paste(this_arm, this_visit, sep = ".")
      this_list[[this_label]] <- this_coefs
    }
    overall_list <- c(overall_list, this_list)
  }

  grid <- data.frame(arm = arm_vec, visit = visit_vec)
  names(grid) <- c(arm, visit)
  list(coefs = overall_list, grid = grid)
}

# --- Custom formatting functions internally --------------------------------
.get_n <- function(data, ...) {
  val <- if (nrow(data) == 0 || isTRUE(is.na(data$n[1]))) {
    ""
  } else {
    as.character(data$n[1])
  }
  list(my_stat = val)
}

.get_adj_mean_se <- function(data, digits, ...) {
  val <- if (nrow(data) == 0 || isTRUE(is.na(data$estimate_est[1]))) {
    ""
  } else {
    sprintf(paste0("%.", digits[1], "f (%.", digits[2], "f)"), data$estimate_est[1], data$se_est[1])
  }
  list(my_stat = val)
}

.get_adj_mean_ci <- function(data, digits, ...) {
  val <- if (nrow(data) == 0 || isTRUE(is.na(data$lower_cl_est[1]))) {
    ""
  } else {
    sprintf(paste0("(%.", digits[1], "f, %.", digits[1], "f)"), data$lower_cl_est[1], data$upper_cl_est[1])
  }
  list(my_stat = val)
}

.get_diff_se <- function(data, digits, ...) {
  val <- if (nrow(data) == 0 || isTRUE(is.na(data$estimate_contr[1]))) {
    ""
  } else {
    sprintf(paste0("%.", digits[1], "f (%.", digits[2], "f)"), data$estimate_contr[1], data$se_contr[1])
  }
  list(my_stat = val)
}

.get_diff_ci <- function(data, digits, ...) {
  val <- if (nrow(data) == 0 || isTRUE(is.na(data$lower_cl_contr[1]))) {
    ""
  } else {
    sprintf(paste0("(%.", digits[1], "f, %.", digits[1], "f)"), data$lower_cl_contr[1], data$upper_cl_contr[1])
  }
  list(my_stat = val)
}

.get_pval <- function(data, digits, ...) {
  val <- if (nrow(data) == 0 || isTRUE(is.na(data$p_value[1]))) {
    ""
  } else {
    pval <- data$p_value[1]
    cutoff <- 10^(-digits[3])
    if (isTRUE(pval < cutoff)) {
      paste0("<", format(cutoff, scientific = FALSE, trim = TRUE))
    } else {
      sprintf(paste0("%.", digits[3], "f"), pval)
    }
  }
  list(my_stat = val)
}

# Geometric mean ratio = exp(difference in adjusted means on the log scale).
# Only meaningful when the MMRM was fit on a log-transformed response.
.get_geom_mean_ratio <- function(data, digits, ...) {
  val <- if (nrow(data) == 0 || isTRUE(is.na(data$estimate_contr[1]))) {
    ""
  } else {
    sprintf(paste0("%.", digits[1], "f"), exp(data$estimate_contr[1]))
  }
  list(my_stat = val)
}

.get_geom_mean_ratio_ci <- function(data, digits, ...) {
  val <- if (nrow(data) == 0 || isTRUE(is.na(data$lower_cl_contr[1]))) {
    ""
  } else {
    sprintf(
      paste0("(%.", digits[1], "f, %.", digits[1], "f)"),
      exp(data$lower_cl_contr[1]), exp(data$upper_cl_contr[1])
    )
  }
  list(my_stat = val)
}
