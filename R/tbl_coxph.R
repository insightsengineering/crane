#' Pairwise Cox Proportional Hazards Table
#'
#' @description
#' Computes pairwise Cox proportional hazards tests and returns a `gtsummary`
#' object. The table splits the results by comparison arms, presenting the
#' p-value, Hazard Ratio, and 95% Confidence Interval in a stacked layout where
#' statistics form the rows of the table.
#'
#' @param model_formula (`formula`)\cr
#'   A `formula` object specifying the survival model,
#'   typically in the form `Surv(time, status) ~ arm + covariates`.
#' @param data (`data.frame`)\cr
#'   A `data.frame` containing the survival data,
#'   including time, status, and the arm variable.
#' @param arm (`character`)\cr
#'   A single character string specifying the name of the column in `data`
#'   that contains the grouping/treatment
#'   **arm variable**. This column **must be a factor**
#'   for correct stratification and comparison.
#' @param ref_group (`character` or `NULL`)\cr
#'   A single character string specifying the level of the `arm` variable
#'   to be used as the **reference group** for
#'   all pairwise comparisons. If `NULL` (the default),
#'   the **first unique level** of the `arm` column is automatically
#'   selected as the reference group.
#'
#' @return A `gtsummary` object summarizing the pairwise Cox PH results.
#'
#' @examplesIf requireNamespace("survival", quietly = TRUE)
#' # Setup sample survival data with 3 arms to test pairwise comparisons
#' library(survival)
#' surv_data <- survival::lung |>
#'   dplyr::mutate(
#'     arm = factor(sample(c("A", "B", "C"), dplyr::n(), replace = TRUE)),
#'     status = status - 1
#'   ) |>
#'   dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))
#'
#' # Generate the gtsummary table
#' tbl_coxph(
#'   data = surv_data,
#'   model_formula = survival::Surv(time, status) ~ arm,
#'   arm = "arm",
#'   ref_group = "A"
#' )
#'
#' # Setup sample survival data with 2 arms to test if table drops comparison
#' # column
#' surv_data_2arm <- survival::lung |>
#'   dplyr::mutate(
#'     arm = factor(sample(c("A", "B"), dplyr::n(), replace = TRUE)),
#'     status = status - 1
#'   ) |>
#'   dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))
#'
#' # Generate the 2-arm gtsummary table
#' tbl_coxph(
#'   data = surv_data_2arm,
#'   model_formula = survival::Surv(time, status) ~ arm,
#'   arm = "arm",
#'   ref_group = "A"
#' )
#'
#' @export
tbl_coxph <- function(data, model_formula, arm, ref_group = NULL) {
  # Do checks of input arguments -----------------------------------------------
  set_cli_abort_call()

  check_not_missing(data)
  check_not_missing(model_formula)
  check_not_missing(arm)

  check_data_frame(data)
  check_class(model_formula, "formula")

  if (!is.factor(data[[arm]])) {
    data[[arm]] <- as.factor(data[[arm]])
  }

  check_factor_has_levels(data)

  # Fit model ------------------------------------------------------------------

  # set reference
  if (!is.null(ref_group)) {
    data[[arm]] <- stats::relevel(data[[arm]], ref = ref_group)
  }

  ref_level <- levels(data[[arm]])[1]
  non_ref_levels <- levels(data[[arm]])[-1]

  # Fit the Cox Proportional Hazards model
  fit <- survival::coxph(model_formula, data = data)
  fit_summary <- summary(fit)

  # Extract coefficients mapping to the pairwise arm comparisons.
  # coxph appends the factor level to the variable name (e.g. "armB", "armC")
  arm_terms <- paste0(arm, non_ref_levels)
  idx <- match(arm_terms, rownames(fit_summary$coefficients))

  # Safely handle any dropped factor levels (e.g., zero variance/missing data)
  valid_mask <- !is.na(idx)
  valid_idx <- idx[valid_mask]
  valid_non_ref <- non_ref_levels[valid_mask]

  # Extract and format the raw statistics into the tidy structure
  df_tidy <- tibble::tibble(
    comparison_arm = valid_non_ref,
    comparison_label = sprintf("%s vs %s", valid_non_ref, ref_level),
    p_num = as.numeric(fit_summary$coefficients[valid_idx, "Pr(>|z|)"]),
    HR = as.numeric(fit_summary$conf.int[valid_idx, "exp(coef)"]),
    ci_lower = as.numeric(fit_summary$conf.int[valid_idx, "lower .95"]),
    ci_upper = as.numeric(fit_summary$conf.int[valid_idx, "upper .95"])
  ) |>
    dplyr::mutate(
      pval_formatted = dplyr::case_when(
        is.na(.data$p_num) ~ NA_character_,
        .data$p_num < 0.0001 ~ "<0.0001",
        TRUE ~ sprintf("%.4f", .data$p_num)
      ),
      hr_formatted = gtsummary::style_ratio(as.numeric(.data$HR), digits = 2),
      ci_formatted = dplyr::case_when(
        is.na(.data$ci_lower) | is.na(.data$ci_upper) ~ "",
        TRUE ~ sprintf(
          "(%s, %s)",
          gtsummary::style_ratio(as.numeric(.data$ci_lower), digits = 2),
          gtsummary::style_ratio(as.numeric(.data$ci_upper), digits = 2)
        )
      )
    )

  # If there is only 1 comparison (2 groups total), do not use tbl_strata
  # to prevent the "A vs B" strata column from generating.
  if (nrow(df_tidy) == 1) {
    res <- .get_single_comp_table(df_tidy)
  } else {
    # If >1 comparison (>2 groups), stack them and explicitly remove
    # the default "Group" column header injected by tbl_stack.
    res <- df_tidy |>
      gtsummary::tbl_strata(
        strata = "comparison_label",
        .combine_with = "tbl_stack",
        .header = "{strata}",
        .tbl_fun = ~ .get_single_comp_table(.x)
      ) |>
      gtsummary::modify_header(groupname_col = " ")
  }

  # Final polish on the combined top-level headers
  res <- res |>
    gtsummary::modify_header(label = "")

  class(res) <- c("tbl_coxph", class(res))

  res
}

#' Build Single Comparison Table for Cox PH
#'
#' @description
#' Internal helper function to build a formatted `gtsummary` table for a single
#' comparison arm. It extracts the pre-formatted p-value, hazard ratio, and
#' confidence interval from the subsetted data and stacks them into rows for the
#' final display.
#'
#' @param data_subset (`data.frame`)\cr
#'   A pre-processed data frame containing the formatted statistics
#'   (`pval_formatted`, `hr_formatted`, `ci_formatted`) for exactly one
#'   treatment comparison.
#'
#' @return A `gtsummary` object containing the stacked summary statistics.
#'
#' @keywords internal
.get_single_comp_table <- function(data_subset) {
  data_subset |>
    gtsummary::tbl_custom_summary(
      by = NULL,
      include = c("pval_formatted", "hr_formatted", "ci_formatted"),
      type = list(gtsummary::everything() ~ "continuous"),
      stat_fns = list(
        pval_formatted ~
          function(data, ...) list(my_stat = data$pval_formatted[1]),
        hr_formatted ~
          function(data, ...) list(my_stat = data$hr_formatted[1]),
        ci_formatted ~
          function(data, ...) list(my_stat = data$ci_formatted[1])
      ),
      statistic = ~"{my_stat}",
      missing = "no"
    ) |>
    gtsummary::modify_table_body(
      ~ .x |>
        dplyr::mutate(
          label = dplyr::case_when(
            .data$variable == "pval_formatted" ~ "p-value (log-rank)",
            .data$variable == "hr_formatted" ~ "Hazard Ratio",
            .data$variable == "ci_formatted" ~ "95% CI",
            TRUE ~ .data$label
          )
        )
    ) |>
    gtsummary::modify_indent(
      columns = "label",
      rows = .data$variable == "ci_formatted",
      indent = 4L
    ) |>
    gtsummary::modify_header(
      gtsummary::all_stat_cols() ~ " "
    ) |>
    gtsummary::modify_footnote(gtsummary::everything() ~ NA)
}
