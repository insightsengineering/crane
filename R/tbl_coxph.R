#' @title Pairwise Cox Proportional Hazards Table
#'
#' @description
#' Computes pairwise Cox proportional hazards tests and returns a `gtsummary`
#' object. The table splits the results by comparison arms, presenting the
#' p-value, Hazard Ratio, and 95% Confidence Interval in a stacked layout where
#' statistics form the rows of the table.
#'
#' @param data (`data.frame`)\cr
#'   The dataset containing the variables for the survival model.
#' @param model_formula (`formula`)\cr
#'   A survival formula, e.g., `survival::Surv(time, status) ~ arm`.
#' @param arm (`character`)\cr
#'   The name of the treatment arm variable (must be a factor).
#' @param ref_group (`character`)\cr
#'   The reference group level for the treatment arm. Defaults to the first
#'   factor level of the `arm` variable.
#'
#' @return A `gtsummary` object summarizing the pairwise Cox PH results.
#'
#' @examples
#' # Setup sample survival data with 3 arms to test pairwise comparisons
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
#' surv_data <- survival::lung |>
#'   dplyr::mutate(
#'     arm = factor(sample(c("A", "B"), dplyr::n(), replace = TRUE)),
#'     status = status - 1
#'   ) |>
#'   dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))
#' # Generate the gtsummary table
#' tbl_coxph(
#'   data = surv_data,
#'   model_formula = survival::Surv(time, status) ~ arm,
#'   arm = "arm",
#'   ref_group = "A"
#' )
#'
#' @export
tbl_coxph <- function(data, model_formula, arm, ref_group = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame.")
  }
  if (!inherits(model_formula, "formula")) {
    cli::cli_abort("{.arg model_formula} must be a formula.")
  }
  if (!is.factor(data[[arm]])) {
    cli::cli_abort("{.arg arm} column must be a factor.")
  }

  # Establish reference level for clear header labelling
  ref_level <- ref_group
  if (is.null(ref_level)) {
    ref_level <- levels(data[[arm]])[1]
  }

  # Delegate computation to the backend calculation engine
  df_res <- get_cox_pairwise_df(
    model_formula = model_formula,
    data = data,
    arm = arm,
    ref_group = ref_group
  )

  # Reshape data dynamically so that `tbl_custom_summary` can evaluate it.
  df_tidy <- df_res |>
    tibble::as_tibble(rownames = "comparison_arm") |>
    dplyr::mutate(
      comparison_label = sprintf("%s vs %s", comparison_arm, ref_level),
      p_num = as.numeric(`p-value (log-rank)`),
      pval_formatted = dplyr::case_when(
        is.na(p_num) ~ NA_character_,
        p_num < 0.0001 ~ "<0.0001",
        TRUE ~ sprintf("%.4f", p_num)
      ),
      hr_formatted = gtsummary::style_ratio(as.numeric(`HR`), digits = 2),
      ci_formatted = as.character(`95% CI`)
    ) |>
    dplyr::mutate(
      ci_formatted = dplyr::case_when(
        is.na(ci_formatted) ~ "",
        grepl("^\\(", ci_formatted) ~ ci_formatted,
        TRUE ~ sprintf("(%s)", ci_formatted)
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
        strata = comparison_label,
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

#' @title Build Single Comparison Table for Cox PH
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
            variable == "pval_formatted" ~ "p-value (log-rank)",
            variable == "hr_formatted" ~ "Hazard Ratio",
            variable == "ci_formatted" ~ "95% CI",
            TRUE ~ label
          )
        )
    ) |>
    gtsummary::modify_indent(
      columns = "label",
      rows = variable == "ci_formatted",
      indent = 4L
    ) |>
    gtsummary::modify_header(
      gtsummary::all_stat_cols() ~ " "
    ) |>
    gtsummary::modify_footnote(gtsummary::everything() ~ NA)
}
