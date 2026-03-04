#' Display MMRM Results in a Formatted Table
#'
#' This function takes a fitted MMRM model object and creates a formatted table,
#' following the style of the MMRM TLG template. It combines baseline summary statistics
#' (if available) with the MMRM results, presenting them in a clear and organized manner.
#'
#' @param df_tidy_mmrm (`data.frame`)\cr
#'   A tidy data frame containing the MMRM results. This should include
#'   columns for the visit, arm, adjusted means, differences, confidence intervals, and
#'   p-values. The data frame should be structured in a way that allows for stratification by visit and arm.
#'   Usually the output of [get_mmrm_results()].
#' @param df_baseline (`data.frame`)\cr
#'   A data frame containing baseline measurements. This should include columns for
#'   the visit, arm, and baseline values. The function will summarize this data
#'   to provide baseline statistics in the final table.
#' @param arm ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The column in `df_tidy_mmrm` and `df_baseline` that identifies the treatment arms.
#'   This will be used to divide the results in columns. First value is reference.
#' @param visit ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The column in `df_tidy_mmrm` and `df_baseline` that identifies the visits.
#'   This will be used to stratify the results in rows.
#' @param baseline_aval ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   The column in `df_baseline` that contains the baseline values to be summarized.
#'
#' @return A 'gtsummary' table object.
#'
#' @export
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("mmrm", quietly = TRUE)
library(mmrm)
fv_dt <- mmrm::fev_data |>
  dplyr::mutate(
    ARMCD = sprintf(
      "%s\n(N = %d)", ARMCD,
      table(mmrm::fev_data$ARMCD)[ARMCD]
    ),
    ARMCD = factor(ARMCD)
  )


# Fit an MMRM model using the FEV data
fit_mmrm <- mmrm::mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fv_dt
)

afit <- tern.mmrm::fit_mmrm(
  vars = list(
    response = "FEV1", covariates = c("RACE", "SEX", "ARMCD"), id = "USUBJID",
    arm = "ARMCD", visit =
      "AVISIT"
  ),
  fv_dt,
  conf_level = 0.95,
  cor_struct = "unstructured",
  weights_emmeans = "equal"
)

tbl_mmrm(tidy(afit), fv_dt |> dplyr::mutate(AVISIT = "Baseline"), arm = "ARMCD", visit = "AVISIT", baseline_aval = "FEV1")

tbl_mmrm <- function(df_tidy_mmrm, df_baseline, arm, visit, baseline_aval) {
  # 3. Build Baseline Table (if baseline data exists)
  gts_baseline <- NULL
  if (nrow(df_baseline) > 0) {
    # Define the Standard Error (SE) function so gtsummary can find it
    se <- function(x, na.rm = TRUE) {
      if (na.rm) x <- stats::na.omit(x)
      stats::sd(x) / sqrt(length(x))
    }

    gts_baseline <- df_baseline |>
      tbl_strata(
        strata = any_of(visit), # or visit, depending on your column name
        .combine_with = "tbl_stack",
        .header = "{strata}",
        .tbl_fun = ~ .x |>
          tbl_roche_summary(
            by = any_of(arm), # or arm, matching the column containing your header
            include = all_of(baseline_aval), # Replace with the actual column name of the score (e.g., AVAL or BASE)
            # continuous2 allows us to return multiple rows of stats for one variable
            type = list(all_of(baseline_aval) ~ "continuous2"),
            # Specify the exact stats you want
            statistic = list(all_of(baseline_aval) ~ c("{N_nonmiss}", "{mean} ({se})")),
            # list(0, c(2, 2)) means: 0 decimals for row 1 (N), and 2 decimals for row 2 (mean, se)
            digits = list(all_of(baseline_aval) ~ c(0, 2, 3))
          ) |>
          modify_table_body(
            ~ .x |>
              mutate(
                label = case_when(
                  label == "N Non-missing" ~ "n",
                  label == "Mean (se)" ~ "Mean (SE)",
                  TRUE ~ label
                )
              ) |>
              # Remove the original continuous2 header row
              filter(row_type != "label") |>
              # THE FIX: Force the remaining stats to act like primary labels
              # so they align exactly with the MMRM table!
              mutate(row_type = "label")
          ) |>
          modify_footnote(everything() ~ NA) |>
          modify_header(all_stat_cols() ~ "{level}")
      )
  }

  # 4. Build Post-Baseline MMRM Table
  gts_mmrm <- df_tidy_mmrm |>
    tbl_strata(
      strata = visit,
      .combine_with = "tbl_stack",
      .header = "{strata}",
      .tbl_fun = ~ .x |>
        tbl_custom_summary(
          by = arm,
          include = c(n, estimate_est, lower_cl_est, estimate_contr, lower_cl_contr, p_value),
          stat_fns = list(
            n ~ .get_n,
            estimate_est ~ .get_adj_mean_se,
            lower_cl_est ~ .get_adj_mean_ci,
            estimate_contr ~ .get_diff_se,
            lower_cl_contr ~ .get_diff_ci,
            p_value ~ .get_pval
          ),
          statistic = ~"{my_stat}",
          missing = "no"
        ) |>
        modify_table_body(
          ~ .x |>
            mutate(
              label = case_when(
                variable == "n" ~ "n",
                variable == "estimate_est" ~ "Adjusted Mean (SE)",
                variable == "lower_cl_est" ~ "95% CI for Adjusted Mean",
                variable == "estimate_contr" ~ "Difference in Adjusted Means (SE)",
                variable == "lower_cl_contr" ~ "95% CI for Difference in Adjusted Means",
                variable == "p_value" ~ "P-value",
                TRUE ~ label
              )
            )
        ) |>
        modify_footnote(everything() ~ NA) |>
        modify_header(all_stat_cols() ~ "{level}") |>
        modify_header(label = "")
    )

  # 5. Stack and Finalize Headers
  # If baseline exists, stack it with MMRM; otherwise just format MMRM
  if (!is.null(gts_baseline)) {
    final_table <- tbl_stack(list(gts_baseline, gts_mmrm))
  } else {
    final_table <- gts_mmrm
  }

  final_table <- final_table |>
    modify_header(
      groupname_col = "Visit",
      label = "Statistics"
    )

  final_table
}


# Define custom formatting functions internally
.get_n <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$n[1])) "-" else as.character(data$n[1])
  list(my_stat = val)
}

.get_adj_mean_se <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$estimate_est[1])) {
    "-"
  } else {
    sprintf("%.2f (%.3f)", data$estimate_est[1], data$se_est[1])
  }
  list(my_stat = val)
}

.get_adj_mean_ci <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$lower_cl_est[1])) {
    "-"
  } else {
    sprintf("(%.2f, %.2f)", data$lower_cl_est[1], data$upper_cl_est[1])
  }
  list(my_stat = val)
}

.get_diff_se <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$estimate_contr[1])) {
    "-"
  } else {
    sprintf("%.2f (%.3f)", data$estimate_contr[1], data$se_contr[1])
  }
  list(my_stat = val)
}

.get_diff_ci <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$lower_cl_contr[1])) {
    "-"
  } else {
    sprintf("(%.2f, %.2f)", data$lower_cl_contr[1], data$upper_cl_contr[1])
  }
  list(my_stat = val)
}

.get_pval <- function(data, ...) {
  val <- if (nrow(data) == 0 || is.na(data$p_value[1])) {
    "-"
  } else {
    sprintf("%.4f", data$p_value[1])
  }
  list(my_stat = val)
}
