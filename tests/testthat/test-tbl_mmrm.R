skip_if_pkg_not_installed(c("mmrm", "emmeans"))

# ------------------------------------------------------------------------------
# 1. SETUP DUMMY DATA FOR TESTING
# ------------------------------------------------------------------------------
# Recreate the example dataset
fv_dt <- mmrm::fev_data |>
  dplyr::mutate(
    ARMCD = sprintf("%s\n(N = %d)", ARMCD, table(mmrm::fev_data$ARMCD)[ARMCD]),
    ARMCD = factor(ARMCD)
  )

# Fit the model
fit_mmrm <- mmrm::mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fv_dt
)

# Prepare baseline data (just mocking VIS1 as Baseline for structural testing)
base_df <- fv_dt |>
  dplyr::filter(AVISIT == "VIS1") |>
  dplyr::mutate(AVISIT = factor("Baseline"))


# ------------------------------------------------------------------------------
# 2. TEST INTERNAL HELPERS (Math & Formatting)
# ------------------------------------------------------------------------------
test_that("se() calculates standard error correctly", {
  x <- c(2, 4, 4, 4, 5, 5, 7, 9)
  expected_se <- sd(x) / sqrt(length(x))
  expect_equal(se(x), expected_se)
  expect_equal(se(c(x, NA), na.rm = TRUE), expected_se)
})

test_that(".get_relative_reduc_df calculates math correctly", {
  # Mock a simple estimates dataframe
  mock_est <- data.frame(
    AVISIT = c("VIS1", "VIS1"),
    ARMCD = factor(c("Placebo", "Trt")),
    estimate = c(10, 8) # Trt is 8, Placebo is 10. Reduction should be (10-8)/10 = 0.2
  )
  res <- .get_relative_reduc_df(mock_est, "ARMCD", "AVISIT")

  expect_equal(nrow(res), 1)
  expect_equal(res$relative_reduc, 0.2)
})

test_that(".get_* formatting functions handle empty and NA data correctly", {
  # Mock an empty dataframe and a Placebo-style NA dataframe
  empty_data <- data.frame(n = numeric(0), estimate_est = numeric(0))
  na_data <- data.frame(n = NA_real_, estimate_est = NA_real_)

  # 1. Empty dataframe handling
  expect_equal(.get_n(empty_data)$my_stat, "")
  expect_equal(.get_adj_mean_se(empty_data, digits = c(2, 3, 4))$my_stat, "")

  # 2. NA data handling (safeguards against the TRUE/FALSE crash for Placebo contrasts)
  expect_equal(.get_n(na_data)$my_stat, "")
  expect_equal(.get_adj_mean_se(na_data, digits = c(2, 3, 4))$my_stat, "")
})

test_that(".get_* formatting functions output correct statistics and digits", {
  mock_data <- data.frame(
    n = 50, estimate_est = 10.123, se_est = 2.456, lower_cl_est = 5.1, upper_cl_est = 15.1,
    estimate_contr = 2.123, se_contr = 1.111, lower_cl_contr = 0.5, upper_cl_contr = 3.5, p_value = 0.045
  )

  # Very small p-value to trigger the regulatory threshold formatting
  mock_small_pval <- data.frame(p_value = 0.00001)

  # Check standard formatting & digit parameters
  expect_equal(.get_n(mock_data)$my_stat, "50")
  expect_equal(.get_adj_mean_se(mock_data, digits = c(1, 2, 4))$my_stat, "10.1 (2.46)")
  expect_equal(.get_adj_mean_ci(mock_data, digits = c(1, 2, 4))$my_stat, "(5.1, 15.1)")
  expect_equal(.get_diff_se(mock_data, digits = c(2, 3, 4))$my_stat, "2.12 (1.111)")
  expect_equal(.get_diff_ci(mock_data, digits = c(2, 3, 4))$my_stat, "(0.50, 3.50)")

  # Check P-value formatting
  expect_equal(.get_pval(mock_data, digits = c(2, 3, 3))$my_stat, "0.045")

  # Check Regulatory P-value threshold (e.g., < 0.0001)
  expect_equal(.get_pval(mock_small_pval, digits = c(2, 3, 4))$my_stat, "<0.0001")
})

# ------------------------------------------------------------------------------
# 3. TEST CORE MMRM EXTRACTION
# ------------------------------------------------------------------------------
test_that("get_mmrm_results outputs expected mmrm_df structure", {
  res <- get_mmrm_results(fit_mmrm, arm = "ARMCD", visit = "AVISIT", conf_level = 0.95)

  # Check class assignment
  expect_s3_class(res, "mmrm_df")
  expect_s3_class(res, "data.frame")

  # Check expected columns exist (verifying the join and suffix logic)
  expected_cols <- c(
    "ARMCD", "AVISIT", "estimate_est", "se_est", "lower_cl_est",
    "estimate_contr", "p_value", "relative_reduc", "conf_level"
  )
  expect_true(all(expected_cols %in% names(res)))

  # Check conf_level was passed through
  expect_equal(res$conf_level[1], 0.95)

  # Check reference arm logic (Placebo should have NA for contrasts)
  ref_arm <- levels(res$ARMCD)[1]
  ref_data <- res[res$ARMCD == ref_arm, ]
  expect_true(all(is.na(ref_data$estimate_contr)))
})


# ------------------------------------------------------------------------------
# 4. TEST GTSUMMARY FORMATTING
# ------------------------------------------------------------------------------
test_that("tbl_mmrm generates stacked table successfully", {
  mmrm_res <- get_mmrm_results(fit_mmrm, arm = "ARMCD", visit = "AVISIT")

  # Run the main table builder
  tbl <- tbl_mmrm(
    mmrm_df = mmrm_res,
    base_df = base_df,
    arm = "ARMCD",
    visit = "AVISIT",
    baseline_aval = "FEV1",
    digits = c(2, 3, 4)
  )

  # Must return a gtsummary object
  expect_s3_class(tbl, "gtsummary")

  # Must contain stacked tables
  expect_s3_class(tbl, "tbl_stack")
  expect_equal(length(tbl$tbls), 2) # this is Baseline + MMRM

  # Check that 95% was injected into the label correctly
  table_labels <- tbl$table_body$label
  expect_true("95% CI for Adjusted Mean" %in% table_labels)
})

test_that("tbl_mmrm falls back cleanly when base_df is empty", {
  mmrm_res <- get_mmrm_results(fit_mmrm, arm = "ARMCD", visit = "AVISIT")
  empty_base <- base_df[0, ] # 0 rows

  tbl_empty <- tbl_mmrm(
    mmrm_df = mmrm_res,
    base_df = empty_base,
    arm = "ARMCD",
    visit = "AVISIT",
    baseline_aval = "FEV1"
  )

  expect_s3_class(tbl_empty, "gtsummary")

  # Should NOT be a stack, just a standalone tbl_strata/tbl_custom_summary
  expect_true(inherits(tbl_empty, "tbl_strata"))
})


test_that("tbl_mmrm handles base_df = NULL gracefully without baseline stacking", {

  # 1. Setup minimal mock mmrm_df to bypass running a real model
  mock_mmrm_df <- data.frame(
    ARM = factor(c("TRT", "PBO")),
    VISIT = factor(c("WEEK 1", "WEEK 1")),
    n = c(50, 50),
    estimate_est = c(10.1, 12.2),
    se_est = c(1.1, 1.2),
    lower_cl_est = c(8.0, 10.0),
    upper_cl_est = c(12.2, 14.4),
    estimate_contr = c(-2.1, NA),
    se_contr = c(1.5, NA),
    lower_cl_contr = c(-5.0, NA),
    upper_cl_contr = c(0.8, NA),
    p_value = c(0.04, NA),
    conf_level = 0.95
  )
  class(mock_mmrm_df) <- c("mmrm_df", "data.frame")

  # 2. Execute tbl_mmrm with base_df = NULL
  result_tbl <- tbl_mmrm(
    mmrm_df = mock_mmrm_df,
    base_df = NULL,
    arm = "ARM",
    visit = "VISIT",
    baseline_aval = "BASE_VAL" # Should be ignored entirely
  )

  # 3. Verify it returns a valid gtsummary object without crashing
  expect_s3_class(result_tbl, "gtsummary")

  # 4. Verify the structural integrity of the resulting table body
  tbl_body <- result_tbl$table_body

  # It should contain the standard MMRM statistic rows
  expect_true(any(tbl_body$label == "Adjusted Mean (SE)"))
  expect_true(any(tbl_body$label == "Difference in Adjusted Means (SE)"))

  # It should NOT contain any baseline summary rows
  # (If base_df was processed, 'BASE_VAL' or 'Mean (SE)' for baseline would exist)
  expect_false(any(tbl_body$variable == "BASE_VAL"))
})
