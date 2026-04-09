# Setup shared test data -------------------------------------------------------

set.seed(42)
surv_data_2arm <- survival::lung |>
  dplyr::mutate(
    arm = factor(sample(c("A", "B"), dplyr::n(), replace = TRUE)),
    status = status - 1
  ) |>
  dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))

surv_data_3arm <- survival::lung |>
  dplyr::mutate(
    arm = factor(sample(c("A", "B", "C"), dplyr::n(), replace = TRUE)),
    status = status - 1
  ) |>
  dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))

# Tests: Input Validation ------------------------------------------------------
test_that("tbl_coxph() errors on invalid inputs", {
  form <- survival::Surv(time, status) ~ arm

  # data must be a data.frame
  expect_error(
    tbl_coxph(data = "not_a_df", model_formula = form, arm = "arm"),
    "must be a data.frame"
  )

  # model_formula must be a formula
  expect_error(
    tbl_coxph(data = surv_data_2arm, model_formula = "time ~ arm", arm = "arm"),
    "must be a formula"
  )

  # arm column must be a factor
  bad_data <- surv_data_2arm
  bad_data$arm <- as.character(bad_data$arm)
  expect_error(
    tbl_coxph(data = bad_data, model_formula = form, arm = "arm"),
    "column must be a factor"
  )
})

# Tests: 2-Arm Configuration (Single Comparison) -------------------------------
test_that("tbl_coxph() works with 2 arms and uses default ref_group", {
  form <- survival::Surv(time, status) ~ arm

  res <- tbl_coxph(
    data = surv_data_2arm,
    model_formula = form,
    arm = "arm"
    # Omitting ref_group to test the default `levels(data[[arm]])[1]` branch
  )

  expect_s3_class(res, "tbl_coxph")
  expect_s3_class(res, "gtsummary")

  # Extract table body to force evaluation of the custom formatters and labels
  # applied in the internal `.get_single_comp_table()` helper
  tb <- res$table_body
  expect_true(all(c("p-value (log-rank)", "Hazard Ratio", "95% CI") %in% tb$label))
})

# Tests: 3-Arm Configuration (Stacked Strata) ----------------------------------
test_that("tbl_coxph() works with >2 arms (uses tbl_strata)", {
  form <- survival::Surv(time, status) ~ arm

  res <- tbl_coxph(
    data = surv_data_3arm,
    model_formula = form,
    arm = "arm",
    ref_group = "A"
  )

  expect_s3_class(res, "tbl_coxph")
  # When >1 comparison, gtsummary natively wraps it in tbl_strata
  expect_s3_class(res, "tbl_strata")
})

# Tests: Formatting Edge Cases (100% Coverage via Mocking) ---------------------
test_that("tbl_coxph() correctly formats extreme p-values, NAs, and CIs", {
  # We mock `get_cox_pairwise_df` to force specific data states that trigger
  # the edge-case branches in our custom dplyr::case_when() formatters.
  mock_backend <- function(...) {
    data.frame(
      `HR` = c(1.5, NA, 2.0),
      `95% CI` = c("(1.0, 2.0)", NA, "1.5, 2.5"), # Note the missing parentheses on row 3
      `p-value (log-rank)` = c(0.00005, NA, 0.05),
      row.names = c("B", "C", "D"),
      check.names = FALSE
    )
  }

  form <- survival::Surv(time, status) ~ arm

  # Create a dummy 4-arm dataset to match the mocked comparisons (A vs B, C, D)
  dummy_4arm <- surv_data_2arm
  dummy_4arm$arm <- factor(
    sample(c("A", "B", "C", "D"), nrow(dummy_4arm), replace = TRUE)
  )

  # Intercept the backend call and inject our edge-case data
  testthat::with_mocked_bindings(
    get_cox_pairwise_df = mock_backend,
    code = {
      res <- tbl_coxph(
        data = dummy_4arm,
        model_formula = form,
        arm = "arm",
        ref_group = "A"
      )

      # Extract the internal tbl_stack to verify formatted values
      tb <- res$table_body

      # Test 1: p < 0.0001 formatter
      # Isolate the exact comparison block and label to ensure strict matching
      pval_block <- tb |> dplyr::filter(groupname_col == "B vs A")
      extreme_pval <- pval_block$stat_0[pval_block$label == "p-value (log-rank)"]
      expect_equal(extreme_pval, "<0.0001")

      # Test 2: NA handlers for P-value and CI
      # NA values should be converted to NA_character_ and "" respectively,
      # which gtsummary renders as empty strings or skips appropriately.
      na_block <- tb |> dplyr::filter(groupname_col == "C vs A")
      ci_na_val <- na_block$stat_0[na_block$label == "95% CI"]
      expect_equal(ci_na_val, "")

      # Test 3: Missing parentheses formatter
      # The raw mocked CI "1.5, 2.5" should have been wrapped in ()
      parens_block <- tb |> dplyr::filter(groupname_col == "D vs A")
      ci_parens_val <- parens_block$stat_0[parens_block$label == "95% CI"]
      expect_equal(ci_parens_val, "(1.5, 2.5)")
    }
  )
})
