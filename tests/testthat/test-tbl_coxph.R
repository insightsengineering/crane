# Setup shared test data -------------------------------------------------------
suppressPackageStartupMessages(library(survival))

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


# Tests: Input Validation & Coercion -------------------------------------------
test_that("tbl_coxph() errors on invalid inputs", {
  form <- survival::Surv(time, status) ~ arm

  # data must be a data.frame
  expect_error(
    tbl_coxph(data = "not_a_df", model_formula = form, arm = "arm")
  )

  # model_formula must be a formula
  expect_error(
    tbl_coxph(data = surv_data_2arm, model_formula = "time ~ arm", arm = "arm")
  )
})

test_that("tbl_coxph() converts character arm to factor natively", {
  form <- survival::Surv(time, status) ~ arm

  char_data <- surv_data_2arm
  char_data$arm <- as.character(char_data$arm)

  # The function should auto-coerce to factor and succeed without error
  expect_no_error(
    res <- tbl_coxph(
      data = char_data,
      model_formula = form,
      arm = "arm"
    )
  )
  expect_s3_class(res, "tbl_coxph")
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
  # check if groupname_col is not present
  expect_false("groupname_col" %in% colnames(tb))
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

  tb <- res$table_body
  expect_true("B vs A" %in% tb$groupname_col)
  expect_true("C vs A" %in% tb$groupname_col)
})


# Tests: Formatting Edge Cases (Extreme P-values) ------------------------------
test_that("tbl_coxph() correctly formats extreme p-values (<0.0001)", {
  # To safely test the `<0.0001` formatter we artificially inflate
  # the sample size of an overlapping dataset.
  lung_huge <- survival::lung[rep(1:nrow(survival::lung), 10), ]
  lung_huge$arm <- factor(ifelse(lung_huge$sex == 1, "A", "B"))

  res <- suppressWarnings(
    tbl_coxph(
      data = lung_huge,
      model_formula = survival::Surv(time, status) ~ arm,
      arm = "arm",
      ref_group = "A"
    )
  )

  tb <- res$table_body

  # Isolate the exact comparison block and label to ensure strict matching
  pval_block <- tb |> dplyr::filter(variable == "pval_formatted")
  extreme_pval <- pval_block$stat_0[pval_block$label == "p-value (log-rank)"]

  expect_equal(extreme_pval, "<0.0001")
})
