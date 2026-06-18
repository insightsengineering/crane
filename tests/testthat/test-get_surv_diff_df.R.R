# tests/testthat/test-get_surv_diff_df.R

library(survival)

# Setup: Create a standard survfit object to be used across multiple tests.
# We use the standard 'lung' dataset and convert 'sex' to a factor to ensure
# clear reference level matching.
surv_data <- survival::lung
surv_data$status <- surv_data$status - 1
surv_data$sex <- factor(surv_data$sex, labels = c("Male", "Female"))
fit_km <- survfit(Surv(time, status) ~ sex, data = surv_data)

test_that("get_surv_diff_df returns a correctly structured data.frame", {
  # Why: Verifying the core functionality and output shape ensures downstream
  # combining with get_surv_times_df() will not fail due to column mismatches.
  res <- get_surv_diff_df(fit_km, times = c(100, 200), reference = "Male")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  expect_equal(
    names(res),
    c("Strata", "Time", "N at Risk", "Survival", "95% CI", "p-value")
  )
  expect_equal(res$Time, c(100, 200))

  # Why: Ensures the strata parsing correctly labels the comparison
  expect_true(all(res$Strata == "Female vs Male (Diff)"))

  # 'N at Risk' should be strictly missing for difference rows
  expect_true(all(is.na(res$`N at Risk`)))
})

test_that("get_surv_diff_df correctly handles scale parameter", {
  # Why: Users often want to present survival as percentages (scale = 100).
  # We test this to ensure the mathematical multiplication is applied to both
  # the estimate and the CI bounds before string formatting.
  res_unscaled <- get_surv_diff_df(
    fit_km,
    times = 100, reference = "Male", scale = 1
  )
  res_scaled <- get_surv_diff_df(
    fit_km,
    times = 100, reference = "Male", scale = 100
  )

  # Extract numeric values from the formatted strings for comparison
  est_unscaled <- as.numeric(res_unscaled$Survival)
  est_scaled <- as.numeric(res_scaled$Survival)

  expect_equal(round(est_scaled, 0), est_unscaled * 100)
})

test_that("get_surv_diff_df correctly handles custom confidence intervals", {
  # Why: Custom confidence levels (e.g., 90% or 99%) should dynamically alter
  # both the statistical calculation and the resulting column header.
  res_90 <- get_surv_diff_df(
    fit_km,
    times = 100, reference = "Male", conf_int = 0.90
  )

  expect_true("90% CI" %in% names(res_90))
  expect_false("95% CI" %in% names(res_90))
})

test_that("get_surv_diff_df triggers aborts on invalid inputs", {
  # Why: Rigorous type-checking ensures the function fails fast and informs the
  # user exactly what went wrong before attempting to call cardx or dplyr.

  # 1. Invalid fit_km object
  expect_error(
    get_surv_diff_df(fit_km = "not_a_model", times = 100, reference = "Male"),
    "`fit_km` must be a survfit object."
  )

  # 2. Invalid times (non-numeric)
  expect_error(
    get_surv_diff_df(fit_km = fit_km, times = "100", reference = "Male"),
    "`times` must be a non-empty numeric vector."
  )

  # 3. Invalid times (empty vector)
  expect_error(
    get_surv_diff_df(fit_km = fit_km, times = numeric(0), reference = "Male"),
    "`times` must be a non-empty numeric vector."
  )
})
