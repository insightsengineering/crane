test_that("cv() computes the coefficient of variation", {
  x <- c(2, 4, 4, 4, 5, 5, 7, 9)
  expect_equal(cv(x), stats::sd(x) / mean(x) * 100)
})

test_that("cv() handles NA and degenerate input", {
  expect_equal(cv(c(1, 2, NA, 3)), cv(c(1, 2, 3)))
  # sd of a single value is NA -> cv is NA
  expect_true(is.na(cv(5)))
  expect_true(is.na(cv(c(NA, NA))))
})

test_that("geom_mean() computes the geometric mean of positive values", {
  expect_equal(geom_mean(c(1, 2, 3, 4, 5)), exp(mean(log(1:5))))
  expect_equal(geom_mean(c(1, 2, NA, 4)), exp(mean(log(c(1, 2, 4)))))
})

test_that("geom_mean() returns NA_real_ for undefined cases", {
  # any non-positive value -> undefined
  expect_identical(geom_mean(c(0, 1, 2)), NA_real_)
  expect_identical(geom_mean(c(-1, 2, 3)), NA_real_)
  # no usable values (all NA) -> NA_real_, not NaN
  res <- geom_mean(c(NA_real_, NA_real_))
  expect_identical(res, NA_real_)
  expect_false(is.nan(res))
})

test_that("geom_cv() computes the geometric coefficient of variation", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(geom_cv(x), sqrt(exp(stats::sd(log(x))^2) - 1) * 100)
})

test_that("geom_cv() drops non-positive values", {
  expect_equal(geom_cv(c(0, 1, 2, 3)), geom_cv(c(1, 2, 3)))
  # a single positive value has undefined variation
  expect_true(is.na(geom_cv(5)))
})

test_that("fmt_3sig() formats to 3 significant figures", {
  expect_identical(fmt_3sig(0.001234), "0.00123")
  expect_identical(fmt_3sig(123456), "123000")
  expect_identical(fmt_3sig(0), "0")
})

test_that("fmt_3sig() is vectorized and handles NA/non-finite", {
  expect_identical(
    fmt_3sig(c(0.001234, NA, 123456, Inf, -Inf, 0)),
    c("0.00123", NA, "123000", NA, NA, "0")
  )
  expect_identical(fmt_3sig(NA_real_), NA_character_)
  expect_length(fmt_3sig(numeric(0)), 0L)
})

test_that("fmt_pct() formats to one decimal place", {
  expect_identical(fmt_pct(45.678), "45.7")
  expect_identical(fmt_pct(0), "0.0")
})

test_that("fmt_pct() is vectorized and handles NA/non-finite", {
  expect_identical(
    fmt_pct(c(45.678, NA, Inf, -Inf)),
    c("45.7", NA, NA, NA)
  )
  expect_identical(fmt_pct(NA_real_), NA_character_)
  expect_length(fmt_pct(numeric(0)), 0L)
})

test_that("pk_imputation_rules() applies the 1/3 rule", {
  # above threshold: only Median, Max, Geom_mean, counts are kept
  expect_identical(pk_imputation_rules("1.2", "Mean", 0.9, TRUE, "1/3"), "ND")
  expect_identical(pk_imputation_rules("1.2", "SD", 0.9, TRUE, "1/3"), "ND")
  expect_identical(pk_imputation_rules("1.2", "Median", 0.9, TRUE, "1/3"), "1.2")
  expect_identical(pk_imputation_rules("1.2", "Max", 0.9, TRUE, "1/3"), "1.2")
  expect_identical(pk_imputation_rules("3", "No. obs.", 0.9, TRUE, "1/3"), "3")
  # at or below threshold: everything is kept
  expect_identical(pk_imputation_rules("1.2", "Mean", 1 / 3, TRUE, "1/3"), "1.2")
  expect_identical(pk_imputation_rules("1.2", "Mean", 0.1, TRUE, "1/3"), "1.2")
})

test_that("pk_imputation_rules() applies the 1/3 rule for both dosing timings", {
  labels <- c("No. obs.", "Mean", "SD", "Median", "Max", "Geom_mean")
  predose <- vapply(
    labels, \(l) pk_imputation_rules("1.2", l, 0.9, FALSE, "1/3"), character(1)
  )
  postdose <- vapply(
    labels, \(l) pk_imputation_rules("1.2", l, 0.9, TRUE, "1/3"), character(1)
  )
  # pre-dose and post-dose currently keep the same set of statistics
  expect_identical(predose, postdose)
  expect_identical(unname(predose[c("Mean", "SD")]), c("ND", "ND"))
})

test_that("pk_imputation_rules() applies the 1/2 rule", {
  expect_identical(pk_imputation_rules("1.2", "Mean", 0.6, TRUE, "1/2"), "ND")
  expect_identical(pk_imputation_rules("1.2", "Median", 0.6, TRUE, "1/2"), "ND")
  expect_identical(pk_imputation_rules("1.2", "Max", 0.6, TRUE, "1/2"), "1.2")
  expect_identical(pk_imputation_rules("1.2", "Mean", 0.5, TRUE, "1/2"), "1.2")
})

test_that("pk_imputation_rules() marks a missing geometric mean as NE", {
  expect_identical(pk_imputation_rules(NA, "Geom_mean", 0.1, TRUE, "1/3"), "NE")
  expect_identical(pk_imputation_rules("NA", "Geom_mean", 0.1, TRUE, "1/3"), "NE")
})

test_that("pk_imputation_rules() returns the value when the rule cannot apply", {
  expect_identical(pk_imputation_rules("1.2", "Mean", NA, TRUE, "1/3"), "1.2")
  expect_identical(pk_imputation_rules("1.2", "Mean", 0.9, TRUE, NULL), "1.2")
})

test_that("exported PK helpers validate their inputs", {
  expect_error(cv("a"), "must be numeric")
  expect_error(geom_cv("a"), "must be numeric")
  expect_error(geom_mean("a"), "must be numeric")
  expect_error(geom_mean(1:5, na.rm = "yes"), "logical")
  expect_error(fmt_3sig("a"), "must be numeric")
  expect_error(fmt_pct("a"), "must be numeric")

  expect_error(pk_imputation_rules("1.2", 1, 0.5, TRUE), "must be a string")
  expect_error(pk_imputation_rules("1.2", "Mean", 0.5, "yes"), "logical")
  expect_error(pk_imputation_rules("1.2", "Mean", 1.5, TRUE), "interval")
})
