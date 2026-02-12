skip_if_pkg_not_installed("mmrm")

test_that("tbl_mmrm() works with mmrm models", {
  skip_on_cran()
  
  # Fit a simple MMRM model
  fit_mmrm <- mmrm::mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  
  # Test that tbl_mmrm creates a table without error
  expect_no_error(
    tbl <- tbl_mmrm(fit_mmrm)
  )
  
  # Check that the result is a gtsummary object
  expect_s3_class(tbl, "gtsummary")
  expect_s3_class(tbl, "tbl_regression")
})

test_that("tbl_mmrm() works with additional arguments", {
  skip_on_cran()
  
  # Fit a simple MMRM model
  fit_mmrm <- mmrm::mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  
  # Test with include argument
  expect_no_error(
    tbl <- tbl_mmrm(fit_mmrm, include = c("RACE", "SEX"))
  )
  
  # Test with conf.level argument
  expect_no_error(
    tbl <- tbl_mmrm(fit_mmrm, conf.level = 0.90)
  )
})

test_that("tbl_mmrm() gives informative error for non-mmrm objects", {
  # Test with wrong object type
  lm_fit <- lm(mpg ~ cyl, data = mtcars)
  
  expect_error(
    tbl_mmrm(lm_fit),
    "must be an object of class"
  )
})

test_that("tbl_mmrm() works like tbl_regression", {
  skip_on_cran()
  
  # Fit a simple MMRM model
  fit_mmrm <- mmrm::mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = mmrm::fev_data
  )
  
  # Both should produce equivalent output
  tbl1 <- tbl_mmrm(fit_mmrm)
  tbl2 <- gtsummary::tbl_regression(fit_mmrm)
  
  # Check that table_body is the same
  expect_equal(
    tbl1$table_body,
    tbl2$table_body
  )
})
