# tests/testthat/test-add_difference_row.R

test_that("add_difference_row.tbl_survfit_times is defunct and throws correct error", {
  # Why: We need a minimal mock object with the correct class to ensure S3
  # method dispatch correctly routes the call to our specific legacy function.
  mock_tbl <- structure(list(), class = c("tbl_survfit_times", "gtsummary"))

  # Why: Since the function's sole purpose is to prevent use and guide migration,
  # we test that it successfully aborts and that the error message explicitly
  # mentions the new extractor function (get_surv_diff_df).
  # Note: Parentheses in the regex must be escaped.
  expect_error(
    add_difference_row(mock_tbl),
    regexp = "Please use the new `get_surv_diff_df\\(\\)` function"
  )

  # Why: Verifying the first part of the error message to ensure the user knows
  # exactly which function is defunct.
  expect_error(
    add_difference_row(mock_tbl),
    regexp = "`add_difference_row\\(\\)` is defunct for `tbl_survfit_times`"
  )
})
