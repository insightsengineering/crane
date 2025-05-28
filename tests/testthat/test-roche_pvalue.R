test_that("roche_pvalue() works", {
  expect_equal(
    c(-1, 0, 0.0000000001, 0.5, 0.9, 1, 1.1) |>
      roche_pvalue(),
    c(NA, "<0.0001", "<0.0001", "0.5000", "0.9000", "1.0000", NA)
  )
})
