test_that("roche_pvalue() works", {
  expect_equal(
    c(-1, 0, 0.0000000001, 0.5, 0.9, 1, 1.1) |>
      roche_pvalue(),
    c(NA, "<0.0001", "<0.0001", "0.5000", "0.9000", "1.0000", NA)
  )
})

test_that("roche_percent() works", {
  expect_equal(
    c(1, 0.0008, 0.9998, 0.998, 0.5, -1, 1.1, 0) |>
      roche_percent(suffix = "%"),
    c("100%", "<0.1%", ">99.9%", "99.8%", "50.0%", NA, NA, "0.0%")
  )

  expect_equal(
    c(1, 0.0008, 0.9998, 0.998, 0.5, -1, 1.1, 0) |>
      roche_percent(suffix = "%", digits = 2),
    c("100.0%", "<0.10%", ">99.90%", "99.80%", "50.00%", NA, NA, "0.00%")
  )

  expect_equal(
    c(1, 0.0008, 0.9998, 0.998, 0.5, -1, 1.1, 0) |>
      roche_percent(suffix = "%", digits = 4),
    c("100.000%", "<0.1000%", ">99.9000%", "99.8000%", "50.0000%", NA, NA, "0.0000%")
  )
})
