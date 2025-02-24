test_that("tbl_demographics() works", {
  expect_silent(
    tbl <-
      gtsummary::trial |>
      tbl_demographics(
        by = trt,
        include = c(age, grade)
      )
  )
})
