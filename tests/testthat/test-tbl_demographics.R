test_that("tbl_demographics() works", {
  expect_silent(
    tbl <-
      trial |>
      tbl_demographics(
        by = trt,
        include = c(age, grade)
      )
  )
})
