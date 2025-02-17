test_that("add_blank_row() works", {
  expect_silent(
    tbl <-
      trial |>
      tbl_demographics(
        by = trt,
        include = c(age, grade)
      ) |>
      add_blank_row()
  )
})


test_that("add_blank_row() messaging", {
  expect_snapshot(
    error = TRUE,
    as_gtsummary(trial[1:5, 1:2]) |>
      add_blank_row()
  )
})
