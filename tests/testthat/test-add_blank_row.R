test_that("add_blank_row() works", {
  expect_silent(
    tbl <-
      gtsummary::trial |>
      tbl_roche_summary(
        by = trt,
        include = c(age, grade)
      ) |>
      add_blank_row()
  )
})


test_that("add_blank_row() messaging", {
  expect_snapshot(
    error = TRUE,
    gtsummary::as_gtsummary(gtsummary::trial[1:5, 1:2]) |>
      add_blank_row()
  )
})
