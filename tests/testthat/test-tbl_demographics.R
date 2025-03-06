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

test_that("tbl_demographics() |> add_overall() works", {
  expect_equal(
    tbl_demographics(
      trial,
      by = trt,
      include = c(age, grade),
      digits = list(grade = list(p = 1)),
      nonmissing = "no"
    ) |>
      add_overall() |>
      as.data.frame(),
    tbl_summary(
      trial,
      by = trt,
      include = c(age, grade),
      type = list(age = "continuous2"),
      statistic = list(age = c("{mean} ({sd})", "{median}", "{min} - {max}")),
      digits = list(grade = list(p = 1)),
      missing = "no"
    ) |>
      add_overall() |>
      as.data.frame()
  )

  expect_snapshot(
    tbl_demographics(
      trial,
      by = trt,
      include = c(age, grade)
    ) |>
      add_overall() |>
      as.data.frame()
  )
})
