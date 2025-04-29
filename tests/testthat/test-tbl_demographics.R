skip_if_not(is_pkg_installed("withr"))

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
  withr::local_options(list(width = 300))
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
      modify_header(label = "") |>
      as.data.frame()
  )

  expect_snapshot(
    tbl_demographics(
      trial,
      by = trt,
      digits = list(grade = list(p = 1)),
      include = c(age, grade)
    ) |>
      add_overall() |>
      as.data.frame()
  )

  # addresses issue #41
  expect_equal(
    trial |>
      dplyr::mutate(grade = fct_expand(grade, "other")) |>
      tbl_demographics(
        by = trt,
        include = grade
      ) |>
      gtsummary::add_overall() |>
      as.data.frame(col_labels = FALSE) |>
      dplyr::pull("stat_0") |>
      dplyr::last(),
    "0"
  )
})
