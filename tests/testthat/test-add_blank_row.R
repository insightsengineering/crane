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

test_that("add_blank_row(age) works", {
  expect_silent(
    tbl <-
      gtsummary::trial |>
      tbl_roche_summary(
        by = trt,
        include = c(age, grade)
      ) |>
      add_blank_row(age)
  )

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age") |>
      dplyr::pull(row_type),
    c("label", "level", "level", "level", NA)
  )
})

test_that("add_blank_row(grade) is last row, then table identical", {
  tbl <- gtsummary::trial |>
    tbl_roche_summary(
      by = trt,
      include = c(age, grade)
    ) |>
    add_blank_row(grade)
  tbl2 <- gtsummary::trial |>
    tbl_roche_summary(
      by = trt,
      include = c(age, grade)
    )

  expect_equal(
    tbl$table_body,
    tbl2$table_body
  )
})

test_that("add_blank_row(grade) is last row, then table identical", {
  tbl <- trial |>
    tbl_roche_summary(
      by = trt, statistic = age ~ "{mean}",
      include = c(age, grade),
      nonmissing = "no"
    ) |>
    add_blank_row(age, row_numbers = c(1, 1))

  expect_equal(
    tbl$table_body |>
      dplyr::filter(variable == "age" | str_detect(variable, "^row_sep")) |>
      dplyr::pull(row_type),
    c("label", NA, NA, "level", NA)
  )
})

test_that("add_blank_row() messaging", {
  expect_snapshot(
    error = TRUE,
    gtsummary::as_gtsummary(gtsummary::trial[1:5, 1:2]) |>
      add_blank_row()
  )
})

test_that("add_blank_row(row_numbers) error", {
  expect_silent(
    out <- gtsummary::as_gtsummary(gtsummary::trial[1:3, 1:2]) |>
      add_blank_row(row_numbers = c(1, 2))
  )

  # expect_equal(
  #   out$table_body[, 1],
  # )
})
