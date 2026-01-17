test_that("add_blank_rows() works", {
  expect_silent(
    tbl <-
      gtsummary::trial |>
      tbl_roche_summary(
        by = trt,
        include = c(age, grade)
      ) |>
      add_blank_rows(variables = everything())
  )
})

test_that("add_blank_rows(variables) works", {
  expect_silent(
    tbl <-
      gtsummary::trial |>
      tbl_roche_summary(
        by = trt,
        include = c(age, grade)
      ) |>
      add_blank_rows(variables = age)
  )

  # one blank row added (the NA value)
  expect_equal(
    tbl$table_body |>
      dplyr::pull(row_type) |>
      is.na() |>
      sum(),
    1L
  )
})

test_that("add_blank_rows(variables=grade) is last row, then table identical", {
  tbl <- gtsummary::trial |>
    tbl_roche_summary(
      by = trt,
      include = c(age, grade)
    ) |>
    add_blank_rows(variables = grade)
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

test_that("add_blank_rows(variable_level) works", {
  # splitting by all variables is the same as splittling by the 'variable' column levels
  expect_equal(
    gtsummary::trial |>
      tbl_roche_summary(
        by = trt,
        include = c(age, grade)
      ) |>
      add_blank_rows(variable_level = variable) |>
      as.data.frame(),
    gtsummary::trial |>
      tbl_roche_summary(
        by = trt,
        include = c(age, grade)
      ) |>
      add_blank_rows(variables = everything()) |>
      as.data.frame()
  )
})

test_that("add_blank_rows() error message", {
  # specifying more than one split method
  expect_snapshot(
    error = TRUE,
    trial |>
      tbl_roche_summary(
        by = trt, statistic = age ~ "{mean}",
        include = c(age, grade),
        nonmissing = "no"
      ) |>
      add_blank_rows(variables = age, row_numbers = c(1, 1))
  )
})

test_that("add_blank_rows(row_numbers) error", {
  expect_silent(
    out <- gtsummary::as_gtsummary(gtsummary::trial[1:3, 1:2]) |>
      add_blank_rows(row_numbers = c(1, 2))
  )

  # Change line 93 in test-add_blank_rows.R to:
  expect_equal(
    dplyr::pull(out$table_body[, 1], trt) |> as.character(), 
    c("Drug A", NA, "Drug B", NA, "Drug A")
  )
})

test_that("add_blank_rows() errors when no variable", {
  expect_snapshot(
    error = TRUE,
    gtsummary::as_gtsummary(gtsummary::trial[1:5, 1:2]) |>
      add_blank_rows(variables = everything())
  )
})
