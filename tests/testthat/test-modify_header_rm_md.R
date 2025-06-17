test_that("strip_md_bold() works with gtsummary table", {
  expect_false(
    tbl_strata(
      trial,
      strata = "grade",
      ~ tbl_summary(.x, include = age)
    ) |>
      modify_header_rm_md(md = "bold") |>
      as.data.frame() |>
      names() |>
      str_detect(pattern = "*", fixed = TRUE) |>
      any()
  )

  expect_false(
    tbl_strata(
      trial,
      strata = "grade",
      ~ tbl_summary(.x, include = age),
      .header = "_{strata}_"
    ) |>
      modify_header_rm_md(md = "italic", type = "underscore") |>
      getElement("table_styling") |>
      getElement("spanning_header") |>
      getElement("spanning_header") |>
      str_detect(pattern = "_", fixed = TRUE) |>
      any()
  )
})
