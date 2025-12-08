test_that("tbl_null_report() works correctly", {
  expect_equal(
    tbl_null_report()$table_body |> nrow(),
    0
  )

  expect_identical(
    tbl_null_report()$table_styling$header$label,
    "No observations met the reporting criteria for this output."
  )

  expect_error(
    tbl_null_report(label = 1)
  )
})
