test_that("tbl_null_report() works correctly", {
  expect_equal(
    tbl_null_report()$table_body |> nrow(),
    0
  )
  expect_identical(
    tbl_null_report()$table_styling$header$label,
    "Null Report: no observations met the reporting criteria for inclusion in this output."
  )
})
