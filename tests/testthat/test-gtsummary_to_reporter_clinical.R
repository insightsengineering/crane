skip_if_pkg_not_installed("reporter")

test_that("gtsummary_to_reporter_clinical exports TXT for gtsummary input", {
  tmp <- tempfile("crane_report_", fileext = ".rtf")

  gts_tbl <- gtsummary::trial |>
    dplyr::select(trt, age, grade) |>
    gtsummary::tbl_summary(by = trt) |>
    gtsummary::add_p()

  out <- gtsummary_to_reporter_clinical(
    gts_obj = gts_tbl,
    file_path = tmp,
    output_types = "TXT",
    save_rds = FALSE
  )

  expect_type(out, "character")
  expect_length(out, 1)
  expect_true(file.exists(out[1]))
  expect_match(tolower(out[1]), "\\.txt$")
})
