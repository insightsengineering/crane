test_that("add_hierarchical_count_row() works", {
  tbl0 <-
    cards::ADAE |>
    # subset the data for a shorter example table
    dplyr::slice(1:10) |>
    tbl_hierarchical(
      by = "TRTA",
      variables = AEDECOD,
      denominator = cards::ADSL,
      id = "USUBJID",
      overall_row = TRUE
    )

  expect_silent(
    tbl <- add_hierarchical_count_row(tbl0, .after = 1L)
  )

  # check the ARD is present
  expect_true("add_hierarchical_count_row" %in% names(gtsummary::gather_ard(tbl)))

  expect_snapshot(
    tbl |>
      as.data.frame(col_label = FALSE) |>
      dplyr::slice(2)
  )

  # check results include overall column
  expect_snapshot(
    tbl0 |>
      add_overall() |>
      add_hierarchical_count_row(.after = 1L) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::slice(2)
  )
})
