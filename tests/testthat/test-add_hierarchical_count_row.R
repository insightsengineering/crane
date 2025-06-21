test_that("add_hierarchical_count_row() works", {
  tbl <-
    cards::ADAE |>
    # subset the data for a shorter example table
    dplyr::slice(1:10) |>
    tbl_hierarchical(
      by = "TRTA",
      variables = AEDECOD,
      denominator = cards::ADSL |> dplyr::rename(TRTA = TRT01A),
      id = "USUBJID",
      overall_row = TRUE
    )

  expect_snapshot(
    tbl |>
      add_hierarchical_count_row(.after = 1L) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::slice(2)
  )

  # check results include overall column
  expect_snapshot(
    tbl |>
      add_overall() |>
      add_hierarchical_count_row(.after = 1L) |>
      as.data.frame(col_label = FALSE) |>
      dplyr::slice(2)
  )
})
