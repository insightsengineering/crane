adlb <- pharmaverseadam::adlb[c("USUBJID", "TRT01A", "PARAMCD", "ATOXGRH", "BTOXGRH", "VISITNUM")]|>
  dplyr::mutate(TRT01A = factor(TRT01A)) |>
  dplyr::filter(PARAMCD %in% c("CHOLES", "GLUC")) |>
  dplyr::slice_max(by = c(USUBJID, PARAMCD), order_by = ATOXGRH, n = 1L, with_ties = FALSE) |>
  labelled::set_variable_labels(
    BTOXGRH = "Baseline  \nNCI-CTCAE Grade",
    ATOXGRH = "Post-baseline  \nNCI-CTCAE Grade"
  )

test_that("tbl_shift() works", {
  expect_silent(
    tbl <-
      tbl_shift(
        data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRT01A,
        data_header =
          pharmaverseadam::adsl[c("USUBJID", "TRT01A")] |>
          dplyr::filter(TRT01A != "Screen Failure")
      )
  )

  expect_snapshot(as.data.frame(tbl))
})



