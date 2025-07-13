adlb <- pharmaverseadam::adlb |>
  dplyr::select("USUBJID", "TRT01A", "PARAM", "PARAMCD", "ATOXGRH", "BTOXGRH", "VISITNUM") |>
  dplyr::mutate(TRT01A = factor(TRT01A)) |>
  dplyr::filter(PARAMCD %in% c("CHOLES", "GLUC")) |>
  dplyr::slice_max(by = c(USUBJID, PARAMCD), order_by = ATOXGRH, n = 1L, with_ties = FALSE) |>
  labelled::set_variable_labels(
    BTOXGRH = "Baseline  \nNCI-CTCAE Grade",
    ATOXGRH = "Post-baseline  \nNCI-CTCAE Grade"
  )
adsl <- pharmaverseadam::adsl[c("USUBJID", "TRT01A")] |>
  dplyr::filter(TRT01A != "Screen Failure")


test_that("tbl_shift(strata_location)", {
  withr::local_options(list(width = 190))

  expect_silent(
    tbl <-
      tbl_shift(
        data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRT01A,
        data_header = adsl,
        strata_location = "new_column"
      )
  )
  expect_snapshot(as.data.frame(tbl))

  expect_silent(
    tbl <-
      tbl_shift(
        data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRT01A,
        data_header = adsl,
        strata_location = "header"
      )
  )
  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_shift(by) messaging", {
  withr::local_options(list(width = 190))

  # expecting message about converting by variable to a factor
  expect_snapshot(
    tbl <-
      tbl_shift(
        data =
          dplyr::filter(adlb, PARAMCD %in% "CHOLES") |>
          dplyr::mutate(TRT01A = as.character(TRT01A)),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRT01A,
        data_header = adsl,
        strata_location = "new_column"
      )
  )
})

test_that("tbl_shift(data_header) messaging", {
  withr::local_options(list(width = 190))

  # expect messaging about the variables included in `data_header`
  expect_snapshot(
    error = TRUE,
    tbl_shift(
      data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"),
      strata = BTOXGRH,
      variable = ATOXGRH,
      data_header = adsl |> dplyr::mutate(asldfk = TRUE),
      strata_location = "new_column"
    )
  )
})


test_that("add_overall.tbl_shift()", {
  withr::local_options(list(width = 190))

  # works with new column
  expect_silent(
    tbl <-
      tbl_shift(
        data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRT01A,
        data_header = adsl,
        strata_location = "new_column"
      ) |>
      add_overall(col_label = "test label N = {n}", last = TRUE)
  )
  expect_snapshot(as.data.frame(tbl))

  # works with strata in header column
  expect_silent(
    tbl <-
      tbl_shift(
        data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRT01A,
        data_header = adsl,
        strata_location = "header"
      ) |>
      add_overall(col_label = "test label N = {n}", last = TRUE)
  )
  expect_snapshot(as.data.frame(tbl))
})


test_that("add_overall.tbl_shift() messaging", {
  withr::local_options(list(width = 190))

  # message about different structures before the merge
  expect_snapshot(
    tbl <-
      tbl_shift(
        data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRT01A,
        data_header = adsl,
        strata_location = "new_column"
      ) |>
      modify_table_body(
        ~ .x |>
          dplyr::filter(dplyr::row_number() %in% 1:5)
      ) |>
      add_overall()
  )

  # expect message about no by variable
  expect_snapshot(
    tbl <-
      tbl_shift(
        data = dplyr::filter(adlb, PARAMCD %in% "CHOLES"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        data_header = adsl,
        strata_location = "new_column"
      ) |>
      add_overall()
  )
})


