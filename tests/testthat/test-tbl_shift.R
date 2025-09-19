# No ATOXGRH or BOTOXGRH in cards::ADLB, generating these variables here
adlb <- cards::ADLB |>
  dplyr::mutate(
    ATOXGRH = factor(dplyr::case_when(
      LBNRIND == "HIGH" & A1HI >= 0 & A1HI <= 30 ~ "0",
      LBNRIND == "HIGH" & A1HI >= 31 & A1HI <= 100 ~ "1",
      LBNRIND == "HIGH" & A1HI >= 100 & A1HI <= 300 ~ "2",
      LBNRIND == "HIGH" & A1HI >= 400 & A1HI <= 500 ~ "3",
      TRUE ~ NA_character_
    )),
    BTOXGRH = factor(dplyr::case_when(
      LBNRIND == "HIGH" & BR2A1HI >= 0 & BR2A1HI <= 0.40 ~ "0",
      LBNRIND == "HIGH" & BR2A1HI >= 0.41 & BR2A1HI <= 0.80 ~ "1",
      LBNRIND == "HIGH" & BR2A1HI >= 0.81 & BR2A1HI <= 1.0 ~ "2",
      LBNRIND == "HIGH" & BR2A1HI >= 1.1 & BR2A1HI <= 1.50 ~ "3",
      TRUE ~ NA_character_
    ))
  ) |>
  dplyr::select("USUBJID", "TRTA", "PARAM", "PARAMCD", "ATOXGRH", "BTOXGRH", "VISITNUM") |>
  dplyr::mutate(TRTA = factor(TRTA)) |>
  dplyr::filter(PARAMCD %in% c("CHOL", "GLUC")) |>
  dplyr::slice_max(by = c(USUBJID, PARAMCD), order_by = ATOXGRH, n = 1L, with_ties = FALSE) |>
  labelled::set_variable_labels(
    BTOXGRH = "Baseline  \nNCI-CTCAE Grade",
    ATOXGRH = "Post-baseline  \nNCI-CTCAE Grade"
  )
adsl <- cards::ADSL[c("USUBJID", "TRTA")] |>
  dplyr::mutate(TRTA = factor(TRTA))


test_that("tbl_shift(strata_location)", {
  withr::local_options(list(width = 190))

  expect_silent(
    tbl <-
      tbl_shift(
        data = dplyr::filter(adlb, PARAMCD %in% "CHOL"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRTA,
        data_header = adsl,
        strata_location = "new_column"
      )
  )
  expect_snapshot(as.data.frame(tbl))

  expect_silent(
    tbl <-
      tbl_shift(
        data = dplyr::filter(adlb, PARAMCD %in% "CHOL"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRTA,
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
          dplyr::filter(adlb, PARAMCD %in% "CHOL") |>
          dplyr::mutate(TRTA = as.character(TRTA)),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRTA,
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
      data = dplyr::filter(adlb, PARAMCD %in% "CHOL"),
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
        data = dplyr::filter(adlb, PARAMCD %in% "CHOL"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRTA,
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
        data = dplyr::filter(adlb, PARAMCD %in% "CHOL"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRTA,
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
        data = dplyr::filter(adlb, PARAMCD %in% "CHOL"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        by = TRTA,
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
        data = dplyr::filter(adlb, PARAMCD %in% "CHOL"),
        strata = BTOXGRH,
        variable = ATOXGRH,
        data_header = adsl,
        strata_location = "new_column"
      ) |>
      add_overall()
  )
})

test_that("add_overall.tbl_shift(strata=NULL) messaging", {
  withr::local_options(list(width = 190))

  expect_no_warning(
    tbl <-
      dplyr::filter(adlb, PARAMCD %in% "CHOL") |>
      tbl_shift(
        variable = BTOXGRH,
        by = ATOXGRH,
        header = "{level}",
        strata_label = "{strata}, N={n}",
        label = list(TRTA = "Actual Treatment"),
        percent = "cell",
        nonmissing = "no"
      ) |>
      modify_spanning_header(all_stat_cols() ~ "Worst Post-baseline NCI-CTCAE Grade")
  )

  expect_snapshot(as.data.frame(tbl))
})
