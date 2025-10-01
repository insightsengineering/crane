# Add grade variation to cards::ADLB `ANRIND` and `BNRIND`
withr::with_seed(123, {
  runif(1)
  adlb <- cards::ADLB |>
    dplyr::mutate(
      ANRIND = ifelse(is.na(ANRIND), NA, sample(c("High", "Low", "Normal"), dplyr::n(), replace = TRUE, prob = c(0.6, 0.2, 0.2))),
      BNRIND = ifelse(is.na(BNRIND), NA, sample(c("High", "Low", "Normal"), dplyr::n(), replace = TRUE, prob = c(0.4, 0.4, 0.2)))
    ) |>
    dplyr::select("USUBJID", "TRTA", "PARAM", "PARAMCD", "ANRIND", "BNRIND", "VISITNUM") |>
    dplyr::mutate(TRTA = factor(TRTA)) |>
    dplyr::filter(PARAMCD %in% c("CHOL", "GLUC")) |>
    dplyr::slice_max(by = c(USUBJID, PARAMCD), order_by = ANRIND, n = 1L, with_ties = FALSE) |>
    labelled::set_variable_labels(
      BNRIND = "Baseline  \nNCI-CTCAE Grade",
      ANRIND = "Post-baseline  \nNCI-CTCAE Grade"
    )
})

adsl <- cards::ADSL[c("USUBJID", "TRTA")] |>
  dplyr::mutate(TRTA = factor(TRTA))


test_that("tbl_shift(strata_location)", {
  withr::local_options(list(width = 190))

  expect_silent(
    tbl <-
      tbl_shift(
        data = dplyr::filter(adlb, PARAMCD %in% "CHOL"),
        strata = BNRIND,
        variable = ANRIND,
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
        strata = BNRIND,
        variable = ANRIND,
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
        strata = BNRIND,
        variable = ANRIND,
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
      strata = BNRIND,
      variable = ANRIND,
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
        strata = BNRIND,
        variable = ANRIND,
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
        strata = BNRIND,
        variable = ANRIND,
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
        strata = BNRIND,
        variable = ANRIND,
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
        strata = BNRIND,
        variable = ANRIND,
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
        variable = BNRIND,
        by = ANRIND,
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
