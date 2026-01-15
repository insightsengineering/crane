df_2params <- cards::ADLB |>
  dplyr::mutate(
    AVISIT = str_trim(AVISIT),
    TRTA = as.factor(TRTA)
  ) |>
  dplyr::filter(
    AVISIT != "End of Treatment",
    PARAMCD %in% c("SODIUM", "K")
  )

df <- df_2params |>
  dplyr::filter(PARAMCD == "SODIUM")


test_that("tbl_baseline_chg() works", {
  withr::local_options(list(width = 120))
  expect_silent(
    tbl <-
      tbl_baseline_chg(
        data = df,
        baseline_level = "Baseline",
        by = "TRTA",
        denominator = cards::ADSL
      )
  )
  expect_snapshot(as.data.frame(tbl)[1:25, 1:5])

  # non-string variable input works
  expect_silent(
    tbl <-
      tbl_baseline_chg(
        data = df,
        baseline_level = "Baseline",
        by = TRTA,
        id = USUBJID,
        visit = AVISIT,
        visit_number = AVISITN,
        analysis_variable = AVAL,
        change_variable = CHG,
        denominator = cards::ADSL
      )
  )
})

test_that("tbl_baseline_chg() works with no `by` variable", {
  withr::local_options(list(width = 120))

  expect_silent(
    tbl <-
      tbl_baseline_chg(
        data = df,
        baseline_level = "Baseline",
        denominator = cards::ADSL
      )
  )
  expect_snapshot(as.data.frame(tbl)[1:25, ])
})

test_that("add_overall.tbl_baseline_chg() works", {
  withr::local_options(list(width = 190))

  # test overall column
  expect_silent(
    tbl <-
      tbl_baseline_chg(
        data = df,
        baseline_level = "Baseline",
        by = "TRTA",
        denominator = cards::ADSL
      ) |>
      add_overall(last = TRUE)
  )
  expect_snapshot(as.data.frame(tbl)[1:25, c(1, 4:9)])
})

test_that("add_overall.tbl_baseline_chg() messaging", {
  withr::local_options(list(width = 190))

  # expect message about no by variable
  expect_snapshot(
    tbl <-
      tbl_baseline_chg(
        data = df,
        baseline_level = "Baseline",
        denominator = cards::ADSL
      ) |>
      add_overall()
  )

  # message about different structures before the merge
  expect_snapshot(
    tbl <-
      tbl_baseline_chg(
        data = df,
        by = "TRTA",
        baseline_level = "Baseline",
        denominator = cards::ADSL
      ) |>
      modify_table_body(
        ~ .x |>
          dplyr::filter(dplyr::row_number() %in% 1:5)
      ) |>
      add_overall()
  )
})

test_that("tbl_baseline_chg() throws error when required arguments are missing", {
  # Missing denominator
  expect_error(
    tbl_baseline_chg(
      data = df,
      baseline_level = "Baseline"
    ),
    "denominator"
  )
  # `by` is not found in data
  expect_error(
    tbl_baseline_chg(
      data = df,
      baseline_level = "Baseline",
      by = "ARM",
      denominator = cards::ADSL
    )
  )

  # warning about baseline level not in the visit variable
  expect_error(
    expect_warning(
      tbl <- tbl_baseline_chg(
        data = df,
        baseline_level = "SCREENING 1",
        by = "TRTA",
        denominator = cards::ADSL
      ),
      "The `baseline_level` \"SCREENING 1\" is not found in the \"AVISIT\" variable."
    )
  )

  # expect message about duplicate visit entries for each subject
  duplicates <- df |>
    dplyr::group_by(USUBJID, AVISIT) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  test_data <- dplyr::bind_rows(df, duplicates)

  expect_snapshot(
    error = TRUE,
    tbl <-
      tbl_baseline_chg(
        data = test_data,
        baseline_level = "Baseline",
        denominator = cards::ADSL
      )
  )
})

test_that("tbl_baseline_chg() messaging", {
  withr::local_options(list(width = 190))

  # expecting message about converting by variable to a factor
  expect_snapshot(
    tbl <-
      tbl_baseline_chg(
        data = df |> dplyr::mutate(TRTA = as.character(TRTA)),
        baseline_level = "Baseline",
        by = "TRTA",
        denominator = cards::ADSL
      )
  )
})

test_that("gather_ard() works on output table", {
  withr::local_options(list(width = 190, pillar.print_min = 2))
  tbl <-
    tbl_baseline_chg(
      data = df,
      baseline_level = "Baseline",
      by = "TRTA",
      denominator = cards::ADSL
    ) |>
    add_overall()

  expect_snapshot(
    gather_ard(tbl)
  )
})

test_that("tbl_baseline_chg(split_by = PARAM) works", {
  withr::local_options(list(width = 120))
  expect_no_error(
    tbl <-
      tbl_baseline_chg(
        data = df_2params,
        baseline_level = "Baseline",
        by = "TRTA",
        split_by = "PARAMCD",
        denominator = cards::ADSL
      )
  )
  expect_equal(length(tbl), 2)
  expect_snapshot(names(tbl))
  expect_snapshot(as.data.frame(tbl[[2]])[1:25, 1:5])

  # non-string variable input works
  expect_no_error(
    tbl <-
      tbl_baseline_chg(
        data = df_2params,
        baseline_level = "Baseline",
        by = TRTA,
        split_by = PARAM,
        id = USUBJID,
        visit = AVISIT,
        visit_number = AVISITN,
        analysis_variable = AVAL,
        change_variable = CHG,
        denominator = cards::ADSL
      )
  )
})
