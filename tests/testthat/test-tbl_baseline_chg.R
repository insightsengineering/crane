adlb <- cards::ADLB |>
  dplyr::mutate(TRTA = as.factor(.data$TRTA))
df <- adlb[!grepl("unscheduled", adlb$VISIT, ignore.case = TRUE), ]

test_that("tbl_baseline_chg() works", {
  withr::local_options(list(width = 120))

  # loading lab data
  expect_silent(
    tbl <-
      tbl_baseline_chg(
        data = df,
        test_variable = "PARAMCD",
        test_cd = "SODIUM",
        baseline_level = "SCREENING 1",
        by = "TRTA",
        denominator = cards::ADSL
      )
  )
  expect_snapshot(as.data.frame(tbl))
})

test_that("add_overall.tbl_baseline_chg() works", {
  withr::local_options(list(width = 190))

  # test overall column
  expect_silent(
    tbl <-
      tbl_baseline_chg(
        data = df,
        test_variable = "PARAMCD",
        test_cd = "SODIUM",
        baseline_level = "SCREENING 1",
        by = "TRTA",
        denominator = cards::ADSL
      ) |>
      add_overall(last = TRUE, col_label = "All Participants  \nN = {n}")
  )
  expect_snapshot(as.data.frame(tbl))
})

test_that("add_overall.tbl_baseline_chg() messaging", {
  withr::local_options(list(width = 190))

  # expect message about no by variable
  expect_snapshot(
    tbl <-
      tbl_baseline_chg(
        data = df,
        test_variable = "PARAMCD",
        test_cd = "SODIUM",
        baseline_level = "SCREENING 1",
        denominator = cards::ADSL
      ) |>
      add_overall()
  )

  # message about different structures before the merge
  expect_snapshot(
    tbl <-
      tbl_baseline_chg(
        data = df,
        test_variable = "PARAMCD",
        test_cd = "SODIUM",
        by = "TRTA",
        baseline_level = "SCREENING 1",
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

  # Missing test_variable
  expect_error(
    tbl_baseline_chg(
      data = df,
      test_cd = "SODIUM",
      baseline_level = "SCREENING 1",
      denominator = cards::ADSL
    ),
    "test_variable"
  )

  # Missing test_cd
  expect_error(
    tbl_baseline_chg(
      data = df,
      test_variable = "PARAMCD",
      baseline_level = "SCREENING 1",
      denominator = df
    ),
    "test_cd"
  )

  # Missing denominator
  expect_error(
    tbl_baseline_chg(
      data = df,
      test_cd = "TEST1",
      test_variable = "TEST",
      baseline_level = "BASELINE"
    ),
    "denominator"
  )
  # `by` is not found in data
  expect_error(
    tbl_baseline_chg(
      data = df,
      test_cd = "SODIUM",
      test_variable = "PARAMCD",
      baseline_level = "SCREENING 1",
      by = "ARM",
      denominator = cards::ADSL
    )
  )

  # warning about baseline level not in the visit variable
  expect_error(
    expect_warning(
      tbl <- tbl_baseline_chg(
        data = df,
        test_variable = "PARAMCD",
        test_cd = "SODIUM",
        baseline_level = "BASELINE",
        by = "TRTA",
        denominator = cards::ADSL
      ),
      "The `baseline_level` \"BASELINE\" is not found in the \"VISIT\" variable."
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
        test_variable = "PARAMCD",
        test_cd = "SODIUM",
        baseline_level = "SCREENING 1",
        by = "TRTA",
        denominator = cards::ADSL
      )
  )
})
