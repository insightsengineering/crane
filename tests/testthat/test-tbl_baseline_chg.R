test_that("tbl_baseline_chg() works", {
  withr::local_options(list(width = 120))
  # loading lab data
  adlb <- cards::ADLB |>
    dplyr::mutate(TRTA = as.factor(.data$TRTA))
  df <- adlb[!grepl("unscheduled", adlb$VISIT, ignore.case = TRUE), ]

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

  adlb <- cards::ADLB |>
    dplyr::mutate(TRTA = as.factor(.data$TRTA))
  df <- adlb[!grepl("unscheduled", adlb$VISIT, ignore.case = TRUE), ]
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

test_that("tbl_baseline_chg() throws error when required arguments are missing", {
  adlb <- cards::ADLB |>
    dplyr::mutate(TRTA = as.factor(.data$TRTA))
  df <- adlb[!grepl("unscheduled", adlb$VISIT, ignore.case = TRUE), ]

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
})

test_that("tbl_baseline_chg throws error if `by` is not found in data", {
  adlb <- cards::ADLB |>
    dplyr::mutate(TRTA = as.factor(.data$TRTA))
  df <- adlb[!grepl("unscheduled", adlb$VISIT, ignore.case = TRUE), ]

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
})
