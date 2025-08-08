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
