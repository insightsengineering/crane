skip_if_not(is_pkg_installed(c("survival", "withr")))

tbl <-
  tbl_survfit_times(
    data = cards::ADTTE,
    by = TRTA,
    times = c(30, 60)
  )

test_that("add_difference_row.tbl_survfit_times() works", {
  withr::local_options(list(width = 200))
  expect_silent(
    tbl1 <- tbl |>
      add_difference_row(reference = "Placebo")
  )
  expect_snapshot(as.data.frame(tbl1))

  # works with different reference column
  expect_silent(
    tbl2 <- tbl |>
      add_difference_row(reference = "Xanomeline Low Dose")
  )
  expect_equal(
    as.data.frame(tbl2) |> names(),
    c("", "Xanomeline Low Dose  \n(N = 84)", "Placebo  \n(N = 86)", "Xanomeline High Dose  \n(N = 84)")
  )

  # works with custom statistics/formats
  expect_silent(
    tbl3 <- tbl |>
      add_difference_row(
        reference = "Placebo",
        statistic = c("{estimate} ({std.error})", "{statistic} (p = {p.value})"),
        pvalue_fun = label_style_pvalue(digits = 3),
        estimate_fun = label_roche_number(digits = 1, scale = 100)
      )
  )
  expect_snapshot(as.data.frame(tbl3))

  # no error if overall column is present
  expect_silent(
    tbl4 <- tbl |>
      add_overall(last = TRUE) |>
      add_difference_row(reference = "Xanomeline High Dose")
  )
  expect_equal(
    as.data.frame(tbl4) |> names(),
    c("", "Xanomeline High Dose  \n(N = 84)", "Placebo  \n(N = 86)", "Xanomeline Low Dose  \n(N = 84)", "All Participants  \nN = 254")
  )
})

test_that("add_difference_row.tbl_survfit_times() error messaging works", {
  withr::local_options(list(width = 200))

  expect_snapshot(
    error = TRUE,
    tbl_survfit_times(
      data = cards::ADTTE,
      times = c(30, 60)
    ) |>
      add_difference_row("Placebo")
  )

  expect_snapshot(
    error = TRUE,
    tbl_survfit_times(
      data = cards::ADTTE,
      by = TRTA,
      times = c(30, 60)
    ) |>
      add_difference_row("No Treatment")
  )
})
