skip_if_pkg_not_installed(c("survival", "withr"))

test_that("tbl_survfit_times() works", {
  withr::local_options(list(width = 120))
  # Using the default value of the `y` argument
  expect_silent(
    tbl <-
      tbl_survfit_times(
        data = cards::ADTTE,
        by = "TRTA",
        times = c(30, 60)
      )
  )
  expect_snapshot(as.data.frame(tbl))

  # Specifying the `y` argument
  expect_silent(
    tbl <-
      tbl_survfit_times(
        data = gtsummary::trial,
        by = "trt",
        y = "survival::Surv(ttdeath, death)",
        times = c(12, 15)
      )
  )
  expect_snapshot(as.data.frame(tbl))

  # works for unstratified models
  expect_silent(
    tbl <-
      tbl_survfit_times(
        data = cards::ADTTE,
        times = c(30, 60),
        method.args = list(conf.int = 0.90)
      )
  )
  expect_snapshot(as.data.frame(tbl))

  # works with NSE inputs in `method.args()`
  expect_equal(
    tbl_survfit_times(
      data = cards::ADTTE,
      times = c(30, 60),
      method.args = list(id = SEX)
    ) |>
      gtsummary::gather_ard() |>
      getElement("tbl_survfit_times") |>
      dplyr::filter(variable == "time") |>
      dplyr::select(-fmt_fun),
    survival::survfit(survival::Surv(time = AVAL, event = 1 - CNSR) ~ 1, data = cards::ADTTE, id = SEX) |>
      cardx::ard_survival_survfit(times = c(30, 60)) |>
      dplyr::filter(variable == "time") |>
      dplyr::select(-fmt_fun)
  )
})

test_that("tbl_survfit_times(by) messaging", {
  withr::local_options(list(width = 120))
  expect_snapshot(
    error = TRUE,
    tbl_survfit_times(
      data = cards::ADTTE,
      by = everything(),
      times = 30
    )
  )

  expect_snapshot(
    error = TRUE,
    tbl_survfit_times(
      data = gtsummary::trial |> dplyr::rename(time = trt),
      by = "time",
      y = "survival::Surv(ttdeath, death)",
      times = 30
    )
  )
})

test_that("add_overall.tbl_survfit_times() works", {
  withr::local_options(list(width = 180))
  expect_snapshot(
    tbl_survfit_times(
      data = cards::ADTTE,
      by = "TRTA",
      times = 30,
      label = "Day {time}"
    ) |>
      add_overall(last = TRUE, col_label = "**All Participants**  \nN = {n}") |>
      as.data.frame()
  )
})
