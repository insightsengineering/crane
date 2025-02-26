skip_if_not(is_pkg_installed(c("survival", "withr")))

test_that("tbl_survfit_quantiles() works", {
  withr::local_options(list(width = 120))
  # Using the default value of the `y` argument
  expect_silent(
    tbl <-
      tbl_survfit_quantiles(
        data = cards::ADTTE,
        by = "TRTA"
      )
  )
  expect_snapshot(as.data.frame(tbl))

  # Specifying the `y` argument
  expect_silent(
    tbl <-
      tbl_survfit_quantiles(
        data = gtsummary::trial,
        by = "trt",
        y = "survival::Surv(ttdeath, death)"
      )
  )
  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_survfit_quantiles(by) messaging", {
  withr::local_options(list(width = 120))
  expect_snapshot(
    error = TRUE,
    tbl_survfit_quantiles(
      data = cards::ADTTE,
      by = everything()
    )
  )

  expect_snapshot(
    error = TRUE,
    tbl_survfit_quantiles(
      data = gtsummary::trial |> dplyr::rename(time = trt),
      by = "time",
      y = "survival::Surv(ttdeath, death)"
    )
  )
})

test_that("tbl_survfit_quantiles(estimate_fun)", {
  withr::local_options(list(width = 120))
  expect_snapshot(
    tbl_survfit_quantiles(
      data = cards::ADTTE,
      by = "TRTA",
      estimate_fun = gtsummary::label_style_number(digits = 3)
    ) |>
      as.data.frame()
  )
})

test_that("tbl_survfit_quantiles(method.args)", {
  # check the methods are passed to the survfit() object
  expect_equal(
    tbl_survfit_quantiles(
      data = cards::ADTTE,
      by = "TRTA",
      method.args = list(conf.int = 0.90, conf.type = "logit")
    ) |>
      gtsummary::gather_ard() |>
      dplyr::filter(context == "survival_survfit") |>
      dplyr::mutate(group1_level = map(group1_level, as.character)) |>
      dplyr::select(-"fmt_fn"),
    cardx::ard_survival_survfit(
      survival::survfit(Surv_CNSR() ~ TRTA, data = cards::ADTTE, conf.int = 0.90, conf.type = "logit"),
      probs = c(0.25, 0.50, 0.75)
    ) |>
      dplyr::mutate(group1_level = map(group1_level, as.character)) |>
      dplyr::select(-"fmt_fn")
  )
})
