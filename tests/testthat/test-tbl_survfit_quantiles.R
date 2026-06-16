skip_if_pkg_not_installed(c("survival", "withr"))

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

  # Specifying the `y` argument as a character string
  expect_silent(
    tbl_str <-
      tbl_survfit_quantiles(
        data = gtsummary::trial,
        by = "trt",
        y = "survival::Surv(ttdeath, death)"
      )
  )
  expect_snapshot(as.data.frame(tbl_str))

  # Specifying the `y` argument as an unquoted expression
  expect_silent(
    tbl_expr <-
      tbl_survfit_quantiles(
        data = gtsummary::trial,
        by = "trt",
        y = survival::Surv(ttdeath, death)
      )
  )
  expect_snapshot(as.data.frame(tbl_expr))

  # works for unstratified models
  expect_silent(
    tbl <-
      tbl_survfit_quantiles(
        data = cards::ADTTE,
        method.args = list(conf.int = 0.90)
      )
  )
  expect_snapshot(as.data.frame(tbl))

  # works with NSE inputs in `method.args()`
  expect_equal(
    tbl_survfit_quantiles(
      data = cards::ADTTE,
      method.args = list(id = SEX)
    ) |>
      gtsummary::gather_ard() |>
      getElement("tbl_survfit_quantiles") |>
      dplyr::filter(variable == "prob") |>
      dplyr::select(-fmt_fun),
    survival::survfit(survival::Surv(time = AVAL, event = 1 - CNSR) ~ 1, data = cards::ADTTE, id = SEX) |>
      cardx::ard_survival_survfit(probs = c(0.25, 0.50, 0.75)) |>
      dplyr::filter(variable == "prob") |>
      dplyr::select(-fmt_fun)
  )
})

test_that("tbl_survfit_quantiles() censoring asterisks logic", {
  tbl <- tbl_survfit_quantiles(
    data = cards::ADTTE,
    by = "TRTA"
  )

  # 1. ARD check: Ensure 'min' and 'max' stats are strictly numeric and do not contain '*'
  ard_minmax <- tbl$cards$tbl_survfit_quantiles |>
    dplyr::filter(stat_name %in% c("min", "max"))

  expect_false(any(grepl("\\*", as.character(unlist(ard_minmax$stat)))))
  expect_true(all(vapply(ard_minmax$stat, is.numeric, FUN.VALUE = logical(1))))

  # 2. Table check: Ensure '*' is printed in the Range row of the final table body
  range_row <- tbl$table_body |> dplyr::filter(label == "Range")

  # ADTTE has censored maximums, so we expect at least one '*' to be rendered in the table columns
  expect_true(any(grepl("\\*", unlist(range_row))))
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
      estimate_fun = label_roche_number(digits = 3, na = NA_character_)
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
      getElement("tbl_survfit_quantiles") |>
      dplyr::filter(context == "survival_survfit") |>
      dplyr::filter(variable == "prob") |>
      dplyr::mutate(group1_level = map(group1_level, as.character)) |>
      dplyr::select(-"fmt_fun"),
    cardx::ard_survival_survfit(
      survival::survfit(survival::Surv(time = AVAL, event = 1 - CNSR) ~ TRTA, data = cards::ADTTE, conf.int = 0.90, conf.type = "logit"),
      probs = c(0.25, 0.50, 0.75)
    ) |>
      dplyr::filter(variable == "prob") |>
      dplyr::mutate(group1_level = map(group1_level, as.character)) |>
      dplyr::select(-"fmt_fun")
  )
})

test_that("add_overall.tbl_survfit_quantiles() works", {
  withr::local_options(list(width = 180))
  expect_snapshot(
    tbl_survfit_quantiles(
      data = cards::ADTTE,
      by = "TRTA"
    ) |>
      add_overall(last = TRUE, col_label = "**All Participants**  \nN = {n}") |>
      as.data.frame()
  )
})
