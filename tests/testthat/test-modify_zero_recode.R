test_that("modify_zero_recode() works", {
  withr::local_options(list(width = 190))
  expect_snapshot(
    gtsummary::trial |>
      dplyr::mutate(
        trt = factor(trt, levels = c("Drug A", "Drug B", "Drug C")),
        grade = factor(grade, levels = c("I", "II", "III", "IV"))
      ) |>
      tbl_summary(
        by = trt,
        include = c(response, grade),
        statistic = response ~ "{n} / {N} ({p}%)",
        missing = "no"
      ) |>
      modify_zero_recode() |>
      as.data.frame()
  )

  expect_true(
    dplyr::tribble(
      ~stat_0, ~stat_recode_value,
      "0 (0%)", "0",
      "0 (0.0%)", "0",
      "0 (NA%)", "0",
      "0 / 12 (0%)", "0 / 12",
      "0 / 2 (0.0%)", "0 / 2",
      "0 / 0 (NA%)", "0 / 0",
      "0/12 (0%)", "0/12",
      # items NOT to be re-coded (since there is no space in front of the percentage)
      "0 / 12(0%)", "0 / 12(0%)",
      "0(0%)", "0(0%)"
    ) |>
      gtsummary::as_gtsummary() |>
      gtsummary::modify_column_unhide(everything()) |>
      modify_zero_recode() |>
      as.data.frame() %>%
      {.[, 1] == .[, 2]} |> # styler: off
      all()
  )
})

test_that("modify_zero_recode() works with non-breaking spaces", {
  expect_true(
    dplyr::tribble(
      ~stat_0, ~stat_recode_value,
      "0\u00A0(0%)", "0",
      "0\u00A0(0.0%)", "0",
      "0\u00A0(NA%)", "0",
      "0\u00A0/\u00A012\u00A0(0%)", "0\u00A0/\u00A012",
      "0\u00A0/\u00A02\u00A0(0.0%)", "0\u00A0/\u00A02",
      "0\u00A0/\u00A00\u00A0(NA%)", "0\u00A0/\u00A00",
      "0/12\u00A0(0%)", "0/12",
      # items NOT to be re-coded (since there is no space in front of the percentage)
      "0\u00A0/\u00A012(0%)", "0\u00A0/\u00A012(0%)",
      "0(0%)", "0(0%)"
    ) |>
      gtsummary::as_gtsummary() |>
      gtsummary::modify_column_unhide(everything()) |>
      modify_zero_recode() |>
      as.data.frame() %>%
      {.[, 1] == .[, 2]} |> # styler: off
      all()
  )
})
