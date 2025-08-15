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
      ~stat_0, ~recode_value,
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
