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

test_that("modify_zero_recode() works with a mix of spaces and non-breaking spaces", {
  expect_true(
    dplyr::tribble(
      ~stat_0, ~stat_recode_value,
      " \u00A0 \u00A0 0\u00A0(0%)", " \u00A0 \u00A0 0",
      "\u00A0 \u00A00 (0.0%)", "\u00A0 \u00A00",
      "0 \u00A0(NA%)", "0",
      "0\u00A0/ 12\u00A0(0%)", "0\u00A0/ 12",
      "0 /\u00A02 (0.0%)", "0 /\u00A02",
      "0 \u00A0/\u00A0 0 (NA%)", "0 \u00A0/\u00A0 0",
      "0/12 \u00A0(0%)", "0/12",
      # items NOT to be re-coded (since there is no space in front of the percentage)
      "0\u00A0/ 12(0%)", "0\u00A0/ 12(0%)",
      " \u00A00(0%)", " \u00A00(0%)"
    ) |>
      gtsummary::as_gtsummary() |>
      gtsummary::modify_column_unhide(everything()) |>
      modify_zero_recode() |>
      as.data.frame() %>%
      {.[, 1] == .[, 2]} |> # styler: off
      all()
  )
})

test_that("modify_zero_recode() handles tricky clinical edge cases", {
  expect_true(
    dplyr::tribble(
      ~stat_0, ~stat_recode_value,

      # 1. LARGE DENOMINATORS (Commas and decimals)
      "0 / 1,234 (0%)", "0 / 1,234",
      "0\u00A0/\u00A01,000\u00A0(0.0%)", "0\u00A0/\u00A01,000",

      # 2. FALSE POSITIVES (Numerator is not zero, but contains zero)
      "10 (0%)", "10 (0%)",
      "10 / 20 (0%)", "10 / 20 (0%)",
      "100 / 1000 (0%)", "100 / 1000 (0%)",

      # 3. NON-ZERO PERCENTAGES (Should NOT be recoded)
      "0 (1%)", "0 (1%)",
      "0 (0.1%)", "0 (0.1%)",
      "0 / 12 (0.5%)", "0 / 12 (0.5%)",

      # 4. MULTI-DECIMAL PRECISION
      "0 (0.00%)", "0",
      "0 / 12 (0.00%)", "0 / 12"
    ) |>
      gtsummary::as_gtsummary() |>
      gtsummary::modify_column_unhide(everything()) |>
      modify_zero_recode() |>
      as.data.frame() %>%
      {.[, 1] == .[, 2]} |> # styler: off
      all()
  )
})
