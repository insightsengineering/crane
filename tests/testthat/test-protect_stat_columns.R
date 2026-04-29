test_that("protect_stat_columns validates input correctly", {
  # Should fail with non-gtsummary objects
  expect_error(protect_stat_columns(mtcars), class = "rlang_error")
  expect_error(protect_stat_columns(NULL), class = "rlang_error")
})

test_that("protect_stat_columns replaces spaces in stat columns", {
  # Create a standard table
  tbl <- trial |>
    tbl_summary(by = trt, include = age) |>
    protect_stat_columns()

  # Get a value from a stat column (e.g., "47 (48%)")
  stat_val <- tbl$table_body$stat_1[1]

  # Check for the non-breaking space (decimal 160)
  # utf8ToInt will return 160 for \u00A0 instead of 32 for " "
  char_codes <- utf8ToInt(stat_val)

  expect_true(160 %in% char_codes)
  expect_false(32 %in% char_codes)
})

test_that("protect_stat_columns preserves spaces in the label column", {
  # We force a label with a clear space: "Patient Age"
  tbl <- trial |>
    tbl_summary(
      include = age,
      label = list(age ~ "Patient Age")
    ) |>
    protect_stat_columns()

  # Row 1 is the variable label row: "Patient Age"
  label_val <- tbl$table_body$label[1]
  char_codes <- utf8ToInt(label_val)

  # VERIFY:
  # 32 (standard space) SHOULD be present because 'label' was excluded.
  # 160 (non-breaking space) SHOULD NOT be present.
  expect_true(32 %in% char_codes, info = "Standard space missing from label")
  expect_false(160 %in% char_codes, info = "Label was incorrectly modified to use NBSP")
})

test_that("protect_stat_columns handles multiple stat columns including p-values", {
  # 1. Create table
  tbl <- trial |>
    tbl_summary(by = trt, include = age) |>
    add_p()

  # 2. Manually force a p-value with a space for testing purposes
  tbl$table_body$p.value[1] <- "< 0.001"
  tbl <- protect_stat_columns(tbl)
  p_val_string <- tbl$table_body$p.value[1]
  char_codes <- utf8ToInt(p_val_string)

  # Should contain 160 (Non-breaking) and NOT 32 (Standard)
  expect_true(160 %in% char_codes)
  expect_false(32 %in% char_codes)
})
