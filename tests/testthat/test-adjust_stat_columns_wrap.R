test_that("adjust_stat_columns_wrap validates input correctly", {
  expect_error(adjust_stat_columns_wrap(mtcars), class = "rlang_error")
  expect_error(adjust_stat_columns_wrap(NULL), class = "rlang_error")
  # Check invalid mode argument
  expect_error(adjust_stat_columns_wrap(trial |> tbl_summary(), mode = "invalid"), class = "rlang_error")
})

test_that("adjust_stat_columns_wrap protects (replaces spaces in stat columns)", {
  tbl <- trial |>
    tbl_summary(by = trt, include = age) |>
    adjust_stat_columns_wrap(mode = "protect")

  stat_val <- tbl$table_body$stat_1[1]
  char_codes <- utf8ToInt(stat_val)

  expect_true(160 %in% char_codes)
  expect_false(32 %in% char_codes)
})

test_that("adjust_stat_columns_wrap unprotects (reverts to standard spaces)", {
  # 1. First protect the table
  tbl_protected <- trial |>
    tbl_summary(by = trt, include = age) |>
    adjust_stat_columns_wrap(mode = "protect")

  # 2. Then unprotect it
  tbl_unprotected <- tbl_protected |>
    adjust_stat_columns_wrap(mode = "unprotect")

  stat_val <- tbl_unprotected$table_body$stat_1[1]
  char_codes <- utf8ToInt(stat_val)

  # 3. Verify it reverted to standard spaces (32) and removed non-breaking (160)
  expect_true(32 %in% char_codes)
  expect_false(160 %in% char_codes)
})

test_that("adjust_stat_columns_wrap preserves spaces in the label column", {
  tbl <- trial |>
    tbl_summary(
      include = age,
      label = list(age ~ "Patient Age")
    ) |>
    adjust_stat_columns_wrap(mode = "protect")

  label_val <- tbl$table_body$label[1]
  char_codes <- utf8ToInt(label_val)

  expect_true(32 %in% char_codes, info = "Standard space missing from label")
  expect_false(160 %in% char_codes, info = "Label was incorrectly modified to use NBSP")
})

test_that("adjust_stat_columns_wrap handles multiple stat columns including p-values", {
  tbl <- trial |>
    tbl_summary(by = trt, include = age) |>
    add_p()

  # Manually force a p-value with a space for testing purposes
  tbl$table_body$p.value[1] <- "< 0.001"

  tbl <- adjust_stat_columns_wrap(tbl, mode = "protect")

  p_val_string <- tbl$table_body$p.value[1]
  char_codes <- utf8ToInt(p_val_string)

  expect_true(160 %in% char_codes)
  expect_false(32 %in% char_codes)
})
