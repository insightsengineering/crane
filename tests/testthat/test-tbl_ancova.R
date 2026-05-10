df_ancova <- cards::ADLB |>
  dplyr::filter(
    PARAMCD == "SODIUM",
    AVISIT == "Week 8"
  ) |>
  dplyr::mutate(TRTA = factor(TRTA))


test_that("tbl_ancova() works with default settings", {
  withr::local_options(list(width = 200))

  expect_silent(
    tbl <- tbl_ancova(
      data = df_ancova,
      formula = CHG ~ TRTA + BASE,
      by = TRTA,
      ref_group = "Placebo"
    )
  )

  expect_s3_class(tbl, "tbl_ancova")
  expect_s3_class(tbl, "gtsummary")

  # table has the expected row labels
  labels <- tbl$table_body$label[tbl$table_body$row_type == "level"]
  expect_true("n" %in% labels)
  expect_true("Adjusted Mean" %in% labels)
  expect_true("Difference in Adjusted Means" %in% labels)
  expect_true("p-value" %in% labels)

  # reference group has blank contrast cells
  ref_col <- tbl$table_body |>
    dplyr::filter(.data$label == "Difference in Adjusted Means") |>
    dplyr::pull("stat_1")
  expect_equal(ref_col, "")

  expect_snapshot(as.data.frame(tbl))
})


test_that("tbl_ancova() works with denominator", {
  expect_silent(
    tbl <- tbl_ancova(
      data = df_ancova,
      formula = CHG ~ TRTA + BASE,
      by = TRTA,
      ref_group = "Placebo",
      denominator = cards::ADSL
    )
  )

  expect_s3_class(tbl, "tbl_ancova")
})


test_that("tbl_ancova() works with conf.level = 0.90", {
  withr::local_options(list(width = 200))

  expect_silent(
    tbl <- tbl_ancova(
      data = df_ancova,
      formula = CHG ~ TRTA + BASE,
      by = TRTA,
      ref_group = "Placebo",
      conf.level = 0.90
    )
  )

  # CI label reflects 90%
  labels <- tbl$table_body$label[tbl$table_body$row_type == "level"]
  expect_true(any(grepl("90%", labels)))
})


test_that("tbl_ancova() errors on invalid ref_group", {
  expect_error(
    tbl_ancova(
      data = df_ancova,
      formula = CHG ~ TRTA + BASE,
      by = TRTA,
      ref_group = "NonexistentArm"
    ),
    "not found"
  )
})


test_that("tbl_ancova() works without covariates", {
  expect_silent(
    tbl <- tbl_ancova(
      data = df_ancova,
      formula = CHG ~ TRTA,
      by = TRTA,
      ref_group = "Placebo"
    )
  )

  expect_s3_class(tbl, "tbl_ancova")
})
