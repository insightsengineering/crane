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
  withr::local_options(list(width = 200))

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

  # header Ns come from ADSL, not from the analysis data
  adsl_n <- cards::ADSL |>
    dplyr::count(TRTA)
  headers <- tbl$table_styling$header |>
    dplyr::filter(grepl("^stat_", .data$column))
  for (i in seq_len(nrow(adsl_n))) {
    matching_header <- headers$label[grepl(adsl_n$TRTA[i], headers$label, fixed = TRUE)]
    expect_true(
      length(matching_header) == 1 && grepl(paste0("N = ", adsl_n$n[i]), matching_header),
      label = paste("Header for", adsl_n$TRTA[i], "should show N =", adsl_n$n[i])
    )
  }

  expect_snapshot(as.data.frame(tbl))
})


test_that("tbl_ancova() denominator headers align when ref_group is not alphabetically first", {
  withr::local_options(list(width = 200))

  # Arms where the reference group ("B: Placebo") sorts after "A: Drug X"
  df_alpha <- df_ancova |>
    dplyr::mutate(ARM = dplyr::case_when(
      TRTA == "Placebo" ~ "B: Placebo",
      TRTA == "Xanomeline High Dose" ~ "A: Drug X",
      TRTA == "Xanomeline Low Dose" ~ "C: Combination"
    ))
  adsl_alpha <- cards::ADSL |>
    dplyr::mutate(ARM = dplyr::case_when(
      TRTA == "Placebo" ~ "B: Placebo",
      TRTA == "Xanomeline High Dose" ~ "A: Drug X",
      TRTA == "Xanomeline Low Dose" ~ "C: Combination"
    ))

  expect_silent(
    tbl <- tbl_ancova(
      data = df_alpha,
      formula = CHG ~ ARM + BASE,
      by = ARM,
      ref_group = "B: Placebo",
      denominator = adsl_alpha
    )
  )

  # verify each column header matches its data:
  # the reference group column must show blank contrast rows
  headers <- tbl$table_styling$header |>
    dplyr::filter(grepl("^stat_", .data$column))
  ref_col <- headers$column[grepl("B: Placebo", headers$label)]

  diff_row <- tbl$table_body |>
    dplyr::filter(.data$label == "Difference in Adjusted Means")
  # reference column must be blank

  expect_equal(diff_row[[ref_col]], "")
  # non-reference columns must have numeric values
  non_ref_cols <- setdiff(headers$column, ref_col)
  for (col in non_ref_cols) {
    expect_false(diff_row[[col]] == "", label = paste("column", col, "should not be blank"))
  }
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
  expect_snapshot(
    tbl_ancova(
      data = df_ancova,
      formula = CHG ~ TRTA + BASE,
      by = TRTA,
      ref_group = "NonexistentArm"
    ),
    error = TRUE
  )
})


test_that("tbl_ancova() works with custom label", {
  withr::local_options(list(width = 200))

  expect_silent(
    tbl <- tbl_ancova(
      data = df_ancova,
      formula = CHG ~ TRTA + BASE,
      by = TRTA,
      ref_group = "Placebo",
      label = "Sodium (mmol/L)"
    )
  )

  expect_s3_class(tbl, "tbl_ancova")

  # the custom label appears in the table body instead of the variable name
  top_label <- tbl$table_body$label[tbl$table_body$row_type == "label"]
  expect_true("Sodium (mmol/L)" %in% top_label)
  expect_false("CHG" %in% top_label)
  expect_false("Change from Baseline" %in% top_label)

  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_ancova() errors on non-string label", {
  expect_snapshot(
    tbl_ancova(
      data = df_ancova,
      formula = CHG ~ TRTA + BASE,
      by = TRTA,
      ref_group = "Placebo",
      label = 123
    ),
    error = TRUE
  )
})


test_that("tbl_ancova() works with Dunnett adjustment", {
  withr::local_options(list(width = 200))

  expect_silent(
    tbl <- tbl_ancova(
      data = df_ancova,
      formula = CHG ~ TRTA + BASE,
      by = TRTA,
      ref_group = "Placebo",
      adjust = "dunnett"
    )
  )

  expect_s3_class(tbl, "tbl_ancova")
  expect_equal(attr(tbl, "adjust"), "dunnett")
  expect_equal(attr(tbl, "method"), "lm")
  expect_equal(attr(tbl, "package"), "stats")
  expect_equal(attr(tbl, "by"), "TRTA")
  expect_equal(attr(tbl, "ref_group"), "Placebo")

  expect_snapshot(as.data.frame(tbl))
})


test_that("tbl_ancova() works without covariates", {
  withr::local_options(list(width = 200))

  expect_silent(
    tbl <- tbl_ancova(
      data = df_ancova,
      formula = CHG ~ TRTA,
      by = TRTA,
      ref_group = "Placebo"
    )
  )

  expect_s3_class(tbl, "tbl_ancova")

  # verify labels are present even without covariates
  labels <- tbl$table_body$label[tbl$table_body$row_type == "level"]
  expect_true("Adjusted Mean" %in% labels)
  expect_true("Difference in Adjusted Means" %in% labels)

  expect_snapshot(as.data.frame(tbl))
})
