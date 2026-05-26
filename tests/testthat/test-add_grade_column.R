skip_on_cran()

ADSL <- cards::ADSL
ADAE_subset <- cards::ADAE |>
  dplyr::filter(
    AESOC %in% unique(cards::ADAE$AESOC)[1:5],
    AETERM %in% unique(cards::ADAE$AETERM)[1:5]
  )

label <- list(
  AEBODSYS = "MedDRA System Organ Class",
  AEDECOD = "MedDRA Preferred Term",
  AETOXGR = "Grade"
)

grade_groups <- list(
  "Grade 1-2" = c("1", "2"),
  "Grade 3-4" = c("3", "4"),
  "Grade 5" = "5"
)

# --- 1. Standalone table formatting ------------------------------------------
test_that("add_grade_column() works on a standalone tbl_hierarchical_rate_by_grade", {
  tbl <- tbl_hierarchical_rate_by_grade(
    ADAE_subset,
    variables = c(AEBODSYS, AEDECOD, AETOXGR),
    denominator = ADSL,
    by = TRTA,
    label = label,
    grade_groups = grade_groups
  )

  # before add_grade_column: no label_grade column, label not blanked
  expect_false("label_grade" %in% names(tbl$table_body))
  expect_true(any(tbl$table_body$label %in% as.character(1:5)))

  result <- tbl |> add_grade_column()

  # after add_grade_column: label_grade exists, grade labels blanked from label
  expect_true("label_grade" %in% names(result$table_body))

  # grade rows have blank label
  grade_rows <- result$table_body$variable == "AETOXGR"
  expect_true(all(result$table_body$label[grade_rows] == ""))

  # label_grade has grade text for grade rows
  expect_true(all(result$table_body$label_grade[grade_rows] != ""))

  # AE rows get "- Any Grade -"
  ae_rows <- result$table_body$variable == "AEDECOD"
  expect_true(all(result$table_body$label_grade[ae_rows] == "- Any Grade -"))

  # SOC label rows get empty label_grade (not "- Any Grade -")
  soc_rows <- result$table_body$variable == "AEBODSYS" &
    result$table_body$label != "- Any adverse events -"
  expect_true(all(result$table_body$label_grade[soc_rows] == ""))
})

# --- 2. Standalone without grade groups --------------------------------------
test_that("add_grade_column() works without grade groups", {
  tbl <- tbl_hierarchical_rate_by_grade(
    ADAE_subset,
    variables = c(AEBODSYS, AEDECOD, AETOXGR),
    denominator = ADSL,
    by = TRTA,
    label = label
  )

  result <- tbl |> add_grade_column()

  expect_true("label_grade" %in% names(result$table_body))
  # no grade-group-specific indentation when none defined
  indent_rows <- result$table_styling$indent |>
    dplyr::filter(column == "label_grade")
  expect_equal(nrow(indent_rows), 0)
})

# --- 3. Metadata extraction from merged tables --------------------------------
test_that("add_grade_column() extracts custom_info from merged tables", {
  tbl <- tbl_hierarchical_rate_by_grade(
    ADAE_subset,
    variables = c(AEBODSYS, AEDECOD, AETOXGR),
    denominator = ADSL,
    by = TRTA,
    label = label,
    grade_groups = grade_groups
  )

  # simulate a merged table structure
  merged <- gtsummary::tbl_merge(
    tbls = list(tbl, tbl),
    tab_spanner = FALSE,
    quiet = TRUE
  )

  result <- merged |> add_grade_column()
  expect_true("label_grade" %in% names(result$table_body))
})

# --- 4. Error when no custom_info metadata ------------------------------------
test_that("add_grade_column() errors when custom_info is missing", {
  # use a plain gtsummary table with no custom_info
  tbl <- gtsummary::tbl_summary(cards::ADSL, include = AGE)

  expect_snapshot(
    add_grade_column(tbl),
    error = TRUE
  )
})

# --- 5. Error when input is not gtsummary ------------------------------------
test_that("add_grade_column() errors on non-gtsummary input", {
  expect_snapshot(
    add_grade_column(data.frame(x = 1)),
    error = TRUE
  )
})

# --- 6. Stats blanking for non-summary rows -----------------------------------
test_that("add_grade_column() blanks stats for SOC label rows", {
  tbl <- tbl_hierarchical_rate_by_grade(
    ADAE_subset,
    variables = c(AEBODSYS, AEDECOD, AETOXGR),
    denominator = ADSL,
    by = TRTA,
    label = label,
    grade_groups = grade_groups
  )

  result <- tbl |> add_grade_column()

  # SOC label rows (not "- Any adverse events -") should have NA stats
  soc_label_rows <- result$table_body |>
    dplyr::filter(
      variable == "AEBODSYS",
      label != "- Any adverse events -"
    )

  stat_cols <- names(soc_label_rows)[grepl("^stat_", names(soc_label_rows))]
  for (col in stat_cols) {
    expect_true(all(is.na(soc_label_rows[[col]])))
  }
})

# --- 7. Zero formatting ("0 (0.0%)" -> "0") ----------------------------------
test_that("add_grade_column() recodes zero statistics", {
  tbl <- tbl_hierarchical_rate_by_grade(
    ADAE_subset,
    variables = c(AEBODSYS, AEDECOD, AETOXGR),
    denominator = ADSL,
    by = TRTA,
    label = label,
    grade_groups = grade_groups
  )

  result <- tbl |> add_grade_column()

  # recoding "0 (0.0%)" -> "0" happens at render time via post_fmt_fun;
  # we verify the formatting function is registered, not the rendered output
  expect_true(nrow(result$table_styling$post_fmt_fun) > 0)
})

# --- 8. Idempotency: calling add_grade_column() twice -------------------------
test_that("add_grade_column() is idempotent when called twice", {
  tbl <- tbl_hierarchical_rate_by_grade(
    ADAE_subset,
    variables = c(AEBODSYS, AEDECOD, AETOXGR),
    denominator = ADSL,
    by = TRTA,
    label = label,
    grade_groups = grade_groups
  )

  result_once <- tbl |> add_grade_column()
  result_twice <- result_once |> add_grade_column()

  # second call should return the same table unchanged
  expect_identical(result_once$table_body, result_twice$table_body)

  # grade labels should not be corrupted
  grade_labels <- result_twice$table_body |>
    dplyr::filter(variable == "AETOXGR") |>
    dplyr::pull(label_grade)
  expect_true(all(grade_labels != ""))
})

# --- 9. Header labels are set correctly --------------------------------------
test_that("add_grade_column() sets correct header labels", {
  tbl <- tbl_hierarchical_rate_by_grade(
    ADAE_subset,
    variables = c(AEBODSYS, AEDECOD, AETOXGR),
    denominator = ADSL,
    by = TRTA,
    label = label,
    grade_groups = grade_groups
  )

  result <- tbl |> add_grade_column()

  # label_grade header should be the grade label
  lg_header <- result$table_styling$header |>
    dplyr::filter(column == "label_grade") |>
    dplyr::pull(label)
  expect_equal(lg_header, "Grade")

  # label header should combine SOC and AE labels
  lbl_header <- result$table_styling$header |>
    dplyr::filter(column == "label") |>
    dplyr::pull(label)
  expect_true(grepl("MedDRA System Organ Class", lbl_header))
  expect_true(grepl("MedDRA Preferred Term", lbl_header))
})
