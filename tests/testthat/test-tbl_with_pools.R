skip_on_cran()

# Setup small, reproducible datasets using {cards}
ADSL_subset <- cards::ADSL |>
  dplyr::filter(TRTA %in% c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")) |>
  dplyr::slice(1:30)

ADAE_subset <- cards::ADAE |>
  dplyr::filter(USUBJID %in% ADSL_subset$USUBJID) |>
  dplyr::slice(1:30)

# Define standard pools for testing
standard_pools <- list(
  "Any Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose"),
  "All Patients" = "all"
)


# --- 1. Input Validation and Error Handling -------------------------------------
test_that("tbl_with_pools() validates inputs correctly", {
  # data is not a data frame
  expect_snapshot(
    tbl_with_pools(data = list(A = 1), pools = standard_pools, by = "TRTA", .tbl_fun = tbl_summary),
    error = TRUE
  )

  # by variable does not exist in data
  expect_snapshot(
    tbl_with_pools(data = ADSL_subset, pools = standard_pools, by = "MISSING_VAR", .tbl_fun = tbl_summary),
    error = TRUE
  )

  # by variable does not exist in denominator
  expect_snapshot(
    tbl_with_pools(
      data = ADAE_subset,
      pools = standard_pools,
      by = "TRTA",
      denominator = data.frame(USUBJID = 1),
      .tbl_fun = tbl_summary
    ),
    error = TRUE
  )

  # pools is malformed
  expect_snapshot(
    tbl_with_pools(data = ADSL_subset, pools = c("Just a vector"), by = "TRTA", .tbl_fun = tbl_summary),
    error = TRUE
  )

  # .tbl_fun is missing/invalid
  expect_snapshot(
    tbl_with_pools(data = ADSL_subset, pools = standard_pools, by = "TRTA", .tbl_fun = "not_a_function"),
    error = TRUE
  )

  # Pass a character vector to denominator instead of a data.frame or NULL
  expect_snapshot(
    tbl_with_pools(
      data = ADSL_subset,
      pools = standard_pools,
      by = "TRTA",
      denominator = c("I am", "not a data frame"),
      keep_original = FALSE,
      .tbl_fun = tbl_summary
    ),
    error = TRUE
  )
})


# --- 2. Core Functionality: Without Denominator (tbl_summary) -----------------
test_that("tbl_with_pools() works with standard functions like tbl_summary", {
  expect_silent(
    tbl <- tbl_with_pools(
      data = ADSL_subset,
      pools = standard_pools,
      by = "TRTA",
      denominator = NULL,
      keep_original = TRUE,
      .tbl_fun = tbl_summary,
      include = c(AGE, SEX)
    )
  )

  # Check if the object is successfully merged
  expect_s3_class(tbl, "tbl_merge")
  expect_s3_class(tbl, "tbl_with_pools")

  # Check if the header labels contain our custom pool names
  header_labels <- tbl$table_styling$header |>
    dplyr::filter(str_detect(column, "^stat_")) |>
    dplyr::pull(label)

  expect_true(any(str_detect(header_labels, "Any Xanomeline")))
  expect_true(any(str_detect(header_labels, "All Patients")))

  # Snapshot the table output
  withr::local_options(list(width = 220))
  expect_snapshot(as.data.frame(tbl))
})


# --- 3. Core Functionality: With Denominator (tbl_hierarchical_rate_and_count) -
test_that("tbl_with_pools() passes the denominator correctly for custom functions", {
  expect_silent(
    tbl <- tbl_with_pools(
      data = ADAE_subset,
      pools = standard_pools,
      by = "TRTA",
      denominator = ADSL_subset,
      keep_original = TRUE,
      .tbl_fun = tbl_hierarchical_rate_and_count,
      variables = c(AEBODSYS, AEDECOD)
    )
  )

  expect_s3_class(tbl, "tbl_merge")
  expect_s3_class(tbl, "tbl_with_pools")

  # Snapshot the table output
  withr::local_options(list(width = 220))
  expect_snapshot(as.data.frame(tbl))
})


# --- 4. Edge Cases: Warnings and Empty Pools ----------------------------------
test_that("tbl_with_pools() warns and skips empty pools properly", {
  empty_pools <- list(
    "Ghost Arm" = "Fake Drug",
    "All Patients" = "all"
  )

  # 1. Denominator is provided, but subset is empty
  expect_warning(
    tbl_with_pools(
      data = ADAE_subset,
      pools = empty_pools,
      by = "TRTA",
      denominator = ADSL_subset,
      .tbl_fun = tbl_hierarchical_rate_and_count,
      variables = c(AEBODSYS, AEDECOD)
    ),
    regexp = "has 0 rows in the data. Skipping."
  )

  # 2. Denominator is NULL, and data subset is empty
  expect_warning(
    tbl_with_pools(
      data = ADSL_subset,
      pools = empty_pools,
      by = "TRTA",
      denominator = NULL,
      .tbl_fun = tbl_summary,
      include = AGE
    ),
    regexp = "has 0 rows in the data. Skipping."
  )

  # 3. Completely empty configuration (all pools skipped, no original kept)
  expect_snapshot(
    tbl_with_pools(
      data = ADSL_subset,
      pools = list("Ghost Arm" = "Fake Drug"),
      by = "TRTA",
      denominator = NULL,
      keep_original = FALSE,
      .tbl_fun = tbl_summary
    ),
    error = TRUE
  )
})


# --- 5. Output Integrity: keep_original flag ----------------------------------
test_that("tbl_with_pools() keep_original = FALSE returns only the pools", {
  expect_silent(
    tbl <- tbl_with_pools(
      data = ADSL_subset,
      pools = standard_pools,
      by = "TRTA",
      denominator = NULL,
      keep_original = FALSE, # ONLY generate the pools
      .tbl_fun = tbl_summary,
      include = AGE
    )
  )

  # Because keep_original = FALSE and we have 2 pools, we should only see 2 main columns
  header_cols <- tbl$table_styling$header |>
    dplyr::filter(str_detect(column, "^stat_"))

  expect_equal(nrow(header_cols), 2)
  expect_true(any(str_detect(header_cols$label, "Any Xanomeline")))
  expect_true(any(str_detect(header_cols$label, "All Patients")))

  # It should still return a tbl_merge object
  expect_s3_class(tbl, "tbl_merge")
  expect_s3_class(tbl, "tbl_with_pools")
})

# --- Setup specific data for edge case testing ---
# We make TRTA explicitly a factor to test the factor regression fix
ADSL_edge <- data.frame(
  USUBJID = c("1", "2", "3", "4"),
  TRTA = factor(c("Drug A", "Drug B", "Drug C", "Drug C")),
  AGE = c(50, 60, 70, 75),
  stringsAsFactors = FALSE
)

# Notice Subject 3 and 4 (Drug C) have NO adverse events in this dataset
ADAE_edge <- data.frame(
  USUBJID = c("1", "2"),
  TRTA = factor(c("Drug A", "Drug B")),
  AEBODSYS = c("SOC1", "SOC2"),
  AEDECOD = c("PT1", "PT2"),
  stringsAsFactors = FALSE
)

# --- 6. Test Factor Coercion Fix ----------------------------------------------
test_that("tbl_with_pools() safely handles factor columns without generating NAs", {
  # If the factor bug existed, creating "Drugs A & B" would throw an NA warning
  expect_silent(
    tbl <- tbl_with_pools(
      data = ADSL_edge,
      pools = list("Drugs A & B" = c("Drug A", "Drug B")),
      by = "TRTA",
      denominator = NULL,
      keep_original = FALSE,
      .tbl_fun = tbl_summary,
      include = AGE
    )
  )

  # Ensure the column was successfully created and not dropped due to NAs
  header_labels <- tbl$table_styling$header$label
  expect_true(any(str_detect(header_labels, "Drugs A & B")))
})


# --- 7. Test Independent Empty Checks: Zero Events but Non-Zero Denom ---------
test_that("tbl_with_pools() skips pools with 0 events to prevent cards engine crash", {
  # Drug C exists in ADSL (n=2) but has 0 records in ADAE.
  # It should trigger the '0 rows in the data' warning to prevent the gtsummary crash.
  # Because keep_original = FALSE and it's the only pool, it will also abort with "No tables were generated".

  expect_snapshot(
    tbl_with_pools(
      data = ADAE_edge,
      pools = list("Drug C Only" = "Drug C"),
      by = "TRTA",
      denominator = ADSL_edge,
      keep_original = FALSE,
      .tbl_fun = tbl_hierarchical_rate_and_count,
      variables = c(AEBODSYS, AEDECOD)
    ),
    error = TRUE
  )
})

# --- 8. Test Independent Empty Checks: Zero Events and NULL Denom -------------
test_that("tbl_with_pools() skips empty data pools when denominator is NULL", {
  # Drug C has 0 records in ADAE_edge. With no denominator, tbl_summary would crash.
  # We expect it to trigger the new '0 rows in the data' warning and skip it.
  expect_warning(
    tbl_with_pools(
      data = ADAE_edge,
      pools = list("Drug C Only" = "Drug C"),
      by = "TRTA",
      denominator = NULL,
      keep_original = TRUE,
      .tbl_fun = tbl_summary,
      include = AEBODSYS
    ),
    regexp = "has 0 rows in the data. Skipping."
  )
})

test_that("tbl_with_pools() skips when denominator has 0 patients but data has >0", {
  # Setup an anomalous scenario where ADAE has an arm completely missing from ADSL
  ADSL_missing_arm <- data.frame(
    USUBJID = c("1", "2"),
    TRTA = factor(c("Drug A", "Drug A")),
    stringsAsFactors = FALSE
  )

  ADAE_extra_arm <- data.frame(
    USUBJID = c("3"),
    TRTA = factor(c("Drug Z")),
    AEBODSYS = c("SOC1"),
    AEDECOD = c("PT1"),
    stringsAsFactors = FALSE
  )

  # Because ADAE has 1 row for Drug Z, it passes the '0 rows in data' check.
  # However, ADSL has 0 rows for Drug Z, triggering the denominator warning.
  # Since keep_original = FALSE and the only pool is skipped, it aborts safely.
  expect_snapshot(
    tbl_with_pools(
      data = ADAE_extra_arm,
      pools = list("Drug Z Pool" = "Drug Z"),
      by = "TRTA",
      denominator = ADSL_missing_arm,
      keep_original = FALSE,
      .tbl_fun = tbl_hierarchical_rate_and_count,
      variables = c(AEBODSYS, AEDECOD)
    ),
    error = TRUE
  )
})

# --- 9. Test rlang::inject Fix (Tidyselect compatibility) ---------------------
test_that("tbl_with_pools() suppresses tidyselect warnings via rlang::inject", {
  # If `by = dplyr::all_of(by)` was still used, this would trigger a deprecation warning
  # or error depending on the gtsummary version. expect_silent ensures complete quiet.
  expect_silent(
    tbl <- tbl_with_pools(
      data = ADSL_edge,
      pools = list("All" = "all"),
      by = "TRTA",
      denominator = NULL,
      keep_original = TRUE,
      .tbl_fun = tbl_summary,
      include = AGE
    )
  )
})
