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

  # Check if the header labels contain our custom pool names
  header_labels <- tbl$table_styling$header |>
    dplyr::filter(stringr::str_detect(column, "^stat_")) |>
    dplyr::pull(label)

  expect_true(any(stringr::str_detect(header_labels, "Any Xanomeline")))
  expect_true(any(stringr::str_detect(header_labels, "All Patients")))

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
    regexp = "has 0 patients in the denominator"
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
    regexp = "has 0 patients in the data"
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
    dplyr::filter(stringr::str_detect(column, "^stat_"))

  expect_equal(nrow(header_cols), 2)
  expect_true(any(stringr::str_detect(header_cols$label, "Any Xanomeline")))
  expect_true(any(stringr::str_detect(header_cols$label, "All Patients")))

  # It should still return a tbl_merge object
  expect_s3_class(tbl, "tbl_merge")
})
