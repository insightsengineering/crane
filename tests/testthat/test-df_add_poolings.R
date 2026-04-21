skip_on_cran()

# --- Setup Dummy Data for Testing ---------------------------------------------
adsl_dummy <- data.frame(
  USUBJID = c("1", "2", "3", "4"),
  TRT01A = c("Drug A", "Drug A", "Drug B", "Drug C"),
  AGE = c(45, 50, 60, 65),
  stringsAsFactors = FALSE
)

adae_dummy <- data.frame(
  USUBJID = c("1", "2", "3"),
  TRT01A = c("Drug A", "Drug A", "Drug B"),
  AEDECOD = c("Headache", "Nausea", "Fatigue"),
  stringsAsFactors = FALSE
)

# A dataset without the arm variable to test selective application
adxx_dummy <- data.frame(
  USUBJID = c("1", "2"),
  PARAMCD = c("PARAM1", "PARAM2"),
  stringsAsFactors = FALSE
)

adam_db_test <- list(
  adsl = adsl_dummy,
  adae = adae_dummy,
  adxx = adxx_dummy
)

standard_pools <- list(
  "Drug A + B" = c("Drug A", "Drug B"),
  "Drug C Only" = "Drug C"
)


# --- 1. Input Validation and Errors -------------------------------------------
test_that("df_add_poolings() validates inputs and throws correct cli errors", {
  # adam_db is not a list
  expect_snapshot(
    df_add_poolings(adam_db = data.frame(), pools = standard_pools),
    error = TRUE
  )

  # adam_db contains a non-dataframe
  expect_snapshot(
    df_add_poolings(adam_db = list(adsl = adsl_dummy, bad = "string"), pools = standard_pools),
    error = TRUE
  )

  # arm_var missing in ALL datasets
  expect_snapshot(
    df_add_poolings(adam_db = list(adxx = adxx_dummy), pools = standard_pools, arm_var = "TRT01A"),
    error = TRUE
  )

  # pools is not a named list
  expect_snapshot(
    df_add_poolings(adam_db = adam_db_test, pools = c("Drug A", "Drug B")),
    error = TRUE
  )

  # pools has missing names
  expect_snapshot(
    df_add_poolings(adam_db = adam_db_test, pools = list("Drug A + B" = c("Drug A", "Drug B"), c("Drug C"))),
    error = TRUE
  )
})


# --- 2. Warnings --------------------------------------------------------------
test_that("df_add_poolings() triggers appropriate warnings", {
  # Warning 1: keep_original = TRUE creates duplicates
  expect_warning(
    df_add_poolings(adam_db_test, pools = standard_pools, keep_original = TRUE),
    regexp = "Preserving original rows while adding pools creates duplicates"
  )

  # Warning 2: 'all' keyword warning
  # We suppress the keep_original warning here to isolate the 'all' warning
  expect_warning(
    expect_warning(
      df_add_poolings(
        adam_db_test,
        pools = list("All Patients" = "all"),
        keep_original = FALSE
      ),
      regexp = "You are adding an 'all' patients pool"
    ),
    regexp = "You are adding an 'all' patients pool"
  )
})


# --- 3. Core Logic: keep_original = FALSE -------------------------------------
test_that("df_add_poolings() accurately subsets when keep_original = FALSE", {
  res <- suppressWarnings(
    df_add_poolings(adam_db_test, pools = standard_pools, keep_original = FALSE)
  )

  # adsl originally had 4 rows.
  # Drug A + B pool captures 3 rows. Drug C Only pool captures 1 row. Total = 4 rows.
  expect_equal(nrow(res$adsl), 4)

  # Check if the labels were successfully mutated
  expect_true(all(res$adsl$TRT01A %in% names(standard_pools)))

  # Ensure the values are correct
  expect_equal(
    sum(res$adsl$TRT01A == "Drug A + B"), 3
  )
})


# --- 4. Core Logic: keep_original = TRUE --------------------------------------
test_that("df_add_poolings() correctly stacks original and pooled rows", {
  res <- suppressWarnings(
    df_add_poolings(adam_db_test, pools = standard_pools, keep_original = TRUE)
  )

  # adsl originally had 4 rows.
  # Original (4) + Drug A+B (3) + Drug C Only (1) = 8 rows total.
  expect_equal(nrow(res$adsl), 8)

  # Check that original labels still exist alongside new ones
  unique_arms <- unique(res$adsl$TRT01A)
  expect_true(all(c("Drug A", "Drug B", "Drug C", "Drug A + B", "Drug C Only") %in% unique_arms))
})


# --- 5. Core Logic: The "all" Keyword -----------------------------------------
test_that("df_add_poolings() creates a total dataset when 'all' is passed", {
  res <- suppressWarnings(
    df_add_poolings(
      adam_db_test,
      pools = list("Total" = "all"),
      keep_original = FALSE
    )
  )

  # With keep_original = FALSE, the "all" pool should perfectly mirror the row count of the original datasets
  expect_equal(nrow(res$adsl), nrow(adsl_dummy))
  expect_equal(nrow(res$adae), nrow(adae_dummy))

  # Every row should now have the "Total" label
  expect_true(all(res$adsl$TRT01A == "Total"))
  expect_true(all(res$adae$TRT01A == "Total"))
})


# --- 6. Dataset Selective Application -----------------------------------------
test_that("df_add_poolings() only modifies datasets containing the arm variable", {
  res <- suppressWarnings(
    df_add_poolings(adam_db_test, pools = standard_pools, keep_original = TRUE)
  )

  # adsl and adae should have their rows expanded
  expect_true(nrow(res$adsl) > nrow(adsl_dummy))
  expect_true(nrow(res$adae) > nrow(adae_dummy))

  # adxx does NOT contain TRT01A, so it should be completely untouched
  expect_equal(res$adxx, adxx_dummy)
  expect_equal(nrow(res$adxx), nrow(adxx_dummy))
})
