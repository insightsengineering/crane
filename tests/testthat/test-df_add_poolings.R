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

# --- Setup specific data for expression testing -------------------------------
adsl_expr <- data.frame(
  USUBJID = c("1", "2", "3", "4", "5"),
  TRT01A = c("Drug A", "Drug A", "Drug B", "Drug B", "Drug C"),
  AGE = c(40, 50, 60, 70, 80),
  BMIFL = c("Y", "N", "Y", "N", "Y"),
  stringsAsFactors = FALSE
)

adae_expr <- data.frame(
  USUBJID = c("1", "1", "3", "5"),
  TRT01A = c("Drug A", "Drug A", "Drug B", "Drug C"),
  BMIFL = c("Y", "Y", "Y", "Y"),
  AEBODSYS = c("SOC1", "SOC2", "SOC1", "SOC2"),
  stringsAsFactors = FALSE
)

adam_db_expr <- list(adsl = adsl_expr, adae = adae_expr)


# --- 7. Test Complex Logical Expressions --------------------------------------
test_that("df_add_poolings() processes rlang::expr() correctly across datasets", {
  complex_pools <- list(
    "High BMI Patients" = rlang::expr(BMIFL == "Y"),
    "Drug A High BMI"   = rlang::expr(TRT01A == "Drug A" & BMIFL == "Y")
  )

  res <- suppressWarnings(
    df_add_poolings(
      adam_db = adam_db_expr,
      pools = complex_pools,
      keep_original = FALSE
    )
  )

  # Check ADSL counts:
  # "High BMI Patients" should have 3 patients (ID 1, 3, 5)
  # "Drug A High BMI" should have 1 patient (ID 1)
  expect_equal(sum(res$adsl$TRT01A == "High BMI Patients"), 3)
  expect_equal(sum(res$adsl$TRT01A == "Drug A High BMI"), 1)

  # Check ADAE counts:
  # "High BMI Patients" should have 4 events (all rows in ADAE have BMIFL == "Y")
  # "Drug A High BMI" should have 2 events (ID 1 has 2 rows)
  expect_equal(sum(res$adae$TRT01A == "High BMI Patients"), 4)
  expect_equal(sum(res$adae$TRT01A == "Drug A High BMI"), 2)
})


# --- 8. Test Expression Evaluation Edge Cases (0-row Subsets) -----------------
test_that("df_add_poolings() safely ignores rlang::expr() evaluating to 0 rows", {
  impossible_pool <- list(
    "Impossible Pool" = rlang::expr(BMIFL == "Z")
  )

  # If a pool results in 0 rows, the function should skip appending it.
  # With keep_original = FALSE and no matching rows, it should return an empty dataframe.
  res <- suppressWarnings(
    df_add_poolings(
      adam_db = adam_db_expr,
      pools = impossible_pool,
      keep_original = FALSE
    )
  )

  expect_equal(nrow(res$adsl), 0)
  expect_equal(nrow(res$adae), 0)

  # With keep_original = TRUE, it should just return the original dataframe untouched
  res_keep <- suppressWarnings(
    df_add_poolings(
      adam_db = adam_db_expr,
      pools = impossible_pool,
      keep_original = TRUE
    )
  )

  expect_equal(nrow(res_keep$adsl), nrow(adsl_expr))
  expect_equal(nrow(res_keep$adae), nrow(adae_expr))
  expect_false("Impossible Pool" %in% res_keep$adsl$TRT01A)
})
