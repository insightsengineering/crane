library(testthat)
library(dplyr)

# Setup dummy data similar to ADLB
set.seed(1)
adlb_test <- cards::ADLB
adlb_test$BNRIND <- ifelse(
  adlb_test$BNRIND != "N",
  sample(c("LOW", "HIGH"), nrow(adlb_test), replace = TRUE),
  "NORMAL"
)

test_that("ard_tabulate_abnormal_by_baseline() works with standard inputs", {
  # We expect it to run and produce an ARD for each abnormality (Low, High)
  # with three rows per abnormality: Not [Abn], [Abn], Total
  res <- adlb_test |>
    ard_tabulate_abnormal_by_baseline(
      postbaseline = LBNRIND,
      baseline = BNRIND,
      id = USUBJID,
      by = TRTA,
      abnormal = list(Low = "LOW", High = "HIGH")
    )

  # Check structure
  expect_s3_class(res, "card")
  expect_true("variable_level" %in% names(res))

  # NEW: Verify our "Magic Trick" worked (variable column should hold the abnormality name)
  expect_true(all(unique(res$variable) %in% c("Low", "High")))

  # Check that the tiers exist in the results
  levels_found <- res$variable_level |>
    unlist() |>
    unique()
  expect_contains(levels_found, c("Not Low", "Low", "Total", "Not High", "High"))

  # Snapshot check for the full output structure
  expect_snapshot(res |> as.data.frame() |> select(variable, variable_level, stat_name, stat_label, stat))
})

test_that("ard_tabulate_abnormal_by_baseline() correctly calculates percentages AND handles missing baselines", {
  # Create a small controlled dataset to verify math and NA handling specs
  # Patient 5 has a missing baseline.
  df_small <- data.frame(
    USUBJID = c("1", "2", "3", "4", "5"),
    TRTA = c("A", "A", "A", "A", "A"),
    BNRIND = c("NORMAL", "NORMAL", "LOW", "LOW", NA),
    LBNRIND = c("LOW", "NORMAL", "LOW", "NORMAL", "LOW"),
    stringsAsFactors = FALSE
  )

  res <- ard_tabulate_abnormal_by_baseline(
    df_small,
    postbaseline = LBNRIND,
    baseline = BNRIND,
    id = USUBJID,
    abnormal = list(Low = "LOW")
  )

  # 1. "Not Low" tier: Should EXCLUDE patient 5 (NA baseline).
  # Patients 1 & 2 only. 1 is Low post-baseline -> n=1, N=2
  not_low_stats <- res |> filter(variable_level == "Not Low")
  expect_equal(not_low_stats$stat[[which(not_low_stats$stat_name == "n")]], 1)
  expect_equal(not_low_stats$stat[[which(not_low_stats$stat_name == "N")]], 2)

  # 2. "Low" tier: Patients 3 & 4. 3 is Low post-baseline -> n=1, N=2
  low_stats <- res |> filter(variable_level == "Low")
  expect_equal(low_stats$stat[[which(low_stats$stat_name == "n")]], 1)
  expect_equal(low_stats$stat[[which(low_stats$stat_name == "N")]], 2)

  # 3. "Total" tier: Should INCLUDE patient 5.
  # Patients 1, 2, 3, 4, 5. (1, 3, 5 are Low) -> n=3, N=5
  total_stats <- res |> filter(variable_level == "Total")
  expect_equal(total_stats$stat[[which(total_stats$stat_name == "n")]], 3)
  expect_equal(total_stats$stat[[which(total_stats$stat_name == "N")]], 5)
})

test_that("ard_tabulate_abnormal_by_baseline() safely drops unobserved abnormalities", {
  # Create data that only contains "LOW" post-baseline, no "SEVERE"
  df_small <- data.frame(
    USUBJID = c("1", "2"),
    BNRIND = c("NORMAL", "NORMAL"),
    LBNRIND = c("LOW", "NORMAL"),
    stringsAsFactors = FALSE
  )

  res <- ard_tabulate_abnormal_by_baseline(
    df_small,
    postbaseline = LBNRIND,
    baseline = BNRIND,
    id = USUBJID,
    # Pass a list where one abnormality doesn't exist in the data
    abnormal = list(Low = "LOW", Severe = "SEVERE")
  )

  # Verify the loop bug is fixed and it only returns "Low"
  expect_equal(unique(res$variable), "Low")
  expect_false("Severe" %in% unique(res$variable))
})

test_that("ard_tabulate_abnormal_by_baseline() handles empty data gracefully", {
  # Test with a subset that has no rows
  empty_data <- adlb_test |> filter(USUBJID == "NON_EXISTENT")

  # It should return NULL based on the internal `if (nrow(data) == 0)` check
  expect_null(
    empty_data |>
      ard_tabulate_abnormal_by_baseline(
        postbaseline = LBNRIND,
        baseline = BNRIND,
        abnormal = list(Low = "LOW")
      )
  )
})

test_that("ard_tabulate_abnormal_by_baseline() handles grouping variables (by)", {
  res <- adlb_test |>
    ard_tabulate_abnormal_by_baseline(
      postbaseline = LBNRIND,
      baseline = BNRIND,
      by = TRTA,
      abnormal = list(High = "HIGH")
    )

  # Ensure 'group1' or the 'by' column exists in the ARD
  expect_true("group1" %in% names(res))
  expect_contains(res$group1_level |> unlist(), unique(adlb_test$TRTA))
})
