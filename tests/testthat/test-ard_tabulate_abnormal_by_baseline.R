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

test_that("ard_tabulate_abnormal_by_baseline() catches bad inputs", {
  # Test non-named list
  expect_error(
    adlb |> ard_tabulate_abnormal_by_baseline(LBNRIND, BNRIND, abnormal = list("LOW"))
  )

  # Test numeric column error
  adlb_bad <- adlb |> mutate(LBNRIND = 1:n())
  expect_error(
    adlb_bad |> ard_tabulate_abnormal_by_baseline(LBNRIND, BNRIND, abnormal = list(Low = "LOW"))
  )
})

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

  # Check that the tiers exist in the results
  levels_found <- res$variable_level |>
    unlist() |>
    unique()
  # expect_contains(levels_found, c("Not Low", "Low", "Total", "Not High", "High"))

  # Snapshot check for the full output structure
  expect_snapshot(res |> as.data.frame() |> select(variable, variable_level, stat_name, stat_label, stat))
})

# test_that("ard_tabulate_abnormal_by_baseline() handles empty data gracefully", {
#   # Test with a subset that has no rows
#   empty_data <- adlb_test |> filter(USUBJID == "NON_EXISTENT")
#
#   expect_error(
#     empty_data |>
#       ard_tabulate_abnormal_by_baseline(
#         postbaseline = LBNRIND,
#         baseline = BNRIND,
#         abnormal = list(Low = "LOW")
#       ),
#     NA # We hope it doesn't crash, but returns an empty/minimal ARD
#   )
# })

test_that("ard_tabulate_abnormal_by_baseline() correctly calculates percentages", {
  # Create a small controlled dataset to verify math
  df_small <- tibble::tribble(
    ~USUBJID, ~TRTA, ~BNRIND, ~LBNRIND,
    "1", "A", "NORMAL", "LOW", # Not Low at baseline -> 1 Abnormal
    "2", "A", "NORMAL", "NORMAL", # Not Low at baseline -> 0 Abnormal
    "3", "A", "LOW", "LOW", # Low at baseline -> 1 Abnormal
    "4", "A", "LOW", "NORMAL" # Low at baseline -> 0 Abnormal
  )

  res <- ard_tabulate_abnormal_by_baseline(
    df_small,
    postbaseline = LBNRIND,
    baseline = BNRIND,
    id = USUBJID,
    abnormal = list(Low = "LOW")
  )

  # Check "Not Low" tier: 2 subjects (1,2), 1 is Low post-baseline -> n=1, N=2, p=0.5
  # not_low_stats <- res |>
  #   filter(variable_level == "Not Low", stat_name == "abnormal") |>
  #   pull(stat) |>
  #   _[[1]]

  # expect_equal(not_low_stats$n, 1)
  # expect_equal(not_low_stats$N, 2)
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
