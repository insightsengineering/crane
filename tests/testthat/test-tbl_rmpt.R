# Prepare test data matching the Roxygen2 examples
setup_test_data <- function() {
  skip_if_not_installed("pharmaverseadam")
  
  df_adsl <- pharmaverseadam::adsl |> dplyr::filter(SAFFL == "Y")
  df_adex <- pharmaverseadam::adex |>
    dplyr::filter(PARAMCD == 'TDURD', PARCAT1 == 'OVERALL', SAFFL == "Y") |>
    dplyr::mutate(
      AVAL_MONTH = AVAL / 30.4375,
      AVAL_CAT = factor(
        dplyr::case_when(
          AVAL_MONTH < 1 ~ "< 1 month",
          AVAL_MONTH >= 1 & AVAL_MONTH < 3 ~ "1 to <3 months",
          AVAL_MONTH >= 3 & AVAL_MONTH < 6 ~ "3 to <6 months",
          TRUE ~ ">=6 months"
        ),
        levels = c("< 1 month", "1 to <3 months", "3 to <6 months", ">=6 months")
      )
    ) |>
    dplyr::select(
      USUBJID,
      SEX,
      ETHNIC,
      RACE,
      AGEGR1,
      AVAL,
      AVAL_MONTH,
      AVAL_CAT,
      TRT01A
    )
  
  list(adsl = df_adsl, adex = df_adex)
}

test_that("tbl_rmpt() works with default parameters (Example 1)", {
  skip_if_not_installed("pharmaverseadam")
  withr::local_options(list(width = 190))
  
  data <- setup_test_data()
  df_adex <- data$adex
  df_adsl <- data$adsl

  expect_silent(
    tbl <- tbl_rmpt(
      data = df_adex,
      variable = AVAL_CAT,
      aval = AVAL,
      by = TRT01A,
      denominator = df_adsl
    )
  )

  expect_s3_class(tbl, "tbl_rmpt")
  expect_s3_class(tbl, "gtsummary")
  expect_true("call_list" %in% names(tbl))
  expect_true("inputs" %in% names(tbl))
  expect_true("cards" %in% names(tbl))
})

test_that("add_overall.tbl_rmpt() works (Example 2)", {
  skip_if_not_installed("pharmaverseadam")
  withr::local_options(list(width = 190))
  
  data <- setup_test_data()
  df_adex <- data$adex
  df_adsl <- data$adsl

  expect_silent(
    tbl <- tbl_rmpt(
      data = df_adex,
      variable = AVAL_CAT,
      aval = AVAL,
      by = TRT01A,
      denominator = df_adsl
    ) |>
      add_overall(last = TRUE)
  )

  expect_s3_class(tbl, "tbl_rmpt")

  # Check that overall column was added
  stat_cols <- dplyr::select(tbl$table_body, gtsummary::all_stat_cols()) |> names()
  expect_true(length(stat_cols) > 6) # Should have more columns with overall
})

test_that("tbl_rmpt() works with custom labels (Example 3)", {
  skip_if_not_installed("pharmaverseadam")
  withr::local_options(list(width = 190))
  
  data <- setup_test_data()
  df_adex <- data$adex
  df_adsl <- data$adsl

  expect_silent(
    tbl <- tbl_rmpt(
      data = df_adex,
      variable = AVAL_CAT,
      aval = AVAL,
      by = TRT01A,
      denominator = df_adsl,
      label = "Treatment Exposure Duration",
      participant_footnote = "Number of patients in each category",
      person_time_footnote = "Total person-time in days"
    )
  )

  expect_s3_class(tbl, "tbl_rmpt")
})

test_that("tbl_rmpt() stores inputs correctly", {
  skip_if_not_installed("pharmaverseadam")
  withr::local_options(list(width = 190))
  
  data <- setup_test_data()
  df_adex <- data$adex
  df_adsl <- data$adsl

  tbl <- tbl_rmpt(
    data = df_adex,
    variable = AVAL_CAT,
    aval = AVAL,
    by = TRT01A,
    denominator = df_adsl
  )

  expect_equal(tbl$inputs$variable, "AVAL_CAT")
  expect_equal(tbl$inputs$aval, "AVAL")
  expect_equal(tbl$inputs$by, "TRT01A")
  expect_equal(tbl$inputs$id, "USUBJID")
})

test_that("add_overall.tbl_rmpt() handles unstratified table", {
  skip_if_not_installed("pharmaverseadam")
  withr::local_options(list(width = 190))
  
  data <- setup_test_data()
  df_adex <- data$adex
  df_adsl <- data$adsl

  tbl_unstratified <- tbl_rmpt(
    data = df_adex,
    variable = AVAL_CAT,
    aval = AVAL,
    by = NULL,
    denominator = df_adsl
  )

  expect_message(
    result <- add_overall(tbl_unstratified),
    "Original table was not stratified"
  )

  expect_identical(result, tbl_unstratified)
})

test_that("tbl_rmpt() validates inputs properly", {
  # Create simple mock data for validation tests (no pharmaverseadam needed)
  mock_data <- data.frame(
    USUBJID = paste0("S", 1:10),
    TRT = rep(c("A", "B"), each = 5),
    AVAL = c(10, 20, 30, 40, 50, 15, 25, 35, 45, 55),
    AVAL_CAT = factor(rep(c("Low", "High"), each = 5))
  )
  
  mock_denominator <- data.frame(
    USUBJID = paste0("S", 1:10),
    TRT = rep(c("A", "B"), each = 5)
  )

  # Test invalid data argument
  expect_error(
    tbl_rmpt(
      data = "not a data frame",
      variable = AVAL_CAT,
      aval = AVAL,
      by = TRT,
      denominator = mock_denominator
    ),
    class = "check_data_frame"
  )

  # Test invalid denominator argument
  expect_error(
    tbl_rmpt(
      data = mock_data,
      variable = AVAL_CAT,
      aval = AVAL,
      by = TRT,
      denominator = "not a data frame"
    ),
    class = "check_data_frame"
  )
  
  # Test missing required arguments
  expect_error(
    tbl_rmpt(
      data = mock_data,
      variable = AVAL_CAT,
      aval = AVAL,
      by = TRT
      # missing denominator
    )
  )
})

test_that("tbl_rmpt() works with different variable names", {
  skip_if_not_installed("pharmaverseadam")
  withr::local_options(list(width = 190))
  
  data <- setup_test_data()
  df_adex <- data$adex
  df_adsl <- data$adsl

  # Test with different stratification variables
  expect_silent(
    tbl <- tbl_rmpt(
      data = df_adex,
      variable = AVAL_CAT,
      aval = AVAL,
      by = SEX,
      denominator = df_adsl
    )
  )

  expect_s3_class(tbl, "tbl_rmpt")
  expect_equal(tbl$inputs$by, "SEX")
})
