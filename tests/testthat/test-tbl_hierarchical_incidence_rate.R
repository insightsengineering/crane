# 1. Create robust dummy data designed to trigger every edge case
adsl_test <- data.frame(
  USUBJID = c("P1", "P2", "P3", "P4", "P5"),
  ARM = c("Trt", "Trt", "Pbo", "Pbo", "Pbo"),
  TRTSDT = as.Date(rep("2023-01-01", 5)),
  TRTEDT = as.Date(c(
    "2023-12-31", "2023-06-30", "2023-12-31", "2023-12-31", "2023-12-31"
  ))
)

adae_test <- data.frame(
  USUBJID = c(
    "P1", # Normal AE
    "P2", # AE with timestamp (triggers substr stripping)
    "P3", # AE prior to TRTSDT (triggers the end_dt fallback in time_var)
    "P4", # AE with missing date string (triggers safe_min_date NA logic)
    "P4" # AE with literal "NA" string (triggers safe_min_date NA logic)
  ),
  ARM = c("Trt", "Trt", "Pbo", "Pbo", "Pbo"),
  AESOC = c("SOC1", "SOC1", "SOC2", "SOC2", "SOC2"),
  AEDECOD = c("PT1", "PT2", "PT3", "PT4", "PT5"),
  AESTDTC = c(
    "2023-06-01", # Standard clean date
    "2023-03-15T12:30", # ISO 8601 with time
    "2022-12-01", # Prior to start date (calculates healthy time)
    "", # Blank string
    "NA" # Literal NA string
  )
)

test_that("tbl_hierarchical_incidence_rate runs with minimum arguments", {
  # Relies entirely on the default values for id, dates, and settings
  tbl_min <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD)
  )

  expect_s3_class(tbl_min, "tbl_hierarchical_incidence_rate")
  expect_s3_class(tbl_min, "gtsummary")
  expect_true("table_body" %in% names(tbl_min))
  expect_true(any(tbl_min$table_body$label == "All Adverse Events"))
})

test_that("tbl_hierarchical_incidence_rate executes standard usage with `by`", {
  tbl <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = ARM,
    start_date = TRTSDT,
    end_date = TRTEDT,
    event_date = AESTDTC,
    n_person_time = 100,
    unit_label = "years"
  )

  expect_s3_class(tbl, "tbl_hierarchical_incidence_rate")
  expect_true("table_body" %in% names(tbl))
})

test_that("tbl_hierarchical_incidence_rate hits missing `by` branch", {
  tbl_no_by <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = NULL,
    n_person_time = 1000,
    unit_label = "months",
    label = list("..ard_hierarchical_overall.." = "Custom Overall AEs")
  )

  expect_true(any(tbl_no_by$table_body$label == "Custom Overall AEs"))
  header_df <- tbl_no_by$table_styling$header
  expect_true(any(grepl("PM", header_df$label)))
})

test_that("tbl_hierarchical_incidence_rate unit_label switch defaults", {
  tbl_custom_unit <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    unit_label = "decades"
  )
  # The functions format the custom unit label by contatenating "Person-"
  # and unit_label in title case
  header_df <- tbl_custom_unit$table_styling$header
  expect_true(any(grepl("Person-Decades", header_df$label)))
})

test_that("tbl_hierarchical_incidence_rate correctly switches event_type math", {
  # 1. Run with first_event (stops clock at first AE)
  tbl_first <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    event_type = "first_event"
  )

  # 2. Run with all occurrences (clock runs to end_date regardless of AE)
  tbl_all <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    event_type = "all"
  )

  # Helper to extract and sum person-time statistics from the underlying ARD
  get_pt <- function(tbl) {
    gtsummary::gather_ard(tbl) |>
      dplyr::bind_rows() |>
      dplyr::pull("tbl_ard_summary") |>
      dplyr::filter(.data$stat_name == "tot_person_time") |>
      dplyr::pull(.data$stat) |>
      unlist() |>
      as.numeric() |>
      sum(na.rm = TRUE)
  }

  # Total Person-Time should be strictly greater when event_type = "all",
  # because the clock does not stop early for patients with AEs (P1, P2).
  expect_true(get_pt(tbl_all) > get_pt(tbl_first))
})

test_that("tbl_hierarchical_incidence_rate fails safely on bad inputs", {
  # 1. Missing Required Arguments
  expect_error(
    tbl_hierarchical_incidence_rate(denominator = adsl_test)
  )
  expect_error(
    tbl_hierarchical_incidence_rate(data = adae_test)
  )

  # 2. Invalid Dataframes
  expect_error(
    tbl_hierarchical_incidence_rate(
      data = "not_a_df", denominator = adsl_test, variables = c(AESOC, AEDECOD)
    )
  )

  # 3. Invalid Numeric/Scalar Inputs
  expect_error(
    tbl_hierarchical_incidence_rate(
      data = adae_test, denominator = adsl_test, variables = c(AESOC, AEDECOD),
      n_person_time = "100"
    )
  )
  expect_error(
    tbl_hierarchical_incidence_rate(
      data = adae_test, denominator = adsl_test, variables = c(AESOC, AEDECOD),
      conf.level = "0.95"
    )
  )
  expect_error(
    tbl_hierarchical_incidence_rate(
      data = adae_test, denominator = adsl_test, variables = c(AESOC, AEDECOD),
      digits = c(2, 3)
    )
  )

  # 4. Invalid Strings/Lists
  expect_error(
    tbl_hierarchical_incidence_rate(
      data = adae_test, denominator = adsl_test, variables = c(AESOC, AEDECOD),
      unit_label = 123
    )
  )
  expect_error(
    tbl_hierarchical_incidence_rate(
      data = adae_test, denominator = adsl_test, variables = c(AESOC, AEDECOD),
      conf.type = TRUE
    )
  )
  expect_error(
    tbl_hierarchical_incidence_rate(
      data = adae_test, denominator = adsl_test, variables = c(AESOC, AEDECOD),
      label = "Not a list"
    )
  )

  expect_error(
    tbl_hierarchical_incidence_rate(
      data = adae_test, denominator = adsl_test, variables = c(AESOC, AEDECOD),
      event_type = "Not all"
    )
  )

  # 5. Invalid Tidy-Select Dimensions
  # Less than 2 variables
  expect_error(
    tbl_hierarchical_incidence_rate(
      data = adae_test, denominator = adsl_test, variables = AESOC
    )
  )
  # Passing vectors to scalar tidy-select arguments
  expect_error(
    tbl_hierarchical_incidence_rate(
      data = adae_test, denominator = adsl_test, variables = c(AESOC, AEDECOD),
      by = c(ARM, USUBJID)
    )
  )
  expect_error(
    tbl_hierarchical_incidence_rate(
      data = adae_test, denominator = adsl_test, variables = c(AESOC, AEDECOD),
      start_date = c(TRTSDT, TRTEDT)
    )
  )
})

test_that("overall_row = FALSE omits the overall summary row", {
  tbl_with <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = ARM,
    start_date = TRTSDT,
    end_date = TRTEDT,
    event_date = AESTDTC,
    overall_row = TRUE
  )

  tbl_without <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = ARM,
    start_date = TRTSDT,
    end_date = TRTEDT,
    event_date = AESTDTC,
    overall_row = FALSE
  )

  # table without overall row should have fewer rows
  expect_lt(
    nrow(tbl_without$table_body),
    nrow(tbl_with$table_body)
  )

  # "All Adverse Events" label should not appear
  expect_false(
    "All Adverse Events" %in% tbl_without$table_body$label
  )
})

test_that("spanning headers are applied for treatment arms", {
  tbl <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = ARM,
    start_date = TRTSDT,
    end_date = TRTEDT,
    event_date = AESTDTC
  )

  spanners <- tbl$table_styling$spanning_header
  expect_gt(nrow(spanners), 0L)

  # each arm should appear in the spanning headers
  expect_true(any(grepl("Pbo", spanners$spanning_header)))
  expect_true(any(grepl("Trt", spanners$spanning_header)))

  # default uses the package convention "{level}  \n(N = {n})"
  expect_true(any(grepl("\\(N = [0-9]+\\)", spanners$spanning_header)))
})

test_that("spanning_label glue template customizes treatment arm headers", {
  tbl <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = ARM,
    start_date = TRTSDT,
    end_date = TRTEDT,
    event_date = AESTDTC,
    spanning_label = "{level} / n={n}"
  )

  spanners <- tbl$table_styling$spanning_header
  # custom format should appear instead of the default parenthesized form
  expect_true(any(grepl("/ n=[0-9]+", spanners$spanning_header)))
  expect_false(any(grepl("\\(N = [0-9]+\\)", spanners$spanning_header)))
})

test_that("add_overall appends overall columns", {
  tbl <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = ARM,
    start_date = TRTSDT,
    end_date = TRTEDT,
    event_date = AESTDTC
  )

  n_cols_before <- length(grep("^stat_", names(tbl$table_body)))

  expect_no_error(
    tbl_overall <- tbl |> add_overall()
  )

  n_cols_after <- length(grep("^stat_", names(tbl_overall$table_body)))

  # add_overall should add 5 new stat columns (count + 4 rate stats)
  expect_equal(n_cols_after, n_cols_before + 5L)

  # overall columns should exist
  expect_true(any(grepl("^stat_0_", names(tbl_overall$table_body))))

  # spanning header should contain the default overall label
  spanners <- tbl_overall$table_styling$spanning_header
  expect_true(any(grepl("All Participants", spanners$spanning_header)))
})

test_that("add_overall accepts a custom col_label", {
  tbl <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = ARM,
    start_date = TRTSDT,
    end_date = TRTEDT,
    event_date = AESTDTC
  )

  tbl_overall <- tbl |> add_overall(col_label = "Overall (N = {N})")

  spanners <- tbl_overall$table_styling$spanning_header
  expect_true(any(grepl("Overall \\(N = ", spanners$spanning_header)))
})

test_that("add_overall with last = TRUE places overall columns at the end", {
  tbl <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = ARM,
    start_date = TRTSDT,
    end_date = TRTEDT,
    event_date = AESTDTC
  )

  expect_no_error(
    tbl_last <- tbl |> add_overall(last = TRUE)
  )

  stat_cols <- grep("^stat_", names(tbl_last$table_body), value = TRUE)
  overall_cols <- grep("^stat_0_", stat_cols, value = TRUE)

  # overall columns should be at the end
  overall_positions <- match(overall_cols, stat_cols)
  expect_true(all(overall_positions > (length(stat_cols) - length(overall_cols))))
})

test_that("add_overall on table without by returns unaltered", {
  tbl_no_by <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = NULL,
    start_date = TRTSDT,
    end_date = TRTEDT,
    event_date = AESTDTC
  )

  expect_message(
    tbl_result <- tbl_no_by |> add_overall(),
    "Original table was not stratified"
  )

  # table should be unchanged
  expect_equal(
    names(tbl_result$table_body),
    names(tbl_no_by$table_body)
  )
})

test_that("gather_ard works after add_overall", {
  tbl <- tbl_hierarchical_incidence_rate(
    data = adae_test,
    denominator = adsl_test,
    variables = c(AESOC, AEDECOD),
    by = ARM,
    start_date = TRTSDT,
    end_date = TRTEDT,
    event_date = AESTDTC
  )

  ard_before <- gtsummary::gather_ard(tbl)
  n_before <- length(ard_before)

  tbl_with_overall <- tbl |> add_overall()
  ard_after <- gtsummary::gather_ard(tbl_with_overall)

  # gather_ard should return more elements after add_overall
  expect_gt(length(ard_after), n_before)
})
