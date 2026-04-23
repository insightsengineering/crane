# tests/testthat/test-tbl_hierarchical_incidence_rate.R

# 1. Create robust dummy data designed to trigger every edge case
adsl_test <- data.frame(
  USUBJID = c("P1", "P2", "P3", "P4", "P5"),
  ARM = c("Trt", "Trt", "Pbo", "Pbo", "Pbo"),
  TRTSDT = as.Date(rep("2023-01-01", 5)),
  TRTEDT = as.Date(
    c("2023-12-31", "2023-06-30", "2023-12-31", "2023-12-31", "2023-12-31")
  )
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

test_that(
  "tbl_hierarchical_incidence_rate executes standard usage with `by`",
  {
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

    # Validate class and structure
    expect_s3_class(tbl, "tbl_hierarchical_incidence_rate")
    expect_s3_class(tbl, "gtsummary")
    expect_true("table_body" %in% names(tbl))

    # Ensure the overall label defaulted correctly since it wasn't provided
    expect_true(any(tbl$table_body$label == "All Adverse Events"))
  }
)

test_that(
  "tbl_hierarchical_incidence_rate hits missing `by` branch and labels",
  {
    # Running without `by` tests the `length(by) == 0` renaming logic
    # Passing a custom overall label tests the label list extraction
    tbl_no_by <- tbl_hierarchical_incidence_rate(
      data = adae_test,
      denominator = adsl_test,
      variables = c(AESOC, AEDECOD),
      by = NULL,
      start_date = TRTSDT,
      end_date = TRTEDT,
      event_date = AESTDTC,
      n_person_time = 1000,
      unit_label = "months", # Hits the "PM" branch of the switch statement
      label = list(
        AESOC = "System Organ Class",
        "..ard_hierarchical_overall.." = "Custom Overall AEs"
      )
    )

    expect_s3_class(tbl_no_by, "gtsummary")
    expect_true(any(tbl_no_by$table_body$label == "Custom Overall AEs"))

    # Ensure the unit label switch successfully parsed "months" into "PM"
    header_df <- tbl_no_by$table_styling$header
    expect_true(any(grepl("PM", header_df$label)))
  }
)

test_that(
  "tbl_hierarchical_incidence_rate unit_label switch defaults correctly",
  {
    # Test the default fallback of the unit_label switch statement
    tbl_custom_unit <- tbl_hierarchical_incidence_rate(
      data = adae_test,
      denominator = adsl_test,
      variables = c(AESOC, AEDECOD),
      start_date = TRTSDT,
      end_date = TRTEDT,
      event_date = AESTDTC,
      # Forces the switch fallback: paste0("Person-", "Decades")
      unit_label = "decades"
    )

    header_df <- tbl_custom_unit$table_styling$header
    expect_true(any(grepl("Person-Decades", header_df$label)))
  }
)

test_that(
  "tbl_hierarchical_incidence_rate fails safely on bad inputs",
  {
    # Validates the checkmate assertions at the top of the wrapper
    expect_error(
      tbl_hierarchical_incidence_rate(
        data = adae_test,
        denominator = adsl_test,
        variables = c(AESOC, AEDECOD),
        # Should fail check_numeric
        n_person_time = "100"
      )
    )

    expect_error(
      tbl_hierarchical_incidence_rate(
        data = adae_test,
        denominator = adsl_test,
        variables = c(AESOC, AEDECOD),
        # Fails check_numeric/bounds based on strictness
        conf.level = 1.5
      )
    )

    # Expect failure if fewer than 2 hierarchical variables are provided
    expect_error(
      tbl_hierarchical_incidence_rate(
        data = adae_test,
        denominator = adsl_test,
        # Only 1 provided
        variables = AESOC
      )
    )
  }
)
