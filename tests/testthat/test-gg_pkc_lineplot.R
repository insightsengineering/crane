# ------------------------------------------------------------------------------
# 1. SETUP MOCK DATA
# ------------------------------------------------------------------------------
# We create a lightweight mock dataset where we deliberately engineered a
# situation where Mean - SD is a negative number (Drug A at ATPTN = 12).
# Drug A at ATPTN 12: Values = 1 and 10. Mean = 5.5, SD = 6.36.
mock_pk_df <- data.frame(
  USUBJID = c("P1", "P1", "P1", "P2", "P2", "P2", "P3", "P3", "P3", "P4", "P4", "P4"),
  TRT = rep(c("Drug A", "Drug B"), each = 6),
  ATPTN = rep(c(0, 4, 12), times = 4),
  AVAL = c(0, 10, 1, 0, 12, 10, 0, 20, 10, 0, 18, 12)
)

# ------------------------------------------------------------------------------
# 2. TEST INPUT VALIDATION & ERROR HANDLING
# ------------------------------------------------------------------------------
test_that("gg_pkc_lineplot catches invalid combinations and missing inputs", {
  # Catch invalid combinations of stat and variability
  expect_error(
    gg_pkc_lineplot(
      mock_pk_df,
      time_var = ATPTN,
      analyte_var = AVAL,
      group = TRT,
      stat = "median",
      variability = "sd"
    ),
    "Invalid combination of stat"
  )

  expect_error(
    gg_pkc_lineplot(
      mock_pk_df,
      time_var = ATPTN,
      analyte_var = AVAL,
      group = TRT,
      stat = "mean",
      variability = "iqr"
    ),
    "Invalid combination of stat"
  )

  # Catch invalid arguments trapped by match.arg
  expect_error(
    gg_pkc_lineplot(
      mock_pk_df,
      time_var = ATPTN,
      analyte_var = AVAL,
      group = TRT,
      stat = "average"
    ),
    "should be one of"
  )

  # Catch missing mandatory arguments
  expect_error(
    gg_pkc_lineplot(data = mock_pk_df, analyte_var = AVAL, group = TRT)
  )
})

# ------------------------------------------------------------------------------
# 3. TEST DATA MANIPULATION & MATH LOGIC
# ------------------------------------------------------------------------------
test_that("gg_pkc_lineplot calculates statistics and floors negative bounds appropriately", {
  # --- Test 1: Linear Scale (log_y = FALSE) ---
  p_sd_linear <- gg_pkc_lineplot(
    mock_pk_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    stat = "mean",
    variability = "sd",
    log_y = FALSE
  )

  # Ensure the ad-hoc class was attached correctly
  expect_s3_class(p_sd_linear, "crane_gg_pkc")

  # Extract the computed statistical data from the ggplot object
  # Layer 1 = Line, Layer 2 = Point, Layer 3 = Errorbar
  pb_linear <- ggplot2::ggplot_build(p_sd_linear)
  err_data_linear <- pb_linear$data[[3]]

  # Test standard Mean and SD calculation
  # Drug B at ATPTN 4: Values = 20 and 18. Mean = 19. SD = 1.414.
  # ymax should be 19 + 1.414 = 20.414
  drug_b_4 <- err_data_linear[round(err_data_linear$x, 0) == 4 & err_data_linear$y > 15, ]
  expect_equal(drug_b_4$y, 19)
  expect_equal(round(drug_b_4$ymax, 3), 20.414)

  # Test Negative Flooring Logic for Linear Scale (1e-5)
  # Drug A at ATPTN 12: Mean = 5.5, SD = 6.36.
  # Mean - SD = -0.86 (Negative!). It should be floored to 1e-5.
  drug_a_12_linear <- err_data_linear[round(err_data_linear$x, 0) == 12 & err_data_linear$y == 5.5, ]
  expect_equal(drug_a_12_linear$ymin, 1e-5)
  expect_equal(round(drug_a_12_linear$ymax, 2), 11.86)

  # --- Test 2: Log Scale (log_y = TRUE) ---
  p_sd_log <- suppressWarnings(gg_pkc_lineplot(
    mock_pk_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    stat = "mean",
    variability = "sd",
    log_y = TRUE
  ))

  pb_log <- suppressWarnings(ggplot2::ggplot_build(p_sd_log))
  err_data_log <- pb_log$data[[3]]

  # Test Negative Bounds Logic for Log Scale (NA_real_)
  # NOTE: Because scale_y_log10() transforms data BEFORE stat_summary computes,
  # calculations are done on log10 values.
  # Drug A at ATPTN 12: AVAL = 1 and 10 -> Log10 values are 0 and 1.
  # Mean = 0.5. SD = 0.707.
  # ymin_val = 0.5 - 0.707 = -0.207. Since -0.207 <= 0, it becomes NA_real_.
  drug_a_12_log <- err_data_log[round(err_data_log$x, 0) == 12 & round(err_data_log$y, 1) == 0.5, ]
  expect_true(is.na(drug_a_12_log$ymin))
  expect_equal(round(drug_a_12_log$ymax, 3), 1.207)
})

test_that("gg_pkc_lineplot calculates median and IQR properly", {
  p_iqr <- gg_pkc_lineplot(
    mock_pk_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    stat = "median",
    variability = "iqr",
    log_y = FALSE
  )

  pb <- ggplot2::ggplot_build(p_iqr)
  err_data <- pb$data[[3]]

  # Drug B at ATPTN 4: Values = 20, 18. Median = 19.
  # IQR 25th = 18.5, 75th = 19.5
  drug_b_4_iqr <- err_data[round(err_data$x, 0) == 4 & err_data$y == 19, ]
  expect_equal(drug_b_4_iqr$ymin, 18.5)
  expect_equal(drug_b_4_iqr$ymax, 19.5)
})

# ------------------------------------------------------------------------------
# 4. TEST GGPLOT THEME AND GEOMS
# ------------------------------------------------------------------------------
test_that("gg_pkc_lineplot applies graphical parameters correctly", {
  # Plot 1: Log scale, LLOQ provided
  p_log_lloq <- gg_pkc_lineplot(
    mock_pk_df[mock_pk_df$AVAL > 0, ],
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    log_y = TRUE,
    lloq = 2.5
  )

  # Verify Y-axis is log-10
  expect_equal(p_log_lloq$scales$get_scales("y")$trans$name, "log-10")

  # Verify geom_hline (LLOQ) was added safely using vapply
  geoms_lloq <- vapply(p_log_lloq$layers, function(x) class(x$geom)[1], character(1))
  expect_true("GeomHline" %in% geoms_lloq)


  # Plot 2: Linear scale, no LLOQ, no variability
  p_linear_none <- gg_pkc_lineplot(
    mock_pk_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    stat = "mean",
    variability = "none",
    log_y = FALSE,
    lloq = NA
  )

  # Verify Y-axis is NOT log-10 (should be "identity" or missing log trans)
  expect_false(identical(p_linear_none$scales$get_scales("y")$trans$name, "log-10"))

  # Verify geom_hline is missing
  geoms_none <- vapply(p_linear_none$layers, function(x) class(x$geom)[1], character(1))
  expect_false("GeomHline" %in% geoms_none)

  # Verify error bars are missing (Only 2 layers: Line and Point)
  expect_equal(length(p_linear_none$layers), 2)
  expect_false("GeomErrorbar" %in% geoms_none)
})

test_that("gg_pkc_lineplot warns when log transforming zeros", {
  # Create the plot using data that STILL contains 0s at ATPTN = 0
  p_zero_log <- gg_pkc_lineplot(
    data = mock_pk_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    log_y = TRUE
  )

  # Capture ALL warnings generated by ggplot_build at once
  warns <- capture_warnings(ggplot2::ggplot_build(p_zero_log))

  # Verify that both expected ggplot2 warnings exist within the captured list
  expect_true(any(grepl("log-10 transformation introduced infinite values", warns)))
  expect_true(any(grepl("Removed .* rows containing non-finite outside the scale range", warns)))
})
