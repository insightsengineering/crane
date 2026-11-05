# ------------------------------------------------------------------------------
# 1. SETUP MOCK DATA
# ------------------------------------------------------------------------------
# One group/timepoint combination deliberately contains an extreme value
# (Drug A at ATPTN = 0 includes 100 among 1:4) so whisker methods can be told
# apart: tukey flags it as an outlier, percentile/minmax do not (the sample is
# too small for the 5th/95th percentiles to fall inside the extreme value).
# Drug A at ATPTN = 4 includes a 0 among 21:24 so the log_y filtering step can
# be tested independently of the whisker logic.
mock_pk_box_df <- data.frame(
  USUBJID = rep(paste0("P", 1:5), times = 4),
  TRT = rep(rep(c("Drug A", "Drug B"), each = 5), times = 2),
  ATPTN = rep(c(0, 4), each = 10),
  AVAL = c(
    1, 2, 3, 4, 100, 10, 11, 12, 13, 14,
    0, 21, 22, 23, 24, 30, 31, 32, 33, 34
  )
)

# ------------------------------------------------------------------------------
# 2. TEST INPUT VALIDATION & ERROR HANDLING
# ------------------------------------------------------------------------------
test_that("gg_pkc_boxplot catches invalid arguments and missing inputs", {
  # Catch invalid `whisker` trapped by match.arg
  expect_error(
    gg_pkc_boxplot(
      mock_pk_box_df,
      time_var = ATPTN,
      analyte_var = AVAL,
      group = TRT,
      whisker = "bogus"
    ),
    "should be one of"
  )

  # Catch missing mandatory arguments
  expect_error(
    gg_pkc_boxplot(data = mock_pk_box_df, analyte_var = AVAL, group = TRT)
  )
})

# ------------------------------------------------------------------------------
# 3. TEST DATA MANIPULATION & MATH LOGIC
# ------------------------------------------------------------------------------
test_that("gg_pkc_boxplot computes tukey whiskers and flags outliers", {
  p_tukey <- gg_pkc_boxplot(
    mock_pk_box_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    whisker = "tukey",
    log_y = FALSE
  )

  expect_s3_class(p_tukey, "crane_gg_pkc_box")

  # Layer 1 = boxplot (hinges + whiskers), Layer 3 = outlier points
  pb <- ggplot2::ggplot_build(p_tukey)
  box_data <- pb$data[[1]]
  outlier_y <- pb$data[[3]]$y

  # Drug A at ATPTN 0: Values = 1, 2, 3, 4, 100.
  # Quartiles (type 1) = 2, 3, 4. IQR = 2, so whiskers clip at 2 - 3 = -1
  # (floored to the min, 1) and 4 + 3 = 7 -- 100 sits well outside and is
  # flagged as an outlier rather than stretching the whisker.
  drug_a_0 <- box_data[box_data$group == 1, ]
  expect_equal(drug_a_0$ymin, 1)
  expect_equal(drug_a_0$lower, 2)
  expect_equal(drug_a_0$middle, 3)
  expect_equal(drug_a_0$upper, 4)
  expect_equal(drug_a_0$ymax, 7)
  expect_true(100 %in% outlier_y)

  # Drug B at ATPTN 0: Values = 10:14, tightly clustered, so nothing is
  # flagged as an outlier.
  drug_b_0 <- box_data[box_data$group == 2, ]
  expect_equal(drug_b_0$ymin, 10)
  expect_equal(drug_b_0$ymax, 14)
})

test_that("gg_pkc_boxplot percentile and minmax whiskers do not flag the same outlier", {
  # With only 5 points per group, the 5th/95th percentiles land on the
  # extremes themselves, so the 100 is absorbed into the whisker instead of
  # being flagged -- unlike the tukey method above.
  p_pct <- gg_pkc_boxplot(
    mock_pk_box_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    whisker = "percentile",
    log_y = FALSE
  )
  pb_pct <- ggplot2::ggplot_build(p_pct)
  drug_a_0_pct <- pb_pct$data[[1]][pb_pct$data[[1]]$group == 1, ]
  expect_equal(drug_a_0_pct$ymin, 1)
  expect_equal(drug_a_0_pct$ymax, 100)
  expect_true(all(is.na(pb_pct$data[[3]]$y)))

  # minmax whiskers span the full range by definition, so no point can ever
  # be flagged as an outlier.
  p_mm <- gg_pkc_boxplot(
    mock_pk_box_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    whisker = "minmax",
    log_y = FALSE
  )
  pb_mm <- ggplot2::ggplot_build(p_mm)
  drug_a_0_mm <- pb_mm$data[[1]][pb_mm$data[[1]]$group == 1, ]
  expect_equal(drug_a_0_mm$ymin, 1)
  expect_equal(drug_a_0_mm$ymax, 100)
  expect_true(all(is.na(pb_mm$data[[3]]$y)))
})

test_that("gg_pkc_boxplot passes quantile_type through to stats::quantile", {
  qt_df <- data.frame(USUBJID = paste0("P", 1:4), TRT = "G1", ATPTN = 0, AVAL = c(1, 2, 3, 4))

  # type 1 (default) picks the order statistic directly: Q1 = 1.
  # type 7 (R's default elsewhere) interpolates: Q1 = 1.75.
  p_type1 <- gg_pkc_boxplot(
    qt_df,
    time_var = ATPTN, analyte_var = AVAL, group = TRT,
    quantile_type = 1, log_y = FALSE
  )
  p_type7 <- gg_pkc_boxplot(
    qt_df,
    time_var = ATPTN, analyte_var = AVAL, group = TRT,
    quantile_type = 7, log_y = FALSE
  )

  expect_equal(ggplot2::ggplot_build(p_type1)$data[[1]]$lower, 1)
  expect_equal(ggplot2::ggplot_build(p_type7)$data[[1]]$lower, 1.75)
})

# ------------------------------------------------------------------------------
# 4. TEST GGPLOT THEME AND GEOMS
# ------------------------------------------------------------------------------
test_that("gg_pkc_boxplot drops non-positive values before log-transforming", {
  # Drug A at ATPTN 4 includes a 0, which would otherwise poison the tukey
  # whisker calculation and warn when scale_y_log10() hits log10(0). Because
  # gg_pkc_boxplot() filters non-positive values up front, the 0 never
  # reaches box_stats() and no warning is raised.
  p_log <- gg_pkc_boxplot(
    mock_pk_box_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    whisker = "tukey",
    log_y = TRUE
  )

  expect_equal(p_log$scales$get_scales("y")$trans$name, "log-10")
  expect_no_warning(ggplot2::ggplot_build(p_log))

  # With the 0 removed, Drug A at ATPTN 4 (now just 21:24) has no outliers.
  pb <- ggplot2::ggplot_build(p_log)
  drug_a_4 <- pb$data[[1]][pb$data[[1]]$group == 3, ]
  expect_equal(drug_a_4$ymin, log10(21))
  expect_equal(drug_a_4$ymax, log10(24))
})

test_that("gg_pkc_boxplot toggles the mean marker layer with show_mean", {
  p_mean <- gg_pkc_boxplot(
    mock_pk_box_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    show_mean = TRUE,
    log_y = FALSE
  )
  p_no_mean <- gg_pkc_boxplot(
    mock_pk_box_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    show_mean = FALSE,
    log_y = FALSE
  )

  geoms_mean <- vapply(p_mean$layers, function(x) class(x$geom)[1], character(1))
  geoms_no_mean <- vapply(p_no_mean$layers, function(x) class(x$geom)[1], character(1))

  # Boxplot + errorbar + outlier points + mean marker
  expect_equal(unname(geoms_mean), c("GeomBoxplot", "GeomErrorbar", "GeomPoint", "GeomPoint"))
  # Boxplot + errorbar + outlier points only
  expect_equal(unname(geoms_no_mean), c("GeomBoxplot", "GeomErrorbar", "GeomPoint"))
})

test_that("gg_pkc_boxplot dodges boxes, whiskers, outliers and the mean marker together", {
  p <- gg_pkc_boxplot(
    mock_pk_box_df,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    show_mean = TRUE,
    log_y = FALSE
  )

  positions <- lapply(p$layers, function(x) x$position)

  expect_true(all(vapply(positions, inherits, logical(1), what = "PositionDodge")))
  widths <- vapply(positions, function(x) x$width, numeric(1))
  expect_true(all(widths == widths[1]))
})

test_that("gg_pkc_boxplot converts time_var to a discrete factor regardless of input type", {
  mock_pk_box_char <- mock_pk_box_df
  mock_pk_box_char$ATPTN <- as.character(mock_pk_box_char$ATPTN)

  p_char <- gg_pkc_boxplot(
    mock_pk_box_char,
    time_var = ATPTN,
    analyte_var = AVAL,
    group = TRT,
    log_y = FALSE
  )

  expect_s3_class(p_char$data$.time, "factor")
  expect_equal(levels(p_char$data$.time), c("0", "4"))
})
