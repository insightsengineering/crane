library(testthat)
library(ggplot2)

# --- Mock Data Setup ---
set.seed(123)
mock_adlb <- data.frame(
  USUBJID = paste0("SUBJ", 1:60),
  ARM = rep(c("Treatment A", "Treatment B"), each = 30),
  AVISIT = rep(c(0, 4, 8), 20),
  AVAL = rnorm(60, mean = 10, sd = 2)
)

# Introduce a missing visit for one group to test the `tidyr::expand` padding logic
mock_adlb <- mock_adlb[!(mock_adlb$ARM == "Treatment A" & mock_adlb$AVISIT == 8), ]


# --- Tests ---

test_that("gg_lineplot input validation catches invalid statistical combinations", {
  # Mean + IQR is mathematically invalid
  expect_error(
    gg_lineplot(
      data = mock_adlb, x = AVISIT, y = AVAL, group = ARM,
      stat = "mean", variability = "iqr"
    ),
    regexp = "Cannot plot IQR around a mean"
  )

  # Median + SD/SE/CI is mathematically invalid
  expect_error(
    gg_lineplot(
      data = mock_adlb, x = AVISIT, y = AVAL, group = ARM,
      stat = "median", variability = "sd"
    ),
    regexp = "Invalid combination of stat"
  )

  expect_error(
    gg_lineplot(
      data = mock_adlb, x = AVISIT, y = AVAL, group = ARM,
      stat = "median", variability = "ci"
    ),
    regexp = "Invalid combination of stat"
  )
})

test_that("gg_lineplot builds correctly with grouped data (Mean + CI)", {
  expect_no_error(
    p <- gg_lineplot(
      data = mock_adlb, x = AVISIT, y = AVAL, group = ARM,
      stat = "mean", variability = "ci"
    )
  )

  # Check object classes
  expect_s3_class(p, "crane_gg_line")
  expect_s3_class(p, "ggplot")

  # Grouped plots should have 3 layers: point, line, errorbar
  expect_equal(length(p$layers), 3)

  # Check that the dynamic labeling successfully replaced the raw .data pronoun
  expect_equal(p$labels$colour, "ARM")
})

test_that("gg_lineplot builds correctly with ungrouped data (Mean + SD)", {
  expect_no_error(
    p <- gg_lineplot(
      data = mock_adlb, x = AVISIT, y = AVAL,
      stat = "mean", variability = "sd"
    )
  )

  expect_s3_class(p, "crane_gg_line")

  # Ungrouped plots should have 2 layers: point and errorbar (no line layer)
  expect_equal(length(p$layers), 2)

  # Ensure the group aesthetic is empty for ungrouped plots
  expect_null(p$mapping$group)
})

test_that("gg_lineplot builds correctly for median and IQR", {
  expect_no_error(
    p <- gg_lineplot(
      data = mock_adlb, x = AVISIT, y = AVAL, group = ARM,
      stat = "median", variability = "iqr"
    )
  )
  expect_s3_class(p, "ggplot")
})

test_that("gg_lineplot correctly skips errorbar layer when variability is 'none'", {
  expect_no_error(
    p_grouped <- gg_lineplot(
      data = mock_adlb, x = AVISIT, y = AVAL, group = ARM,
      stat = "mean", variability = "none"
    )
  )
  # Grouped with no variability: point, line (2 layers)
  expect_equal(length(p_grouped$layers), 2)

  expect_no_error(
    p_ungrouped <- gg_lineplot(
      data = mock_adlb, x = AVISIT, y = AVAL,
      stat = "mean", variability = "none"
    )
  )
  # Ungrouped with no variability: point only (1 layer)
  expect_equal(length(p_ungrouped$layers), 1)
})

test_that("gg_lineplot converts factor y to numeric automatically", {
  # Create a copy of the data and force AVAL to be a factor
  mock_factor_y <- mock_adlb
  mock_factor_y$AVISIT <- as.factor(mock_factor_y$AVISIT)

  # The plot should build without error and silently convert AVAL back to numeric
  expect_no_error(
    p_factor <- gg_lineplot(
      data = mock_factor_y,
      x = AVISIT,
      y = AVAL,
      stat = "mean",
      variability = "none"
    )
  )

  expect_s3_class(p_factor, "crane_gg_line")
})


test_that("gg_lineplot converts character y to numeric automatically", {
  # Create a copy of the data and force AVAL to be a factor
  mock_factor_y <- mock_adlb
  mock_factor_y$AVISIT <- as.character(mock_factor_y$AVISIT)

  # The plot should build without error and silently convert AVAL back to numeric
  expect_no_error(
    p_factor <- gg_lineplot(
      data = mock_factor_y,
      x = AVISIT,
      y = AVAL,
      stat = "mean",
      variability = "none"
    )
  )

  expect_s3_class(p_factor, "crane_gg_line")
})

test_that("gg_lineplot informs users about numeric vs categorical x-axis", {
  # 1. Numeric x-axis hits the final `else` branch (encourages using factor)
  expect_message(
    gg_lineplot(
      data = mock_adlb,
      x = AVISIT,
      y = AVAL,
      group = ARM,
      stat = "mean",
      variability = "none"
    ),
    regexp = "We encourage you to supply `x` as a factor"
  )

  # 2. True categorical factor hits the "Categorical X-axis detected" branch
  mock_adlb_cat <- mock_adlb
  mock_adlb_cat$AVISIT <- factor(ifelse(mock_adlb_cat$AVISIT == 0, "Baseline", "Week 4"))

  expect_message(
    gg_lineplot(
      data = mock_adlb_cat,
      x = AVISIT,
      y = AVAL,
      group = ARM,
      stat = "mean",
      variability = "none"
    ),
    regexp = "Categorical X-axis detected"
  )
})
