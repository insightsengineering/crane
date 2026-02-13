# Helper function to create test data
create_test_data <- function() {
  set.seed(123)

  # Create ADaM-like data
  adlb <- data.frame(
    USUBJID = rep(paste0("SUBJ-", 1:20), each = 3),
    ARM = rep(c(rep("Treatment A", 10), rep("Treatment B", 10)), each = 3),
    AVISIT = rep(factor(c("Baseline", "Week 4", "Week 8")), 20),
    AVAL = rnorm(60, mean = 12, sd = 2)
  )

  adsl <- data.frame(
    USUBJID = paste0("SUBJ-", 1:20),
    ARM = c(rep("Treatment A", 10), rep("Treatment B", 10))
  )

  list(adlb = adlb, adsl = adsl)
}

# Test calc_stats function
#' @description Test calc_stats function
#' @param x Numeric vector
#' @param conf_level Confidence level
#' @param decimal_places Number of decimal places
test_that("calc_stats works correctly", {
  set.seed(456)
  x <- rnorm(100, mean = 10, sd = 2)

  # Test default parameters
  result <- calc_stats(x)
  expect_is(result, "list")
  expect_names(result, c("n", "mean", "mean_ci", "mean_ci_lwr", "mean_ci_upr", "median", "sd"))
  expect_equal(result$n, 100)
  expect_gt(result$mean, 9)
  expect_lt(result$mean, 11)
  expect_gt(result$sd, 1.5)
  expect_lt(result$sd, 2.5)

  # Test custom confidence level
  result_90 <- calc_stats(x, conf_level = 0.90)
  expect_lt(abs(result_90$mean_ci_lwr - result_90$mean_ci_upr),
            abs(result$mean_ci_lwr - result$mean_ci_upr))

  # Test custom decimal places
  result_3dec <- calc_stats(x, decimal_places = 3)
  expect_match(result_3dec$mean_ci, "^[0-9.]+ [0-9.]+$")

  # Test with NA values
  x_na <- c(x, NA, NA)
  result_na <- calc_stats(x_na)
  expect_equal(result_na$n, 100)  # NA values should be excluded

  # Test with all NA values
  x_all_na <- rep(NA, 10)
  result_all_na <- calc_stats(x_all_na)
  expect_equal(result_all_na$n, 0)
  expect_true(is.na(result_all_na$mean))
  expect_true(is.na(result_all_na$sd))
})

# Test g_lineplot_without_table function
#' @description Test g_lineplot_without_table function
test_that("g_lineplot_without_table works correctly", {
  # Create test statistics data
  df_stats <- data.frame(
    AVISIT = factor(c("Baseline", "Week 4", "Week 8")),
    ARM = c("Treatment", "Treatment", "Treatment"),
    mean = c(10.5, 12.3, 14.1),
    mean_ci_lwr = c(9.2, 11.0, 12.8),
    mean_ci_upr = c(11.8, 13.6, 15.4)
  )

  # Test basic functionality
  plot <- g_lineplot_without_table(
    df_stats = df_stats,
    x = "AVISIT",
    mid = "mean",
    whiskers = c("mean_ci_lwr", "mean_ci_upr")
  )
  expect_is(plot, "ggplot")

  # Test with stratification
  df_stats_strat <- rbind(
    transform(df_stats, ARM = "Treatment A"),
    transform(df_stats, ARM = "Treatment B", mean = mean + 2,
              mean_ci_lwr = mean_ci_lwr + 2, mean_ci_upr = mean_ci_upr + 2)
  )

  plot_strat <- g_lineplot_without_table(
    df_stats = df_stats_strat,
    x = "AVISIT",
    mid = "mean",
    strata_N = "ARM",
    whiskers = c("mean_ci_lwr", "mean_ci_upr")
  )
  expect_is(plot_strat, "ggplot")

  # Test different mid_type options
  plot_points <- g_lineplot_without_table(
    df_stats = df_stats,
    x = "AVISIT",
    mid = "mean",
    whiskers = c("mean_ci_lwr", "mean_ci_upr"),
    mid_type = "p"
  )
  expect_is(plot_points, "ggplot")

  plot_lines <- g_lineplot_without_table(
    df_stats = df_stats_strat,
    x = "AVISIT",
    mid = "mean",
    strata_N = "ARM",
    whiskers = c("mean_ci_lwr", "mean_ci_upr"),
    mid_type = "l"
  )
  expect_is(plot_lines, "ggplot")
})

# Test g_lineplot_table function
#' @description Test g_lineplot_table function
test_that("g_lineplot_table works correctly", {
  # Create test statistics data
  df_stats <- data.frame(
    AVISIT = factor(c("Baseline", "Week 4", "Week 8")),
    ARM = c("Treatment", "Treatment", "Treatment"),
    n = c(50, 48, 45),
    mean = c(10.5, 12.3, 14.1),
    mean_ci = c("9.20 11.80", "11.00 13.60", "12.80 15.40")
  )

  # Test basic functionality
  table_plot <- g_lineplot_table(
    df_stats = df_stats,
    x = "AVISIT",
    group_var = "ARM",
    table = c("n", "mean")
  )
  expect_is(table_plot, "ggplot")

  # Test without group_var
  table_plot_no_group <- g_lineplot_table(
    df_stats = df_stats,
    x = "AVISIT",
    group_var = NULL,
    table = c("n", "mean")
  )
  expect_is(table_plot_no_group, "ggplot")

  # Test with different decimal places
  table_plot_3dec <- g_lineplot_table(
    df_stats = df_stats,
    x = "AVISIT",
    group_var = "ARM",
    table = c("n", "mean"),
    decimal_places = 3
  )
  expect_is(table_plot_3dec, "ggplot")
})

test_that("g_lineplot_with_table works correctly", {
  # Create test statistics data
  df_stats <- data.frame(
    AVISIT = factor(c("Baseline", "Week 4", "Week 8", "Baseline", "Week 4", "Week 8")),
    ARM = c(rep("Treatment A", 3), rep("Treatment B", 3)),
    ARM_N = c(rep("Treatment A (N = 50)", 3), rep("Treatment B (N = 48)", 3)),
    n = c(50, 48, 45, 48, 46, 44),
    mean = c(10.5, 12.3, 14.1, 10.8, 11.5, 12.2),
    mean_ci_lwr = c(9.2, 11.0, 12.8, 9.5, 10.2, 10.9),
    mean_ci_upr = c(11.8, 13.6, 15.4, 12.1, 12.8, 13.5),
    mean_ci = c("9.20 11.80", "11.00 13.60", "12.80 15.40",
               "9.50 12.10", "10.20 12.80", "10.90 13.50")
  )
  df_stats$ARM_N <- factor(df_stats$ARM_N)

  # Test without table (should return ggplot)
  plot_only <- g_lineplot_with_table(
    df_stats = df_stats,
    x = "AVISIT",
    group_var = "ARM",
    strata_N = "ARM_N",
    table = NULL
  )
  expect_is(plot_only, "ggplot")

  # Test with table (should return cowplot object)
  if (requireNamespace("cowplot", quietly = TRUE)) {
    plot_with_table <- g_lineplot_with_table(
      df_stats = df_stats,
      x = "AVISIT",
      group_var = "ARM",
      strata_N = "ARM_N",
      table = c("n", "mean", "mean_ci"),
      title = "Mean Values with 95% CI by Visit",
      rel_height_plot = 0.6
    )
    expect_is(plot_with_table, "ggplot")  # cowplot objects inherit from ggplot
  }
})

test_that("preprocess_lineplot_data works correctly", {
  test_data <- create_test_data()
  adlb <- test_data$adlb
  adsl <- test_data$adsl

  # Test basic functionality
  df_stats <- preprocess_lineplot_data(
    df = adlb,
    alt_counts_df = adsl,
    x = "AVISIT",
    y = "AVAL",
    group_var = "ARM",
    subject_var = "USUBJID"
  )

  expect_is(df_stats, "data.frame")
  expect_true(all(c("AVISIT", "ARM", "n", "mean", "mean_ci", "mean_ci_lwr", "mean_ci_upr", "median", "sd", "ARM_N") %in% names(df_stats)))
  expect_equal(nrow(df_stats), 6)  # 2 arms * 3 visits
  expect_gt(min(df_stats$n), 0)

  # Test with custom confidence level using ...
  df_stats_90ci <- preprocess_lineplot_data(
    df = adlb,
    alt_counts_df = adsl,
    x = "AVISIT",
    y = "AVAL",
    group_var = "ARM",
    subject_var = "USUBJID",
    conf_level = 0.90
  )

  expect_is(df_stats_90ci, "data.frame")
  # 90% CI should be narrower than 95% CI
  ci_range_90 <- abs(df_stats_90ci$mean_ci_upr - df_stats_90ci$mean_ci_lwr)
  ci_range_95 <- abs(df_stats$mean_ci_upr - df_stats$mean_ci_lwr)
  expect_lt(mean(ci_range_90, na.rm = TRUE), mean(ci_range_95, na.rm = TRUE))

  # Test with custom decimal places using ...
  df_stats_3dec <- preprocess_lineplot_data(
    df = adlb,
    alt_counts_df = adsl,
    x = "AVISIT",
    y = "AVAL",
    group_var = "ARM",
    subject_var = "USUBJID",
    decimal_places = 3
  )

  expect_is(df_stats_3dec, "data.frame")
  # Check that mean values have 3 decimal places
  expect_match(df_stats_3dec$mean[1], "^[0-9]+\.[0-9]{3}$")

  # Test without grouping variable
  df_stats_ungrouped <- preprocess_lineplot_data(
    df = adlb,
    x = "AVISIT",
    y = "AVAL",
    group_var = NULL
  )

  expect_is(df_stats_ungrouped, "data.frame")
  expect_equal(nrow(df_stats_ungrouped), 3)  # 3 visits only
  expect_true("ARM_N" %in% names(df_stats_ungrouped) == FALSE)

  # Test with custom calc_stats function
  custom_calc_stats <- function(x, conf_level = 0.95, decimal_places = 2) {
    n <- sum(!is.na(x))
    if (n == 0) return(list(n = 0, mean = NA, mean_ci_lwr = NA, mean_ci_upr = NA, median = NA, sd = NA))

    m <- mean(x, na.rm = TRUE)
    list(
      n = n,
      mean = round(m * 2, decimal_places),  # Double the mean for testing
      mean_ci_lwr = round(m * 2 - 1, decimal_places),
      mean_ci_upr = round(m * 2 + 1, decimal_places),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE)
    )
  }

  df_stats_custom <- preprocess_lineplot_data(
    df = adlb,
    alt_counts_df = adsl,
    x = "AVISIT",
    y = "AVAL",
    group_var = "ARM",
    subject_var = "USUBJID",
    calc_stats = custom_calc_stats
  )

  expect_is(df_stats_custom, "data.frame")
  # Custom function should double the means
  original_means <- df_stats$mean
  custom_means <- df_stats_custom$mean
  expect_gt(mean(custom_means, na.rm = TRUE), mean(original_means, na.rm = TRUE))
})
