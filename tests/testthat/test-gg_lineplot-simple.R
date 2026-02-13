# Copyright (c) 2024 Roche
# SPDX-License-Identifier: MIT

#' @title Simple tests for gg_lineplot functions
#' @description Basic tests that can run without full package installation

# Load required libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

# Load the functions
source("R/gg_lineplot.R")

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
cat("Testing calc_stats function...\n")
set.seed(456)
x <- rnorm(100, mean = 10, sd = 2)

result <- calc_stats(x)
if (is.list(result) && all(c("n", "mean", "mean_ci", "mean_ci_lwr", "mean_ci_upr", "median", "sd") %in% names(result))) {
  cat("✓ calc_stats basic test passed\n")
} else {
  cat("✗ calc_stats basic test failed\n")
}

# Test custom confidence level
result_90 <- calc_stats(x, conf_level = 0.90)
if (abs(result_90$mean_ci_lwr - result_90$mean_ci_upr) < abs(result$mean_ci_lwr - result$mean_ci_upr)) {
  cat("✓ calc_stats confidence level test passed\n")
} else {
  cat("✗ calc_stats confidence level test failed\n")
}

# Test g_lineplot_without_table function
cat("Testing g_lineplot_without_table function...\n")
df_stats <- data.frame(
  AVISIT = factor(c("Baseline", "Week 4", "Week 8")),
  ARM = c("Treatment", "Treatment", "Treatment"),
  mean = c(10.5, 12.3, 14.1),
  mean_ci_lwr = c(9.2, 11.0, 12.8),
  mean_ci_upr = c(11.8, 13.6, 15.4)
)

plot <- tryCatch(
  {
    g_lineplot_without_table(
      df_stats = df_stats,
      x = "AVISIT",
      mid = "mean",
      whiskers = c("mean_ci_lwr", "mean_ci_upr")
    )
  },
  error = function(e) NULL
)

if (!is.null(plot) && inherits(plot, "ggplot")) {
  cat("✓ g_lineplot_without_table basic test passed\n")
} else {
  cat("✗ g_lineplot_without_table basic test failed\n")
}

# Test g_lineplot_table function
cat("Testing g_lineplot_table function...\n")
df_stats_table <- data.frame(
  AVISIT = factor(c("Baseline", "Week 4", "Week 8")),
  ARM = c("Treatment", "Treatment", "Treatment"),
  n = c(50, 48, 45),
  mean = c(10.5, 12.3, 14.1),
  mean_ci = c("9.20 11.80", "11.00 13.60", "12.80 15.40")
)

table_plot <- tryCatch(
  {
    g_lineplot_table(
      df_stats = df_stats_table,
      x = "AVISIT",
      group_var = "ARM",
      table = c("n", "mean")
    )
  },
  error = function(e) NULL
)

if (!is.null(table_plot) && inherits(table_plot, "ggplot")) {
  cat("✓ g_lineplot_table basic test passed\n")
} else {
  cat("✗ g_lineplot_table basic test failed\n")
}

# Test preprocess_lineplot_data function
cat("Testing preprocess_lineplot_data function...\n")
test_data <- create_test_data()
adlb <- test_data$adlb
adsl <- test_data$adsl

df_stats_preprocessed <- tryCatch(
  {
    preprocess_lineplot_data(
      df = adlb,
      alt_counts_df = adsl,
      x = "AVISIT",
      y = "AVAL",
      group_var = "ARM",
      subject_var = "USUBJID"
    )
  },
  error = function(e) NULL
)

if (!is.null(df_stats_preprocessed) && is.data.frame(df_stats_preprocessed)) {
  cat("✓ preprocess_lineplot_data basic test passed\n")

  # Test that it has expected columns
  expected_cols <- c("AVISIT", "ARM", "n", "mean", "mean_ci", "mean_ci_lwr", "mean_ci_upr", "median", "sd", "ARM_N")
  if (all(expected_cols %in% names(df_stats_preprocessed))) {
    cat("✓ preprocess_lineplot_data columns test passed\n")
  } else {
    cat("✗ preprocess_lineplot_data columns test failed\n")
    cat("Missing columns:", setdiff(expected_cols, names(df_stats_preprocessed)), "\n")
  }

  # Test custom confidence level using ...
  df_stats_90ci <- tryCatch(
    {
      preprocess_lineplot_data(
        df = adlb,
        alt_counts_df = adsl,
        x = "AVISIT",
        y = "AVAL",
        group_var = "ARM",
        subject_var = "USUBJID",
        conf_level = 0.90
      )
    },
    error = function(e) NULL
  )

  if (!is.null(df_stats_90ci) && is.data.frame(df_stats_90ci)) {
    cat("✓ preprocess_lineplot_data ... arguments test passed\n")
  } else {
    cat("✗ preprocess_lineplot_data ... arguments test failed\n")
  }
} else {
  cat("✗ preprocess_lineplot_data basic test failed\n")
}

cat("\nAll basic tests completed!\n")
