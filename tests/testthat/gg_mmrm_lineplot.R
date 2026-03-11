# ------------------------------------------------------------------------------
# 1. SETUP MOCK DATA
# ------------------------------------------------------------------------------
# Create a lightweight mock object that mimics get_mmrm_results() output
mock_mmrm_df <- data.frame(
  ARM = factor(c("TRT", "PBO", "TRT", "PBO")),
  VISIT = factor(c("VIS1", "VIS1", "VIS2", "VIS2")),
  estimate_est = c(-1.5, -0.5, -2.5, -0.8),
  se_est = c(0.2, 0.2, 0.3, 0.3),
  lower_cl_est = c(-1.9, -0.9, -3.1, -1.4),
  upper_cl_est = c(-1.1, -0.1, -1.9, -0.2)
)
# Force the S3 class so it passes the check_class validation
class(mock_mmrm_df) <- c("mmrm_df", "data.frame")

# ------------------------------------------------------------------------------
# 2. TEST INPUT VALIDATION (Coverage for all `if` stops)
# ------------------------------------------------------------------------------
test_that("gg_mmrm_lineplot validates graphical parameters correctly", {

  # Dodge Width
  expect_error(
    gg_mmrm_lineplot(mock_mmrm_df, "ARM", "VISIT", dodge_width = "0.2"),
    "`dodge_width` must be a single numeric value."
  )
  expect_error(
    gg_mmrm_lineplot(mock_mmrm_df, "ARM", "VISIT", dodge_width = c(0.1, 0.2)),
    "`dodge_width` must be a single numeric value."
  )

  # Hline
  expect_error(
    gg_mmrm_lineplot(mock_mmrm_df, "ARM", "VISIT", hline = "0"),
    "`hline` must be a single numeric value or NULL."
  )

  # Legend Position
  expect_error(
    gg_mmrm_lineplot(mock_mmrm_df, "ARM", "VISIT", legend_pos = list(0, 0)),
    "`legend_pos` must be a character string \\(e.g., 'bottom'\\) or a numeric vector of length 2."
  )
})

# ------------------------------------------------------------------------------
# 3. TEST DATA MANIPULATION & MATH LOGIC
# ------------------------------------------------------------------------------
test_that("gg_mmrm_lineplot processes Baseline and CI math correctly", {
  p_ci <- gg_mmrm_lineplot(mock_mmrm_df, arm = "ARM", visit = "VISIT", error_bar = "ci")
  plot_data <- p_ci$data

  # 1. Check Baseline Injection
  expect_true("Baseline" %in% plot_data$Visit)
  base_rows <- plot_data[plot_data$Visit == "Baseline", ]
  expect_equal(nrow(base_rows), 2) # TRT and PBO
  expect_true(all(base_rows$est == 0))
  expect_true(all(base_rows$plot_y == 0))

  # 2. Check Math Inversion for Confidence Intervals (CI)
  # For TRT VIS1: est was -1.5, lcl was -1.9, ucl was -1.1
  # Since plot_y = -est (1.5), ymin must be -ucl (1.1) and ymax must be -lcl (1.9)
  trt_vis1 <- plot_data[plot_data$Arm == "TRT" & plot_data$Visit == "VIS1", ]
  expect_equal(trt_vis1$plot_y, 1.5)
  expect_equal(trt_vis1$ymin, 1.1)
  expect_equal(trt_vis1$ymax, 1.9)

  # 3. Check Y-Axis Label
  expect_equal(p_ci$labels$y, "Mean (\u00B1 95% CI) Change from Baseline")
})

test_that("gg_mmrm_lineplot processes SE math correctly", {
  p_se <- gg_mmrm_lineplot(mock_mmrm_df, arm = "ARM", visit = "VISIT", error_bar = "se")
  plot_data <- p_se$data

  # For TRT VIS1: est was -1.5, se was 0.2
  # plot_y = 1.5, ymin = 1.5 - 0.2 = 1.3, ymax = 1.5 + 0.2 = 1.7
  trt_vis1 <- plot_data[plot_data$Arm == "TRT" & plot_data$Visit == "VIS1", ]
  expect_equal(trt_vis1$ymin, 1.3)
  expect_equal(trt_vis1$ymax, 1.7)

  # Check Y-Axis Label
  expect_equal(p_se$labels$y, "Mean (\u00B1 SE) Change from Baseline")
})

# ------------------------------------------------------------------------------
# 4. TEST GGPLOT THEME AND GEOMS
# ------------------------------------------------------------------------------
test_that("gg_mmrm_lineplot applies dynamic theming (hline and legend)", {

  # Plot 1: Numeric legend, default hline (0)
  p_numeric <- gg_mmrm_lineplot(mock_mmrm_df, "ARM", "VISIT", legend_pos = c(0.1, 0.1))

  # Check legend justification evaluates to c(0, 0)
  expect_equal(p_numeric$theme$legend.justification, c(0, 0))

  # Verify geom_hline exists (usually the first geom added if hline is not NULL)
  geoms <- sapply(p_numeric$layers, function(x) class(x$geom)[1])
  expect_true("GeomHline" %in% geoms)


  # Plot 2: String legend, hline = NULL
  p_string <- gg_mmrm_lineplot(mock_mmrm_df, "ARM", "VISIT", legend_pos = "bottom", hline = NULL)

  # Check legend justification evaluates to "center"
  expect_equal(p_string$theme$legend.justification, "center")
  expect_equal(p_string$theme$legend.position, "bottom")

  # Verify geom_hline is completely missing
  geoms_null <- sapply(p_string$layers, function(x) class(x$geom)[1])
  expect_false("GeomHline" %in% geoms_null)
})
