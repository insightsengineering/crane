library(ggplot2)
library(testthat)

# --- Shared Mock Objects for Utility Tests ---

# 1. Dummy continuous base plot
p_dummy <- ggplot2::ggplot(mtcars, aes(x = mpg, y = disp)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(limits = c(0, 40), breaks = c(0, 10, 20, 30, 40))

# 2. Dummy discrete base plot for categorical X-axis tests
df_plot_discrete <- data.frame(
  Visit = factor(c("Baseline", "Week 4")),
  Value = c(10, 20)
)
p_discrete <- ggplot2::ggplot(df_plot_discrete, aes(x = Visit, y = Value)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_discrete(limits = c("Baseline", "Week 4"))

# 3. Dummy dataframe for KM engine tests (Continuous)
df_km_dummy <- data.frame(
  `0` = c(10, 20),
  `10` = c(8, 15),
  check.names = FALSE
)
rownames(df_km_dummy) <- c("Group A", "Group B")

# 4. Dummy dataframe for PK engine tests (Continuous)
df_pk_dummy <- data.frame(
  Statistic = c("Mean", "SD"),
  `0` = c(10.5, 2.1),
  `10` = c(8.4, 1.8),
  check.names = FALSE
)

# 5. Dummy dataframe for PK engine tests (Discrete/Categorical)
df_pk_discrete <- data.frame(
  Statistic = c("Mean", "SD"),
  `Baseline` = c(10.5, 2.1),
  `Week 4` = c(8.4, 1.8),
  check.names = FALSE
)


# --- Tests for df2gg_aligned() ---

test_that("df2gg_aligned input validation and error handling works", {
  # Fails if df is not a data.frame
  expect_error(
    df2gg_aligned(df = list(A = 1), gg_plt = p_dummy, type = "KM"),
    regexp = "must be a data.frame or tibble"
  )

  # Fails if gg_plt is not a ggplot
  expect_error(
    df2gg_aligned(df = df_km_dummy, gg_plt = "not_a_plot", type = "KM"),
    regexp = "must be a valid ggplot2 object"
  )

  # Fails if PK/GEN table has less than 2 columns
  expect_error(
    df2gg_aligned(
      df = data.frame(Statistic = "Mean"),
      gg_plt = p_dummy,
      type = "PK"
    ),
    regexp = "must have at least 2 columns"
  )
})

test_that("df2gg_aligned works for KM and GEN types", {
  # Tests the "KM" branch mapping rownames
  expect_no_error(
    res_km <- df2gg_aligned(df = df_km_dummy, gg_plt = p_dummy, type = "KM")
  )
  expect_s3_class(res_km, "ggplot")

  # Tests the "GEN" branch mapping first column
  expect_no_error(
    res_gen <- df2gg_aligned(df = df_pk_dummy, gg_plt = p_dummy, type = "GEN")
  )
  expect_s3_class(res_gen, "ggplot")
})

test_that("df2gg_aligned aborts on continuous fallback failure", {
  # Tests the continuous fallback error branch
  df_bad <- data.frame(
    Statistic = c("Mean"),
    `BadA` = 1,
    `BadB` = 2,
    check.names = FALSE
  )
  expect_error(
    df2gg_aligned(df = df_bad, gg_plt = p_dummy, type = "PK"),
    regexp = "None of the table columns could be coerced to numeric coordinates"
  )
})

test_that("df2gg_aligned handles categorical columns mapped to discrete x-axis", {
  expect_no_error(
    res <- df2gg_aligned(df = df_pk_discrete, gg_plt = p_discrete, type = "PK")
  )
  expect_s3_class(res, "ggplot")
})

test_that("df2gg_aligned catches mismatched categorical column names", {
  # Create a table with completely unmapped categorical names
  df_pk_mismatch <- data.frame(
    Statistic = c("Mean", "SD"),
    `Month 12` = c(10.5, 2.1), # "Month 12" is not in the p_discrete limits!
    `Month 24` = c(8.4, 1.8),
    check.names = FALSE
  )

  # When plotted against p_discrete, it should trigger the Line 115 abort
  expect_error(
    df2gg_aligned(df = df_pk_mismatch, gg_plt = p_discrete, type = "PK"),
    regexp = "None of the table columns match the categorical X-axis levels"
  )
})

test_that("df2gg_aligned aborts when discrete limits are missing", {
  # 1. Pre-build the plot OUTSIDE the mock
  pb_mocked <- ggplot2::ggplot_build(p_discrete)

  # 2. Duck Typing: Overwrite the scale object with a standard R list.
  # This guarantees our mock functions will execute and return NULL limits.
  pb_mocked$layout$panel_scales_x[[1]] <- list(
    is_discrete = function() TRUE,
    get_limits = function() NULL
  )

  # 3. The mock returns the object with our fake scale
  mock_build_no_limits <- function(plot) {
    pb_mocked
  }

  # 4. Run the engine to trigger the Line 110 abort
  testthat::with_mocked_bindings(
    code = {
      expect_error(
        df2gg_aligned(df = df_pk_discrete, gg_plt = p_discrete, type = "PK"),
        regexp = "Cannot extract discrete limits from the plot for alignment."
      )
    },
    ggplot_build = mock_build_no_limits,
    .package = "ggplot2"
  )
})

test_that("df2gg_aligned fully formats a PK table with title and x-axis", {
  expect_no_error(
    res <- df2gg_aligned(
      df = df_pk_dummy, gg_plt = p_dummy, type = "PK",
      title = "PK Summary", xlab = "Time (hr)", show_xaxis = TRUE
    )
  )
  expect_s3_class(res, "ggplot")
})

test_that("df2gg_aligned formats x-axis correctly when xlab is NULL", {
  expect_no_error(
    res <- df2gg_aligned(
      df = df_pk_dummy, gg_plt = p_dummy, type = "PK",
      xlab = NULL, show_xaxis = TRUE
    )
  )
  expect_s3_class(res, "ggplot")
})

# --- Tests for df2gg_floating() ---

test_that("df2gg_floating works when col_labels = FALSE", {
  df_float <- data.frame(Statistic = c("N", "Median"), Value = c(50, 15.5))
  res <- df2gg_floating(df = df_float, gg_plt = p_dummy, col_labels = FALSE)
  expect_s3_class(res, "ggplot")
})

test_that("df2gg_floating works with column labels, NAs, and background fill", {
  df_float_na <- data.frame(Statistic = c("N", "Median"), Value = c(50, NA))
  expect_no_error(
    res <- df2gg_floating(
      df = df_float_na, gg_plt = p_dummy,
      col_labels = TRUE, bg_fill = "white"
    )
  )
  expect_s3_class(res, "ggplot")
})

# --- Tests for gg_varname_extraction() ---

test_that("gg_varname_extraction extracts standard symbols and pronouns", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = .data[["disp"]]))
  expect_equal(gg_varname_extraction(p$mapping$x), "mpg")
  expect_equal(gg_varname_extraction(p$mapping$y), "disp")
})

test_that("gg_varname_extraction returns NULL for complex or invalid inputs", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg / 2, y = log(disp)))
  expect_null(gg_varname_extraction(p$mapping$x))
  expect_null(gg_varname_extraction(NULL))
})

# --- Tests for .calc_stats() ---

test_that(".calc_stats calculates mean and variabilities correctly", {
  val <- c(10, 20, 30)

  # SD branch
  res_sd <- .calc_stats(val, stat = "mean", variability = "sd", conf_level = 0.95)
  expect_equal(res_sd$y, 20)
  expect_equal(res_sd$ymin, 20 - stats::sd(val))
  expect_equal(res_sd$ymax, 20 + stats::sd(val))

  # SE branch
  res_se <- .calc_stats(val, stat = "mean", variability = "se", conf_level = 0.95)
  se_val <- stats::sd(val) / sqrt(3)
  expect_equal(res_se$ymin, 20 - se_val)

  # CI branch
  res_ci <- .calc_stats(val, stat = "mean", variability = "ci", conf_level = 0.95)
  ci_val <- stats::qt(0.975, df = 2) * se_val
  expect_equal(res_ci$ymin, 20 - ci_val)

  # None branch
  res_none <- .calc_stats(
    val,
    stat = "mean", variability = "none", conf_level = 0.95
  )
  expect_equal(res_none$ymin, 20)
  expect_equal(res_none$ymax, 20)

  # Default switch branch (unrecognized variability falls back to 0)
  res_other <- .calc_stats(
    val,
    stat = "mean", variability = "unknown", conf_level = 0.95
  )
  expect_equal(res_other$ymin, 20)
  expect_equal(res_other$ymax, 20)
})

test_that(".calc_stats calculates median and variabilities correctly", {
  val <- 1:10

  # IQR branch
  res_iqr <- .calc_stats(val, stat = "median", variability = "iqr", conf_level = 0.95)
  expect_equal(res_iqr$y, 5.5)
  expect_equal(res_iqr$ymin, unname(stats::quantile(val, 0.25)))
  expect_equal(res_iqr$ymax, unname(stats::quantile(val, 0.75)))

  # None/Fallback branch
  res_none <- .calc_stats(val, stat = "median", variability = "none", conf_level = 0.95)
  expect_equal(res_none$ymin, 5.5)
  expect_equal(res_none$ymax, 5.5)
})

test_that(".calc_stats handles empty and NA vectors cleanly", {
  # Zero length
  res_empty <- .calc_stats(
    numeric(0),
    stat = "mean", variability = "sd", conf_level = 0.95
  )
  expect_true(is.na(res_empty$y))
  expect_true(is.na(res_empty$ymin))

  # Only NAs
  res_na <- .calc_stats(
    c(NA_real_, NA_real_),
    stat = "mean", variability = "sd", conf_level = 0.95
  )
  expect_true(is.na(res_na$y))
})

# --- Tests for gg_add_stats() ---

test_that("gg_add_stats adds stat_summary layer to a ggplot object", {
  p_base <- ggplot2::ggplot(mtcars, ggplot2::aes(x = factor(cyl), y = mpg)) +
    ggplot2::geom_point()

  # Function runs and returns a ggplot object
  expect_no_error(
    res_plot <- gg_add_stats(gg_plt = p_base, stat = "mean", variability = "ci")
  )
  expect_s3_class(res_plot, "ggplot")

  # Verify the stat_summary layer was successfully added (base point + stat)
  expect_equal(length(res_plot$layers), 2)
  expect_true(inherits(res_plot$layers[[2]]$stat, "StatSummary"))
})
