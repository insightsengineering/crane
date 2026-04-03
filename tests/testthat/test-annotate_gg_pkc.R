test_that("annotate_pkc_df() warns when given a standard ggplot object", {
  df_pk <- data.frame(time = 1:5, conc = 1:5, arm = "A")

  # A standard ggplot object without the crane_gg_pkc class
  p_standard <- ggplot2::ggplot(df_pk, ggplot2::aes(x = time, y = conc, colour = arm))

  # Expect the specific warning
  expect_warning(
    annotate_pkc_df(gg_plt = p_standard, data = df_pk),
    regexp = "This function was specifically designed"
  )
})

test_that("annotate_pkc_df() works with automatic variable extraction", {
  df_pk <- data.frame(
    time = rep(c(0, 1, 2, 4), each = 4),
    conc = runif(16, 10, 100),
    arm = rep(c("Cohort A", "Cohort B"), times = 8)
  )

  # Use gg_pkc_lineplot to generate the properly structured plot
  p <- gg_pkc_lineplot(
    data = df_pk,
    time_var = time,
    analyte_var = conc,
    group = arm,
    log_y = FALSE
  )

  # Test that the function runs without error and returns a plot
  expect_no_error(
    res <- annotate_pkc_df(gg_plt = p, data = df_pk)
  )

  # Check that the output is still a plot object (ggplot/cowplot)
  expect_s3_class(res, "ggplot")
})

test_that("annotate_pkc_df() works when variables are explicitly provided", {
  df_pk <- data.frame(
    time_hr = rep(c(0, 1, 2), each = 4),
    pk_conc = runif(12, 10, 100),
    cohort = rep(c("A", "B"), times = 6)
  )

  p <- gg_pkc_lineplot(
    data = df_pk,
    time_var = time_hr,
    analyte_var = pk_conc,
    group = cohort,
    log_y = FALSE
  )

  # Explicitly override auto-extraction
  expect_no_error(
    res <- annotate_pkc_df(
      gg_plt = p,
      data = df_pk,
      time_var = "time_hr",
      analyte_var = "pk_conc",
      group = "cohort",
      summary_stats = c("n", "median", "iqr")
    )
  )

  expect_s3_class(res, "ggplot")
})

test_that("annotate_pkc_df() throws error when variables are missing", {
  df_pk <- data.frame(time = 1:5, conc = 1:5)

  # Plot mapping only x and y, no colour (group)
  p_no_group <- ggplot2::ggplot(df_pk, ggplot2::aes(x = time, y = conc)) +
    ggplot2::geom_point()

  # Manually inject the class to bypass the warning and ONLY test the error logic
  class(p_no_group) <- c("crane_gg_pkc", class(p_no_group))

  # Should fail because `group` cannot be extracted and isn't provided
  expect_error(
    annotate_pkc_df(gg_plt = p_no_group, data = df_pk),
    "Missing variables. Specify `time_var`"
  )
})

test_that("annotate_pkc_df() correctly handles invalid summary_stats", {
  df_pk <- data.frame(
    time = c(1, 1, 2, 2),
    conc = c(10, 12, 15, 18),
    arm = c("A", "A", "B", "B")
  )

  p <- gg_pkc_lineplot(df_pk, time, conc, arm, log_y = FALSE)

  # Pass ONLY an invalid statistic.
  expect_error(
    annotate_pkc_df(
      gg_plt = p,
      data = df_pk,
      summary_stats = "invalid_stat"
    ),
    "should be one of"
  )
})

test_that("get_var helper correctly extracts .data[[var]] mappings", {
  df_pk <- data.frame(
    time_var = 1:5,
    conc = 1:5,
    arm = c("A", "B", "A", "B", "A")
  )

  # gg_pkc_lineplot internally maps variables using .data[[var]],
  # so this natively tests the helper's extraction logic!
  p <- gg_pkc_lineplot(df_pk, time_var, conc, arm, log_y = FALSE)

  expect_no_error(
    res <- annotate_pkc_df(gg_plt = p, data = df_pk)
  )

  expect_s3_class(res, "ggplot")
})

test_that("annotate_pkc_df handles complex aesthetic mappings", {
  df_dummy <- data.frame(
    TIME = c(1, 2, 3),
    CONC = c(10, 5, 2.5),
    TRT = c("A", "A", "A")
  )

  # Build a plot using a mathematical expression in the aesthetic mapping
  p_complex <- ggplot2::ggplot(
    df_dummy,
    ggplot2::aes(x = TIME / 24, y = log(CONC), colour = TRT)
  ) +
    ggplot2::geom_point()

  # Tag to suppress class warning
  class(p_complex) <- c("crane_gg_pkc", class(p_complex))

  # The complex 'x' and 'y' aesthetics will cause `get_var()` to fall back
  # to `return(NULL)`, triggering the missing variable error.
  expect_error(
    annotate_pkc_df(gg_plt = p_complex, data = df_dummy),
    regexp = "Missing variables"
  )
})

test_that("annotate_pkc_df formats numeric digits correctly", {
  # 1. Setup Data
  set.seed(123)
  df_pk <- data.frame(
    time = rep(c(0, 1, 2, 4), each = 4),
    conc = runif(16, 10, 100),
    arm = rep(c("Cohort A", "Cohort B"), times = 8)
  )

  p <- gg_pkc_lineplot(
    data = df_pk,
    time_var = time,
    analyte_var = conc,
    group = arm,
    log_y = FALSE
  )

  # 2. Mock df2gg_aligned to intercept the data frame before plotting
  mock_df2gg <- function(df, ...) {
    return(df) # Return the data frame instead of generating the ggplot
  }

  # 3. Run the function using the mocked binding
  res_df <- testthat::with_mocked_bindings(
    {
      annotate_pkc_df(
        gg_plt = p,
        data = df_pk,
        summary_stats = c("n", "mean", "sd"),
        digits = c(0, 2, 2)
      )
    },
    df2gg_aligned = mock_df2gg
  )

  # 4. Verify we intercepted the data frame
  expect_s3_class(res_df, "data.frame")

  # 5. Extract Time '0' values for n, Mean, and SD from the first treatment group
  # (Using trimws() ensures we don't fail due to hidden trailing spaces)
  val_n <- trimws(res_df[grepl("n$", trimws(res_df$Group)), "0"][1])
  val_mean <- trimws(res_df[grepl("Mean$", trimws(res_df$Group)), "0"][1])
  val_sd <- trimws(res_df[grepl("SD$", trimws(res_df$Group)), "0"][1])

  # 6. Assertions using Regular Expressions

  # Check 'n' has NO decimal point (e.g., "2" not "2.0")
  expect_false(grepl("\\.", val_n))

  # Check 'mean' has a decimal point followed by EXACTLY 2 digits (e.g., "55.50")
  expect_true(grepl("\\.[0-9]{2}$", val_mean))

  # Check 'sd' has a decimal point followed by EXACTLY 2 digits (e.g., "12.34")
  expect_true(grepl("\\.[0-9]{2}$", val_sd))
})
