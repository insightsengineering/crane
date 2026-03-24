test_that("annotate_pkc_df() warns when given a standard ggplot object", {
  df_pk <- data.frame(time = 1:5, conc = 1:5, arm = "A")

  # A standard ggplot object without the crane_gg_pkc class
  p_standard <- ggplot2::ggplot(df_pk, ggplot2::aes(x = time, y = conc, colour = arm))

  # Expect the specific warning
  expect_warning(
    annotate_pkc_df(data = df_pk, gg_plt = p_standard),
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
    res <- annotate_pkc_df(data = df_pk, gg_plt = p)
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
      data = df_pk,
      gg_plt = p,
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
    annotate_pkc_df(data = df_pk, gg_plt = p_no_group),
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
      data = df_pk,
      gg_plt = p,
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
    res <- annotate_pkc_df(data = df_pk, gg_plt = p)
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
    annotate_pkc_df(data = df_dummy, gg_plt = p_complex),
    regexp = "Missing variables"
  )
})
