# File: test-annotate_gg_pkc.R

test_that("annotate_gg_pkc() works with automatic variable extraction", {
  # 1. Create mock PK data
  df_pk <- data.frame(
    time = rep(c(0, 1, 2, 4), each = 4),
    conc = runif(16, 10, 100),
    arm = rep(c("Cohort A", "Cohort B"), times = 8)
  )

  # 2. Create base plot with mappings
  p <- ggplot2::ggplot(
    df_pk,
    ggplot2::aes(x = time, y = conc, colour = arm)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line()

  # 3. Test that the function runs without error and returns a plot
  expect_no_error(
    res <- annotate_gg_pkc(data = df_pk, gg_plt = p)
  )

  # Check that the output is still a plot object (ggplot/cowplot)
  expect_s3_class(res, "ggplot")
})

test_that("annotate_gg_pkc() works when variables are explicitly provided", {
  df_pk <- data.frame(
    time_hr = rep(c(0, 1, 2), each = 4),
    pk_conc = runif(12, 10, 100),
    cohort = rep(c("A", "B"), times = 6)
  )

  # Base plot WITHOUT colour mapping
  p <- ggplot2::ggplot(df_pk, ggplot2::aes(x = time_hr, y = pk_conc)) +
    ggplot2::geom_point()

  # Should run without error because we manually supply time_var, analyte_var,
  # and group
  expect_no_error(
    res <- annotate_gg_pkc(
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

test_that("annotate_gg_pkc() throws error when variables are missing", {
  df_pk <- data.frame(
    time = 1:5,
    conc = 1:5
  )

  # Plot mapping only x and y, no colour (group)
  p_no_group <- ggplot2::ggplot(df_pk, ggplot2::aes(x = time, y = conc)) +
    ggplot2::geom_point()

  # Should fail because `group` cannot be extracted and isn't provided
  expect_error(
    annotate_gg_pkc(data = df_pk, gg_plt = p_no_group),
    "Missing variables. Specify `time_var`"
  )
})

test_that("annotate_gg_pkc() correctly handles invalid summary_stats", {
  # Provide slightly more data so `gtsummary` won't fail internally
  # if execution ever accidentally reaches it
  df_pk <- data.frame(
    time = c(1, 1, 2, 2),
    conc = c(10, 12, 15, 18),
    arm = c("A", "A", "B", "B")
  )

  p <- ggplot2::ggplot(df_pk, ggplot2::aes(x = time, y = conc, colour = arm))

  # Pass ONLY an invalid statistic.
  # (Because `match.arg` with `several.ok = TRUE` will silently ignore
  # invalid elements if a valid element like "n" is also present!)
  expect_error(
    annotate_gg_pkc(
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

  # Plot using the newer .data[[]] pronoun syntax in mapping
  p <- ggplot2::ggplot(
    df_pk,
    ggplot2::aes(
      x = .data[["time_var"]],
      y = .data[["conc"]],
      colour = .data[["arm"]]
    )
  )

  expect_no_error(
    res <- annotate_gg_pkc(data = df_pk, gg_plt = p)
  )

  expect_s3_class(res, "ggplot")
})

test_that("annotate_gg_pkc handles complex aesthetic mappings gracefully", {
  # 1. Create a dummy PK dataset
  df_dummy <- data.frame(
    TIME = c(1, 2, 3), 
    CONC = c(10, 5, 2.5), 
    TRT = c("A", "A", "A")
  )
  
  # 2. Build a plot using a mathematical expression in the aesthetic mapping
  p_complex <- ggplot2::ggplot(
    df_dummy, 
    ggplot2::aes(x = TIME / 24, y = log(CONC), colour = TRT)
  ) +
    ggplot2::geom_point()
  
  # 3. The complex 'x' and 'y' aesthetics will cause `get_var()` to fall back 
  # to `return(NULL)`, which then triggers the main function's missing variable error.
  expect_error(
    annotate_gg_pkc(data = df_dummy, gg_plt = p_complex),
    regexp = "Missing variables"
  )
})