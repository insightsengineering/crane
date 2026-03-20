library(ggplot2)

# --- Shared Mock Objects for Utility Tests ---

# 1. Dummy base plot to align tables against
p_dummy <- ggplot2::ggplot(mtcars, aes(x = mpg, y = disp)) +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(limits = c(0, 40), breaks = c(0, 10, 20, 30, 40))

# 2. Dummy dataframe for KM engine tests
df_km_dummy <- data.frame(
  `0` = c(10, 20),
  `10` = c(8, 15),
  check.names = FALSE
)
rownames(df_km_dummy) <- c("Group A", "Group B")

# 3. Dummy dataframe for PK engine tests
df_pk_dummy <- data.frame(
  Statistic = c("Mean", "SD"),
  `0` = c(10.5, 2.1),
  `10` = c(8.4, 1.8),
  check.names = FALSE
)


# --- Tests for df2gg_aligned() ---

test_that("df2gg_aligned input validation and error handling works", {
  
  # Fails if df is not a data.frame (Coverage Line 72)
  expect_error(
    df2gg_aligned(df = list(A = 1), gg_plt = p_dummy, type = "KM"),
    regexp = "must be a data.frame or tibble"
  )
  
  # Fails if gg_plt is not a ggplot (Coverage Line 76)
  expect_error(
    df2gg_aligned(df = df_km_dummy, gg_plt = "not_a_plot", type = "KM"),
    regexp = "must be a valid ggplot2 object"
  )
  
  # Fails if PK table has less than 2 columns (Coverage Line 87)
  expect_error(
    df2gg_aligned(df = data.frame(Statistic = "Mean"), gg_plt = p_dummy, type = "PK"),
    regexp = "must have at least 2 columns"
  )
})

test_that("df2gg_aligned warns and handles non-numeric column names", {
  
  # Create a dataframe with one valid numeric column and one invalid character column
  df_bad_cols <- data.frame(
    `10` = c(5, 10), 
    `BadName` = c(20, 30), 
    check.names = FALSE
  )
  rownames(df_bad_cols) <- c("Group A", "Group B")
  
  # Expect the specific warning to trigger and drop the bad column (Coverage Lines 101-103)
  expect_warning(
    res <- df2gg_aligned(df = df_bad_cols, gg_plt = p_dummy, type = "KM"),
    regexp = "cannot be coerced to numeric"
  )
  
  # Ensure it still successfully returns a plot object despite the warning
  expect_s3_class(res, "ggplot")
})


# --- Tests for df2gg_floating() ---

test_that("df2gg_floating works when col_labels = FALSE", {
  
  # Dataframe for floating table
  df_float <- data.frame(
    Statistic = c("N", "Median"),
    Value = c(50, 15.5)
  )
  
  # Run the engine with col_labels explicitly turned off (Coverage Line 309)
  res <- df2gg_floating(
    df = df_float, 
    gg_plt = p_dummy, 
    col_labels = FALSE
  )
  
  # Verify it returns a valid cowplot/ggplot object
  expect_s3_class(res, "ggplot")
})