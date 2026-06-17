skip_if_pkg_not_installed(c("survival", "withr", "cards", "gtsummary"))

test_that("tbl_survfit_times() works for stratified data", {
  withr::local_options(list(width = 120))

  fit <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ TRTA,
    data = cards::ADTTE
  )
  df <- get_surv_times_df(fit, times = c(30, 60))

  expect_silent(tbl <- tbl_survfit_times(df))
  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_survfit_times() works for unstratified data", {
  withr::local_options(list(width = 120))

  fit <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ 1,
    data = cards::ADTTE
  )
  # Also testing passing a custom confidence interval representation
  df <- get_surv_times_df(fit, times = c(30, 60), conf_int = 0.90)

  expect_silent(tbl <- tbl_survfit_times(df))
  expect_snapshot(as.data.frame(tbl))
})

test_that("tbl_survfit_times() adapts to user modifications (dropping columns)", {
  withr::local_options(list(width = 120))

  fit <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ 1,
    data = cards::ADTTE
  )
  df <- get_surv_times_df(fit, times = 30)

  # User dynamically drops the N at Risk column and renames CI
  df_custom <- df[, c("Strata", "Time", "Survival")]

  expect_silent(tbl <- tbl_survfit_times(df_custom))

  # Ensure the resulting gtsummary table doesn't break and only contains Survival
  tbl_df <- as.data.frame(tbl)
  expect_false(any(grepl("N at Risk", tbl_df[[1]])))
  expect_snapshot(tbl_df)
})

test_that("tbl_survfit_times() catches invalid inputs", {
  withr::local_options(list(width = 120))

  # Fails when not a data frame
  expect_error(
    tbl_survfit_times(list(Time = 30, Survival = 0.5)),
    "`surv_df` must be a data.frame",
    fixed = TRUE
  )

  fit <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ 1,
    data = cards::ADTTE
  )
  df <- get_surv_times_df(fit, times = c(30, 60))

  # Fails when missing mandatory columns
  df_missing_time <- df[, c("Strata", "Survival")]
  expect_error(
    tbl_survfit_times(df_missing_time),
    "`surv_df` must contain at least 'Strata' and 'Time' columns",
    fixed = TRUE
  )
})

test_that("add_overall.tbl_survfit_times() acts as a legacy guard", {
  fit <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ TRTA,
    data = cards::ADTTE
  )
  df <- get_surv_times_df(fit, times = 30)
  tbl <- tbl_survfit_times(df)

  # Check that the class was properly assigned
  expect_s3_class(tbl, "tbl_survfit_times")

  # Check that calling add_overall() throws our informative migration error
  expect_error(
    gtsummary::add_overall(tbl),
    "`add_overall()` is defunct for `tbl_survfit_times`",
    fixed = TRUE
  )
})