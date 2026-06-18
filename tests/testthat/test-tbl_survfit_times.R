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

test_that("tbl_survfit_times correctly renders the p-value row from combined data", {
  # 1. Setup data and fit the model
  surv_data <- survival::lung
  surv_data$status <- surv_data$status - 1
  surv_data$sex <- factor(surv_data$sex, labels = c("Male", "Female"))

  fit_km <- survival::survfit(survival::Surv(time, status) ~ sex, data = surv_data)

  # 2. Extract standard stats and differences
  df_surv <- get_surv_times_df(fit_km, times = 100)
  df_diff <- get_surv_diff_df(fit_km, times = 100, reference = "Male")

  # 3. Combine and render
  combined_df <- dplyr::bind_rows(df_surv, df_diff)
  res_tbl <- tbl_survfit_times(combined_df)

  # 4. Extract the underlying table body from the gtsummary object
  tbl_body <- res_tbl$table_body

  # 5. Assertions
  # The `.get_single_time_table` maps the column name to the `variable`
  # or `label` column inside gtsummary's internal structure.
  expect_true(
    "p-value" %in% tbl_body$variable || "p-value" %in% tbl_body$label,
    info = "The 'p-value' row was dropped and did not render in the final gtsummary table."
  )

  # Optionally, extract the specific row to ensure a value actually populated
  p_val_row <- tbl_body[tbl_body$variable == "p-value" | tbl_body$label == "p-value", ]
  expect_false(
    all(is.na(p_val_row)),
    info = "The p-value row was created, but all values inside it are NA."
  )
})
