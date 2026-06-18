skip_if_pkg_not_installed(c("survival"))

test_that("get_surv_times_df() works for stratified and unstratified models", {
  # Stratified Model
  fit_strat <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ TRTA,
    data = cards::ADTTE
  )

  df_strat <- get_surv_times_df(fit_strat, times = c(30, 60))

  expect_s3_class(df_strat, "data.frame")
  expect_equal(
    colnames(df_strat),
    c("Strata", "Time", "N at Risk", "Survival", "95% CI")
  )
  expect_equal(unique(df_strat$Time), c(30, 60))
  expect_true(length(unique(df_strat$Strata)) > 1)

  # Unstratified Model
  fit_unstrat <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ 1,
    data = cards::ADTTE
  )

  df_unstrat <- get_surv_times_df(fit_unstrat, times = c(30, 60))

  expect_s3_class(df_unstrat, "data.frame")
  expect_equal(unique(df_unstrat$Strata), "All")
})

test_that("get_surv_times_df() applies scale and conf_int correctly", {
  fit <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ 1,
    data = cards::ADTTE
  )

  df_scaled <- get_surv_times_df(
    fit,
    times = 30,
    conf_int = 0.90,
    scale = 100
  )

  # CI column should rename automatically based on conf_int
  expect_true("90% CI" %in% colnames(df_scaled))
  expect_false("95% CI" %in% colnames(df_scaled))

  # Ensure the value string reflects the x100 scaling
  # (e.g., "95.00" vs "0.95")
  surv_val <- as.numeric(df_scaled$Survival[1])
  expect_true(surv_val > 1)
})

test_that("get_surv_times_df() catches invalid inputs", {
  expect_error(
    get_surv_times_df(cards::ADTTE, times = c(30, 60)),
    "`fit_km` must be a survfit object",
    fixed = TRUE
  )

  fit <- survival::survfit(
    survival::Surv(AVAL, 1 - CNSR) ~ 1,
    data = cards::ADTTE
  )

  expect_error(
    get_surv_times_df(fit, times = NULL),
    "`times` must be a non-empty numeric vector",
    fixed = TRUE
  )
})
