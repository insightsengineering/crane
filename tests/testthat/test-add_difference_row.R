skip_if_pkg_not_installed(c("survival", "withr", "cards"))

# 1. New Decoupled Setup
fit_strat <- survival::survfit(
  survival::Surv(AVAL, 1 - CNSR) ~ TRTA,
  data = cards::ADTTE
)
surv_df <- get_surv_times_df(fit_strat, times = c(60, 120))
tbl <- tbl_survfit_times(surv_df)

test_that("add_difference_row.tbl_survfit_times() works", {
  withr::local_options(list(width = 200))
  
  # Standard use (times inferred automatically)
  expect_silent(
    tbl1 <- tbl |>
      add_difference_row(fit = fit_strat, reference = "Placebo")
  )
  expect_snapshot(as.data.frame(tbl1))

  # Works with different reference column
  expect_silent(
    tbl2 <- tbl |>
      add_difference_row(fit = fit_strat, reference = "Xanomeline Low Dose")
  )
  expect_equal(
    as.data.frame(tbl2) |> names(),
    c("", "Xanomeline Low Dose  \n(N = 84)", "Placebo  \n(N = 86)", "Xanomeline High Dose  \n(N = 84)")
  )

  # Works with custom statistics/formats
  expect_silent(
    tbl3 <- tbl |>
      add_difference_row(
        fit = fit_strat,
        reference = "Placebo",
        statistic = c("{estimate} ({std.error})", "{statistic} (p = {p.value})"),
        pvalue_fun = label_style_pvalue(digits = 3),
        estimate_fun = label_roche_number(digits = 1, scale = 100)
      )
  )
  expect_snapshot(as.data.frame(tbl3))

  # Works when times are explicitly provided (bypassing inference)
  expect_silent(
    tbl4 <- tbl |>
      add_difference_row(fit = fit_strat, reference = "Placebo", times = c(60, 120))
  )
  expect_snapshot(as.data.frame(tbl4))
})

test_that("add_difference_row.tbl_survfit_times() error messaging works", {
  withr::local_options(list(width = 200))

  # Error 1: Model has no stratification variable
  fit_unstrat <- survival::survfit(survival::Surv(AVAL, 1 - CNSR) ~ 1, data = cards::ADTTE)
  df_unstrat <- get_surv_times_df(fit_unstrat, times = c(30, 60))
  
  expect_snapshot(
    error = TRUE,
    tbl_survfit_times(df_unstrat) |>
      add_difference_row(fit = fit_unstrat, reference = "Placebo")
  )

  # Error 2: Reference level does not exist
  expect_snapshot(
    error = TRUE,
    tbl_survfit_times(surv_df) |>
      add_difference_row(fit = fit_strat, reference = "No Treatment")
  )
})