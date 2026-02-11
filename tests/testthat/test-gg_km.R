# pre-processing the km fit
anl <- cards::ADTTE |>
  dplyr::mutate(is_event = CNSR == 0)
by <- "TRTP"
anl[[by]] <- factor(anl[[by]], levels = c(
  "Placebo",
  "Xanomeline Low Dose",
  "Xanomeline High Dose"
))
group_sym <- rlang::sym(by)
model_formula <- rlang::new_formula(
  lhs = rlang::expr(Surv(AVAL, is_event)),
  rhs = rlang::expr(!!group_sym)
)
fit_kmg01 <- survival::survfit(model_formula, anl)

test_that("gg_km() works with default inputs", {
  expect_no_error(
    surv_plot_data <- process_survfit(fit_kmg01)
  )

  expect_no_error(
    suppressWarnings(
      coxph_tbl <- get_cox_pairwise_df(
        model_formula,
        data = anl,
        arm = by
      )
    )
  )

  expect_no_error(
    plt_kmg01 <- gg_km(surv_plot_data) |>
      annotate_surv_med(fit_kmg01) |>
      annotate_coxph(coxph_tbl) |>
      annotate_riskdf(fit_kmg01)
  )

  # annotate_riskdf() is correctly aligned with gg_km
  expect_no_error(
    plt_aligned <- gg_km(surv_plot_data) |>
      annotate_riskdf(fit_kmg01)
  )
})

test_that("df2gg() works with proper x-axis and without", {
  # Example using proper x-axis
  df <- as.data.frame(matrix(c(
    #  0,  250, 500, 750, 1000  <-- (Reference)
    54,  28,  10,   3,    0,
    59,  35,  16,   5,    1,
    54,  25,   4,   0,    0
  ), nrow = 3, byrow = TRUE))

  # Set names manually
  colnames(df) <- c("0", "250", "500", "750", "1000")
  rownames(df) <- c("A", "B", "C")

  # Example with proper x-axis
  expect_no_error(
    null <- df2gg(df, font_size = 8, add_proper_xaxis = TRUE)
  )

  # Example without proper x-axis
  expect_no_error(
    null <- df2gg(df, font_size = 8, add_proper_xaxis = FALSE, hline = FALSE)
  )
})
