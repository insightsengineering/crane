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
  lhs = rlang::expr(survival::Surv(AVAL, is_event)),
  rhs = rlang::expr(!!group_sym)
)
fit_kmg01 <- survival::survfit(model_formula, anl)

test_that("gg_km() works and handles annotations correctly", {
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

  # 1. Test floating annotations (These can be safely piped together)
  expect_no_error(
    plt_floats <- gg_km(surv_plot_data) |>
      annotate_surv_med(fit_kmg01) |>
      annotate_coxph(coxph_tbl)
  )

  # 2. Test aligned annotations (Must be applied directly to the pure ggplot)
  expect_no_error(
    suppressWarnings(
      plt_aligned <- gg_km(surv_plot_data) |>
        annotate_riskdf(fit_kmg01)
    )
  )
})

test_that("df2gg engines (aligned and floating) work correctly", {
  # Example dataframe with proper timepoint columns
  df <- as.data.frame(matrix(c(
    54,  28,  10,   3,    0,
    59,  35,  16,   5,    1,
    54,  25,   4,   0,    0
  ), nrow = 3, byrow = TRUE))

  colnames(df) <- c("0", "250", "500", "750", "1000")
  rownames(df) <- c("A", "B", "C")

  # Dummy plot to align and float against
  p_dummy <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(
      limits = c(0, 1000),
      breaks = c(0, 250, 500, 750, 1000)
    )

  # 1. Test df2gg_aligned (The aligned engine)
  expect_no_error(
    res_aligned <- df2gg_aligned(
      df = df,
      gg_plt = p_dummy,
      type = "KM",
      show_xaxis = TRUE
    )
  )

  # 2. Test df2gg_floating (The floating engine)
  expect_no_error(
    res_floating <- df2gg_floating(
      df = df,
      gg_plt = p_dummy,
      font_size = 8,
      col_labels = TRUE,
      hline = FALSE
    )
  )
})
