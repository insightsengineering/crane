skip_if_pkg_not_installed("survival")
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

# Extract pre-calculated data frame for annotate_surv_med
surv_df <- get_surv_times_df(fit_kmg01, times = c(100, 200))

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
      annotate_surv_med(surv_tbl = surv_df) |>
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

test_that("plotlist attribute is preserved through annotate_* stacking", {
  surv_plot_data <- process_survfit(fit_kmg01)
  skip_if_not_installed("coin")
  suppressWarnings(
    coxph_tbl <- get_cox_pairwise_df(model_formula, data = anl, arm = by)
  )

  base_plt <- gg_km(surv_plot_data)

  # Single annotation: annotate_riskdf -> df2gg_aligned
  suppressWarnings(
    plt_risk <- base_plt |> annotate_riskdf(fit_kmg01)
  )
  plist_risk <- attr(plt_risk, "plotlist")
  expect_true(!is.null(plist_risk))
  expect_named(plist_risk, c("main", "table"))
  expect_s3_class(plist_risk$main, "ggplot")
  expect_s3_class(plist_risk$table, "ggplot")

  # Single floating annotation: annotate_surv_med -> df2gg_floating
  plt_med <- base_plt |> annotate_surv_med(surv_tbl = surv_df)
  plist_med <- attr(plt_med, "plotlist")
  expect_true(!is.null(plist_med))
  expect_named(plist_med, c("main", "table"))
  expect_identical(plist_med$main$data, base_plt$data)

  # Stacked floating annotations: surv_med |> coxph
  plt_stacked <- base_plt |>
    annotate_surv_med(surv_tbl = surv_df) |>
    annotate_coxph(coxph_tbl)

  plist_top <- attr(plt_stacked, "plotlist")
  expect_true(!is.null(plist_top))
  expect_named(plist_top, c("main", "table"))

  # The nested main carries the previous plotlist
  plist_nested <- attr(plist_top$main, "plotlist")
  expect_true(!is.null(plist_nested))
  expect_named(plist_nested, c("main", "table"))
  expect_identical(plist_nested$main$data, base_plt$data)
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
