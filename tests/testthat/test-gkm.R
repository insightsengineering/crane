skip_on_cran()

anl <- cards::ADTTE |>
  dplyr::mutate(is_event = CNSR == 0)
by <- "TRTP"

test_that("test gkm() works", {
  GROUP_SYM <- rlang::ensym(by)
  model_formula <- rlang::new_formula(
    lhs = rlang::expr(Surv(AVAL, is_event)),
    rhs = rlang::expr(!!GROUP_SYM)
  )

  fit_kmg01 <- survival::survfit(model_formula, anl)


  expect_no_error(surv_plot_data <- h_data_plot(fit_kmg01))

  expect_no_error(
    suppressWarnings(
      coxph_tbl <- get_cox_pairwise_tbl(
        model_formula,
        data = anl,
        arm = by
      )
    )
  )

  expect_no_error(
    plt_kmg01 <- g_km(surv_plot_data,
      xlab = "Time (Days)",
      ylim = c(0.9, 1)
    ) %>%
      annot_surv_med(fit_kmg01) %>%
      annot_cox_ph(coxph_tbl) %>%
      annot_at_risk(fit_kmg01)
  )
})
