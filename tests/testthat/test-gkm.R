skip_on_cran()

anl <- cards::ADTTE |>
  dplyr::mutate(is_event = CNSR == 0) %>%
  dplyr::mutate(TRTP = as.factor(TRTP))

variables <- list(tte = "AVAL", is_event = "is_event", arm = "TRTP")

test_that("test gkm() works", {
  fit_kmg01 <- survfit(ggsurvfit::Surv_CNSR(AVAL, CNSR) ~ TRTP, anl)
  variables <- list(tte = "AVAL", is_event = "is_event", arm = "TRTP")

  expect_no_error(surv_plot_data <- h_data_plot(fit_kmg01))

  expect_no_error(
    suppressWarnings(
      coxph_tbl <- h_tbl_coxph_pairwise(
        df = anl,
        variables = variables
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
