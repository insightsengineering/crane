skip_on_cran()

anl <- cards::ADTTE |>
  dplyr::mutate(is_event = CNSR == 0)
by <- "TRTP"
anl[[by]] <- factor(anl[[by]], levels = c(
  "Placebo",
  "Xanomeline Low Dose",
  "Xanomeline High Dose"
))

test_that("test gkm() works", {
  GROUP_SYM <- rlang::sym(by)
  model_formula <- rlang::new_formula(
    lhs = rlang::expr(Surv(AVAL, is_event)),
    rhs = rlang::expr(!!GROUP_SYM)
  )

  fit_kmg01 <- survival::survfit(model_formula, anl)


  expect_no_error(surv_plot_data <- process_survfit(fit_kmg01))

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
    plt_kmg01 <- gg_km(surv_plot_data,
      xlab = "Time (Days)",
      ylim = c(0.9, 1)
    ) %>%
      annot_surv_med(fit_kmg01) %>%
      annot_cox_ph(coxph_tbl) %>%
      annot_at_risk(fit_kmg01)
  )
})
