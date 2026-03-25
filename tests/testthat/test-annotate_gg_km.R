library(ggplot2)
library(survival)

# ------------------------------------------------------------------------------
# 1. SETUP MOCK DATA
# ------------------------------------------------------------------------------
set.seed(123)
use_lung <- survival::lung
use_lung$arm <- factor(sample(c("A", "B"), nrow(use_lung), replace = TRUE))
use_lung$status <- use_lung$status - 1
use_lung <- na.omit(use_lung)

# Stratified fit (Multiple groups)
fit_strat <- survival::survfit(Surv(time, status) ~ arm, data = use_lung)
# Unstratified fit (Single group)
fit_single <- survival::survfit(Surv(time, status) ~ 1, data = use_lung)

# Plots
p_base <- ggplot(use_lung, aes(x = time, y = status, color = arm)) +
  geom_step() +
  theme_classic()
p_cow <- cowplot::ggdraw(p_base) # Used to trigger cowplot errors

# Cox Data
cox_df <- data.frame(Term = "B vs A", HR = "1.2", p = "0.05")

# ------------------------------------------------------------------------------
# 2. TESTS FOR annotate_riskdf()
# ------------------------------------------------------------------------------
test_that("annotate_riskdf() handles all branches and inputs", {
  # Happy path: Stratified
  expect_no_error(res <- annotate_riskdf(p_base, fit_strat))
  expect_s3_class(res, "ggplot")

  # Happy path: Unstratified
  expect_no_error(annotate_riskdf(p_base, fit_single))

  # Error: Reject Cowplot
  expect_error(
    annotate_riskdf(p_cow, fit_strat),
    "must be a pure ggplot object"
  )

  # Error: Reject non-survfit
  expect_error(
    annotate_riskdf(p_base, list()),
    "must be a survfit object"
  )

  # Error: Invalid rel_height
  expect_error(
    annotate_riskdf(p_base, fit_strat, rel_height_plot = 2),
    "between 0 and 1"
  )
})

# ------------------------------------------------------------------------------
# 3. TESTS FOR annotate_surv_med()
# ------------------------------------------------------------------------------
test_that("annotate_surv_med() handles all branches and inputs", {
  # Success: Stratified + Custom Position
  expect_no_error(
    annotate_surv_med(
      p_base,
      fit_strat,
      table_position = c(x = 0.5, y = 0.5, w = 0.1, h = 0.1)
    )
  )

  # Success: Cowplot input
  expect_no_error(annotate_surv_med(p_cow, fit_strat))

  # Success: Unstratified
  expect_no_error(annotate_surv_med(p_base, fit_single))

  # Error: Invalid plot
  expect_error(
    annotate_surv_med(list(), fit_strat),
    "must be a ggplot or cowplot object"
  )

  # Error: Invalid survfit
  expect_error(
    annotate_surv_med(p_base, list()),
    "must be a survfit object"
  )

  # Fill Logic: Custom color and FALSE
  expect_no_error(annotate_surv_med(p_base, fit_strat, fill = "red"))
  expect_no_error(annotate_surv_med(p_base, fit_strat, fill = FALSE))
})

# ------------------------------------------------------------------------------
# 4. TESTS FOR annotate_coxph()
# ------------------------------------------------------------------------------
test_that("annotate_coxph() handles all branches and inputs", {
  # Success: Standard use
  expect_no_error(annotate_coxph(p_base, cox_df))

  # Error: Invalid plot
  expect_error(
    annotate_coxph(list(), cox_df),
    "must be a ggplot or cowplot object"
  )

  # Error: Invalid dataframe (
  expect_error(annotate_coxph(p_base, list()), "must be a data.frame")

  # Fill Logic: Custom color
  expect_no_error(annotate_coxph(p_base, cox_df, fill = "blue"))
})
