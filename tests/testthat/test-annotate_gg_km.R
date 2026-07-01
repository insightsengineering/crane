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

test_that("annotate_riskdf() uses the plot's custom x-axis breaks (#278)", {
  my_breaks <- c(0, 100, 200, 300, 400, 500, 600, 700, 800)
  p_breaks <- p_base + scale_x_continuous(breaks = my_breaks)

  # the helper reads the resolved breaks back from the plot
  expect_identical(
    as.numeric(.get_plot_xticks(p_breaks)),
    as.numeric(my_breaks)
  )

  # the risk table is built at those same times (one column per break)
  annot_tbl <- summary(fit_strat, times = .get_plot_xticks(p_breaks), extend = TRUE)
  expect_setequal(unique(annot_tbl$time), my_breaks)

  expect_no_error(annotate_riskdf(p_breaks, fit_strat))
})

test_that(".get_plot_xticks() returns NULL on a discrete x-axis", {
  # a discrete x-axis returns character values, not numeric breaks
  p_disc <- ggplot(use_lung, aes(x = arm, y = status)) +
    geom_col()
  expect_null(.get_plot_xticks(p_disc))

  # numeric-looking factor levels ("1", "2", "3") must also be treated as
  # discrete, not mistaken for numeric breaks
  use_lung$grp <- factor(sample(c("1", "2", "3"), nrow(use_lung), replace = TRUE))
  p_num_lvls <- ggplot(use_lung, aes(x = grp, y = status)) +
    geom_col()
  expect_null(.get_plot_xticks(p_num_lvls))
})

test_that("annotate_riskdf() errors early on a discrete x-axis", {
  # a KM axis is always continuous time; a categorical axis has no coordinates
  # to align the risk table to, so it errors with an informative message
  p_disc <- ggplot(use_lung, aes(x = arm, y = status)) +
    geom_col()
  expect_snapshot(annotate_riskdf(p_disc, fit_strat), error = TRUE)
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
