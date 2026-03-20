# Setup shared mock data and objects for the test suite
library(survival)
library(ggplot2)

# 1. Mock Survival Data & Fit
use_lung <- survival::lung
use_lung$arm <- factor(
  sample(c("A", "B", "C"), nrow(use_lung), replace = TRUE)
)
use_lung$status <- use_lung$status - 1
use_lung <- na.omit(use_lung)

# FIX: Filter out time points > 1000 to prevent ggplot scaling warnings
use_lung <- use_lung[use_lung$time <= 1000, ]

fit_mock <- survival::survfit(Surv(time, status) ~ arm, data = use_lung)

# 2. Mock ggplot2 base plot
p_base <- ggplot2::ggplot(use_lung, aes(x = time, y = status, color = arm)) +
  ggplot2::geom_step() +
  ggplot2::scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 250))

# 3. Mock cowplot object
p_cow <- cowplot::ggdraw(p_base)

# 4. Mock Cox-PH summary table
cox_mock <- data.frame(
  `HR (95% CI)` = c("0.85 (0.6-1.1)", "1.20 (0.9-1.5)"),
  `p-value` = c("0.150", "0.045"),
  check.names = FALSE
)
rownames(cox_mock) <- c("B vs A", "C vs A")

# --- Tests for annotate_riskdf ---

test_that("annotate_riskdf generates a cowplot object", {
  res <- annotate_riskdf(gg_plt = p_base, fit_km = fit_mock)

  # 1. Verify it is a ggplot object on the outside
  expect_s3_class(res, "ggplot")

  # 2. Look inside the plot layers to prove it was built by cowplot
  has_cowplot_layer <- any(vapply(res$layers, function(layer) {
    inherits(layer$geom, "GeomDrawGrob")
  }, logical(1)))

  # 3. Expect that the cowplot-specific drawing layer exists
  expect_true(has_cowplot_layer)
})

test_that("annotate_riskdf throws errors for invalid inputs", {
  # Fails if passed a cowplot object
  expect_error(
    annotate_riskdf(gg_plt = p_cow, fit_km = fit_mock),
    regexp = "must be a pure ggplot object"
  )

  # Fails if passed a non-survfit object
  expect_error(
    annotate_riskdf(gg_plt = p_base, fit_km = data.frame()),
    regexp = "must be a survfit object"
  )

  # Fails for out-of-bounds relative height
  expect_error(
    annotate_riskdf(gg_plt = p_base, fit_km = fit_mock, rel_height_plot = 1.5),
    regexp = "must be between 0 and 1"
  )
})


# --- Tests for annotate_surv_med ---

test_that("annotate_surv_med generates a cowplot/ggplot object", {
  res <- annotate_surv_med(gg_plt = p_base, fit_km = fit_mock)

  # cowplot::ggdraw returns an object with classes "gg", "ggplot"
  expect_s3_class(res, "ggplot")
})

test_that("annotate_surv_med accepts a cowplot object as input", {
  res <- annotate_surv_med(gg_plt = p_cow, fit_km = fit_mock)
  expect_s3_class(res, "ggplot")
})

test_that("annotate_surv_med respects custom coordinates and sizes", {
  res <- annotate_surv_med(
    gg_plt = p_base,
    fit_km = fit_mock,
    x = 0.5,
    y = 0.5,
    h = 0.3,
    font_size = 12
  )
  expect_s3_class(res, "ggplot")
})

test_that("annotate_surv_med throws errors for invalid inputs", {
  expect_error(
    annotate_surv_med(gg_plt = p_base, fit_km = "not_a_fit"),
    regexp = "must be a survfit object"
  )
})


# --- Tests for annotate_coxph ---

test_that("annotate_coxph generates a cowplot/ggplot object", {
  res <- annotate_coxph(gg_plt = p_base, coxph_tbl = cox_mock)
  expect_s3_class(res, "ggplot")
})

test_that("annotate_coxph accepts a cowplot object as input", {
  res <- annotate_coxph(gg_plt = p_cow, coxph_tbl = cox_mock)
  expect_s3_class(res, "ggplot")
})

test_that("annotate_coxph respects custom arguments without failing", {
  res <- annotate_coxph(
    gg_plt = p_base,
    coxph_tbl = cox_mock,
    fill = FALSE,
    w = 0.5
  )
  expect_s3_class(res, "ggplot")
})

test_that("annotate_coxph throws errors for invalid inputs", {
  expect_error(
    annotate_coxph(gg_plt = "not_a_plot", coxph_tbl = cox_mock),
    regexp = "must be a ggplot or cowplot object"
  )

  expect_error(
    annotate_coxph(gg_plt = p_base, coxph_tbl = list(a = 1)),
    regexp = "must be a data.frame"
  )
})

test_that("annotate_riskdf handles unstratified (null) models correctly", {
  # 1. Create a model with NO strata (~ 1)
  fit_null <- survival::survfit(survival::Surv(time, status) ~ 1, data = use_lung)

  # 2. Dummy plot for the alignment engine
  p_null <- ggplot2::ggplot() +
    ggplot2::scale_x_continuous(limits = c(0, 1000), breaks = c(0, 500, 1000))

  # 3. Test that the unstratified block builds the table without error
  expect_no_error(
    res_null <- annotate_riskdf(gg_plt = p_null, fit_km = fit_null)
  )

  expect_s3_class(res_null, "ggplot")
})

test_that("annotate_surv_med throws an error for invalid plot inputs", {
  # Test that passing a string instead of a plot triggers the abort
  expect_error(
    annotate_surv_med(gg_plt = "this_is_not_a_plot", fit_km = fit_mock),
    regexp = "must be a ggplot or cowplot object"
  )
})
