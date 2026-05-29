skip_if_pkg_not_installed(c("survival", "dplyr", "coin"))
suppressPackageStartupMessages(library(survival))

# Setup shared test data
set.seed(42)
test_df_2grp <- survival::veteran |>
  dplyr::mutate(
    treatment = factor(sample(c("DrugX", "Placebo"), dplyr::n(), replace = TRUE)),
    event = status,
    age_group = cut(age, breaks = c(-Inf, 55, 56, Inf))
  ) |>
  dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))

test_df_3grp <- survival::veteran |>
  dplyr::mutate(
    treatment = factor(sample(c("DrugX", "DrugY", "Placebo"), dplyr::n(), replace = TRUE)),
    event = status
  ) |>
  dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))

test_that("get_cox_pairwise_df() works with two arms", {
  expect_no_error(
    result <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_2grp,
      arm = "treatment",
      ref_group = "Placebo"
    )
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  expect_named(result, c("HR", "95% CI", "p-value (log-rank)"))
  expect_false(anyNA(result[["HR"]]))
  expect_false(anyNA(result[["95% CI"]]))
})

test_that("get_cox_pairwise_df() returns non-NA hazard ratios with more than two arms", {
  expect_no_error(
    result <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_3grp,
      arm = "treatment",
      ref_group = "Placebo"
    )
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_true(all(c("DrugX", "DrugY") %in% rownames(result)))
  expect_named(result, c("HR", "95% CI", "p-value (log-rank)"))
  expect_false(anyNA(result[["HR"]]))
  expect_false(anyNA(result[["95% CI"]]))
  expect_false(anyNA(result[["p-value (log-rank)"]]))
})

test_that("get_cox_pairwise_df() uses first factor level as default ref_group", {
  expect_no_error(
    result <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_3grp,
      arm = "treatment"
    )
  )
  expect_equal(nrow(result), 2L)
  first_lvl <- levels(test_df_3grp$treatment)[1]
  expect_false(first_lvl %in% rownames(result))
})

test_that("get_cox_pairwise_df() errors with non-formula model_formula", {
  expect_error(
    get_cox_pairwise_df(
      model_formula = "Surv(time, event) ~ treatment",
      data = test_df_2grp,
      arm = "treatment"
    )
  )
})

test_that("get_cox_pairwise_df() errors when arm column is not a factor", {
  bad_data <- test_df_2grp
  bad_data[["treatment"]] <- as.character(bad_data[["treatment"]])
  expect_error(
    get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = bad_data,
      arm = "treatment"
    )
  )
})

test_that("get_cox_pairwise_df() errors when ref_group has multiple elements", {
  expect_error(
    get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_3grp,
      arm = "treatment",
      ref_group = c("Placebo", "DrugX")
    ),
    "must contain exactly 2 arms"
  )
})

test_that("get_cox_pairwise_df() works with all valid 'ties' methods", {
  ties_methods <- c("exact", "efron", "breslow")
  for (t_method in ties_methods) {
    expect_no_error(
      res <- get_cox_pairwise_df(
        model_formula = Surv(time, event) ~ treatment,
        data = test_df_2grp,
        arm = "treatment",
        ties = t_method
      )
    )
    expect_s3_class(res, "data.frame")
  }
})

test_that("get_cox_pairwise_df() works with all valid 'test' methods", {
  test_methods <- c(
    "log-rank",
    "gehan-breslow",
    "tarone",
    "peto",
    "prentice",
    "fleming-harrington",
    "likelihood-ratio"
  )

  for (t_method in test_methods) {
    expect_no_error(
      res <- get_cox_pairwise_df(
        model_formula = Surv(time, event) ~ treatment,
        data = test_df_2grp,
        arm = "treatment",
        test = t_method
      )
    )

    if (t_method == "log-rank") {
      expected_colname <- "p-value (log-rank)"
    } else {
      expected_colname <- paste0("p-value (", tools::toTitleCase(t_method), ")")
    }

    expect_true(expected_colname %in% names(res))
    expect_false(anyNA(res[[expected_colname]]))
  }
})

test_that("get_cox_pairwise_df() catches invalid 'ties' and 'test' arguments", {
  expect_error(
    get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_2grp,
      arm = "treatment",
      ties = "invalid_tie_method"
    ),
    "should be one of"
  )

  expect_error(
    get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_2grp,
      arm = "treatment",
      test = "invalid_test_method"
    ),
    "should be one of"
  )
})

test_that("get_cox_pairwise_df() works for formula with covariates via likelihood-ratio", {
  expect_no_error(
    res_covariate <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment + karno,
      data = test_df_2grp,
      arm = "treatment",
      test = "likelihood-ratio"
    )
  )
  expect_s3_class(res_covariate, "data.frame")
  expect_false(anyNA(res_covariate[["HR"]]))
  expect_false(anyNA(res_covariate[["p-value (Likelihood-Ratio)"]]))
})

test_that("get_cox_pairwise_df() works for formula with complex strata()", {
  # 1. Test log-rank with a single strata variable (uses coin engine)
  expect_no_error(
    res_strata_lr <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment + strata(prior),
      data = test_df_2grp,
      arm = "treatment",
      ties = "efron",
      test = "log-rank"
    )
  )
  expect_s3_class(res_strata_lr, "data.frame")
  expect_false(anyNA(res_strata_lr[["p-value (log-rank)"]]))

  # 2. Test log-rank with multiple strata variables inside a single strata() call
  expect_no_error(
    res_strata_multi <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment + strata(prior, trt),
      data = test_df_2grp,
      arm = "treatment",
      ties = "efron",
      test = "log-rank"
    )
  )
  expect_s3_class(res_strata_multi, "data.frame")
  expect_false(anyNA(res_strata_multi[["p-value (log-rank)"]]))

  # 3. Ensure the likelihood-ratio test properly handles complex strata natively
  expect_warning(
    res_strata_cox <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment + strata(prior) + karno,
      data = test_df_2grp,
      arm = "treatment",
      test = "likelihood-ratio"
    ),
    "Stratified formula detected"
  )
  expect_s3_class(res_strata_cox, "data.frame")
  expect_false(anyNA(res_strata_cox[["p-value (Likelihood-Ratio)"]]))
})

test_that("get_cox_pairwise_df() respects conf.int passed via ...", {
  res_95 <- get_cox_pairwise_df(
    model_formula = Surv(time, event) ~ treatment,
    data = test_df_2grp,
    arm = "treatment"
  )

  expect_no_error(
    res_99 <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_2grp,
      arm = "treatment",
      conf.int = 0.99
    )
  )

  expect_true("99% CI" %in% names(res_99))
  expect_false("95% CI" %in% names(res_99))
  expect_false(res_95[["95% CI"]] == res_99[["99% CI"]])
})

test_that("get_cox_pairwise_df() handles robust = TRUE correctly via ...", {
  expect_no_error(
    res_robust <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_2grp,
      arm = "treatment",
      test = "likelihood-ratio",
      ties = "efron",
      robust = TRUE
    )
  )

  expect_s3_class(res_robust, "data.frame")
  expect_false(anyNA(res_robust[["HR"]]))
  expect_false(anyNA(res_robust[["p-value (Likelihood-Ratio)"]]))
})

test_that("get_cox_pairwise_df() dispatches to the correct LRT engine based on strata", {
  comp_df <- test_df_2grp
  comp_df$treatment <- factor(comp_df$treatment, levels = c("Placebo", "DrugX"))

  res_no_strata <- suppressWarnings(
    get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_2grp,
      arm = "treatment",
      ref_group = "Placebo",
      test = "likelihood-ratio"
    )
  )

  fit_full_reg <- survival::survreg(Surv(time, event) ~ treatment, data = comp_df, dist = "exponential")
  fit_null_reg <- survival::survreg(Surv(time, event) ~ 1, data = comp_df, dist = "exponential")

  loglik_full <- fit_full_reg$loglik[length(fit_full_reg$loglik)]
  loglik_null <- fit_null_reg$loglik[length(fit_null_reg$loglik)]
  stat <- 2 * (loglik_full - loglik_null)
  df <- length(fit_full_reg$coefficients) - length(fit_null_reg$coefficients)
  expected_pval_reg <- stats::pchisq(stat, df, lower.tail = FALSE)

  expect_equal(res_no_strata[["p-value (Likelihood-Ratio)"]], expected_pval_reg)

  expect_warning(
    res_strata <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment + strata(celltype),
      data = test_df_2grp,
      arm = "treatment",
      ref_group = "Placebo",
      test = "likelihood-ratio",
      ties = "efron"
    ),
    "Stratified formula detected"
  )

  fit_full_cox <- survival::coxph(Surv(time, event) ~ treatment + strata(celltype), data = comp_df, ties = "efron")
  fit_null_cox <- survival::coxph(Surv(time, event) ~ strata(celltype), data = comp_df, ties = "efron")
  anova_res <- stats::anova(fit_null_cox, fit_full_cox, test = "Chisq")
  expected_pval_cox <- anova_res[2, ncol(anova_res)]

  expect_equal(res_strata[["p-value (Likelihood-Ratio)"]], expected_pval_cox)
})

# --- NEW TESTS TO IMPROVE COVERAGE ---

test_that("get_cox_pairwise_df() catches invalid dots arguments", {
  expect_error(
    get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_2grp,
      arm = "treatment",
      invalid_arg_1 = "test",
      invalid_arg_2 = 42
    ),
    "Invalid argument\\(s\\) passed via"
  )
})

test_that("get_cox_pairwise_df() errors on robust = TRUE with exact ties", {
  expect_error(
    get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_2grp,
      arm = "treatment",
      ties = "exact",
      robust = TRUE
    ),
    "Robust standard errors cannot be calculated with the .*exact.* tie-handling method"
  )
})

test_that("get_cox_pairwise_df() handles weighted log-rank tests with strata", {
  expect_no_error(
    res <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment + strata(celltype),
      data = test_df_2grp,
      arm = "treatment",
      test = "gehan-breslow"
    )
  )
  expect_s3_class(res, "data.frame")
  expect_false(anyNA(res[["p-value (Gehan-Breslow)"]]))
})

test_that("get_cox_pairwise_df() aborts weighted log-rank with covariate adjustment", {
  expect_error(
    get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment + karno + strata(celltype),
      data = test_df_2grp,
      arm = "treatment",
      test = "gehan-breslow"
    ),
    "This log-rank test method does not support covariate adjustment"
  )
})

test_that("get_cox_pairwise_df() handles weighted log-rank tests with multiple strata", {
  # Create safe strata variables where intersections are guaranteed to have >= 2 obs
  safe_df <- test_df_2grp |>
    dplyr::mutate(
      strata_1 = factor(sample(c("A", "B"), dplyr::n(), replace = TRUE)),
      strata_2 = factor(sample(c("X", "Y"), dplyr::n(), replace = TRUE))
    )

  expect_no_error(
    res_strata_multi_coin <- get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment + strata(strata_1, strata_2),
      data = safe_df,
      arm = "treatment",
      test = "gehan-breslow"
    )
  )
  expect_s3_class(res_strata_multi_coin, "data.frame")
})

test_that("get_cox_pairwise_df() aborts when coin package is missing", {
  # Intercept rlang::check_installed directly inside the rlang namespace
  testthat::with_mocked_bindings(
    check_installed = function(pkg, reason = NULL, ...) {
      if (identical(pkg, "coin")) {
        cli::cli_abort(paste("The {.pkg coin} package is required", reason))
      }
    },
    .package = "rlang",
    code = {
      expect_error(
        get_cox_pairwise_df(
          model_formula = Surv(time, event) ~ treatment,
          data = test_df_2grp,
          arm = "treatment",
          test = "gehan-breslow"
        ),
        "The .*coin.* package is required"
      )
    }
  )
})
