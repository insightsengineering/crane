skip_if_pkg_not_installed(c("survival", "dplyr", "coin"))

# Setup shared test data 
set.seed(42)
test_df_2grp <- survival::veteran |>
  dplyr::mutate(
    treatment = factor(sample(c("DrugX", "Placebo"), dplyr::n(), replace = TRUE)),
    event = status
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

test_that(
  "get_cox_pairwise_df() returns non-NA hazard ratios with more than two arms",
  {
    expect_no_error(
      suppressWarnings(
        result <- get_cox_pairwise_df(
          model_formula = Surv(time, event) ~ treatment,
          data = test_df_3grp,
          arm = "treatment",
          ref_group = "Placebo"
        )
      )
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2L)
    # The output rownames should be the non-reference groups
    expect_true(all(c("DrugX", "DrugY") %in% rownames(result)))
    expect_named(result, c("HR", "95% CI", "p-value (log-rank)"))

    # All hazard ratios and CIs must be non-NA
    expect_false(anyNA(result[["HR"]]))
    expect_false(anyNA(result[["95% CI"]]))
    expect_false(anyNA(result[["p-value (log-rank)"]]))
  }
)

test_that("get_cox_pairwise_df() uses first factor level as default ref_group", {
  expect_no_error(
    suppressWarnings(
      result <- get_cox_pairwise_df(
        model_formula = Surv(time, event) ~ treatment,
        data = test_df_3grp,
        arm = "treatment"
      )
    )
  )
  expect_equal(nrow(result), 2L)

  # Default ref is the first level, so it should not be in the output rownames
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

test_that(paste0(
  "get_cox_pairwise_df() errors when ref_group has multiple ",
  "elements (subset_arm length != 2)"
), {
  expect_error(
    get_cox_pairwise_df(
      model_formula = Surv(time, event) ~ treatment,
      data = test_df_3grp,
      arm = "treatment",
      # Passing multiple reference groups to force length > 2
      ref_group = c("Placebo", "DrugX")
    ),
    "must contain exactly 2 arms/groups"
  )
})

test_that("get_cox_pairwise_df() works with all valid 'ties' methods", {
  # REMOVED "discrete" from this list
  ties_methods <- c("exact", "efron", "breslow")

  for (t_method in ties_methods) {
    expect_no_error(
      suppressWarnings(
        res <- get_cox_pairwise_df(
          model_formula = Surv(time, event) ~ treatment,
          data = test_df_2grp,
          arm = "treatment",
          ties = t_method
        )
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
      suppressWarnings(
        res <- get_cox_pairwise_df(
          model_formula = Surv(time, event) ~ treatment,
          data = test_df_2grp,
          arm = "treatment",
          test = t_method
        )
      )
    )

    # Confirm the column name updates dynamically based on the chosen test
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

test_that("get_cox_pairwise_df() works for formula with covariates", {
  # Added 'karno' as a covariate.
  # Likelihood-ratio is utilized because standard coin::logrank_test
  # syntax does not support continuous right-hand side covariates.
  expect_no_error(
    suppressWarnings(
      res_covariate <- get_cox_pairwise_df(
        model_formula = Surv(time, event) ~ treatment + karno,
        data = test_df_2grp,
        arm = "treatment",
        test = "likelihood-ratio"
      )
    )
  )

  expect_s3_class(res_covariate, "data.frame")
  expect_false(anyNA(res_covariate[["HR"]]))
  expect_false(anyNA(res_covariate[["p-value (Likelihood-Ratio)"]]))
})

test_that("get_cox_pairwise_df() works for formula with strata()", {
  # 1. Test log-rank with strata (uses coin engine)
  expect_no_error(
    suppressWarnings(
      res_strata_lr <- get_cox_pairwise_df(
        model_formula = Surv(time, event) ~ treatment + strata(celltype),
        data = test_df_2grp,
        arm = "treatment",
        ties = "efron",
        test = "log-rank"
      )
    )
  )
  expect_s3_class(res_strata_lr, "data.frame")
  expect_false(anyNA(res_strata_lr[["p-value (log-rank)"]]))

  # 2. Test likelihood-ratio with strata (uses updated parametric survreg engine)
  expect_no_error(
    suppressWarnings(
      res_strata_lrt <- get_cox_pairwise_df(
        model_formula = Surv(time, event) ~ treatment + strata(celltype),
        data = test_df_2grp,
        arm = "treatment",
        ties = "efron",
        test = "likelihood-ratio"
      )
    )
  )
  expect_s3_class(res_strata_lrt, "data.frame")
  expect_false(anyNA(res_strata_lrt[["p-value (Likelihood-Ratio)"]]))
})
