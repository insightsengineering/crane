skip_if_pkg_not_installed(c("survival", "dplyr"))

# Setup shared test data
set.seed(42)
surv_data_2arm <- survival::lung |>
  dplyr::mutate(
    arm = factor(sample(c("A", "B"), dplyr::n(), replace = TRUE)),
    status = status - 1
  ) |>
  dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))

surv_data_3arm <- survival::lung |>
  dplyr::mutate(
    arm = factor(sample(c("A", "B", "C"), dplyr::n(), replace = TRUE)),
    status = status - 1
  ) |>
  dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.)))

test_that("get_cox_pairwise_df() works with two arms", {
  expect_no_error(
    suppressWarnings(
      result <- get_cox_pairwise_df(
        model_formula = survival::Surv(time, status) ~ arm,
        data = surv_data_2arm,
        arm = "arm",
        ref_group = "A"
      )
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
          model_formula = survival::Surv(time, status) ~ arm,
          data = surv_data_3arm,
          arm = "arm",
          ref_group = "A"
        )
      )
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2L)
    expect_equal(rownames(result), c("B", "C"))
    expect_named(result, c("HR", "95% CI", "p-value (log-rank)"))
    # All hazard ratios and CIs must be non-NA (this was the bug)
    expect_false(anyNA(result[["HR"]]))
    expect_false(anyNA(result[["95% CI"]]))
    expect_false(anyNA(result[["p-value (log-rank)"]]))
  }
)

test_that("get_cox_pairwise_df() uses first factor level as default ref_group", {
  expect_no_error(
    suppressWarnings(
      result <- get_cox_pairwise_df(
        model_formula = survival::Surv(time, status) ~ arm,
        data = surv_data_3arm,
        arm = "arm"
      )
    )
  )
  expect_equal(nrow(result), 2L)
  # Default ref is "A", so comparisons are "B" and "C"
  expect_equal(rownames(result), c("B", "C"))
})

test_that("get_cox_pairwise_df() errors with non-formula model_formula", {
  expect_error(
    get_cox_pairwise_df(
      model_formula = "Surv(time, status) ~ arm",
      data = surv_data_2arm,
      arm = "arm"
    )
  )
})

test_that("get_cox_pairwise_df() errors when arm column is not a factor", {
  bad_data <- surv_data_2arm
  bad_data[["arm"]] <- as.character(bad_data[["arm"]])
  expect_error(
    get_cox_pairwise_df(
      model_formula = survival::Surv(time, status) ~ arm,
      data = bad_data,
      arm = "arm"
    )
  )
})

test_that(paste0(
  "get_cox_pairwise_df() errors when ref_group has multiple ",
  "elements (subset_arm length != 2)"
), {
  expect_error(
    get_cox_pairwise_df(
      model_formula = survival::Surv(time, status) ~ arm,
      data = surv_data_3arm,
      arm = "arm",
      # Passing multiple reference groups to force length > 2
      ref_group = c("A", "B")
    ),
    "must contain exactly 2 arms/groups"
  )
})
