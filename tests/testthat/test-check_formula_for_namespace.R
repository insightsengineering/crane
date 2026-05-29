# test-check_formula_for_namespace.R

test_that(".check_formula_for_namespace() passes valid formulas", {
  # Simple formula
  expect_no_error(.check_formula_for_namespace(y ~ x))

  # Survival formula without namespace
  expect_no_error(.check_formula_for_namespace(Surv(time, status) ~ arm))

  # Complex formula with strata
  expect_no_error(.check_formula_for_namespace(Surv(time, status) ~ arm + strata(site)))
})

test_that(".check_formula_for_namespace() aborts when a namespace is present", {
  expected_msg <- "must be specified without namespace"

  # Namespace on a function call (e.g., survival::Surv)
  expect_error(
    .check_formula_for_namespace(survival::Surv(time, status) ~ arm),
    regexp = expected_msg
  )

  # Namespace on the Right-Hand Side variable
  expect_error(
    .check_formula_for_namespace(Surv(time, status) ~ mypkg::arm),
    regexp = expected_msg
  )

  # Namespace on the Left-Hand Side variable
  expect_error(
    .check_formula_for_namespace(mypkg::y ~ x),
    regexp = expected_msg
  )

  # Namespace buried inside a strata() call
  expect_error(
    .check_formula_for_namespace(Surv(time, status) ~ arm + strata(mypkg::site)),
    regexp = expected_msg
  )
})

test_that(".check_formula_for_namespace() correctly parses long formulas", {
  # deparse() splits long formulas into a character vector of multiple lines.
  # This tests that `paste(..., collapse = " ")` effectively reconstructs it
  # so the regex doesn't miss namespaces split across lines.

  # Generate a very long valid formula
  long_rhs <- paste(paste0("covar_", 1:50), collapse = " + ")
  long_formula_valid <- stats::as.formula(paste("Surv(time, status) ~", long_rhs))

  expect_no_error(.check_formula_for_namespace(long_formula_valid))

  # Generate a very long invalid formula with a namespace at the very end
  long_formula_invalid <- stats::as.formula(paste("Surv(time, status) ~", long_rhs, "+ pkg::bad_var"))

  expect_error(
    .check_formula_for_namespace(long_formula_invalid),
    regexp = "must be specified without namespace"
  )
})
