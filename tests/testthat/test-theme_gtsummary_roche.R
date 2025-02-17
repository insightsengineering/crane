test_that("theme_gtsummary_roche() works", {
  # check structure of theme
  expect_snapshot(
    check_gtsummary_theme(theme_gtsummary_roche(set_theme = FALSE))
  )
})
