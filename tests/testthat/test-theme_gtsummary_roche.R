test_that("theme_gtsummary_roche() works", {
  # check structure of theme
  expect_snapshot(
    gtsummary::check_gtsummary_theme(theme_gtsummary_roche(set_theme = FALSE))
  )
})
