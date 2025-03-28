test_that("theme_gtsummary_roche() works", {
  # check structure of theme
  expect_snapshot(
    gtsummary::check_gtsummary_theme(theme_gtsummary_roche(set_theme = FALSE))
  )
})

test_that("theme_gtsummary_roche() adds relevant {flextable} directives", {
  # check user_added2+ are present
  cmds <- as_flex_table(tbl_summary(data.frame(1)), return_calls = TRUE)
  added_cmds_roche_specific <- cmds[grepl("user_added[2-9]", names(cmds))]
  expect_snapshot(
    added_cmds_roche_specific
  )
})
