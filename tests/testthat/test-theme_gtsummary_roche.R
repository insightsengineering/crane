test_that("theme_gtsummary_roche() works", {
  # check structure of theme
  expect_snapshot(
    gtsummary::check_gtsummary_theme(theme_gtsummary_roche(set_theme = FALSE))
  )
})

test_that("theme_gtsummary_roche() adds relevant {flextable} directives", {
  # check user_added2+ are present
  cmds <-
    gtsummary::with_gtsummary_theme(
      theme_gtsummary_roche(),
      as_flex_table(gtsummary::tbl_summary(data.frame(1)), return_calls = TRUE)
    )

  added_cmds_roche_specific <- cmds[grepl("user_added[0-9]", names(cmds))]
  expect_snapshot(
    added_cmds_roche_specific
  )
})

test_that("theme_gtsummary_roche() no errors with a gt print", {
  expect_silent(
    gtsummary::with_gtsummary_theme(
      theme_gtsummary_roche(),
      tbl_summary(data.frame(1)) |> as_gt()
    )
  )
})
