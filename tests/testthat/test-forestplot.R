skip_on_cran()
skip_if_pkg_not_installed("broom.helpers")
skip_if_pkg_not_installed("ggtext")
skip_if_pkg_not_installed("magick")
skip_if_pkg_not_installed("patchwork")
skip_if_pkg_not_installed("webshot2")

test_that("test g_forest() works", {
  expect_no_error(tbl <-
    trial %>%
    tbl_roche_subgroups(
      subgroups = c("grade", "stage"),
      rsp = "response",
      by = "trt",
      ~ glm(response ~ trt, data = .x) |>
        gtsummary::tbl_regression(
          show_single_row = trt,
          exponentiate = TRUE
        )
    ))
  expect_no_error(g_forest(tbl, levels(factor(trial$trt))))
})
