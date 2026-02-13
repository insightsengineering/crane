skip_on_cran()

test_that("tbl_roche_subgroups() works", {
  expect_silent(
    tbl <- trial |>
      tbl_roche_subgroups(
        subgroups = c("grade", "stage"),
        rsp = "response",
        by = "trt",
        ~ glm(response ~ trt, data = .x) |>
          gtsummary::tbl_regression(
            show_single_row = trt,
            exponentiate = TRUE
          )
      )
  )

  tbl_only_labs_stats <- tbl$table_body |>
    dplyr::select(dplyr::starts_with("label_"), dplyr::starts_with("stat_"))

  expect_snapshot(tbl_only_labs_stats)
})
