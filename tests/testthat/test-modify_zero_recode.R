test_that("modify_zero_recode() works", {
  withr::local_options(list(width = 190))
  expect_snapshot(
    gtsummary::trial |>
      dplyr::mutate(
        trt = factor(trt, levels = c("Drug A", "Drug B", "Drug C")),
        grade = factor(grade, levels = c("I", "II", "III", "IV"))
      ) |>
      tbl_summary(
        by = trt,
        include = c(response, grade),
        statistic = response ~ "{n} / {N} ({p}%)",
        missing = "no"
      ) |>
      modify_zero_recode() |>
      as.data.frame()
  )
})
