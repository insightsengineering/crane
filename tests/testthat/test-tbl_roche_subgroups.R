skip_on_cran()

test_that("tbl_roche_subgroups(time_to_event=NULL) works", {
  expect_error(
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
      ),
    NA
  )

  expect_snapshot(
    tbl$table_body |>
      dplyr::select(
        dplyr::starts_with("label"),
        dplyr::starts_with("stat"),
        dplyr::starts_with("estimate"),
        dplyr::starts_with("ci"),
        dplyr::starts_with("p.value")
      )
  )
})

test_that("tbl_roche_subgroups(time_to_event) works", {
  set.seed(3210)
  df_adtte <- data.frame(
    time = rexp(10, rate = 0.1),
    status = sample(c(0, 1), 10, replace = TRUE),
    arm = sample(c("Arm A", "Arm B"), 10, replace = TRUE),
    grade = sample(c("I", "II"), 10, replace = TRUE),
    strata = sample(c("1", "2"), 10, replace = TRUE)
  ) |>
    mutate(arm = relevel(factor(arm), ref = "Arm A"))

  expect_error(
    tbl <- df_adtte |>
      tbl_roche_subgroups(
        rsp = status,
        by = arm,
        time_to_event = time,
        subgroups = c(grade, strata),
        # Build a dummy table that inherits the correct row structure
        .tbl_fun = ~ gtsummary::tbl_summary(data.frame(1))
      ),
    NA
  )

  expect_snapshot(
    tbl$table_body |>
      dplyr::select(
        dplyr::starts_with("label"),
        dplyr::starts_with("stat")
      )
  )
})
