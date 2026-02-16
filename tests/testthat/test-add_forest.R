skip_on_cran()
tbl <-
  trial |>
  dplyr::select(age, marker, grade, response) |>
  tbl_uvregression(
    y = response,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    hide_n = TRUE
  ) |>
  modify_column_merge(
    pattern = "{estimate} (95% CI {ci}; {p.value})",
    rows = !is.na(estimate)
  ) |>
  modify_header(estimate = "**Odds Ratio**") |>
  bold_labels()

test_that("add_forest(table_engine = 'flextable') works", {
  expect_warning(
    add_forest(tbl, table_engine = "flextable"),
    "Less than 2 spanning headers detected."
  )
  expect_warning(
    add_forest(tbl, table_engine = "gt"),
    "Less than 2 spanning headers detected."
  )

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
      ) |>
      add_forest(
        pvalue = starts_with("p.value"),
        table_engine = "flextable"
      ),
    NA
  )
})
