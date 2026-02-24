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

test_that("add_forest handles extreme limits and character NA p-values safely", {
  # 1. SETUP: Create a basic model and table
  df_dummy <- data.frame(
    status = c(1, 0, 1, 0, 1, 1, 0, 0),
    age = c(50, 45, 60, 30, 40, 55, 35, 25)
  )

  tbl_base <- glm(status ~ age, data = df_dummy, family = binomial) |>
    tbl_regression(exponentiate = TRUE)

  # 2. CORRUPT THE DATA: Force the edge cases
  tbl_edge_cases <- tbl_base

  # Inject the bad data directly into the dataframe to bypass gtsummary's formatters
  tbl_edge_cases$table_body <- tbl_edge_cases$table_body |>
    dplyr::mutate(
      # Issue 1 Trigger: Extreme estimates > 1.0
      estimate = 564637495,
      conf.low = 0,
      conf.high = Inf,

      # Issue 2 Trigger: Overwrite the numeric p.value with the literal string "NA"
      p.value = NA
    )

  # 3. TEST: Run add_forest
  # We expect absolutely no errors (Issue 2 fixed) and no warnings (Issue 1 fixed)
  expect_no_error(
    expect_warning(
      out_gt <- tbl_edge_cases |> add_forest(table_engine = "gt"),
      "Less than 2 spanning headers detected."
    )
  )

  expect_no_warning(
    # gt delays rendering until print time, so we force it to render the HTML
    # to trigger any latent ggplot geom_vline warnings.
    suppressMessages(gt::as_raw_html(out_gt))
  )

  # Ensure it works for flextable too
  expect_no_error(
    expect_warning(
      out_flex <- tbl_edge_cases |> add_forest(table_engine = "flextable"),
      "Less than 2 spanning headers detected."
    )
  )
})
