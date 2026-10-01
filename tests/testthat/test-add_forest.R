skip_on_cran()
tbl <-
  trial |>
  dplyr::select(age, marker, grade, response) |>
  gtsummary::tbl_uvregression(
    y = response,
    method = glm,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    hide_n = TRUE
  ) |>
  gtsummary::modify_column_merge(
    pattern = "{estimate} (95% CI {ci}; {p.value})",
    rows = !is.na(estimate)
  ) |>
  gtsummary::modify_header(estimate = "**Odds Ratio**") |>
  gtsummary::bold_labels()

test_that("add_forest(table_engine = 'flextable') works", {
  expect_warning(
    add_forest(tbl, table_engine = "flextable"),
    "Less than 2 spanning headers detected."
  )

  # the "gt" engine was removed in 0.4.0, crane renders with flextable only (#271)
  expect_error(
    add_forest(tbl, table_engine = "gt"),
    class = "lifecycle_error_deprecated"
  )
  expect_error(
    add_forest(tbl, table_engine = "not_an_engine"),
    "flextable"
  )

  expect_error(
    forest_ft <- trial |>
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

  # default 5pt L/R padding would push the fixed-width image out of its cell (#270)
  gg <- which(forest_ft$col_keys == "ggplot")
  expect_identical(unique(forest_ft$body$styles$pars$padding.left$data[, gg]), 0)
  expect_identical(unique(forest_ft$body$styles$pars$padding.right$data[, gg]), 0)

  # shrunk font drops the paragraph mark's descent so consecutive plots abut
  expect_identical(unique(forest_ft$body$styles$text$font.size$data[, gg]), 1)
  expect_false(any(forest_ft$body$styles$text$font.size$data[, -gg] == 1))
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

  # gg_chunk() renders eagerly, so latent geom_vline warnings surface here
  expect_no_error(
    expect_warning(
      out_flex <- tbl_edge_cases |> add_forest(table_engine = "flextable"),
      "Less than 2 spanning headers detected."
    )
  )

  expect_s3_class(out_flex, "flextable")
})
