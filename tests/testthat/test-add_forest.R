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

  # The plot column has a fixed width and holds an image of exactly that width.
  # The default 5pt horizontal cell padding would push the image past the column
  # and overflow the page in docx; zeroing it makes the image fit the cell
  # content box exactly so the table stays on the page (#270).
  gg <- which(forest_ft$col_keys == "ggplot")
  expect_identical(unique(forest_ft$body$styles$pars$padding.left$data[, gg]), 0)
  expect_identical(unique(forest_ft$body$styles$pars$padding.right$data[, gg]), 0)

  # Word ignores `w:spacing w:line="0"` unless it carries `w:lineRule="exact"`,
  # which flextable cannot emit, so the paragraph mark's font descent reserves
  # white space under each inline plot and breaks the vertical reference line
  # between rows. The plot column's font is shrunk to collapse that descent.
  expect_identical(unique(forest_ft$body$styles$text$font.size$data[, gg]), 1)
  expect_false(any(forest_ft$body$styles$text$font.size$data[, -gg] == 1))

  # the declared column widths exceed any standard page, and a fixed layout makes
  # Word render them verbatim and overflow the right edge (#270)
  expect_identical(forest_ft$properties$layout, "autofit")
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
  # We expect absolutely no errors (Issue 2 fixed) and no warnings (Issue 1 fixed).
  # flextable renders the ggplots eagerly through gg_chunk(), so any latent
  # geom_vline warning surfaces during add_forest() itself. The gt engine used to
  # need a forced as_raw_html() render here; it was removed in 0.4.0 (#271).
  expect_no_error(
    expect_warning(
      out_flex <- tbl_edge_cases |> add_forest(table_engine = "flextable"),
      "Less than 2 spanning headers detected."
    )
  )

  expect_s3_class(out_flex, "flextable")
})
