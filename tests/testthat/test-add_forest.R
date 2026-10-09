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

  # the "gt" engine was removed in 0.4.0.9000, crane renders with flextable only (#271)
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

test_that("add_forest() warns when {magick} is missing (#270)", {
  # rlang otherwise emits the once-per-session warning only on the first call
  withr::local_options(rlib_warning_verbosity = "verbose")

  # flextable needs magick to draw gg_chunk() images in PDF/PNG/SVG exports
  expect_warning(.warn_if_no_magick(installed = FALSE), "Install .*magick")
  expect_no_warning(.warn_if_no_magick(installed = TRUE))
})

test_that("add_forest(row_height, table_width) draws compact rows that fit the page (#270)", {
  tbl_sub <- trial |>
    tbl_roche_subgroups(
      subgroups = c("grade", "stage"),
      rsp = "response",
      by = "trt",
      ~ glm(response ~ trt, data = .x) |>
        gtsummary::tbl_regression(show_single_row = trt, exponentiate = TRUE)
    )
  forest_ft <- add_forest(tbl_sub, table_width = "L8")
  gg <- which(forest_ft$col_keys == "ggplot")
  n <- flextable::nrow_part(forest_ft, "body")

  # the table fills the L8 text width; the forest column keeps 2.5in
  expect_equal(sum(forest_ft$body$colwidths), 11.69 - (3.30 + 3.35) / 2.54)
  expect_equal(forest_ft$body$colwidths[[gg]], 2.5)
  expect_identical(forest_ft$properties$layout, "fixed")

  # exact rows; one merged forest cell whose picture is as tall as all rows
  expect_identical(unique(forest_ft$body$hrule), "exact")
  expect_equal(forest_ft$body$spans$columns[, gg], c(n, rep(0, n - 1)))
  expect_equal(forest_ft$body$content$data[[1, gg]]$height, sum(forest_ft$body$rowheights))

  # picture paragraphs take single spacing from the "header" style, not a bare w:line
  expect_identical(unique(forest_ft$body$styles$pars$word_style$data[, gg]), "header")
  expect_true(all(is.na(forest_ft$body$styles$pars$line_spacing$data[, gg])))

  # column labels end on the same line
  label_row <- flextable::nrow_part(forest_ft, "header")
  expect_identical(unique(forest_ft$header$styles$cells$vertical.align$data[label_row, ]), "bottom")

  # a page that is too narrow warns; a non-positive row height errors
  expect_warning(add_forest(tbl_sub, table_width = 3), "needs")
  expect_error(add_forest(tbl_sub, row_height = 0), "row_height")
})

test_that(".forest_page() maps page sizes to text width and font size (#270)", {
  expect_equal(.forest_page("P8")$width, 8.27 - (3.66 + 2.11) / 2.54)
  expect_equal(.forest_page("L6")$font_size, 6)
  expect_equal(.forest_page(7)$width, 7)
  expect_null(.forest_page(NULL)$width)
  expect_error(.forest_page("A4"), "page size")
})
