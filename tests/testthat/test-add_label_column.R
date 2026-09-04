base_tbl <- function() {
  gtsummary::trial |>
    dplyr::select(trt, grade) |>
    gtsummary::tbl_summary(by = trt, include = grade) |>
    gtsummary::remove_row_type(type = "header")
}

# read a single column's styling flag from a gtsummary table
col_flag <- function(x, col, field) {
  x$table_styling$header[[field]][x$table_styling$header$column == col]
}

test_that("add_label_column() inserts label0 before label", {
  expect_no_error(
    out <- add_label_column(
      base_tbl(),
      value = ifelse(dplyr::row_number() == 1L, "Grade", NA_character_)
    )
  )

  # label0 exists and sits directly before label in the body
  cols <- names(out$table_body)
  expect_true("label0" %in% cols)
  expect_equal(which(cols == "label0"), which(cols == "label") - 1L)

  # the value expression is evaluated against the table body: stratum on the
  # first row, NA (rendered blank) elsewhere
  expect_equal(out$table_body$label0, c("Grade", NA_character_, NA_character_))

  # class preserved
  expect_s3_class(out, "gtsummary")
})

test_that("add_label_column() sets headers, alignment and indent", {
  out <- add_label_column(
    base_tbl(),
    value = ifelse(dplyr::row_number() == 1L, "Grade", NA_character_),
    header = "Group",
    label_header = "Category"
  )

  expect_equal(col_flag(out, "label0", "label"), "Group")
  expect_equal(col_flag(out, "label", "label"), "Category")
  # both label columns are left-aligned
  expect_equal(col_flag(out, "label0", "align"), "left")
  expect_equal(col_flag(out, "label", "align"), "left")

  # label indent reset to 0 so value rows sit flush left; the last indent
  # instruction recorded for `label` is the 0-space reset applied by the helper
  label_indent <- out$table_styling$indent[
    out$table_styling$indent$column == "label",
  ]
  expect_equal(label_indent$n_spaces[nrow(label_indent)], 0L)
})

test_that("add_label_column() default header is 'Value' and label unchanged", {
  before <- base_tbl()
  out <- add_label_column(before, value = NA_character_)

  expect_equal(col_flag(out, "label0", "label"), "Value")
  # label_header defaults to NULL, so the label header is left untouched
  expect_equal(
    col_flag(out, "label", "label"),
    col_flag(before, "label", "label")
  )
})

test_that("add_label_column() can place the value column after label", {
  out <- add_label_column(
    base_tbl(),
    value = as.character(dplyr::row_number()),
    .after = "label"
  )

  cols <- names(out$table_body)
  expect_equal(which(cols == "label0"), which(cols == "label") + 1L)
})

test_that("add_label_column(indent = NULL) leaves label indentation untouched", {
  before <- base_tbl()
  out <- add_label_column(before, value = NA_character_, indent = NULL)

  # no new indent instruction is added for `label`
  n_before <- sum(before$table_styling$indent$column == "label")
  n_after <- sum(out$table_styling$indent$column == "label")
  expect_equal(n_after, n_before)
})

test_that("add_label_column() checks inputs", {
  expect_error(add_label_column(mtcars, value = 1), "class <gtsummary>")
  expect_error(
    add_label_column(base_tbl(), value = 1, header = 1),
    "must be a string"
  )
  expect_error(
    add_label_column(base_tbl(), value = 1, indent = "x"),
    "must be a scalar integer"
  )
})
