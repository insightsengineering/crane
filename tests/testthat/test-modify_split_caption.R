tld <- trial |> # table_listing_data
  dplyr::select(trt, age, marker, stage) |>
  dplyr::filter(stage %in% c("T2", "T3")) |> # down sampling
  dplyr::slice_head(n = 2, by = c(trt, stage))

# get the hide flag of a single column from a gtsummary table
hide_flag <- function(x, col) {
  x$table_styling$header$hide[x$table_styling$header$column == col]
}

test_that("modify_split_caption() labels each page and hides the split column", {
  expect_no_error(
    out <- tbl_listing(tld, split_by_rows = list(variable_level = "trt")) |>
      modify_split_caption(spl_col = "trt")
  )

  # one page per treatment level, class preserved
  expect_s3_class(out, "tbl_split")
  expect_s3_class(out[[1]], "tbl_listing")

  # default pattern is applied on every page and kept in sync with the
  # variable_level attribute
  expect_equal(as.character(out[[1]]$table_styling$caption), "Parameter: Drug B")
  expect_equal(attr(out[[1]], "variable_level"), "Parameter: Drug B")
  expect_equal(as.character(out[[2]]$table_styling$caption), "Parameter: Drug A")
  expect_equal(attr(out[[2]], "variable_level"), "Parameter: Drug A")

  # the split column is hidden by default on every page
  expect_true(hide_flag(out[[1]], "trt"))
  expect_true(hide_flag(out[[2]], "trt"))
})

test_that("modify_split_caption() honors a custom glue pattern", {
  out <- tbl_listing(tld, split_by_rows = list(variable_level = "trt")) |>
    modify_split_caption(spl_col = "trt", pattern = "Treatment: {spl_level}")

  expect_equal(as.character(out[[1]]$table_styling$caption), "Treatment: Drug B")
})

test_that("modify_split_caption(hide_spl_col = FALSE) keeps the split column", {
  out <- tbl_listing(tld, split_by_rows = list(variable_level = "trt")) |>
    modify_split_caption(spl_col = "trt", hide_spl_col = FALSE)

  expect_false(hide_flag(out[[1]], "trt"))
  # caption is still applied
  expect_equal(as.character(out[[1]]$table_styling$caption), "Parameter: Drug B")
})

test_that("modify_split_caption() is silent when the column is absent or already hidden", {
  base <- tbl_listing(tld, split_by_rows = list(variable_level = "trt"))

  # absent column -> no error, no caption change beyond the default label
  expect_no_error(
    out_absent <- modify_split_caption(base, spl_col = "not_a_column")
  )
  expect_equal(as.character(out_absent[[1]]$table_styling$caption), "Parameter: Drug B")

  # applying twice is idempotent (column already hidden the second time)
  expect_no_error(
    out_twice <- base |>
      modify_split_caption(spl_col = "trt") |>
      modify_split_caption(spl_col = "trt")
  )
  expect_true(hide_flag(out_twice[[1]], "trt"))
})

test_that("modify_split_caption() skips row-number splits that lack a level", {
  # row_numbers splits do not set the variable_level attribute
  out <- tbl_listing(tld, split_by_rows = list(row_numbers = c(2, 3))) |>
    modify_split_caption(spl_col = "trt", hide_spl_col = FALSE)

  expect_null(out[[1]]$table_styling$caption)
})

test_that("modify_split_caption() works on a single (non-list) split element", {
  one <- tbl_listing(tld, split_by_rows = list(variable_level = "trt"))[[1]]
  out <- modify_split_caption(one, spl_col = "trt")

  expect_s3_class(out, "tbl_listing")
  expect_equal(as.character(out$table_styling$caption), "Parameter: Drug B")
  expect_true(hide_flag(out, "trt"))
})

test_that("modify_split_caption() checks its inputs", {
  one <- tbl_listing(tld, split_by_rows = list(variable_level = "trt"))[[1]]

  expect_error(
    modify_split_caption(data.frame(a = 1), spl_col = "trt"),
    "must be class"
  )
  expect_error(
    modify_split_caption(one),
    "cannot be missing"
  )
  expect_error(
    modify_split_caption(one, spl_col = "trt", pattern = 5),
    "must be a string"
  )
})
