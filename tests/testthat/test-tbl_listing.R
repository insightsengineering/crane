tld <- trial |> # table_listing_data
  dplyr::select(trt, age, marker, stage) |>
  dplyr::filter(stage %in% c("T2", "T3")) |> # down sampling
  dplyr::slice_head(n = 2, by = c(trt, stage)) # down sampling

test_that("tbl_listing() works with default values", {
  expect_snapshot(
    tbl_listing(tld)$table_body # no keys, no sorting, no highlight
  )
})

test_that("tbl_listing(add_blanks_rows) works with standard values", {
  expect_no_error(
    out <- tbl_listing(tld, add_blank_rows = list(variable_level = "age"))
  )

  # Check that the blank rows are inserted correctly every 2 rows
  which_rows_all_nas <- apply(out$table_body, 1, function(x) all(is.na(x))) |>
    which()

  expect_equal(
    which_rows_all_nas,
    seq(2, nrow(out$table_body), by = 2) # rows with all NAs in age
  )

  expect_snapshot(
    dplyr::slice_head(out$table_body, n = 5)
  )
})

test_that("tbl_listing(split_by_rows) works with standard values", {
  expect_no_error(
    out <- tbl_listing(tld, split_by_rows = list(row_numbers = c(2, 3)))
  )
  expect_length(out, 3) # 3 tables are created

  expect_snapshot(
    out[[2]]$table_body
  )
})

test_that("tbl_listing(split_by_rows, add_blank_rows) works with standard values", {
  expect_no_error(
    out <- tld |>
      tbl_listing(
        split_by_rows = list(row_numbers = c(2, 3)),
        add_blank_rows = list(variable_level = "trt")
      )
  )
  expect_length(out, 3) # 3 tables are created

  expect_snapshot(
    out[[3]]$table_body |> dplyr::slice_head(n = 4)
  )
})

test_that("tbl_listing(split_by_columns) works with standard values", {
  grps <- list(c("trt", "stage", "age"), c("trt", "stage", "marker"))
  expect_no_error(
    out <- tbl_listing(tld, split_by_columns = list(groups = grps)) |>
      remove_duplicate_keys(keys = "trt")
  )
  expect_length(out, 2) # 2 tables are created

  expect_snapshot(
    out[[2]]$table_body |> dplyr::slice_head(n = 4)
  )
})

test_that("tbl_listing(split_by_rows + split_by_columns) works with standard values", {
  grps <- list(c("trt", "stage", "age"), c("trt", "stage", "marker"))
  expect_no_error(
    out <- tbl_listing(tld,
      split_by_rows = list(row_numbers = c(2, 6)),
      split_by_columns = list(groups = grps),
      add_blank_rows = list(variable_level = "trt")
    )
  )
  expect_length(out, 6) # 6 tables are created

  expect_s3_class(out, "tbl_split")
  expect_s3_class(out[[4]], "tbl_listing")

  expect_snapshot(
    out[[4]]$table_body
  )
})

test_that("remove_duplicate_keys() works with standard values", {
  w_duplicated_keys <- tbl_listing(tld) |>
    remove_duplicate_keys(keys = trt)
  wo_duplicated_keys <- tbl_listing(tld)

  expect_snapshot(w_duplicated_keys$table_body[seq(3), ])
  expect_snapshot(wo_duplicated_keys$table_body[seq(3), ])
})

test_that("remove_duplicate_keys() works with unique and duplicated values", {
  # Regression test for #129
  w_duplicated_keys <- tbl_listing(tld) |>
    remove_duplicate_keys(keys = c(trt, marker)) # marker is unique, trt is not

  expect_snapshot(w_duplicated_keys$table_body[seq(4), ]) # trt has duplicates, marker does not
})

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

  # default pattern is applied and kept in sync with the variable_level attribute
  expect_equal(as.character(out[[1]]$table_styling$caption), "Parameter: Drug B")
  expect_equal(attr(out[[1]], "variable_level"), "Parameter: Drug B")

  # the split column is hidden by default
  expect_true(hide_flag(out[[1]], "trt"))
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
