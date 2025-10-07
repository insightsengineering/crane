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
