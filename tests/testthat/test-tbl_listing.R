tld <- trial |> # table_listing_data
  dplyr::select(trt, age, marker, stage) |>
  dplyr::filter(stage %in% c("T2", "T3")) |> # down sampling
  dplyr::slice_head(n = 2, by = c(trt, stage)) # down sampling

test_that("tbl_listing() works with default values", {
  expect_snapshot(
    tbl_listing(tld)$table_body # no keys, no sorting, no highlight
  )
})

test_that("tbl_listing(hide_duplicate_keys = FALSE) works with standard values", {
  w_duplicated_keys <- tbl_listing(tld) |>
    lst_highlight_columns(columns = trt)
  wo_duplicated_keys <- tbl_listing(tld)

  expect_snapshot(w_duplicated_keys$table_body[seq(3), ])
  expect_snapshot(wo_duplicated_keys$table_body[seq(3), ])
})

test_that("tbl_listing(blank_rows_by) works with standard values", {
  expect_no_error(
    out <- tbl_listing(tld, blank_rows_by = age)
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

test_that("tbl_listing(row_split) works with standard values", {
  expect_no_error(
    out <- tbl_listing(tld, row_split = list(row_numbers = c(2, 3)))
  )
  expect_length(out, 3) # 3 tables are created

  expect_snapshot(
    out[[2]]$table_body
  )
})

test_that("tbl_listing(row_split, blank_rows_by) works with standard values", {
  expect_no_error(
    out <- tbl_listing(tld, row_split = list(row_numbers = c(2, 3)), blank_rows_by = trt)
  )
  expect_length(out, 3) # 3 tables are created

  expect_snapshot(
    out[[3]]$table_body |> dplyr::slice_head(n = 4)
  )
})

test_that("tbl_listing(col_split) works with standard values", {
  grps <- list(c("trt", "stage", "age"), c("trt", "stage", "marker"))
  expect_no_error(
    out <- tbl_listing(tld, col_split = list(groups = grps)) |>
      lst_highlight_columns(columns = trt)
  )
  expect_length(out, 2) # 2 tables are created

  expect_snapshot(
    out[[2]]$table_body |> dplyr::slice_head(n = 4)
  )
})

test_that("tbl_listing(row_split + col_split) works with standard values", {
  grps <- list(c("trt", "stage", "age"), c("trt", "stage", "marker"))
  expect_no_error(
    out <- tbl_listing(tld,
      row_split = list(row_numbers = c(2, 6)),
      col_split = list(groups = grps),
      blank_rows_by = trt
    )
  )
  expect_length(out, 6) # 6 tables are created

  expect_s3_class(out, "tbl_split")
  expect_s3_class(out[[4]], "tbl_listing")

  expect_snapshot(
    out[[4]]$table_body
  )
})
