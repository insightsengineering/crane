tld <- trial |> # table_listing_data
  dplyr::select(trt, age, marker, stage) |>
  dplyr::filter(stage %in% c("T2", "T3")) |> # down sampling
  dplyr::slice_head(n = 2, by = c(trt, stage)) # down sampling

test_that("tbl_listing() works with default values", {
  expect_snapshot(
    tbl_listing(tld)$table_body # no keys, no sorting, no highilight
  )
})

test_that("tbl_listing(keys, order_by) works with standard values", {
  expect_no_error(
    out <- tbl_listing(tld, keys = c(trt, stage), order_by = age)
  )

  expect_s3_class(out, "tbl_listing")

  expect_snapshot(
    head(out$table_body, n = 5) # highlight but no sorting
  )

  # Checking column values
  expect_setequal(
    names(tld),
    names(out$table_body)
  )

  # Checking printed labels
  expect_setequal(
    sapply(tld, attr, which = "label") |> unname(),
    sapply(out$table_body, attr, which = "label") |> unname()
  )
})

test_that("tbl_listing(hide_duplicate_keys = FALSE) works with standard values", {
  w_duplicated_keys <- tbl_listing(tld, keys = c(trt, stage), hide_duplicate_keys = TRUE)$table_body |>
    head(n = 3)
  wo_duplicated_keys <- tbl_listing(tld, keys = c(trt, stage), hide_duplicate_keys = FALSE)$table_body |>
    head(n = 3)

  expect_snapshot(w_duplicated_keys)
  expect_snapshot(wo_duplicated_keys)
})

test_that("tbl_listing(blank_rows_by) works with standard values", {
  expect_no_error(
    out <- tbl_listing(tld, keys = c(trt, stage), blank_rows_by = age)
  )

  # Check that the blank rows are inserted correctly every 2 rows
  which_rows_all_nas <- apply(out$table_body, 1, function(x) all(is.na(x))) |>
    which()

  expect_equal(
    which_rows_all_nas,
    seq(2, nrow(out$table_body), by = 2) # rows with all NAs in age
  )

  expect_snapshot(
    head(out$table_body, n = 5)
  )
})
