#' Create listings from a data frame
#'
#' This function creates a listing from a data frame. Common uses
#' rely on few pre-processing steps, such as ensuring unique values in key columns or split
#' by rows or columns. They are described in the note section.
#'
#' @param data (`data.frame`)\cr
#'   a data frame containing the data to be displayed in the listing.
#' @param keys ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to be highlighted on the left of the listing.
#' @param blank_rows_by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns where changing values is highlighted by a blank row. It depends substantially on the columns'
#'   sorting. See [crane::add_blank_row()] for more information. Defaults to `NULL`.
#' @param hide_duplicate_keys (`logical`)\cr
#'   whether to add blank values where key columns have duplicate values. Defaults to `TRUE`.
#'
#' @note
#' Common pre-processing steps for the data frame that may be common:
#'  * Unique values - this should be enforced in pre-processing by users.
#'  * `NA` values - they are not printed by default in `{gtsummary}`. You can make them explicit if
#'    they need to be displayed in the listing. See example 3.
#'  * Split by rows - you can split the data frame by rows by using `split_by_rows`.
#'    See example 4.
#'  * Split by columns - you can split the data frame by columns and then apply `tbl_listing()` to each subset.
#'    See example 5.
#'  * Split in post-processing is not suggested if `hide_duplicate_keys = TRUE`.
#'
#' @examplesIf rlang::is_installed("labelled")
#' # Load the trial dataset
#' trial_data <- trial |>
#'   dplyr::select(trt, age, marker, stage) |>
#'   dplyr::filter(stage %in% c("T2", "T3")) |>
#'   dplyr::slice_head(n = 2, by = c(trt, stage)) |> # downsampling
#'   dplyr::arrange(trt, stage) # key columns should be sorted
#'
#' # Example 1 --------------------------------
#' tbl_listing(trial_data, keys = c(trt, stage))
#' tbl_listing(trial_data, keys = c(trt, stage), hide_duplicate_keys = TRUE)
#'
#' # Example 2 --------------------------------
#' # make NAs explicit
#' trial_data_na <- trial_data |>
#'   mutate(across(everything(), ~ tidyr::replace_na(labelled::to_character(.), "-")))
#' tbl_listing(trial_data_na, keys = c(trt, stage))
#'
#' # Example 3 --------------------------------
#' # Add blank rows for first key column
#' lst <- tbl_listing(trial_data_na, keys = c(trt, stage), blank_rows_by = trt)
#' lst
#'
#' # Can add them also manually in post-processing
#' lst |> add_blank_row(row_numbers = seq(2))
#'
#' # Example 4 --------------------------------
#' # Split by rows
#' list_lst <- tbl_listing(trial_data, keys = stage, row_split = list(variables = trt))
#' list_lst[[2]]
#'
#' # Example 6 --------------------------------
#' # Split by columns
#' show_header_names(lst)
#' grps <- list(c("trt", "stage", "age"), c("trt", "stage", "marker"))
#' list_lst <- tbl_listing(trial_data, keys = stage, col_split = list(groups = grps))
#' list_lst[[2]]
#'
#' @export
tbl_listing <- function(data,
                        keys = NULL,
                        row_split = list(),
                        col_split = list(),
                        blank_rows_by = NULL,
                        hide_duplicate_keys = FALSE) {
  set_cli_abort_call()

  # Checks -----------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_scalar_logical(hide_duplicate_keys)

  # Process arguments ------------------------
  cards::process_selectors(data, keys = {{ keys }})
  cards::process_selectors(data, blank_rows_by = {{ blank_rows_by }})
  tbl_listing_inputs <- as.list(environment())

  # Reorder the full set of cols to ensure key columns are first
  ordercols <- c(keys, setdiff(names(data), keys))
  data <- data[, ordercols]

  # Build the listing ----------------------------------------------------------
  x <- gtsummary::as_gtsummary(data)
  x$inputs <- tbl_listing_inputs

  # Add blank rows by col ------------------------------------------------------
  if (length(blank_rows_by) > 0) {
    # Find where groups change in the specified columns
    grp_diffs <- data |>
      dplyr::group_by(across(all_of(blank_rows_by))) |>
      dplyr::group_indices() |>
      diff()

    # Add a blank row where the group changes
    x <- add_blank_row(x, row_numbers = which(grp_diffs != 0))
  }

  # add class and attributes ---------------------------------------------------
  structure(
    x,
    class = c("tbl_listing", "gtsummary")
  )

  # Split it if requested ------------------------------------------------------
  if (length(row_split) > 0 && length(col_split) > 0) {
    cli::cli_abort(
      c(
        "You can only split by rows or by columns, not both.",
        i = "Please choose one of the two options."
      ), call = get_cli_abort_call()
    )
  }

  if (length(row_split) > 0) {
    row_split <- c(list(x = x), row_split)
    x <- rlang::exec(gtsummary::tbl_split_by_rows, !!!row_split)
  }
  if (length(col_split) > 0) {
    col_split <- c(list(x = x), col_split)
    x <- rlang::exec(gtsummary::tbl_split_by_columns, !!!col_split)
  }

  # Return and highlight keys if requested -------------------------------------
  .highlight_keys(x)
}

# Add blank values for key duplicates if requested -----------------------------
.highlight_keys <- function(x, blank_str = "") {
  if (is.list(x) && inherits(x[[1]], "gtsummary")) {
    return(lapply(x, .highlight_keys, blank_str = blank_str))
  }
  do_it_flag <- x$inputs$hide_duplicate_keys
  keys <- x$inputs$keys

  # Check if keys are unique
  if (isTRUE(do_it_flag) && any(duplicated(x$table_body[keys]))) {
    # Create a new data frame with blank values for duplicates
    for (kcol in keys) {
      tmp_label_attr <- attr(x$table_body[[kcol]], "label") # not losing the label attribute
      kcol_vec <- as.character(x$table_body[[kcol]])
      attr(kcol_vec, "label") <- tmp_label_attr
      cur_key <- paste0("", kcol_vec) # used to force into a character vector
      disp <- c(TRUE, utils::tail(cur_key, -1) != utils::head(cur_key, -1))
      kcol_vec[!disp] <- blank_str
      x$table_body[[kcol]] <- kcol_vec
    }
  }

  x
}
