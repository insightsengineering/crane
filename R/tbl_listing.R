#' Create listings from a data frame
#'
#' This function creates a listing from a data frame. Common uses
#' rely on few pre-processing steps, such as ensuring unique values in key columns or split
#' by rows or columns. They are described in the note section.
#'
#' @param data (`data.frame`)\cr
#'   a data frame containing the data to be displayed in the listing.
#' @param row_split (`list`)\cr
#'   parameters passed to [gtsummary::tbl_split_by_rows()].
#' @param col_split (`list`)\cr
#'   parameters passed to [gtsummary::tbl_split_by_columns()].
#' @param blank_rows_by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns where changing values is highlighted by a blank row. It depends substantially on the columns'
#'   sorting. See [crane::add_blank_row()] for more information. Defaults to `NULL`.
#'
#' @note
#' Common pre-processing steps for the data frame that may be common:
#'  * Unique values - this should be enforced in pre-processing by users.
#'  * `NA` values - they are not printed by default in `{gtsummary}`. You can make them explicit if
#'    they need to be displayed in the listing. See example 3.
#'  * Sorting key columns and moving them to the front. See the examples pre-processing.
#'
#'  ## Splitting the listing
#'  * Split by rows - you can split the data frame by rows by using `row_split` parameter. You can use the same
#'    parameters used in [gtsummary::tbl_split_by_rows()]. See example 4.
#'  * Split by columns - you can split the data frame by columns by using `col_split` parameter. Use the same
#'    parameters from [gtsummary::tbl_split_by_rows()]. See example 5.
#'
#' @examplesIf rlang::is_installed("labelled")
#' # Load the trial dataset
#' trial_data <- trial |>
#'   dplyr::select(trt, age, marker, stage) |>
#'   dplyr::filter(stage %in% c("T2", "T3")) |>
#'   dplyr::slice_head(n = 2, by = c(trt, stage)) |> # downsampling
#'   dplyr::arrange(trt, stage) |> # key columns should be sorted
#'   dplyr::relocate(trt, stage) # key columns should be first
#'
#' # Example 1 --------------------------------
#' out <- tbl_listing(trial_data)
#' out
#' out |> lst_highlight_columns(columns = trt)
#'
#' # Example 2 --------------------------------
#' # make NAs explicit
#' trial_data_na <- trial_data |>
#'   mutate(across(everything(), ~ tidyr::replace_na(labelled::to_character(.), "-")))
#' tbl_listing(trial_data_na)
#'
#' # Example 3 --------------------------------
#' # Add blank rows for first key column
#' lst <- tbl_listing(trial_data_na, blank_rows_by = trt)
#' lst
#'
#' # Can add them also manually in post-processing
#' lst |> add_blank_row(row_numbers = seq(2))
#'
#' # Example 4 --------------------------------
#' # Split by rows
#' list_lst <- tbl_listing(trial_data, row_split = list(row_numbers = c(2, 3, 4)))
#' list_lst[[2]]
#'
#' # Example 5 --------------------------------
#' # Split by columns
#' show_header_names(lst)
#' grps <- list(c("trt", "stage", "age"), c("trt", "stage", "marker"))
#' list_lst <- tbl_listing(trial_data, col_split = list(groups = grps))
#' list_lst[[2]]
#'
#' # Example 6 --------------------------------
#' # Split by rows and columns
#' list_lst <- tbl_listing(trial_data,
#'   row_split = list(row_numbers = c(2, 3, 4)), col_split = list(groups = grps)
#' )
#' length(list_lst) # 8 tables are flatten out
#' list_lst[[2]]
#'
#' @export
tbl_listing <- function(data,
                        row_split = list(),
                        col_split = list(),
                        blank_rows_by = NULL) {
  set_cli_abort_call()

  # Checks -----------------------------------
  check_not_missing(data)
  check_data_frame(data)

  # Process arguments ------------------------
  cards::process_selectors(data, blank_rows_by = {{ blank_rows_by }})
  tbl_listing_inputs <- as.list(environment())

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
  if (length(row_split) > 0) {
    row_split <- c(list(x = x), row_split)
    x <- rlang::exec(gtsummary::tbl_split_by_rows, !!!row_split)
  }
  if (length(col_split) > 0) {
    col_split <- c(list(x = x), col_split)
    x <- rlang::exec(gtsummary::tbl_split_by_columns, !!!col_split)
  }

  # Return the listing ---------------------------------------------------------
  x
}

#' @describeIn tbl_listing adds blank values for duplicate columns.
#'
#' @param columns ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to highlight for duplicate values. If `NULL`, nothing is done.
#' @param blank_str (`string`)\cr
#'   string to use for blank values. Defaults to `""`. It should not be changed.
#'
#' @examples
#' # Example 7 --------------------------------
#' # Hide duplicate columns in post-processing
#' out <- list_lst |>
#'   lst_highlight_columns(columns = c(trt, stage))
#' out[[2]]
#'
#' @export
lst_highlight_columns <- function(x, columns = NULL, blank_str = "") {
  if (is.list(x) && inherits(x[[1]], "gtsummary")) {
    return(map(x, lst_highlight_columns, columns = {{ columns }}, blank_str = blank_str))
  }

  # Checks -----------------------------------
  cards::process_selectors(x$table_body, columns = {{ columns }})
  check_string(blank_str)

  # Check if columns are unique
  if (any(duplicated(x$table_body[columns]))) {
    # Create a new data frame with blank values for duplicates
    for (kcol in columns) {
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
