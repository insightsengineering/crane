#' Create listings from a data frame
#'
#' This function creates a listing from a data frame, highlighting key columns. Common uses
#' rely on few pre-processing steps, such as ensuring unique values in key columns or split
#' by rows or columns. They are described in the note section.
#'
#' @param data (`data.frame`)\cr
#'   a data frame containing the data to be displayed in the listing.
#' @param keys ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to be highlighted on the left of the listing.
#' @param order_by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to be sorted. It defaults to `keys`.
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
#'  * Split by rows - you can split the data frame by rows and then apply `tbl_listing()` to each subset.
#'    See example 5.
#'  * Split by columns - you can split the data frame by columns and then apply `tbl_listing()` to each subset.
#'    See example 6.
#'  * Split in post-processing is not suggested if `hide_duplicate_keys = TRUE`.
#'
#' @examplesIf rlang::is_installed("labelled")
#' # Load the trial dataset
#' trial_data <- trial |>
#'   dplyr::select(trt, age, marker, stage) |>
#'   dplyr::filter(stage %in% c("T2", "T3")) |>
#'   dplyr::slice_head(n = 2, by = c(trt, stage)) # downsampling
#'
#' # Example 1 --------------------------------
#' tbl_listing(trial_data, keys = c(trt, stage))
#'
#' # Example 2 --------------------------------
#' # Sort by trt stage and marker
#' tbl_listing(trial_data, keys = c(trt, stage), order_by = c(trt, stage, marker))
#'
#' # Example 3 --------------------------------
#' # make NAs explicit
#' trial_data_na <- trial_data |>
#'   mutate(across(everything(), ~ tidyr::replace_na(labelled::to_character(.), "-")))
#' tbl_listing(trial_data_na, keys = c(trt, stage))
#'
#' # Example 4 --------------------------------
#' # Add blank rows for first key column
#' lst <- tbl_listing(trial_data_na, keys = c(trt, stage), blank_rows_by = trt)
#' lst
#'
#' # Can add them also manually in post-processing
#' lst |> add_blank_row(row_numbers = seq(2))
#'
#' # Example 5 --------------------------------
#' # Split by rows
#' trial_data_split <- trial_data |>
#'   split(trial_data$trt)
#' list_lst <- lapply(trial_data_split, tbl_listing, keys = c(trt, stage))
#' # names(list_lst) # keeps names
#' list_lst[[2]]
#'
#' # Example 6 --------------------------------
#' # Split by columns
#' column_groups <- list(
#'   age = c("trt", "age"),
#'   marker = c("trt", "marker")
#' )
#' trial_data_split <- lapply(column_groups, function(cols) trial_data[, cols, drop = FALSE])
#' list_lst <- lapply(trial_data_split, tbl_listing, keys = trt)
#' # names(list_lst) # keeps names
#' list_lst[[2]]
#'
#' @export
tbl_listing <- function(data,
                        keys = NULL,
                        order_by = all_of(keys),
                        blank_rows_by = NULL,
                        hide_duplicate_keys = TRUE) {
  set_cli_abort_call()

  # Checks -----------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_scalar_logical(hide_duplicate_keys)

  # Process arguments ------------------------
  cards::process_selectors(data, keys = {{ keys }})
  cards::process_selectors(data, order_by = {{ order_by }})
  cards::process_selectors(data, blank_rows_by = {{ blank_rows_by }})

  tbl_listing_inputs <- as.list(environment())

  # Sorting ----------------------------------
  if (!is.null(order_by)) {
    data <- data |>
      dplyr::arrange(across(all_of(order_by)))

    # Inform about happened sorting if interactive
    if (interactive()) {
      main_message <-
        if (identical(order_by, keys)) {
          "Sorting incoming data by key columns."
        } else {
          "Sorting incoming data by column{?s} {.val {order_by}}."
        }

      cli::cli_inform(
        c(
          "v" = main_message,
          "i" = "If you want to change the sorting, please sort the data frame before passing it to `tbl_listing()`."
        ),
        call = get_cli_abort_call()
      )
    }
  }

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

  # Highlight keys if requested ------------------------------------------------
  x <- .highlight_keys(x)

  # Return with class and attributes -------------------------------------------
  structure(
    x,
    class = c("tbl_listing", "gtsummary")
  )
}

# Add blank values for key duplicates if requested -----------------------------
.highlight_keys <- function(x, blank_str = "") {
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
      disp <- c(TRUE, tail(cur_key, -1) != head(cur_key, -1))
      kcol_vec[!disp] <- blank_str
      x$table_body[[kcol]] <- kcol_vec
    }
  }

  x
}
