#' Create listings from a data frame
#'
#' @param data (`data.frame`)\cr
#'   a data frame containing the data to be displayed in the listing.
#' @param keys ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to be highlighted on the left of the listing.
#' @param order_by ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to be sorted. It defaults to `keys`.
#' @param do_not_print_duplicated_keys (`logical`)\cr
#'   whether to add blank values where key columns have duplicate values. Defaults to `TRUE`.
#'
#' @details
#' This function converts a data frame into a listing format and it relies on the user's pre-processing
#' to decide which column to show and select unique or non-unique rows. Also sorting depends on the user
#' pre-processing.
#'
#' @note
#' `NA` values are not printed by default in `{gtsummary}`. Please consider making them explicit if
#' you want them to be displayed in the listing.
#'
#' @examples
#' # Example 1 --------------------------------
#' trial_data <- trial |>
#'   dplyr::select(trt, age, marker, stage)
#'
#' tbl_listing(trial_data, keys = c(trt, stage))
#'
#' @export
tbl_listing <- function(data,
                        keys = NULL,
                        order_by = keys,
                        do_not_print_duplicated_keys = TRUE) {
  set_cli_abort_call()

  # Checks -----------------------------------
  check_not_missing(data)
  check_data_frame(data)
  check_scalar_logical(do_not_print_duplicated_keys)

  # Process arguments ------------------------
  cards::process_selectors(data, keys = {{ keys }})
  cards::process_selectors(data, order_by = {{ order_by }})

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

  # return with class and attributes -------------------------------------------
  structure(
    x,
    class = c("tbl_listing", "gtsummary")
  )
}

#' `as_*()` overloading for `tbl_listing`
#'
#' @inheritParams gtsummary::as_gt
#'
#' @keywords internal
#' @name tbl_listing_as_overloading
NULL

#' @rdname tbl_listing_as_overloading
#' @export
as_gt <- function(x, ...) {
  UseMethod("as_gt")
}

#' @rdname tbl_listing_as_overloading
#' @export
as_flex_table <- function(x, ...) {
  UseMethod("as_flex_table")
}

#' @rdname tbl_listing_as_overloading
#' @exportS3Method
as_gt.tbl_listing <- function(x, ...) {
  set_cli_abort_call()

  # Highlight keys if requested ---------------------------------------------
  if(x$inputs$do_not_print_duplicated_keys) {
    x$table_body <- .highlight_keys(x$table_body, x$inputs$keys)
  }

  gtsummary::as_gt(x, ...)
}

#' @rdname tbl_listing_as_overloading
#' @exportS3Method
as_flex_table.tbl_listing <- function(x, ...) {
  set_cli_abort_call()

  # Highlight keys if requested ---------------------------------------------
  if(x$inputs$do_not_print_duplicated_keys) {
    x$table_body <- .highlight_keys(x$table_body, x$inputs$keys)
  }

  gtsummary::as_flex_table(x, ...)
}

# print.tbl_listing <- function(x,
#                                print_engine = c("gt", "flextable", "huxtable", "kable", "kable_extra", "tibble"),
#                                ...) {
#   set_cli_abort_call()
#
#   # Highlight keys if requested ---------------------------------------------
#   if(x$inputs$do_not_print_duplicated_keys) {
#     x$table_body <- .highlight_keys(x$table_body, x$inputs$keys)
#   }
#   class(x) <- c("gtsummary")
#
#   # Print the gtsummary object ---------------------------------------------
#   print(
#     x,
#     print_engine = print_engine,
#     ...
#   )
# }

# Add blank values for key duplicates if requested -----------------------------
.highlight_keys <- function(data, keys, blank_str = "") {
    # Check if keys are unique
    if (any(duplicated(data[keys]))) {
      # Create a new data frame with blank values for duplicates
      for (kcol in keys) {
        kcol_vec <- as.character(data[[kcol]])
        cur_key <- paste0("", kcol_vec) # used to force into a character vector
        disp <- c(TRUE, tail(cur_key, -1) != head(cur_key, -1))
        kcol_vec[!disp] <- blank_str
        data[[kcol]] <- kcol_vec
      }
    }

  data
}
