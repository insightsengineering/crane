#' Create listings from a data frame
#'
#' @param df (`data.frame`)\cr
#'   a data frame containing the data to be displayed in the listing.
#' @param key_cols ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to be used as key columns in the listing.
#' @param sort_cols ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   column names to be used for sorting the data frame before creating the listing. It defaults to `key_cols`.
#' @param add_blank_key_duplicates (`logical`)\cr
#'   whether to add blank values where key columns have duplicate values. Defaults to `TRUE`.
#' @param ... other objects that will be added to the `{gtsummary}` object list.
#'
#' @details
#' This function converts a data frame into a listing format and it relies on the user's pre-processing
#' to decide which column to show and select unique or non-unique rows. Also sorting depends on the user
#' pre-processing.
#'
#' @examples
#' # Example 1 --------------------------------
#' trial_df <- trial |>
#'   dplyr::select(trt, age, marker, stage)
#'
#' tbl_listing(trial_df, key_cols = c(trt, stage))
#'
#' @export
tbl_listing <- function(df,
                        key_cols = NULL,
                        sort_cols = NULL,
                        add_blank_key_duplicates = TRUE,
                        ...) {
  # Checks -----------------------------------
  check_not_missing(df)
  check_data_frame(df)
  check_scalar_logical(add_blank_key_duplicates)

  row_all_na <- apply(df, 1, function(x) all(is.na(x)))
  if (any(row_all_na)) {
    cli::cli_warn("Rows that only contain NA values have been trimmed.")
    df <- df[!row_all_na, ]
  }

  # Explicit NAs -----------------------------
  df <- df |>
    mutate(across(everything(), ~ replace_na(as.character(.), "NA")))

  # Process arguments ------------------------
  if (is.null(sort_cols)) {
    sort_cols <- rlang::enquo(key_cols)
  }
  cards::process_selectors(df, key_cols = {{ key_cols }}, sort_cols = {{ sort_cols }})

  # Sorting ----------------------------------
  if (!is.null(sort_cols)) {
    sort_miss <- setdiff(sort_cols, names(df))
    if (length(sort_miss) > 0) {
      stop(
        "The following columns were specified as sorting columns (sort_cols) but are missing from df: ",
        paste0("`", sort_miss, "`", collapse = ", ")
      )
    }
    o <- do.call(order, df[sort_cols])
    if (is.unsorted(o)) {
      if (interactive()) {
        cli::cli_inform(c(
          paste(
            "Sorting incoming data by",
            if (identical(sort_cols, key_cols)) {
              "key columns"
            } else {
              paste0("column", if (length(sort_cols) > 1) "s", " ", paste0("`", sort_cols, "`", collapse = ", "))
            },
            i = "If you want to change the sorting, please sort the data frame before passing it to `tbl_listing()`."
          )
        ))
      }
      df <- df[o, ]
    }
  }

  # Reorder the full set of cols to ensure key columns are first
  ordercols <- c(key_cols, setdiff(names(df), key_cols))
  df <- df[, ordercols]

  # Add blank rows for key duplicates if requested -----------------------------
  if (add_blank_key_duplicates) {
    # Check if key_cols are unique
    if (any(duplicated(df[key_cols]))) {
      # Create a new data frame with blank rows for duplicates
      blank_str <- ""
      for (kcol in key_cols) {
        kcol_vec <- as.character(df[[kcol]])
        cur_key <- paste0("", kcol_vec) # used to force into a character vector
        disp <- c(TRUE, tail(cur_key, -1) != head(cur_key, -1))
        kcol_vec[!disp] <- blank_str
        df[[kcol]] <- kcol_vec
      }
    }
  }

  # Build the listing ----------------------------------------------------------
  #  (inspired by `gtsummary::tbl_summary()`)
  x <- list() # empty gtsummary object

  # table_body -----------------------------------------------------------------
  x$table_body <- df

  # table_styling --------------------------------------------------------------
  x$table_styling$header <-
    dplyr::tibble(
      column = names(x$table_body),
      hide = FALSE,
      align = "center",
      interpret_label = "gt::md"
    ) |>
    dplyr::mutate(
      label = map_chr(.data$column, ~ attr(df[[.x]], "label") %||% .x),
      align = ifelse(.data$column %in% "label", "left", .data$align)
    )

  x$table_styling$spanning_header <-
    dplyr::tibble(
      level = integer(),
      column = character(),
      spanning_header = character(),
      text_interpret = character(),
      remove = logical()
    )

  x$table_styling$footnote_header <-
    dplyr::tibble(
      column = character(),
      footnote = character(), text_interpret = character(),
      replace = logical(), remove = logical()
    )

  x$table_styling$footnote_body <-
    dplyr::tibble(
      column = character(), rows = list(),
      footnote = character(), text_interpret = character(),
      replace = logical(), remove = logical()
    )

  x$table_styling$footnote_spanning_header <-
    dplyr::tibble(
      column = character(), footnote = character(),
      level = integer(), text_interpret = character(),
      replace = logical(), remove = logical()
    )

  x$table_styling$abbreviation <-
    dplyr::tibble(
      column = character(),
      abbreviation = character(),
      text_interpret = character()
    )

  x$table_styling$source_note <-
    dplyr::tibble(
      id = integer(),
      source_note = character(),
      text_interpret = character(),
      remove = logical()
    )

  x$table_styling$text_format <-
    dplyr::tibble(
      column = character(), rows = list(),
      format_type = character(), undo_text_format = logical()
    )

  x$table_styling$indent <-
    # if there is a label column, make it indent 0 (which makes it easier to modify later)
    if ("label" %in% x$table_styling$header$column) {
      dplyr::tibble(
        column = "label",
        rows = list(rlang::expr(TRUE)),
        n_spaces = 0L
      )
    } else {
      dplyr::tibble(column = character(), rows = list(), n_spaces = integer())
    }

  x$table_styling$fmt_missing <-
    dplyr::tibble(column = character(), rows = list(), symbol = character())
  x$table_styling$fmt_fun <-
    dplyr::tibble(column = character(), rows = list(), fmt_fun = list())
  x$table_styling$cols_merge <-
    dplyr::tibble(column = character(), rows = list(), pattern = character())
  x$table_styling$post_fmt_fun <-
    dplyr::tibble(column = character(), rows = list(), fmt_fun = list())

  # adding other objects to list -----------------------------------------------
  x <- c(x, list(...))

  # return object with class and attributes ------------------------------------
  structure(
    x,
    class = c("tbl_listing", "gtsummary"),
    tbl_args = list(
      key_cols = key_cols,
      sort_cols = sort_cols,
      add_blank_key_duplicates = add_blank_key_duplicates
    )
  )
}
