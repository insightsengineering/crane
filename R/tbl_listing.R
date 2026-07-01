#' Create listings from a data frame
#'
#' This function creates a listing from a data frame. Common uses
#' rely on few pre-processing steps, such as ensuring unique values in key columns or split
#' by rows or columns. They are described in the note section.
#'
#' @param data (`data.frame`)\cr
#'   a data frame containing the data to be displayed in the listing.
#' @param split_by_rows,split_by_columns,add_blank_rows (named `list`)\cr
#'   - `split_by_rows`: Named list of arguments that are passed to `gtsummary::tbl_split_by_rows()`.
#'   - `split_by_columns`: Named list of arguments that are passed to `gtsummary::tbl_split_by_columns()`.
#'   - `add_blank_rows`: Named list of arguments that are passed to `crane::add_blank_rows()`.
#'     `add_blank_rows()` is applied after table splitting and applied to each table individually.
#'
#'  _Variable names passed in these named lists must be character vectors; tidyselect/unquoted syntax is not accepted._
#'
#' @param x (`tbl_listing` or `list`)\cr
#'   a `tbl_listing` object or a list of `tbl_listing` objects.
#' @param keys ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to highlight for duplicate values. If `NULL`, nothing is done.
#' @param value (`string`)\cr
#'   string to use for blank values. Defaults to `NA`. It should not be changed.
#' @param spl_col (`string`)\cr
#'   name of the column that the listing was split by (i.e. the `variable_level`
#'   passed to [gtsummary::tbl_split_by_rows()]). Used to build the caption and,
#'   when `hide_spl_col = TRUE`, to hide that column on each page.
#' @param pattern (`string`)\cr
#'   a [glue][glue::glue] pattern used to build each page's caption from the
#'   split level. The `{spl_level}` token is replaced by the split value.
#'   Defaults to `"Parameter: {spl_level}"`, matching parameter-split tables
#'   such as laboratory listings split by `PARAM`.
#' @param hide_spl_col (`flag`)\cr
#'   whether to hide `spl_col` on each split page, since it is redundant after a
#'   single-level split. Defaults to `TRUE`. Silently does nothing when the
#'   column is absent or already hidden.
#'
#' @name tbl_listing
#' @note
#' Common pre-processing steps for the data frame that may be common:
#'  * Unique values - this should be enforced in pre-processing by users.
#'  * `NA` values - they are not printed by default in `{gtsummary}`. You can make them explicit if
#'    they need to be displayed in the listing. See example 3.
#'  * Sorting key columns and moving them to the front. See the examples pre-processing.
#'
#'  ## Splitting the listing
#'  * Split by rows - you can split the data frame by rows by using `split_by_rows` parameter. You can use the same
#'    parameters used in [gtsummary::tbl_split_by_rows()]. See example 4.
#'  * Split by columns - you can split the data frame by columns by using `split_by_columns` parameter. Use the same
#'    parameters from [gtsummary::tbl_split_by_rows()]. See example 5.
#' @returns A table listing of class "tbl_listing".
#' @examplesIf crane:::is_pkg_installed("labelled")
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
#' out |> remove_duplicate_keys(keys = "trt")
#'
#' # Example 2 --------------------------------
#' # make NAs explicit
#' trial_data_na <- trial_data |>
#'   mutate(across(everything(), ~ tidyr::replace_na(labelled::to_character(.), "-")))
#' tbl_listing(trial_data_na)
#'
#' # Example 3 --------------------------------
#' # Add blank rows for first key column
#' lst <- tbl_listing(trial_data_na, add_blank_rows = list(variable_level = "trt"))
#' lst
#'
#' # Can add them also manually in post-processing
#' lst |> add_blank_rows(row_numbers = seq(2))
#'
#' # Example 4 --------------------------------
#' # Split by rows
#' list_lst <- tbl_listing(trial_data, split_by_rows = list(row_numbers = c(2, 3, 4)))
#' list_lst[[2]]
#'
#' # Example 5 --------------------------------
#' # Split by columns
#' show_header_names(lst)
#' grps <- list(c("trt", "stage", "age"), c("trt", "stage", "marker"))
#' list_lst <- tbl_listing(trial_data, split_by_columns = list(groups = grps))
#' list_lst[[2]]
#'
#' # Example 6 --------------------------------
#' # Split by rows and columns
#' list_lst <- tbl_listing(trial_data,
#'   split_by_rows = list(row_numbers = c(2, 3, 4)), split_by_columns = list(groups = grps)
#' )
#' length(list_lst) # 8 tables are flatten out
#' list_lst[[2]]
#'
#' # Example 7 --------------------------------
#' # Hide duplicate columns in post-processing
#' out <- list_lst |>
#'   remove_duplicate_keys(keys = c("trt", "stage"))
#' out[[2]]
#'
#' # Example 8 --------------------------------
#' # Label each split page and hide the redundant split column
#' by_stage <- tbl_listing(trial_data, split_by_rows = list(variable_level = "stage")) |>
#'   modify_split_caption(spl_col = "stage", pattern = "Stage: {spl_level}")
#' by_stage[[1]]
NULL

#' @export
#' @rdname tbl_listing
tbl_listing <- function(data,
                        split_by_rows = list(),
                        split_by_columns = list(),
                        add_blank_rows = list()) {
  set_cli_abort_call()

  # Checks -----------------------------------
  check_not_missing(data)
  check_data_frame(data)

  # Process arguments ------------------------
  tbl_listing_inputs <- as.list(environment())

  # Build the listing ----------------------------------------------------------
  x <- gtsummary::as_gtsummary(data)
  x$inputs <- tbl_listing_inputs

  # add class and attributes ---------------------------------------------------
  x <- structure(
    x,
    class = c("tbl_listing", "gtsummary")
  )

  # Split it if requested ------------------------------------------------------
  if (!is_empty(split_by_rows)) {
    x <- exec(gtsummary::tbl_split_by_rows, x = x, !!!split_by_rows)
  }
  if (!is_empty(split_by_columns)) {
    x <- exec(gtsummary::tbl_split_by_columns, x = x, !!!split_by_columns)
  }

  # Add blank rows, if requested -----------------------------------------------
  if (!is_empty(add_blank_rows)) {
    if (inherits(x, "list")) {
      x <- x |>
        lapply(FUN = \(.x) exec("add_blank_rows", x = .x, !!!add_blank_rows)) |>
        structure(class = class(x))
    } else {
      x <- exec("add_blank_rows", x = x, !!!add_blank_rows)
    }
  }

  # Return the listing ---------------------------------------------------------
  x
}

#' @export
#' @rdname tbl_listing
remove_duplicate_keys <- function(x, keys = NULL, value = NA) {
  if (is.list(x) && inherits(x[[1]], "gtsummary")) {
    return(map(x, remove_duplicate_keys, keys = {{ keys }}, value = value))
  }

  # Checks -----------------------------------
  cards::process_selectors(x$table_body, keys = {{ keys }})
  check_scalar(value)

  # Create a new data frame with blank values for duplicates
  for (kcol in keys) {
    # Check if keys are unique
    if (anyDuplicated(x$table_body[kcol]) > 0L) {
      tmp_label_attr <- attr(x$table_body[[kcol]], "label") # not losing the label attribute
      kcol_vec <- as.character(x$table_body[[kcol]])
      attr(kcol_vec, "label") <- tmp_label_attr
      cur_key <- paste0("", kcol_vec) # used to force into a character vector
      disp <- c(TRUE, utils::tail(cur_key, -1) != utils::head(cur_key, -1))
      kcol_vec[!disp] <- value
      x$table_body[[kcol]] <- kcol_vec
    }
  }

  x
}

#' @export
#' @rdname tbl_listing
modify_split_caption <- function(x,
                                 spl_col,
                                 pattern = "Parameter: {spl_level}",
                                 hide_spl_col = TRUE) {
  set_cli_abort_call()

  # map over a list of split tables --------------------------------------------
  if (is.list(x) && inherits(x[[1]], "gtsummary")) {
    return(
      map(
        x,
        modify_split_caption,
        spl_col = spl_col,
        pattern = pattern,
        hide_spl_col = hide_spl_col
      ) |>
        structure(class = class(x))
    )
  }

  # checks ---------------------------------------------------------------------
  check_not_missing(x)
  check_class(x, "gtsummary")
  check_not_missing(spl_col)
  check_string(spl_col)
  check_string(pattern)
  check_scalar_logical(hide_spl_col)

  # build the caption from the split level -------------------------------------
  # `variable_level` is the gtsummary-native attribute set by
  # tbl_split_by_rows(variable_level = ); row-number splits do not set it, so
  # those pages are skipped silently.
  spl_level <- attr(x, "variable_level")
  if (!is_empty(spl_level)) {
    caption <- glue::glue(pattern)
    x <- gtsummary::modify_caption(x, as.character(caption))
    # keep the attribute in sync so decorators that read it show the same text
    attr(x, "variable_level") <- as.character(caption)
  }

  # hide the split column, if present and not already hidden --------------------
  if (isTRUE(hide_spl_col) && spl_col %in% x$table_styling$header$column) {
    x <- gtsummary::modify_column_hide(x, columns = all_of(spl_col))
  }

  x
}
