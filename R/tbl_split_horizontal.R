#' Horizontal Table Splitting
#'
#' Split a gtsummary into parts horizontally (that is, splits by columns in
#' `x$table_body`), and return a list of split tables.
#' Run `gtsummary::show_header_names()` to print all column names to split by.
#'
#' @param x (`gtsummary`)\cr
#'   a gtsummary table
#' @param keys ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to be repeated in each table split.
#' @param groups (list of `character` vectors)\cr
#'   list of column names that appear in `x$table_body`.
#'   Each group of column names represent
#'
#' @returns a gtsummary table
#' @name tbl_split
#'
#' @examples
#' # Example 1 ----------------------------------
#' tbl <- tbl_demographics(cards::ADSL, by = ARM, include = c(AGE, SEX))
#'
#' # print column names
#' gtsummary::show_header_names(tbl)
#'
#' tbl |>
#'   tbl_split_horizontal(
#'     keys = c("label", "stat_1"),
#'     groups = list("stat_2", "stat_3")
#'   )
NULL

#' @export
#' @name tbl_split
tbl_split_horizontal <- function(x, keys = first_column(x), groups) {
  # check inputs ---------------------------------------------------------------
  set_cli_abort_call()
  check_class(x, "gtsummary")
  cards::process_selectors(x$table_body, keys = {{ keys }})
  check_class(groups, "list")
  cards::check_list_elements(
    groups,
    predicate = is.character,
    error_msg = "Each element of the {.arg groups} argument's list must be a {.cls character} vector."
  )

  # check all variables in groups appear in the table
  columns_do_not_exist <-
    unlist(groups) |>
    setdiff(x$table_styling$header$column)
  if (!is_empty(columns_do_not_exist)) {
    cli::cli_abort()
  }

  # check that every un-hidden column is present
  missing_cols <-
    x$table_styling$header[!x$table_styling$header$hide,][["column"]] |>
    setdiff(c(keys, unlist(groups)))
  if (!is_empty(missing_cols)) {
    cli::cli_inform(
      c("The following columns were not listed in either {.arg keys} or {.arg groups} argument: {.val {missing_cols}}",
        "i" = "These columns have been added to the end of {.arg groups}.",
        "*" = "Run {.fun gtsummary::show_header_names} for a list of all column names.")
    )
    groups <- c(groups, list(missing_cols))
  }

  # splitting table ------------------------------------------------------------
  result <- vector(mode = "list", length = length(groups))
  for (i in seq_along(groups)) {
    result[[i]] <- gtsummary::modify_column_hide(x, columns = -all_of(union(keys, groups[[i]])))
  }

  # return list of tbls --------------------------------------------------------
  result |>
    structure(class = c("tbl_split", "list"))
}

#' @export
#' @name tbl_split
first_column <- function(x) {
  set_cli_abort_call()
  check_class(x, "gtsummary")

  x$table_styling$header[!x$table_styling$header$hide,][["column"]][1]
}
